## Network (k-partite) graph visualization for panel data
## Part of panelView package
## Requires igraph (Suggests dependency)
##
## Supports k >= 2 fixed-effect dimensions.
## For k = 2 this is equivalent to a bipartite graph.
## Duplicate observations produce weighted (thicker) edges.

.pv_build_graph <- function(data, index_names) {
    k <- length(index_names)

    ## create node list — one set per FE dimension
    all_nodes <- list()
    for (i in seq_along(index_names)) {
        vals <- unique(data[[index_names[i]]])
        all_nodes[[i]] <- data.frame(
            name = paste0(index_names[i], ":", vals),
            label = as.character(vals),
            node_type = index_names[i],
            stringsAsFactors = FALSE
        )
    }
    vertices_df <- do.call(rbind, all_nodes)

    ## create edge list — for each observation, connect all pairs of FE levels
    edge_list <- list()
    for (i in 1:(k - 1)) {
        for (j in (i + 1):k) {
            from <- paste0(index_names[i], ":", data[[index_names[i]]])
            to   <- paste0(index_names[j], ":", data[[index_names[j]]])
            edge_list[[length(edge_list) + 1]] <- data.frame(
                from = from, to = to, stringsAsFactors = FALSE
            )
        }
    }
    edges_df <- do.call(rbind, edge_list)

    ## aggregate duplicate edges into weights
    if (nrow(edges_df) == 0) {
        ## no edges: return graph with isolated nodes
        g <- igraph::make_empty_graph(directed = FALSE)
        g <- igraph::add_vertices(g, nrow(vertices_df),
                                  name = vertices_df$name,
                                  label = vertices_df$label,
                                  node_type = vertices_df$node_type)
    } else {
        edges_agg <- aggregate(
            list(weight = rep(1, nrow(edges_df))),
            by = list(from = edges_df$from, to = edges_df$to),
            FUN = sum
        )

        g <- igraph::graph_from_data_frame(edges_agg, directed = FALSE,
                                           vertices = vertices_df)
        igraph::E(g)$weight <- edges_agg$weight
    }

    ## vertex attributes
    igraph::V(g)$degree <- igraph::degree(g)
    igraph::V(g)$is_singleton <- (igraph::V(g)$degree == 1)

    ## connected components
    comp <- igraph::components(g)
    igraph::V(g)$component <- comp$membership

    g
}


.pv_layout_graph <- function(g, layout_type, k) {
    if (layout_type == "bipartite" && k > 2) {
        ## bipartite layout not meaningful for k > 2; fall back to fr
        layout_type <- "fr"
    }
    coords <- switch(layout_type,
        "fr"        = igraph::layout_with_fr(g),
        "bipartite" = {
            ## For bipartite layout, set the 'type' attribute
            ## First FE dimension = FALSE, second = TRUE
            node_types <- igraph::V(g)$node_type
            unique_types <- unique(node_types)
            igraph::V(g)$type <- (node_types == unique_types[2])
            igraph::layout_as_bipartite(g)
        },
        "circle"    = igraph::layout_in_circle(g)
    )

    node_df <- data.frame(
        x = coords[, 1],
        y = coords[, 2],
        name = igraph::V(g)$name,
        node_type = igraph::V(g)$node_type,
        label = igraph::V(g)$label,
        degree = igraph::V(g)$degree,
        is_singleton = igraph::V(g)$is_singleton,
        component = igraph::V(g)$component,
        stringsAsFactors = FALSE
    )

    node_df
}


.pv_plot_graph <- function(s) {
    ## suppress R CMD check notes for non-standard evaluation
    x <- y <- xend <- yend <- node_type <- label <- component <- NULL
    grp <- NULL

    ## extract parameters
    data        <- s$data
    index_names <- s$index_names
    k           <- length(index_names)
    show.singletons     <- s$show.singletons
    highlight.components <- s$highlight.components
    layout_type <- s$layout
    node.size   <- s$node.size
    show.labels <- s$show.labels
    edge.color  <- s$edge.color
    main        <- s$main
    cex.main    <- s$cex.main
    cex.lab     <- s$cex.lab
    cex.legend  <- s$cex.legend
    theme.bw    <- s$theme.bw
    legendOff   <- s$legendOff

    ## count nodes per FE dimension for info
    n_per_fe <- vapply(index_names, function(nm) length(unique(data[[nm]])),
                       integer(1))
    N_total <- sum(n_per_fe)

    ## large panel warning
    n_obs <- nrow(data)
    n_edge_pairs <- k * (k - 1) / 2
    est_edges <- n_obs * n_edge_pairs
    if (est_edges > 5000) {
        message("Graph has ~", est_edges, " edges (", N_total, " nodes across ",
                k, " FE dimensions). Consider a subset for clearer visualization.\n")
    }

    ## build graph
    g <- .pv_build_graph(data, index_names)

    ## compute layout
    node_df <- .pv_layout_graph(g, layout_type, k)

    ## build edge data.frame with weights
    el <- igraph::as_edgelist(g)
    if (nrow(el) > 0) {
        edge_weights <- igraph::E(g)$weight
        if (is.null(edge_weights)) edge_weights <- rep(1, nrow(el))
        from_idx <- match(el[, 1], node_df$name)
        to_idx   <- match(el[, 2], node_df$name)
        edge_df <- data.frame(
            x    = node_df$x[from_idx],
            y    = node_df$y[from_idx],
            xend = node_df$x[to_idx],
            yend = node_df$y[to_idx],
            weight = edge_weights
        )
    } else {
        edge_df <- data.frame(x = numeric(0), y = numeric(0),
                              xend = numeric(0), yend = numeric(0),
                              weight = numeric(0))
    }

    ## ---- color scheme ----
    ## palette for k FE dimensions
    fe_palette <- c("#2E86AB", "#E8630A", "#7FB069", "#8B5CF6", "#F59E0B",
                    "#06B6D4", "#EC4899", "#D7263D")
    ## user-supplied color overrides
    color <- s$color
    if (!is.null(color) && length(color) >= k) {
        fe_colors <- color[seq_len(k)]
    } else {
        if (k <= length(fe_palette)) {
            fe_colors <- fe_palette[seq_len(k)]
        } else {
            fe_colors <- rep(fe_palette, length.out = k)
        }
    }
    names(fe_colors) <- index_names

    col_singleton <- if (!is.null(s$singleton.color)) s$singleton.color else "#D7263D"
    col_edge <- if (!is.null(edge.color) && edge.color != "gray70") {
        edge.color
    } else {
        "#CCCCCC"
    }

    ## shapes for k FE dimensions: circle, square, triangle-up, diamond, triangle-down
    fe_shapes <- c(21, 22, 24, 23, 25)
    if (k <= length(fe_shapes)) {
        shape_vals <- fe_shapes[seq_len(k)]
    } else {
        shape_vals <- rep(fe_shapes, length.out = k)
    }
    names(shape_vals) <- index_names

    ## adaptive edge styling: fewer edges → darker and thicker
    ## user can override with edge.alpha and edge.width parameters
    n_total_edges <- nrow(edge_df)
    auto_alpha <- if (n_total_edges > 5000) 0.6
                  else if (n_total_edges > 2000) 0.6
                  else if (n_total_edges > 500) 0.55
                  else if (n_total_edges > 100) 0.6
                  else if (n_total_edges > 30) 0.75
                  else 0.85
    auto_lw <- if (n_total_edges > 5000) 0.25
               else if (n_total_edges > 2000) 0.25
               else if (n_total_edges > 100) 0.35
               else 0.5
    ## user overrides
    edge_alpha <- if (!is.null(s$edge.alpha)) s$edge.alpha else auto_alpha
    edge_lw    <- if (!is.null(s$edge.width)) s$edge.width else auto_lw
    ## darker edge color for sparse graphs
    if (col_edge == "#CCCCCC") {
        if (n_total_edges <= 100) col_edge <- "#888888"
        else if (n_total_edges <= 500) col_edge <- "#AAAAAA"
        else col_edge <- "#BBBBBB"
    }

    ## start ggplot
    p <- ggplot()

    ## component hulls
    comp_info <- igraph::components(g)
    n_comp <- comp_info$no

    if (isTRUE(highlight.components) && n_comp > 1) {
        hull_colors <- c("#2E86AB", "#E8630A", "#7FB069", "#D7263D",
                         "#8B5CF6", "#F59E0B", "#06B6D4", "#EC4899")
        if (n_comp > length(hull_colors)) {
            hull_colors <- rep(hull_colors, length.out = n_comp)
        }

        for (ci in seq_len(n_comp)) {
            comp_nodes <- node_df[node_df$component == ci, , drop = FALSE]
            if (nrow(comp_nodes) >= 3) {
                hull_idx <- grDevices::chull(comp_nodes$x, comp_nodes$y)
                cx <- mean(comp_nodes$x[hull_idx])
                cy <- mean(comp_nodes$y[hull_idx])
                pad <- 0.08
                hx <- comp_nodes$x[hull_idx] + pad * (comp_nodes$x[hull_idx] - cx)
                hy <- comp_nodes$y[hull_idx] + pad * (comp_nodes$y[hull_idx] - cy)
                hull_data <- data.frame(x = hx, y = hy, grp = ci)
                p <- p + geom_polygon(data = hull_data,
                                      aes(x = x, y = y, group = grp),
                                      alpha = 0.06,
                                      fill = hull_colors[ci],
                                      color = hull_colors[ci],
                                      linewidth = 0.3,
                                      linetype = "dashed",
                                      show.legend = FALSE)
            }
        }
    }

    ## edges — linewidth scaled by sqrt(weight) for multi-edges
    if (nrow(edge_df) > 0) {
        max_weight <- max(edge_df$weight)
        if (max_weight > 1) {
            ## scale linewidth by weight
            edge_df$lw <- edge_lw * sqrt(edge_df$weight / max_weight) * 2
            ## also modulate alpha slightly for heavy edges
            edge_df$alpha <- pmin(edge_alpha * sqrt(edge_df$weight), 1)
            for (i in seq_len(nrow(edge_df))) {
                p <- p + geom_segment(
                    data = edge_df[i, , drop = FALSE],
                    aes(x = x, y = y, xend = xend, yend = yend),
                    color = col_edge,
                    linewidth = edge_df$lw[i],
                    alpha = edge_df$alpha[i]
                )
            }
        } else {
            ## uniform linewidth
            p <- p + geom_segment(data = edge_df,
                                  aes(x = x, y = y, xend = xend, yend = yend),
                                  color = col_edge, linewidth = edge_lw,
                                  alpha = edge_alpha)
        }
    }

    ## split nodes by singleton status
    singleton_df     <- node_df[node_df$is_singleton == TRUE, , drop = FALSE]
    non_singleton_df <- node_df[node_df$is_singleton == FALSE, , drop = FALSE]

    ## non-singleton nodes — filled shapes with white stroke for separation
    if (nrow(non_singleton_df) > 0) {
        p <- p + geom_point(data = non_singleton_df,
                            aes(x = x, y = y, shape = node_type,
                                fill = node_type),
                            size = node.size, color = "white",
                            stroke = 0.4, show.legend = TRUE)
    }

    ## singleton nodes
    if (nrow(singleton_df) > 0) {
        if (isTRUE(show.singletons)) {
            ## glow ring behind singleton
            p <- p + geom_point(data = singleton_df,
                                aes(x = x, y = y),
                                size = node.size + 3,
                                color = col_singleton, alpha = 0.25,
                                shape = 16, show.legend = FALSE)
            ## singleton node itself
            p <- p + geom_point(data = singleton_df,
                                aes(x = x, y = y, shape = node_type,
                                    fill = node_type),
                                size = node.size, color = col_singleton,
                                stroke = 0.8, show.legend = FALSE)
        } else {
            ## draw as regular nodes
            p <- p + geom_point(data = singleton_df,
                                aes(x = x, y = y, shape = node_type,
                                    fill = node_type),
                                size = node.size, color = "white",
                                stroke = 0.4, show.legend = FALSE)
        }
    }

    ## shape + fill scales — use actual FE column names as legend labels
    fe_labels <- setNames(index_names, index_names)
    p <- p + scale_shape_manual(
        values = shape_vals,
        labels = fe_labels,
        name = NULL
    )
    p <- p + scale_fill_manual(
        values = fe_colors,
        labels = fe_labels,
        name = NULL
    )

    ## labels
    do_labels <- FALSE
    label_df <- NULL
    if (show.labels == "auto") {
        if (N_total <= 50) do_labels <- TRUE
        label_df <- node_df
    } else if (show.labels == "all") {
        do_labels <- TRUE
        label_df <- node_df
    } else if (show.labels == "singletons") {
        do_labels <- TRUE
        label_df <- node_df[node_df$is_singleton == TRUE, , drop = FALSE]
    }
    ## "none" → do_labels stays FALSE

    if (do_labels && !is.null(label_df) && nrow(label_df) > 0) {
        y_range <- diff(range(node_df$y))
        nudge <- if (y_range > 0) 0.03 * y_range else 0.1
        p <- p + geom_text(data = label_df,
                           aes(x = x, y = y, label = label),
                           size = cex.lab / 4, nudge_y = nudge,
                           check_overlap = TRUE, show.legend = FALSE,
                           color = "#333333",
                           family = "sans")
    }

    ## title
    if (is.null(main)) {
        if (k == 2) {
            main <- "Panel Structure: Bipartite Graph"
        } else {
            main <- paste0("Panel Structure: ", k, "-partite Graph")
        }
    } else if (main == "") {
        main <- NULL
    }

    ## clean minimal theme
    p <- p + theme(
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.border     = element_blank(),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        axis.title       = element_blank(),
        panel.grid       = element_blank(),
        plot.margin      = margin(10, 10, 10, 10)
    )

    ## title
    if (!is.null(main)) {
        p <- p + ggtitle(main) +
            theme(plot.title = element_text(size = cex.main, hjust = 0.5,
                                            color = "#333333"))
    }

    ## legend
    if (isTRUE(legendOff) || legendOff == 1) {
        p <- p + theme(legend.position = "none")
    } else {
        p <- p + guides(fill = guide_legend(override.aes = list(size = 3.5)),
                        shape = guide_legend(override.aes = list(size = 3.5)))
        p <- p + theme(legend.position = "bottom",
                       legend.text = element_text(size = cex.legend,
                                                  color = "#333333"),
                       legend.background = element_blank(),
                       legend.key = element_blank())
    }

    ## print plot
    suppressWarnings(print(p))

    ## build return list
    deg <- igraph::degree(g)
    comp <- igraph::components(g)

    ## singletons — one row per singleton node, with all FE columns filled
    ## A singleton node has degree 1: it appears in exactly one observation.
    ## Each row shows the singleton's value and its connected FE levels.
    singleton_rows <- list()
    for (i in seq_along(index_names)) {
        nm <- index_names[i]
        vals <- unique(data[[nm]])
        node_names <- paste0(nm, ":", vals)
        node_deg <- deg[node_names]
        s_vals <- vals[node_deg == 1]
        for (sv in s_vals) {
            ## find the one row where this singleton appears
            match_rows <- data[data[[nm]] == sv, index_names, drop = FALSE]
            row <- match_rows[1, , drop = FALSE]  # degree 1 → exactly 1 unique combo
            row$singleton_fe <- nm
            singleton_rows[[length(singleton_rows) + 1]] <- row
        }
    }
    if (length(singleton_rows) > 0) {
        singletons <- do.call(rbind, singleton_rows)
        rownames(singletons) <- NULL
    } else {
        singletons <- data.frame(matrix(ncol = length(index_names) + 1, nrow = 0))
        colnames(singletons) <- c(index_names, "singleton_fe")
    }

    ## multi-edges (count > 1) — return with FE columns parsed from node names
    edge_weights <- igraph::E(g)$weight
    if (!is.null(edge_weights)) {
        multi_mask <- edge_weights > 1
        if (any(multi_mask)) {
            mel <- igraph::as_edgelist(g)[multi_mask, , drop = FALSE]
            ## parse "fe_name:value" back to separate columns
            parse_node <- function(node_str) {
                pos <- regexpr(":", node_str, fixed = TRUE)
                list(fe = substr(node_str, 1, pos - 1),
                     val = substr(node_str, pos + 1, nchar(node_str)))
            }
            from_parsed <- parse_node(mel[, 1])
            to_parsed   <- parse_node(mel[, 2])
            me_list <- list()
            me_list[[from_parsed$fe[1]]] <- from_parsed$val
            me_list[[to_parsed$fe[1]]]   <- to_parsed$val
            me_list[["count"]] <- edge_weights[multi_mask]
            multi_edges <- as.data.frame(me_list, stringsAsFactors = FALSE)
        } else {
            multi_edges <- data.frame(count = numeric(0),
                                      stringsAsFactors = FALSE)
        }
    } else {
        multi_edges <- data.frame(count = numeric(0),
                                  stringsAsFactors = FALSE)
    }

    out <- list(
        graph = g,
        singletons = singletons,
        multi_edges = multi_edges,
        components = comp$membership,
        n_components = comp$no,
        plot = p
    )

    return(invisible(out))
}
