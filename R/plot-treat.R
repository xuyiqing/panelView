.pv_plot_treat <- function(s) {
    with(s, {


        if (is.null(xlab)==TRUE) {
            xlab <- index[2]
        } else if (xlab == "") {
            xlab <- NULL
        }
        if (is.null(ylab)==TRUE) {
            ylab <- index[1]
            if (collapse.history == TRUE) {
                ylab <- "Number of Units"
            }
        } else if (ylab == "") {
            ylab <- NULL
        }

        if (is.null(main)==TRUE) {
            if (collapse.history == TRUE) {
                main <- "Unique Treatment Histories"
            } else {
                if (ignore.treat == 0) {
                    main <- "Treatment Status"
                } else {
                    main <- "Missing Values"
                }
            }
        } else if (main == "") {
            main <- NULL
        }

        ## cat(N)

        units <- rep(rev(1:N), each = TT)
        period <- rep(1:TT, N)


        ## replicate data
        m <- as.matrix(obs.missing[show,])
        all <- unique(na.omit(c(m)))

        col <- breaks <- label <- NULL 

        ## set breaks, colors and labels
        
        if (d.bi == FALSE && ignore.treat == 0) { ## >2 treatment level

            tr.col <- c("#5B8AA6","#C66B5A","#6B7A99","#B07AA1","#BE8C3F","#7A8C5A",
                "#4A6FA5","#A6573F","#506683","#8F5B7A","#A07733","#5E6E45")

            if (treat.type == "discrete") {
                for (i in 1:n.levels) {
                    breaks <- c(breaks, d.levels[i])
                    #label <- c(label, paste("Treatment level: ", d.levels[i], sep = ""))
                    label <- c(label, paste(d.levels[i], sep = ""))
                }
                col <- tr.col[1:n.levels]                

            } else {
                cat("Continuous treatment.\n")
                #col <- c("#87CEEB", "#00008B")
                interval <- (max(d.levels)-min(d.levels))/4
                m[m >= min(d.levels) & m < min(d.levels)+interval] <- min(d.levels)
                m[m >= min(d.levels)+interval & m < min(d.levels)+2*interval] <- min(d.levels)+interval
                m[m >= min(d.levels)+2*interval & m < min(d.levels)+3*interval] <- min(d.levels)+2*interval
                m[m >= min(d.levels)+3*interval & m < min(d.levels)+4*interval] <- min(d.levels)+3*interval
                m[m >= max(d.levels)] <- max(d.levels)
                
                breaks <- c(min(d.levels), min(d.levels)+interval, min(d.levels)+2*interval, min(d.levels)+3*interval, max(d.levels))
                col <- c("#c6dbef","#4292c6", "#1f78b4", "#08519c", "#042b53")
                #label <- "Treatment Levels"
                for (i in 1:length(breaks)) {
                    label <- c(label, paste(breaks[i], sep = ""))
                }
                treat.type <- "discrete"
            }

             # missing values
            if (-200 %in% all) {
                col <- c(col,"#FFFFFF")
                breaks <- c(breaks, -200)
                label <- c(label,"Missing")
            }

        } else { ## binary treatment indicator

            ## theme-dependent binary palette (control / treated-pre / treated-post)
            if (identical(theme, "red")) {
                ## Under Control = dusty pink in the same hue family as the
                ## "#B83A4B" treatment, so the two cohort-window colors
                ## share a story (cohort = warm) while the unused / faded
                ## cells stay grey. Earlier shades of grey collided with
                ## the unused-cell default grey85, making the two control
                ## bands indistinguishable.
                pv.ctl  <- "#E8B5BC"
                pv.tpre <- "grey45"
                pv.tpst <- "#B83A4B"
            } else {
                pv.ctl  <- "#B0C4DE"
                pv.tpre <- "#4671D5"
                pv.tpst <- "#06266F"
            }

            if (0 %in% all) { ## have pre and post: general DID type data

                ## control
                if (-1 %in% all) {
                    col <- c(col, pv.ctl)
                    breaks <- c(breaks, -1)
                    label <- c(label,"Controls")
                }

                ## treated pre
                col <- c(col, pv.tpre)
                breaks <- c(breaks, 0)
                label <- c(label,"Treated (Pre)")

                ## treated post
                if (1 %in% all) {
                    col <- c(col, pv.tpst)
                    breaks <- c(breaks, 1)
                    label <- c(label,"Treated (Post)")
                }

            } else { # do not have pre and post

                ## control
                if (-1 %in% all) {
                    col <- c(col, pv.ctl)
                    breaks <- c(breaks, -1)
                    if (ignore.treat == 0) {
                        ## if (pre.post == TRUE) {
                            label <- c(label,"Under Control")
                        ## } else {
                        ##     label <- c(label,"Control")
                        ## }
                    } else {
                        label <- c(label, "Observed")
                    }

                }

                ## treated
                if (1 %in% all) {
                    col <- c(col, pv.tpst)
                    breaks <- c(breaks, 1)
                    ## if (pre.post == TRUE) {
                        label <- c(label,"Under Treatment")
                    ## } else {
                    ##     label <- c(label,"Treated")
                    ## }
                }

            }

            # missing values
            if (-200 %in% all) {
                col <- c(col,"#FFFFFF")
                breaks <- c(breaks, -200)
                label <- c(label,"Missing")
            }
            
            ## adjust DID: treated units on top
            ## if (length(id) >1 && 1 %in% all && by.treatment == TRUE) {

                ## 1. sort treated
            ##     missing.tr <- which(apply(m == 1, 2, sum) > 0)
            ##     if (length(missing.tr) > 1) {
            ##         tr.count <- TT - apply(m == 1, 2, sum)[missing.tr]
            ##         if (length(unique(tr.count)) > 1) {
            ##             TR <- cbind(missing.tr, tr.count)
            ##             TR <- TR[order(TR[, 2]),]
            ##             missing.tr <- TR[, 1]
            ##         }
            ##     }
                ## 2. check controls
            ##     missing.co <- NULL
            ##     if (length(missing.tr) < N) {
            ##         missing.co <- setdiff(1:N, missing.tr)
            ##     } 
                ## 3. re-order id
            ##     m <- as.matrix(m[,c(missing.tr, missing.co)])
            ##     id <- id[c(missing.tr, missing.co)]
            ## }

            ## sort units 
            if (length(id) > 1 && ignore.treat == 0 && d.bi == TRUE) {

                if (by.timing == TRUE) {
                    co.seq <- which(unit.type == 1) ## unit.type: 1 for control; 2 for treated; 3 for reversal
                    tr.seq <- setdiff(1:N, co.seq)
                    dataT0 <- cbind.data.frame(tr.seq, T0, co.total)
                    names(dataT0) <- c("id", "T0", "co.total")
                    dataT0 <- dataT0[order(dataT0[, "T0"], dataT0[, "co.total"], dataT0[, "id"]),] ## order of by.timing

                    tr.seq <- dataT0[,"id"]
                    missing.seq <- c(tr.seq, co.seq)

                    m <- as.matrix(m[,missing.seq])
                    id <- id[missing.seq]
                    ## Keep the sample column ordering in sync with m:
                    ## by.timing reshuffles units, and the sample matrix was
                    ## originally indexed in raw-id order so it has to follow.
                    if (!is.null(sample)) {
                        sample <- sample[, missing.seq, drop = FALSE]
                    }

                }

            }

        }

        ## user-defined color setting and legend
        ##
        ## color = NULL                   --- theme defaults for used cells +
        ##                                    sample-mode defaults for unused
        ## color = c(...) (unnamed)       --- positional override of the used
        ##                                    palette, length must equal the
        ##                                    active break count
        ## color = c(name = "...", ...)   --- named override. Supported names:
        ##     control          --- Under Control
        ##     treated          --- Under Treatment ( = treated.post)
        ##     treated.pre      --- Treated (Pre), three-state binary only
        ##     missing          --- Missing observations
        ##     unused.control   --- Not used: Under Control
        ##     unused.treated   --- Not used: Under Treatment
        ## Unspecified names keep their theme default.
        user_unused <- NULL   # named character vector for unused-side slots
        if (!is.null(color)) {
            if (treat.type == "discrete") {
                if (!is.null(names(color)) && any(nzchar(names(color)))) {
                    nm <- names(color)
                    used_names   <- c("control", "treated", "treated.pre", "missing")
                    unused_names <- c("unused.control", "unused.treated")
                    bad <- setdiff(nm, c(used_names, unused_names))
                    if (length(bad) > 0L) {
                        stop("Unknown color name(s): ",
                             paste(bad, collapse = ", "),
                             ". Allowed names: ",
                             paste(c(used_names, unused_names), collapse = ", "),
                             ".", call. = FALSE)
                    }
                    used_overrides <- color[intersect(nm, used_names)]
                    user_unused    <- color[intersect(nm, unused_names)]

                    if (length(used_overrides) > 0L) {
                        ## map each label position to a name. Labels carry
                        ## the human strings ("Under Control", etc.); map
                        ## via the underlying breaks (-1, 0, 1, -200).
                        label_to_name <- function(b, lab) {
                            bi <- suppressWarnings(as.integer(as.character(b)))
                            if (!is.na(bi)) {
                                if (bi == -1L) return("control")
                                if (bi == 0L  && grepl("Pre", lab))
                                    return("treated.pre")
                                if (bi == 0L) return("treated")
                                if (bi == 1L) return("treated")
                                if (bi == -200L) return("missing")
                            }
                            NA_character_
                        }
                        slot_names <- mapply(label_to_name, breaks, label,
                                             USE.NAMES = FALSE)
                        hit <- match(names(used_overrides), slot_names)
                        ok  <- !is.na(hit)
                        if (any(ok)) {
                            col[hit[ok]] <- used_overrides[ok]
                            cat("Set used-cell colors for: ",
                                paste(names(used_overrides)[ok],
                                      collapse = ", "), ".\n", sep = "")
                        }
                    }
                } else if (length(col) == length(color)) {
                    cat(paste("Specified colors in the order of: ",
                              paste(label, collapse = ", "), ".\n", sep = ""))
                    col <- color
                } else {
                    stop(paste("Length of \"color\" should be equal to ",
                               length(col),
                               ", or use a named vector with any of: ",
                               "control, treated, treated.pre, missing, ",
                               "unused.control, unused.treated.\n", sep = ""))
                }
            }
        }
        
        if (!is.null(legend.labs)) {
            if (treat.type == "discrete") { ## discrete treatment indicator
                if (length(legend.labs) != length(label)) {
                    warning("Incorrect number of labels in the legends. Using default.\n")
                } else {
                    cat(paste("Specified labels in the order of: ", paste(label, collapse = ", "), ".\n", sep = ""))
                    label <- legend.labs
                }
            } 
            #else {
             #   if (length(legend.labs) != 1) {
              #      warning("The length of label should be equal to 1.\n")
               # } else {
                #    label <- legend.labs
                #}
            #}
        } 

        ## start plot 
        #if (treat.type == "continuous" && ignore.treat == 0 && leave.gap == 1) {
        #m2 <- NULL
        #m2 <- m
        #m2 <- replace(m2, m2 == -200, NA) # if NA in the first and last period, then this period will disappear
        #res <- c(m2)
        #}
        #else{        
        res <- c(m)
        #}

        ## subset sample the same way obs.missing is subset, then flatten
        ## column-major so it lines up with c(m). sample is TRUE for cells
        ## the estimator actually used.
        if (!is.null(sample)) {
            if (!identical(dim(sample), dim(obs.missing))) {
                stop(sprintf(
                    "\"sample\" dimensions (%d x %d) must match the panel (%d x %d).",
                    nrow(sample), ncol(sample),
                    nrow(obs.missing), ncol(obs.missing)
                ))
            }
            sample_m <- as.matrix(sample[show, , drop = FALSE])
        }

        data <- cbind.data.frame(units=units, period=period, res=res)
        if (!is.null(sample)) {
            data$used <- c(sample_m)
        }


        if (leave.gap == 0) {
            data <- na.omit(data)
        }

        ## When a sample matrix is supplied, retag unused cells with a
        ## "u"-prefixed res key so a single fill scale and a single legend
        ## cover both the used palette and the unused palette. Extending
        ## breaks / col / label here keeps scale_fill_manual the source of
        ## truth.
        ##
        ## Missing cells (res = "-200") are NOT retagged --- a missing cell
        ## cannot be "used" by any estimator, so the used/unused distinction
        ## is meaningless. Keep the single "Missing" legend entry.
        ##
        ## Default: control / treated unused share one grey tier, so the
        ## legend has a single "Not used" entry. Naming `unused.control` and
        ## `unused.treated` separately in `color` opts back into the split.
        if (!is.null(sample)) {
            res_chr <- as.character(data$res)
            is_miss <- res_chr == "-200"

            mc <- c("#BFBFBF", "#BFBFBF")
            if (!is.null(user_unused) && length(user_unused) > 0L) {
                if ("unused.control" %in% names(user_unused)) {
                    mc[1] <- user_unused[["unused.control"]]
                }
                if ("unused.treated" %in% names(user_unused)) {
                    mc[2] <- user_unused[["unused.treated"]]
                }
            }
            collapse_unused <- identical(mc[1], mc[2])

            if (collapse_unused) {
                ## single "Not used" tier --- one key, one legend entry.
                data$res <- ifelse(data$used | is_miss, res_chr, "u")
                if ("u" %in% unique(data$res)) {
                    breaks <- c(breaks, "u")
                    col    <- c(col,    mc[1])
                    label  <- c(label,  "Not used")
                }
            } else {
                ## split: one unused entry per status (control / treated).
                data$res <- ifelse(data$used | is_miss,
                                   res_chr, paste0("u", res_chr))
                unused_keys   <- paste0("u", as.character(breaks))
                unused_cols   <- vapply(breaks, function(b) {
                    bi <- suppressWarnings(as.integer(as.character(b)))
                    if (!is.na(bi) && bi >= 0) mc[2] else mc[1]
                }, character(1))
                unused_labels <- paste("Not used:", label)
                drop_miss <- unused_keys == "u-200"
                unused_keys   <- unused_keys[!drop_miss]
                unused_cols   <- unused_cols[!drop_miss]
                unused_labels <- unused_labels[!drop_miss]
                keep <- unused_keys %in% unique(data$res)
                breaks <- c(breaks, unused_keys[keep])
                col    <- c(col,    unused_cols[keep])
                label  <- c(label,  unused_labels[keep])
            }
        }

        data[,"res"] <- as.factor(data[,"res"])
        
        ## check if N >= 200
        if (dim(m)[2] >= 200) {
            if (axis.lab == "both") {
                axis.lab <- "time"
            }
            else if (axis.lab == "unit") {
                axis.lab <- "off"
            }
        }

        ## background color
        if (is.null(background)==FALSE) {
            grid.color <- border.color <- background.color <- legend.color <- background
        } else if (theme.bw == TRUE) {
            ## modern look: white plot/legend background; tile borders stay subtle grey
            grid.color <- border.color <- "grey90"
            background.color <- legend.color <- "white"
        } else {
            grid.color <- border.color <- background.color <- legend.color <- "grey90"
        }


        
        id <- rev(id)
        p <- ggplot(data, aes(x = period, y = units,
                              fill = res))
        

        if (gridOff == FALSE) {
            p <- p + geom_tile(colour=grid.color, linewidth=0.1)
        } else {
            p <- p + geom_tile()
        }

        p <- p + labs(x = xlab, y = ylab, title=main)
        if (theme.bw == TRUE) {
            p <- p + theme_bw(base_size = 11)
        } else {
            p <- p + theme_bw()
        }

        #if (treat.type == "discrete") {
            p <- p + scale_fill_manual("Treatment level: ", breaks = breaks, values = col, labels=label)
            if (n.levels < 3) {
                p <- p + theme(legend.title=element_blank())
            }
        #} else {
            #p <- p + scale_fill_gradient(low = col[1], high = col[2], na.value="white") + guides(fill=guide_legend(title= label))
        #}

        if (theme.bw == TRUE) {
            title.style <- element_text(size=cex.main, hjust = 0, face="plain",
                                        margin = margin(8, 0, 8, 0))
        } else {
            title.style <- element_text(size=cex.main, hjust = 0.5, face="bold",
                                        margin = margin(8, 0, 8, 0))
        }

        p <- p +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(fill=NA,color=border.color, linewidth=0.5, linetype="solid"),
              axis.line = element_blank(),
              axis.ticks = element_blank(),
              axis.title=element_text(size=cex.lab),
              axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
              axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)),
              axis.text = element_text(color="black", size=cex.axis),
              axis.text.x = element_text(size = cex.axis.x, angle = angle, hjust=x.h, vjust=x.v),
              axis.text.y = element_text(size = cex.axis.y),
              plot.background = element_rect(fill = background.color),
              legend.background = element_rect(fill = legend.color),
              legend.position = legend.pos,
              legend.margin = margin(0, 5, 5, 0),
              legend.text = element_text(margin = margin(r = 10, unit = "pt"), size = cex.legend),
              plot.title = title.style)
                      

        if (axis.lab == "both") {
            p <- p + scale_x_continuous(expand = c(0, 0), breaks = T.b, labels = time.label[T.b]) +
            scale_y_continuous(expand = c(0, 0), breaks = N.b, labels = id[N.b])
        }
        else if (axis.lab == "unit") {
            p <- p + scale_x_continuous(expand = c(0, 0), breaks = T.b, labels = NULL) +
            scale_y_continuous(expand = c(0, 0), breaks = N.b, labels = id[N.b])            
        }
        else if (axis.lab == "time") {
            p <- p + scale_x_continuous(expand = c(0, 0), breaks = T.b, labels = time.label[T.b]) +
            scale_y_continuous(expand = c(0, 0), breaks = N.b, labels = NULL)
        }
        else if (axis.lab == "off") {
            p <- p + scale_x_continuous(expand = c(0, 0), breaks = 1:length(show), labels = NULL) +
            scale_y_continuous(expand = c(0, 0), breaks = 1:N, labels = NULL)
        }
        
        if (length(all) >= 4 && length(all) < 6) {
            p <- p + guides(fill=guide_legend(nrow=2,byrow=TRUE))
        }
        suppressWarnings(print(p))
        ## end of missing plot




    })
}

