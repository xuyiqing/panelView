# Tests for type = "network" (k-partite graph visualization)
# Backward-compatible aliases: "graph" and "singleton" both map to "network"

library(panelView)

## Helper: run panelview() suppressing the plot device output
pv <- function(...) {
    pdf(NULL)
    on.exit(dev.off())
    panelview(...)
}

## Helper: get singleton values for a given FE dimension
get_singletons <- function(result, fe_name) {
    s <- result$singletons
    if (nrow(s) == 0) return(character(0))
    s[[fe_name]][s$singleton_fe == fe_name]
}

# -----------------------------------------------------------------------
# 2.1 Basic Functionality — Balanced Panel
# -----------------------------------------------------------------------

test_that("network type works with balanced panel", {
    skip_if_not_installed("igraph")
    data(turnout, package = "panelView")
    result <- pv(turnout, ~1, index = c("abb", "year"), type = "network")

    expect_true(is.list(result))
    expect_equal(result$n_components, 1)
    expect_length(get_singletons(result, "abb"), 0)
    expect_length(get_singletons(result, "year"), 0)
    expect_true(inherits(result$graph, "igraph"))
    expect_true(inherits(result$plot, "gg") || inherits(result$plot, "ggplot"))
    expect_equal(igraph::vcount(result$graph), 47 + 24)
    expect_equal(igraph::ecount(result$graph), 47 * 24)
})

# -----------------------------------------------------------------------
# 2.1b Backward-compatible alias: "graph"
# -----------------------------------------------------------------------

test_that("graph alias works", {
    skip_if_not_installed("igraph")
    data(turnout, package = "panelView")
    result <- pv(turnout, ~1, index = c("abb", "year"), type = "graph")

    expect_true(is.list(result))
    expect_true(inherits(result$graph, "igraph"))
    expect_equal(igraph::vcount(result$graph), 47 + 24)
})

# -----------------------------------------------------------------------
# 2.2 Unbalanced Panel with Singletons
# -----------------------------------------------------------------------

test_that("network type identifies singletons", {
    skip_if_not_installed("igraph")
    df <- data.frame(
        unit = c("A", "A", "A", "B", "B", "C"),
        time = c(1, 2, 3, 1, 2, 4)
    )
    result <- pv(df, ~1, index = c("unit", "time"), type = "network")

    expect_equal(igraph::vcount(result$graph), 7)
    expect_equal(igraph::ecount(result$graph), 6)
    expect_true("C" %in% get_singletons(result, "unit"))
    expect_true(3 %in% get_singletons(result, "time"))
    expect_true(4 %in% get_singletons(result, "time"))
    expect_equal(result$n_components, 2)
})

# -----------------------------------------------------------------------
# 2.3 Disconnected Components
# -----------------------------------------------------------------------

test_that("network type detects disconnected components", {
    skip_if_not_installed("igraph")
    df <- data.frame(
        unit = c("A", "A", "B", "B", "C", "C", "D", "D"),
        time = c(1, 2, 1, 2, 3, 4, 3, 4)
    )
    result <- pv(df, ~1, index = c("unit", "time"), type = "network")

    expect_equal(result$n_components, 2)
    expect_length(get_singletons(result, "unit"), 0)
    expect_length(get_singletons(result, "time"), 0)
    expect_equal(igraph::ecount(result$graph), 8)
})

# -----------------------------------------------------------------------
# 2.4 Type Alias: "singleton"
# -----------------------------------------------------------------------

test_that("singleton alias works", {
    skip_if_not_installed("igraph")
    df <- data.frame(unit = c("A", "A", "B"), time = c(1, 2, 1))
    result <- pv(df, ~1, index = c("unit", "time"), type = "singleton")

    expect_true(inherits(result$graph, "igraph"))
    expect_true(is.list(result))
})

# -----------------------------------------------------------------------
# 2.5 Layout Algorithms
# -----------------------------------------------------------------------

test_that("all layout algorithms work", {
    skip_if_not_installed("igraph")
    data(turnout, package = "panelView")
    for (lay in c("fr", "bipartite", "circle")) {
        result <- pv(turnout, ~1, index = c("abb", "year"),
                     type = "network", layout = lay)
        expect_true(inherits(result$graph, "igraph"),
                    info = paste("layout =", lay))
        expect_true(inherits(result$plot, "gg") || inherits(result$plot, "ggplot"),
                    info = paste("layout =", lay))
    }
})

# -----------------------------------------------------------------------
# 2.6 Invalid Layout
# -----------------------------------------------------------------------

test_that("invalid layout errors", {
    skip_if_not_installed("igraph")
    data(turnout, package = "panelView")
    expect_error(
        pv(turnout, ~1, index = c("abb", "year"),
           type = "network", layout = "invalid"),
        regexp = "'arg' should be one of"
    )
})

# -----------------------------------------------------------------------
# 2.7 show.singletons = FALSE
# -----------------------------------------------------------------------

test_that("show.singletons FALSE works", {
    skip_if_not_installed("igraph")
    df <- data.frame(
        unit = c("A", "A", "B"),
        time = c(1, 2, 1)
    )
    result <- pv(df, ~1, index = c("unit", "time"), type = "network",
                 show.singletons = FALSE)

    expect_true(is.list(result$singletons))
})

# -----------------------------------------------------------------------
# 2.8 highlight.components = FALSE
# -----------------------------------------------------------------------

test_that("highlight.components FALSE works", {
    skip_if_not_installed("igraph")
    df <- data.frame(
        unit = c("A", "A", "C", "C"),
        time = c(1, 2, 3, 4)
    )
    result <- pv(df, ~1, index = c("unit", "time"), type = "network",
                 highlight.components = FALSE)

    expect_equal(result$n_components, 2)
})

# -----------------------------------------------------------------------
# 2.9 show.labels Options
# -----------------------------------------------------------------------

test_that("show.labels options work", {
    skip_if_not_installed("igraph")
    df <- data.frame(unit = c("A", "A", "B"), time = c(1, 2, 1))
    for (opt in c("auto", "all", "singletons", "none")) {
        expect_no_error(
            pv(df, ~1, index = c("unit", "time"), type = "network",
               show.labels = opt),
            message = paste("show.labels =", opt)
        )
    }
})

# -----------------------------------------------------------------------
# 2.10 Custom node.size
# -----------------------------------------------------------------------

test_that("custom node.size works", {
    skip_if_not_installed("igraph")
    df <- data.frame(unit = c("A", "A", "B"), time = c(1, 2, 1))
    result <- pv(df, ~1, index = c("unit", "time"), type = "network",
                 node.size = 5)
    expect_true(is.list(result))
})

# -----------------------------------------------------------------------
# 2.11 Invalid node.size
# -----------------------------------------------------------------------

test_that("invalid node.size errors", {
    skip_if_not_installed("igraph")
    df <- data.frame(unit = c("A", "A", "B"), time = c(1, 2, 1))
    expect_error(
        pv(df, ~1, index = c("unit", "time"), type = "network", node.size = -1),
        regexp = "node.size"
    )
})

# -----------------------------------------------------------------------
# 3.1 Single Unit, Single Time Period
# -----------------------------------------------------------------------

test_that("single unit single time works", {
    skip_if_not_installed("igraph")
    df <- data.frame(unit = "A", time = 1)
    result <- pv(df, ~1, index = c("unit", "time"), type = "network")

    expect_equal(igraph::vcount(result$graph), 2)
    expect_equal(igraph::ecount(result$graph), 1)
    expect_equal(result$n_components, 1)
    expect_length(get_singletons(result, "unit"), 1)
    expect_length(get_singletons(result, "time"), 1)
})

# -----------------------------------------------------------------------
# 3.2 All Units Are Singletons
# -----------------------------------------------------------------------

test_that("all singletons case works", {
    skip_if_not_installed("igraph")
    df <- data.frame(
        unit = c("A", "B", "C"),
        time = c(1, 2, 3)
    )
    result <- pv(df, ~1, index = c("unit", "time"), type = "network")

    expect_length(get_singletons(result, "unit"), 3)
    expect_length(get_singletons(result, "time"), 3)
    expect_equal(result$n_components, 3)
})

# -----------------------------------------------------------------------
# 3.5 Numeric Unit and Time IDs
# -----------------------------------------------------------------------

test_that("numeric IDs work", {
    skip_if_not_installed("igraph")
    df <- data.frame(unit = c(1, 1, 2, 2), time = c(2001, 2002, 2001, 2002))
    result <- pv(df, ~1, index = c("unit", "time"), type = "network")

    expect_true(inherits(result$graph, "igraph"))
    expect_equal(igraph::vcount(result$graph), 4) # 2 units + 2 times
    expect_equal(igraph::ecount(result$graph), 4) # 4 observations
})

# -----------------------------------------------------------------------
# 3.6 Factor Unit IDs
# -----------------------------------------------------------------------

test_that("factor IDs work", {
    skip_if_not_installed("igraph")
    df <- data.frame(
        unit = factor(c("X", "X", "Y", "Y")),
        time = c(1, 2, 1, 2)
    )
    result <- pv(df, ~1, index = c("unit", "time"), type = "network")

    expect_true(inherits(result$graph, "igraph"))
})

# -----------------------------------------------------------------------
# 3.7 Formula with Variables (Not ~1)
# -----------------------------------------------------------------------

test_that("formula with variables works for network", {
    skip_if_not_installed("igraph")
    data(turnout, package = "panelView")
    result <- pv(turnout, turnout ~ policy_edr,
                 index = c("abb", "year"), type = "network")

    expect_true(inherits(result$graph, "igraph"))
    ## Same structure as ~1 — outcome and treatment ignored for network type
    expect_equal(igraph::vcount(result$graph), 47 + 24)
})

# -----------------------------------------------------------------------
# 4.1–4.4 Regression: Existing Types Unaffected
# -----------------------------------------------------------------------

test_that("existing treat type unaffected", {
    data(turnout, package = "panelView")
    expect_no_error(
        pv(turnout, turnout ~ policy_edr, index = c("abb", "year"),
           type = "treat")
    )
})

test_that("existing missing type unaffected", {
    data(turnout, package = "panelView")
    expect_no_error(
        pv(turnout, turnout ~ 1, index = c("abb", "year"),
           type = "missing")
    )
})

test_that("existing outcome type unaffected", {
    data(turnout, package = "panelView")
    expect_no_error(
        pv(turnout, turnout ~ policy_edr, index = c("abb", "year"),
           type = "outcome")
    )
})

test_that("existing bivariate type unaffected", {
    data(turnout, package = "panelView")
    expect_no_error(
        pv(turnout, turnout ~ policy_edr, index = c("abb", "year"),
           type = "bivariate")
    )
})

# -----------------------------------------------------------------------
# 5.1 Graph Structure Invariants (k = 2)
# -----------------------------------------------------------------------

test_that("graph structure invariants hold", {
    skip_if_not_installed("igraph")

    panels <- list(
        data.frame(unit = c("A", "A", "B", "B"),
                   time = c(1, 2, 1, 2)),
        data.frame(unit = c("A", "A", "A", "B", "B", "C"),
                   time = c(1, 2, 3, 1, 2, 4)),
        data.frame(unit = c("X", "Y", "Z"),
                   time = c(10, 20, 30))
    )

    for (i in seq_along(panels)) {
        df <- panels[[i]]
        result <- pv(df, ~1, index = c("unit", "time"), type = "network")
        g <- result$graph

        n_units <- length(unique(df$unit))
        n_times <- length(unique(df$time))

        expect_equal(igraph::vcount(g), n_units + n_times,
                     info = paste("panel", i, "vcount"))
        expect_equal(igraph::ecount(g), nrow(df),
                     info = paste("panel", i, "ecount"))
    }
})

# -----------------------------------------------------------------------
# 5.2 Singleton Identification Is Correct
# -----------------------------------------------------------------------

test_that("singleton identification is correct", {
    skip_if_not_installed("igraph")

    df <- data.frame(
        unit = c("A", "A", "A", "B", "B", "C", "D"),
        time = c(1, 2, 3, 1, 2, 4, 5)
    )
    result <- pv(df, ~1, index = c("unit", "time"), type = "network")

    ## Compute expected singletons from the data
    unit_counts <- table(df$unit)
    time_counts <- table(df$time)

    expected_unit_singletons <- names(unit_counts[unit_counts == 1])
    expected_time_singletons <- as.numeric(names(time_counts[time_counts == 1]))

    ## C and D appear once each
    expect_true(setequal(get_singletons(result, "unit"), expected_unit_singletons))
    ## Times 3, 4, 5 appear once each
    expect_true(setequal(get_singletons(result, "time"), expected_time_singletons))
})

# -----------------------------------------------------------------------
# 6.1 k = 3: Three-way Fixed Effects
# -----------------------------------------------------------------------

test_that("k = 3 tripartite graph works", {
    skip_if_not_installed("igraph")
    df <- data.frame(
        worker = c("A", "A", "B", "B"),
        firm   = c(1, 1, 2, 2),
        year   = c(2020, 2021, 2020, 2021)
    )
    result <- pv(df, ~1, index = c("worker", "firm", "year"), type = "network",
                 show.labels = "all")

    g <- result$graph
    ## 2 workers + 2 firms + 2 years = 6 nodes
    expect_equal(igraph::vcount(g), 6)
    ## Each of 4 observations creates 3 edges (w-f, w-y, f-y)
    ## But some are duplicates that get aggregated:
    ## w-f: A-1, A-1, B-2, B-2 → 2 unique edges (weight 2 each)
    ## w-y: A-2020, A-2021, B-2020, B-2021 → 4 unique edges
    ## f-y: 1-2020, 1-2021, 2-2020, 2-2021 → 4 unique edges
    ## Total unique edges = 2 + 4 + 4 = 10
    expect_equal(igraph::ecount(g), 10)
    expect_equal(result$n_components, 1)

    ## Check singletons is a data frame with FE columns
    expect_true(is.data.frame(result$singletons))
    expect_true("worker" %in% names(result$singletons))
    expect_true("firm" %in% names(result$singletons))

    ## Check multi-edges exist (A-1 and B-2 have weight 2)
    expect_true(nrow(result$multi_edges) > 0)
})

# -----------------------------------------------------------------------
# 6.2 k = 3: Disconnected Components
# -----------------------------------------------------------------------

test_that("k = 3 disconnected components", {
    skip_if_not_installed("igraph")
    df <- data.frame(
        worker = c("A", "B"),
        firm   = c(1, 2),
        year   = c(2020, 2021)
    )
    result <- pv(df, ~1, index = c("worker", "firm", "year"), type = "network")

    ## 2 workers + 2 firms + 2 years = 6 nodes
    expect_equal(igraph::vcount(result$graph), 6)
    ## Each observation creates 3 edges, no overlaps: 2 * 3 = 6
    expect_equal(igraph::ecount(result$graph), 6)
    ## Two isolated triangles
    expect_equal(result$n_components, 2)
})

# -----------------------------------------------------------------------
# 6.3 Duplicate Observations (Multi-edges)
# -----------------------------------------------------------------------

test_that("duplicate observations produce weighted edges", {
    skip_if_not_installed("igraph")
    df <- data.frame(
        w = c("A", "A", "B", "B", "B"),
        f = c(1, 1, 1, 2, 2)
    )
    result <- pv(df, ~1, index = c("w", "f"), type = "network",
                 show.labels = "all")

    g <- result$graph
    ## 2 workers + 2 firms = 4 nodes
    expect_equal(igraph::vcount(g), 4)
    ## Unique edges: A-1 (weight 2), B-1 (weight 1), B-2 (weight 2) = 3
    expect_equal(igraph::ecount(g), 3)

    ## Check multi-edges reported
    expect_true(nrow(result$multi_edges) >= 1)
    ## A-1 should have weight 2
    weights <- igraph::E(g)$weight
    expect_true(max(weights) == 2)
})

# -----------------------------------------------------------------------
# 6.4 Network index validation
# -----------------------------------------------------------------------

test_that("network type rejects invalid index", {
    skip_if_not_installed("igraph")
    df <- data.frame(a = 1, b = 2, c = 3)
    expect_error(
        pv(df, ~1, index = c("a", "nonexistent"), type = "network"),
        regexp = "index"
    )
    ## Single index should also fail
    expect_error(
        pv(df, ~1, index = c("a"), type = "network"),
        regexp = "index"
    )
})
