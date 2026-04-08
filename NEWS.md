# panelView 1.3.0

## New features

* Add `type = "network"` to visualize the connectivity structure of multi-way
  fixed effects as a k-partite graph (Correia 2016). The new plot identifies
  singletons, connected components, and non-unique (duplicate) observations via
  weighted edges, and supports two or more fixed-effect dimensions as well as
  formula-based missingness.
* New parameters for the network plot: `show.singletons`,
  `highlight.components`, `layout`, `node.size`, `show.labels`, `edge.color`,
  `edge.alpha`, `edge.width`, and `singleton.color`.
* Add `igraph` to `Suggests` (used only by `type = "network"`).
* Add a new tutorial chapter (chapter 4) documenting the network
  visualization, and expand the Quarto tutorial index and references.

## Improvements

* Update tutorial chapter 1 and use consistent `U`-prefix naming for simulated
  units in chapter 4.
* Update `DESCRIPTION` to describe the new k-partite graph functionality.

# panelView 1.2.1

## Bug fixes

* Remove spurious runtime warnings: fix deprecated `group_by_all()` by
  switching to `group_by(across(everything()))`; drop unused
  `position = "identity"` and `stat = "identity"` in ggplot2 layers.
* Fix a `margin()` vector-argument warning and a false-positive `by.cohort`
  warning.
* Address ggplot2 `size` deprecation and tighten `class()` comparisons.
* Fix a bivariate plot title bug.

## Internal

* Refactor monolithic `panelView.R` into focused plot files (one file per plot
  type).
* Add a testthat suite (36 tests).
* Replace the legacy vignette with a Quarto-based manual and add a changelog
  chapter.
* Add `ARCHITECTURE.md` with Mermaid diagrams documenting the package
  structure.
* `R CMD check --as-cran`: 0 errors, 0 warnings, 0 notes.

# panelView 1.1.17

Add links to the [JSS paper](https://doi.org/10.18637/jss.v107.i07).

# panelView 1.1.16

* Add `collapse.history` to allow users to collapse units by treatment history
  in a `treat` plot.
* Add `show.missing` to output missing-data summary statistics for the key
  variables.
* Add `axis.lab.angle` to allow users to change the angle of the x-axis
  labels.
* Allow `pre.post` to be applied to an `outcome` plot.
* Change the color scheme in the `outcome` plot.
* Miscellaneous bug fixes.

# panelView 1.1.11

* Add `by.cohort` to plot average outcome trajectories of units sharing the
  same treatment history (when the number of unique histories is under 20).

# panelView 1.1.10

* Add `by.group.side` to arrange subfigures of `by.group = TRUE` in a row
  rather than a column.
* Add `display.all` to show all units when the number of units exceeds 500
  (otherwise a random sample of 500 units is shown).

# panelView 1.1.8

* Add `leave.gap` to keep gaps in time using white bars when the time variable
  is unevenly spaced (e.g., due to missing data).
* Add `type = "missing"` to plot missingness in data.

# panelView 1.1.7

* Rename the main function from `panelView` to `panelview` for consistency
  with the Stata version.
* In outcome plots, use a dot to represent the last-period observation of a
  unit that is treated in the last period.

# panelView 1.1.6

* Plot time series of outcome and treatment in one graph
  (`type = "bivar"`). By default, plots mean D and Y against time in the same
  graph; use `by.unit = TRUE` to plot each unit. New `style`, `ylim`, and
  `lwd` options control line/bar styles, axis limits, and line width.

# panelView 1.1.4

* Add `treat.type` to control whether the treatment variable is treated as
  continuous (`"continuous"`) or discrete (`"discrete"`).

# panelView 1.1.2

* Rename plot `type`s: `"treat"` (formerly `"missing"`) for treatment status
  and `"outcome"` (formerly `"raw"`) for raw outcomes.
* Allow more than two treatment levels.
* Add `pre.post` to distinguish pre- and post-treatment observations for
  treated units in a DID setting.
* Rename `by.treatment` to `by.timing` and `treatment` to `ignore.treat`.
* Add fontsize options.

# panelView 1.0.5

Fix typos. CRAN release.

# panelView 1.0.4

* Allow plotting treated units on top of control units in the "missing" plot.
* Streamline the `color` option for both the "missing" and "raw" plots.

# panelView 1.0.3

* Allow changing the color of bricks in the "missing" plot.
* Allow leaving the treatment blank in both the "missing" and "raw" plots.
