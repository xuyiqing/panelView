## Modern theme helper (v1.4.0+).
##
## .pv_modern_title(cex.main, theme.bw): title element_text matching the
##   modern look when theme.bw=TRUE (plain, left-aligned), or the legacy
##   look (bold, centered) when theme.bw=FALSE.
##
## .pv_modern_theme_bw(cex.main, base_size=11): drop-in replacement for
##   `theme_bw()` returning the modern base: theme_bw at base_size, no
##   minor grid, only major-x grid retained.  For trajectory plots only;
##   tile / heatmap plots keep their own theme block (no gridlines).

.pv_modern_title <- function(cex.main, theme.bw) {
    if (isTRUE(theme.bw)) {
        ggplot2::element_text(size = cex.main, hjust = 0, face = "plain",
                              margin = ggplot2::margin(8, 0, 8, 0))
    } else {
        ggplot2::element_text(size = cex.main, hjust = 0.5, face = "bold",
                              margin = ggplot2::margin(8, 0, 8, 0))
    }
}

.pv_modern_theme_bw <- function(base_size = 11) {
    ggplot2::theme_bw(base_size = base_size) +
        ggplot2::theme(panel.grid.minor   = ggplot2::element_blank(),
                       panel.grid.major.y = ggplot2::element_blank(),
                       panel.grid.major.x = ggplot2::element_line(colour = "grey92",
                                                                  linewidth = 0.3))
}

## Suptitle (top grob) for grid.arrange-based by.group / by.group.side
## layouts.  Modern look: plain, left-aligned.  Legacy: bold, centered.
.pv_modern_top_grob <- function(main, cex.main.top, theme.bw) {
    if (isTRUE(theme.bw)) {
        grid::textGrob(main,
                       x = grid::unit(0.02, "npc"), hjust = 0,
                       gp = grid::gpar(fontsize = cex.main.top, font = 1))
    } else {
        grid::textGrob(main,
                       gp = grid::gpar(fontsize = cex.main.top, font = 2))
    }
}
