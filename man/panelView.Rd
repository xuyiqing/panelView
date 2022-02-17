\name{panelview}
\alias{panelview}
\title{Panel data visualization}
\description{Visualizes missing values, treatment and outcome variables, and their relationships in panel data}
\usage{panelview(data, formula = NULL, Y = NULL, D = NULL,
            X = NULL, index,
            ignore.treat = FALSE, type = "treat",
            outcome.type = "continuous",
            treat.type = NULL, by.group = FALSE,
            by.timing = FALSE, theme.bw = TRUE,
            xlim = NULL, ylim = NULL,
            xlab = NULL, ylab = NULL,
            gridOff = FALSE, legendOff = FALSE,
            legend.labs = NULL, main = NULL,
            pre.post = FALSE, id = NULL, show.id = NULL,
            color = NULL, axis.adjust = FALSE, axis.lab = "both",
            axis.lab.gap = c(0, 0), shade.post = TRUE,
            cex.main = 15, cex.main.sub = 12, cex.axis = 8,
            cex.axis.x = NULL, cex.axis.y = NULL,
            cex.lab = 12, cex.legend = 12, background = NULL,
            style = NULL, by.unit = FALSE, lwd = 0.2, leave.gap = FALSE)
}
\arguments{
  \item{data}{a data frame. The panel does not have to be balanced.}
  \item{formula}{an object of class "formula": a symbolic description of the model to be fitted. The first variable on the right-hand-side is designated as the treatment indicator if \code{ignore.treat = FALSE}. If there is not any covariates, the formula should be like \code{Y~1}, where \code{Y} is the outcome variable.}
  \item{Y}{variable name of the outcome. Ignored if \code{formula} is provided.}
  \item{D}{variable name of the treatment. Ignored if \code{formula} is provided.}
  \item{X}{variable name of the time-varying covariates. Ignored if \code{formula} is provided.}
  \item{index}{a two-element string vector specifying the unit (group) and time indicators. Must be of length 2.}
  \item{ignore.treat}{a logical flag indicating whether there is a treatment variable. Default value is \code{ignore.treat = FALSE}.}
  \item{type}{a string that specifies the type of the plot. Must be either \code{"treat"} (default), which plots the treatment status of each unit at each time point, \code{"missing"}, which plots the missing-data, \code{"outcome"}, which plots the raw outcome data, or \code{"bivariate"}, which plots time series of outcome and treatment in one graph.}
  \item{outcome.type}{a string that specifies the type of outcome variable. Must be either \code{"continuous"}(default) or \code{"discrete"}. For a continuous variable, time series lines for specified units will be plotted, and for discrete response, jitter-ed points at each time period will be plotted.}
  \item{treat.type}{a string that specifies the type of treatment variable. Must be either \code{"continuous"} or \code{"discrete"}. The default is NULL, which means the option will be decided based on the number of unique treatment values: if the number if bigger than 10, it will be set as "continuous"; otherwise, it will be set as "discrete".}
  \item{by.group}{a logic flag indicating whether the data should be plotted in separate groups based on treatment status changes for the outcome plot. }
  \item{by.timing}{a logic flag indicating whether the units should be sorted based on the timing of receiving the treatment for the treat plot.}
  \item{theme.bw}{a logical flag specifying whether to use a black-and-white theme.}
  \item{xlim}{a two-element numeric vector specifying the range of x-axis. When the class of time variable is string, must specify the range of strings to be shown, e.g. \code{xlim=c(1,30)}.}
  \item{ylim}{a two-element numeric vector specifying the range of y-axis.}
  \item{xlab}{a string indicating the label of the x-axis.}
  \item{ylab}{a string indicating the label of the y-axis.}
  \item{gridOff}{a logical flag controlling whether to show the grid lines on the treat plot..}
  \item{legendOff}{a logical flag controlling whether to show the legend.}
  \item{legend.labs}{a vector specifying the legend labels. Ignored when \code{legendOff=TRUE}.}
  \item{main}{a string that controls the title of the plot.}
  \item{pre.post}{a logical flag indicating whether to distinguish control status of treated units from that of control units. Only used for DID-type data in the treat plot.}
  \item{id}{a vector specifying units to be shown in the plot. Useful when the number of units is very large.}
  \item{show.id}{a numeric vector or sequence specifying the sorted order of units to be shown in the \code{"treat"} plot. Useful when the number of units is very large. Ignored if \code{!is.null("id")}.}
  \item{color}{a string vector specifying color setting for the plot.}
  \item{axis.adjust}{a logic flag indicating whether to adjust labels on the x-axis.  Useful when the class of time variable is string and there are many time periods.}
  \item{axis.lab}{a string indicating whether labels on the x- and y-axis will be shown. There are four options: \code{"both"} (default): labels on both axes will be shown; \code{"unit"}: only labels on y-axis will be shown; \code{"time"}: only labels on the x-axis will be shown; "none": no labels will be shown.}
  \item{axis.lab.gap}{a numeric vector setting the gaps between labels on the x- or y-axis for the plot. Default is \code{axis.lab.gap = c(0, 0)}, which means that all labels will be shown. Useful for datasets with large N or T.}
  \item{shade.post}{a logical flag controlling whether to shade the post-treatment periods. Ignored if \code{type = "treat"} or no treatment variable is supplied.}
  \item{cex.main}{a numeric value (pt) specifying the fontsize of the main title.}
  \item{cex.main.sub}{a numeric value (pt) specifying the fontsize of the subtitles. Ignored if \code{type = "treat"} or \code{by.group = FALSE.}}
  \item{cex.axis}{a numeric value (pt) specifying the fontsize of the texts on the axes; overwritten by \code{cex.axis.x} or \code{cex.axis.y}.}
  \item{cex.axis.x}{a numeric value (pt) specifying the fontsize of the texts on the x-axis.}
  \item{cex.axis.y}{a numeric value (pt) specifying the fontsize of the texts on the y-axis.}
  \item{cex.lab}{a numeric value (pt) specifying the fontsize of the axis titles.}
  \item{cex.legend}{a numeric value (pt) specifying the fontsize of the legend.}
  \item{background}{a character specifying the background color.}
  \item{by.unit}{a logic flag indicating whether to plot by each specified units or to plot mean D and Y against time in the same graph.}
  \item{style}{a string vector to set line/connected line/bar styles for the outcome and treatment variables.}
  \item{lwd}{a numeric value (pt) specifying the line width when plotting time series of treatment and outcome variables.}
  \item{leave.gap}{a logical flag indicating whether to keep time gaps as  white bars if time is not evenly distributed (possibly due to missing data). Default value is \code{leave.gap = FALSE}.}
}

\details{
  panelview visualizes the treatment status, missing values, and raw outcome data of a time-series cross-sectional dataset.
}

\author{
  Hongyu Mou <hongyumou5@gmail.com>

  Licheng Liu <liulch@mit.edu>

  Yiqing Xu <yiqingxu@stanford.edu>
}


\examples{
library(panelview)
data(panelview)
panelview(turnout ~ policy_edr + policy_mail_in + policy_motor,
          data = turnout, index = c("abb","year"))
}




