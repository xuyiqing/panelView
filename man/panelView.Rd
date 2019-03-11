\name{panelView}
\alias{panelView}
\title{Panel Data Visualizations}
\description{Visualizes panel data}
\usage{panelView(data, formula = NULL, Y = NULL, D = NULL,
            X = NULL, index, na.rm = TRUE, 
            ignore.treat = FALSE, outcome.type = "continuous", 
            type = "treat", by.group = FALSE, 
            by.timing = FALSE, theme.bw = FALSE,
            xlim = NULL, ylim = NULL, 
            xlab = NULL, ylab = NULL, 
            gridOff = FALSE, legendOff = FALSE, 
            legend.labs = NULL, main = NULL,
            pre.post = FALSE, id = NULL, show.id = NULL,
            color = NULL, axis.adjust = FALSE, axis.lab = "both", 
            axis.lab.gap = c(0, 0), shade.post = TRUE,
            cex.main = 15, cex.main.sub = 12, cex.axis = 8,
            cex.lab = 12, cex.legend = 12, background = NULL)
}
\arguments{
  \item{data}{a data frame. The panel does not have to be balanced.}
  \item{formula}{an object of class "formula": a symbolic description of the model to be fitted. The first variable on the right-hand-side is designated as the treatment indicator if \code{ignore.treat = FALSE}. If there is not any covariates, the formula should be like \code{Y~1}, where \code{Y} is the outcome variable.}
  \item{Y}{variable name of the outcome. Ignored if \code{formula} is provided.}
  \item{D}{variable name of the treatment. Ignored if \code{formula} is provided.}
  \item{X}{variable name of the time-varying covariates. Ignored if \code{formula} is provided.}
  \item{index}{a two-element string vector specifying the unit (group) and time indicators. Must be of length 2.}
  \item{na.rm}{a logical flag indicating whether to list-wise delete missing data. The algorithm will report an error if missing data exist when \code{na.rm = FALSE}.}
  \item{ignore.treat}{a logical flag indicating whether there is a treatment variable. Default value is \code{ignore.treat = FALSE}.}
  \item{outcome.type}{a string that specifies the type of outcome variable. Must be either \code{"continuous"}(default) or \code{"discrete"}. For a continuous variable, time series lines for specified units will be plotted, and for discrete response, jitter-ed points at each time period will be plotted.}
  \item{type}{a string that specifies the type of the plot. Must be either \code{"treat"} (default), which plots the treatment/missing-data status of each unit at each time point) or \code{"outcome"}, which plots the raw outcome data}
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
  \item{cex.axis}{a numeric value (pt) specifying the fontsize of the texts on the axes.}
  \item{cex.lab}{a numeric value (pt) specifying the fontsize of the axis titles.}
  \item{cex.legend}{a numeric value (pt) specifying the fontsize of the legend.}
  \item{background}{a character specifying the background color.}
}

\details{
  panelView visualizes the treatment status, missing values, and raw outcome data of a time-series cross-sectional dataset.

  For more details, see \url{http://yiqingxu.org/software/panelView/panelView.html}.
}

\author{
  Licheng Liu <liulch.16@sem.tsinghua.edu.cn>

  Yiqing Xu <yiqingxu@ucsd.edu>
}


\examples{
library(panelView)
data(panelView)
panelView(turnout ~ policy_edr + policy_mail_in + policy_motor, 
          data = turnout, index = c("abb","year"))
}




