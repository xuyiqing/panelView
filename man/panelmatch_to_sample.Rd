\name{panelmatch_to_sample}
\alias{panelmatch_to_sample}
\title{Convert a PanelMatch object to a panelView sample mask}
\description{
Walks a \code{PanelMatch} object's matched sets and returns a logical
\eqn{T \times N} matrix marking the panel cells used in estimation,
suitable for passing as \code{sample=} to \code{\link{panelview}}. For
each treated event, the treated unit and each matched control
(positive weight) are marked \code{TRUE} over
\eqn{[t-\code{lag},\,t+\max(\code{lead})]} --- the lag window covers
the matching support and the lead window covers the outcome leads
PanelMatch uses on the LHS, so within each event the light-blue
pre-period and dark-blue post-onset cells render as one contiguous
band. \code{panelview()} also accepts a \code{PanelMatch} object
directly via \code{sample=}; in that case this helper is called
internally with default arguments.
}
\usage{
panelmatch_to_sample(pm, pd, qoi = NULL,
                     include = c("treated", "controls"),
                     weight.threshold = 0)
}
\arguments{
  \item{pm}{A \code{PanelMatch} object (CRAN package \pkg{PanelMatch}).}
  \item{pd}{The \code{PanelData} object passed to \code{PanelMatch()},
    or any \code{data.frame} carrying the same \code{unit.id} and
    \code{time.id} columns. Used to define the panel axes (sorted
    unique units / times).}
  \item{qoi}{One of \code{"att"}, \code{"atc"}. If \code{NULL} (default)
    and \code{pm} carries a single matched-set list, that one is used;
    if both are present \code{"att"} is preferred.}
  \item{include}{Character vector. Any of \code{"treated"} (the treated
    unit) and \code{"controls"} (matched controls). Default both.}
  \item{weight.threshold}{Numeric. Controls with weight strictly
    greater than this are marked. Default \code{0} keeps any positive
    weight.}
}
\value{
A logical matrix with rows = sorted unique times (rownames are
character), columns = sorted unique units (colnames are character).
\code{TRUE} = cell appears in at least one matched set's full window
under the chosen \code{include} / \code{weight.threshold} rules.
}
\examples{
\dontrun{
library(PanelMatch)
data(dem)
pd <- PanelData(panel.data = dem, unit.id = "wbcode2",
                time.id = "year", treatment = "dem", outcome = "y")
pm <- PanelMatch(panel.data = pd, lag = 4,
                 refinement.method = "mahalanobis",
                 covs.formula = ~ I(lag(tradewb, 1:4)),
                 qoi = "att", lead = 0:3, match.missing = TRUE)
mask <- panelmatch_to_sample(pm, pd)
panelview(pd, formula = y ~ dem, index = c("wbcode2", "year"),
          type = "treat", sample = mask)

## convenience: panelview() accepts the PanelMatch object directly
panelview(pd, formula = y ~ dem, index = c("wbcode2", "year"),
          type = "treat", sample = pm)
}
}
\seealso{
\code{\link{panelview}}
}
\author{
Yiqing Xu
}
