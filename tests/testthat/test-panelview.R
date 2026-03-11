library(panelView)
data(panelView)

## Helper: run panelview() suppressing the plot device output
pv <- function(...) {
    pdf(NULL)
    on.exit(dev.off())
    panelview(...)
}

# -----------------------------------------------------------------------
# type = "treat"
# -----------------------------------------------------------------------

test_that("type=treat runs with turnout (binary treatment)", {
    expect_no_error(pv(turnout ~ policy_edr + policy_mail_in + policy_motor,
                       data = turnout, index = c("abb", "year")))
})

test_that("type=treat runs with capacity (multi-level treatment)", {
    expect_no_error(pv(Capacity ~ demo + lnpop + lngdp,
                       data = capacity, index = c("ccode", "year")))
})

test_that("type=treat: by.timing = TRUE", {
    expect_no_error(pv(turnout ~ policy_edr + policy_mail_in + policy_motor,
                       data = turnout, index = c("abb", "year"),
                       by.timing = TRUE))
})

test_that("type=treat: pre.post = TRUE", {
    expect_no_error(pv(turnout ~ policy_edr + policy_mail_in + policy_motor,
                       data = turnout, index = c("abb", "year"),
                       pre.post = TRUE))
})

test_that("type=treat: collapse.history = TRUE", {
    expect_no_error(pv(turnout ~ policy_edr + policy_mail_in + policy_motor,
                       data = turnout, index = c("abb", "year"),
                       collapse.history = TRUE))
})

test_that("type=treat: D= interface works", {
    expect_no_error(pv(D = "policy_edr", data = turnout,
                       index = c("abb", "year")))
})

test_that("type=treat: show.id subsets units", {
    expect_no_error(pv(Capacity ~ demo + lnpop + lngdp,
                       data = capacity, index = c("ccode", "year"),
                       show.id = c(1:25)))
})

test_that("type=treat: leave.gap = TRUE with uneven time", {
    turnout2 <- turnout[!(turnout$year %in% c("1924", "1928", "1940")), ]
    expect_no_error(pv(turnout ~ policy_edr + policy_mail_in + policy_motor,
                       data = turnout2, index = c("abb", "year"),
                       leave.gap = TRUE))
})

test_that("type=treat: continuous treatment (polity2)", {
    expect_no_error(pv(Capacity ~ polity2 + lngdp,
                       data = capacity, index = c("ccode", "year"),
                       gridOff = TRUE))
})

# -----------------------------------------------------------------------
# type = "missing" / "miss" (backward-compatible alias)
# -----------------------------------------------------------------------

test_that("type=missing runs on capacity", {
    expect_no_error(pv(Capacity ~ 1, data = capacity,
                       index = c("ccode", "year"), type = "missing"))
})

test_that("type=miss alias works (backward compat)", {
    expect_no_error(pv(Capacity ~ 1, data = capacity,
                       index = c("ccode", "year"), type = "miss"))
})

test_that("type=missing: Y= interface works", {
    expect_no_error(pv(Y = "Capacity", data = capacity,
                       index = c("ccode", "year"), type = "missing"))
})

# -----------------------------------------------------------------------
# type = "outcome"
# -----------------------------------------------------------------------

test_that("type=outcome runs with turnout (continuous)", {
    expect_no_error(pv(turnout ~ policy_edr + policy_mail_in + policy_motor,
                       data = turnout, index = c("abb", "year"),
                       type = "outcome"))
})

test_that("type=outcome: pre.post = FALSE", {
    expect_no_error(pv(turnout ~ policy_edr + policy_mail_in + policy_motor,
                       data = turnout, index = c("abb", "year"),
                       type = "outcome", pre.post = FALSE))
})

test_that("type=outcome: by.group = TRUE", {
    expect_no_error(pv(turnout ~ policy_edr + policy_mail_in + policy_motor,
                       data = turnout, index = c("abb", "year"),
                       type = "outcome", by.group = TRUE))
})

test_that("type=outcome: by.group.side = TRUE", {
    expect_no_error(pv(turnout ~ policy_edr + policy_mail_in + policy_motor,
                       data = turnout, index = c("abb", "year"),
                       type = "outcome", by.group.side = TRUE))
})

test_that("type=outcome: by.cohort = TRUE", {
    expect_no_error(pv(turnout ~ policy_edr + policy_mail_in + policy_motor,
                       data = turnout, index = c("abb", "year"),
                       type = "outcome", by.cohort = TRUE))
})

test_that("type=outcome: ignore.treat (formula Y ~ 1)", {
    expect_no_error(pv(turnout ~ 1, data = turnout,
                       index = c("abb", "year"), type = "outcome"))
})

test_that("type=outcome: discrete outcome", {
    expect_no_error(pv(Y ~ D, data = simdata, index = c("id", "time"),
                       type = "outcome", outcome.type = "discrete"))
})

test_that("type=outcome: discrete outcome by.group = TRUE", {
    expect_no_error(pv(Y ~ D, data = simdata, index = c("id", "time"),
                       type = "outcome", outcome.type = "discrete",
                       by.group = TRUE))
})

test_that("type=outcome: multi-level treatment (polity2, >5 levels)", {
    expect_no_error(pv(Capacity ~ polity2 + lngdp,
                       data = capacity, index = c("ccode", "year"),
                       type = "outcome", legendOff = TRUE))
})

test_that("type=outcome: id= subsets units", {
    expect_no_error(pv(turnout ~ policy_edr + policy_mail_in + policy_motor,
                       data = turnout, index = c("abb", "year"),
                       type = "outcome", id = c("AL", "AR", "CT")))
})

test_that("type=raw alias works (backward compat for outcome)", {
    expect_no_error(pv(turnout ~ policy_edr + policy_mail_in + policy_motor,
                       data = turnout, index = c("abb", "year"),
                       type = "raw"))
})

# -----------------------------------------------------------------------
# type = "bivariate" / "bivar" (backward-compatible alias)
# -----------------------------------------------------------------------

test_that("type=bivariate: continuous Y, discrete D, aggregate", {
    expect_no_error(pv(turnout ~ policy_edr + policy_mail_in + policy_motor,
                       data = turnout, index = c("abb", "year"),
                       type = "bivariate", style = c("c", "b")))
})

test_that("type=bivariate: continuous Y, continuous D, aggregate", {
    expect_no_error(pv(lnpop ~ polity2, data = capacity,
                       index = c("country", "year"), type = "bivariate"))
})

test_that("type=bivariate: discrete Y, discrete D, aggregate", {
    expect_no_error(pv(Y ~ D, data = simdata, index = c("id", "time"),
                       type = "bivariate", outcome.type = "discrete"))
})

test_that("type=bivar alias works (backward compat)", {
    expect_no_error(pv(lnpop ~ demo, data = capacity,
                       index = c("country", "year"), type = "bivar"))
})

test_that("type=bivariate: by.unit = TRUE, continuous Y discrete D", {
    expect_no_error(pv(turnout ~ policy_edr,
                       data = turnout, index = c("abb", "year"),
                       type = "bivariate", by.unit = TRUE,
                       show.id = c(1:12)))
})

test_that("type=bivariate: by.unit = TRUE, continuous Y continuous D", {
    expect_no_error(pv(lnpop ~ polity2, data = capacity,
                       index = c("country", "year"),
                       type = "bivariate", by.unit = TRUE,
                       show.id = c(1:12)))
})

test_that("type=bivariate: by.unit = TRUE, discrete Y discrete D", {
    expect_no_error(pv(Y ~ D, data = simdata, index = c("id", "time"),
                       type = "bivariate", by.unit = TRUE,
                       outcome.type = "discrete",
                       id = unique(simdata$id)[1:12]))
})

# -----------------------------------------------------------------------
# Input validation — errors
# -----------------------------------------------------------------------

test_that("invalid type raises error", {
    expect_error(pv(turnout ~ policy_edr, data = turnout,
                    index = c("abb", "year"), type = "invalid"),
                 regexp = "should be one of|arg")
})

test_that("missing Y with type=outcome raises error", {
    expect_error(pv(data = turnout, index = c("abb", "year"),
                    type = "outcome"))
})

test_that("invalid leave.gap raises error", {
    expect_error(pv(turnout ~ policy_edr, data = turnout,
                    index = c("abb", "year"), leave.gap = "yes"),
                 regexp = "leave.gap")
})

test_that("invalid axis.lab.angle raises error", {
    expect_error(pv(turnout ~ policy_edr, data = turnout,
                    index = c("abb", "year"), axis.lab.angle = 120),
                 regexp = "axis.lab.angle")
})

test_that("type=missing combined with ignore.treat raises error", {
    expect_error(pv(Capacity ~ demo, data = capacity,
                    index = c("ccode", "year"),
                    type = "missing", ignore.treat = TRUE),
                 regexp = "missing")
})

test_that("by.cohort outside type=outcome raises error", {
    expect_error(pv(turnout ~ policy_edr, data = turnout,
                    index = c("abb", "year"),
                    by.cohort = TRUE, type = "treat"),
                 regexp = "by.cohort")
})
