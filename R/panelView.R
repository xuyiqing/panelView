## A pre-view function for TSCS data
## 2022-08-14

##---------------------------------------------------------------##
## preview of data treatment status, missing values and outcome ##
##---------------------------------------------------------------##

panelview <- function(data, # a data frame (long-form)
                      formula = NULL,
                      Y = NULL,
                      D = NULL,
                      X = NULL,
                      index, # c(unit, time) indicators
                      ignore.treat = FALSE,
                      type = "treat", ## treat, miss(ing), outcome, or bivar(iate), fraction
                      outcome.type = "continuous", # continuous or discrete
                      treat.type = NULL, # discrete or continuous
                      by.group = FALSE, # (color pre-treatment treated differently)
                      by.group.side = FALSE,
                      by.timing = FALSE,
                      theme.bw = TRUE,
                      xlim = NULL, 
                      ylim = NULL,
                      xlab = NULL, 
                      ylab = NULL,
                      gridOff = FALSE,
                      legendOff = FALSE,
                      legend.labs = NULL,
                      main = NULL,
                      pre.post = NULL, # only used for treat & outcome plots
                      id = NULL,
                      show.id = NULL,
                      color = NULL,
                      axis.adjust = FALSE,
                      axis.lab = "both",
                      axis.lab.gap = c(0, 0),
                      axis.lab.angle = NULL,
                      shade.post = FALSE,
                      cex.main = 15,
                      cex.main.sub = 12,
                      cex.axis = 8,
                      cex.axis.x = NULL,
                      cex.axis.y = NULL,
                      cex.lab = 12, 
                      cex.legend = 12,
                      background = NULL, # background color
                      style = NULL, ## bar, connected line, or line
                      by.unit = FALSE,
                      lwd = 0.2,
                      leave.gap = FALSE,
                      display.all = NULL,
                      by.cohort = FALSE,
                      collapse.history = NULL,
                      report.missing = FALSE
                    ) {
        
    ## ------------------------- ##
    ## parse variable.           ##
    ## ------------------------- ##

    if (is.data.frame(data) == FALSE || length(class(data)) > 1) {
        data <- as.data.frame(data)
    }

    if (is.factor(data[,index[1]]) == TRUE) {
        sumcheck <- sum(suppressWarnings(as.numeric(levels(data[,index[1]])))[data[,index[1]]])
        
        if (is.na(sumcheck)) { #units are texts as factor
            data[,index[1]] <- as.character(data[,index[1]])
        }
        else {
            data[,index[1]] <- as.numeric(levels(data[,index[1]]))[data[,index[1]]] #units are numbers as factor
        }
    }



    ## number of units
    N0 <- length(unique(data[, index[1]]))
    if (N0 <= 500) {        
        if (is.null(collapse.history)) {
            collapse.history <- FALSE
        } 
        if (is.null(display.all)) {
            display.all <- FALSE
        }
    } else { # more than 500 units
        if (!is.null(collapse.history)) {
            if (is.null(display.all)) {
                display.all <- FALSE
            }            
        } else { # collapse.history not specified
            if (is.null(display.all)) { # display.all not specified
                if (type != "outcome") { # type != "outcome" sss
                    collapse.history <- TRUE 
                    display.all <- FALSE
                } else {
                    collapse.history <- FALSE
                    display.all <- FALSE
                }
            } else { # display.all specified
                collapse.history <- FALSE
            }
        }
    }
    

    ## normalize type argument (supports partial matching and backward-compatible aliases)
    type <- match.arg(type, c("treat", "missing", "miss", "outcome", "raw", "bivariate", "bivar"))
    if (type == "miss")  type <- "missing"
    if (type == "bivar") type <- "bivariate"
    if (type == "raw")   type <- "outcome"

    ## remove missing values
    if (is.logical(leave.gap) == FALSE & !leave.gap%in%c(0, 1)) {
        stop("\"leave.gap\" is not a logical flag.")
    } 

    if (is.logical(by.cohort) == FALSE & !by.cohort%in%c(0, 1)) {
        stop("\"by.cohort\" is not a logical flag.")
    } 
    
    if (is.logical(display.all) == FALSE & !display.all%in%c(0, 1)) {
        stop("\"display.all\" is not a logical flag.")
    }

    if (is.logical(by.group.side) == FALSE & !by.group.side%in%c(0, 1)) {
        stop("\"by.group.side\" is not a logical flag.")
    }

    if (is.logical(by.unit) == FALSE & !by.unit%in%c(0, 1)) {
        stop("\"by.unit\" is not a logical flag.")
    }

    if (is.logical(axis.adjust) == FALSE & !axis.adjust%in%c(0, 1)) {
        stop("\"axis.adjust\" is not a logical flag.")
    }

    if (is.null(axis.lab.angle) == FALSE) {
        if (is.numeric(axis.lab.angle) == FALSE) {
            stop("\"axis.lab.angle\" must be numeric.")
        } else if (axis.lab.angle < 0 | axis.lab.angle > 90) {
            stop("\"axis.lab.angle\" needs to be in [0, 90].")
        } 
    }
    
    # pre.post
    if (is.null(pre.post) == TRUE) {
        if (type == "outcome") {
            pre.post <- TRUE
        } else  {
            pre.post <- FALSE
        }
    }
    if (is.logical(pre.post) == FALSE & !pre.post%in%c(0, 1)) {
        stop("\"pre.post\" is not a logical flag.")
    }    

    if (is.logical(theme.bw) == FALSE & !theme.bw%in%c(0, 1)) {
        stop("\"theme.bw\" is not a logical flag.")
    }

    if (is.logical(by.timing) == FALSE & !by.timing%in%c(0, 1)) {
        stop("\"by.timing\" is not a logical flag.")
    }

    if (is.logical(by.group) == FALSE & !by.group%in%c(0, 1)) {
        stop("\"by.group\" is not a logical flag.")
    }

    if (is.logical(ignore.treat) == FALSE & !ignore.treat%in%c(0, 1)) {
        stop("\"ignore.treat\" is not a logical flag.")
    }


    if (by.group.side == TRUE) {
        if (by.group == FALSE) {
            by.group <- TRUE
        }
    }

    if (by.group == TRUE) {
        if (is.null(by.cohort)==FALSE) {
            warning("option \"by.cohort\" is not allowed with \"by.group = TRUE\" or \"by.group.side = TRUE\". Ignored.")
        }
    }


    if (type == "missing") {
        if (ignore.treat == 1) {
            stop("option \"type = missing\" should not be combined with \"ignoretreat = TRUE\"")
        }
    }

    if (type != "outcome" & by.cohort == TRUE) {
        stop("option \"by.cohort = TRUE\" should be combined with \"type = \'outcome\'\"")
    }

    if (type == "outcome" & collapse.history == TRUE) { # sss
        stop("option \"collapse.history = TRUE\" should not be combined with \"type = \'outcome\'\"")
    }

            
    if (!is.null(formula)) { # with formula

        if (formula[[1]] != "~") { # no "Y/D/X = var" or "var1 ~ var2"
            stop("need to specify \"Y\"/\"D\"/\"X\" or \"formula\"")
        }

        varnames <- all.vars(formula)
    
        Y <- formula[[2]] # left hand side of the formula

        if (is.numeric(Y) == FALSE) { # Y is a variable
            ## outcome
            Y <- varnames[1]
            ## treatment indicator and covariates
            if (length(varnames) == 1) { ## only y
                D <- X <- NULL
                ignore.treat <- 1

                if (type == "treat") { # Y ~ 1, type(treat)
                    message("\"type = treat\" not allowed. Plot \"type = missing\" instead.\n")
                    type <- "missing"
                }
            } else if (length(varnames) == 2) {
                if (ignore.treat == 0) {
                    D <- varnames[2]
                    X <- NULL
                } else {
                    D <- NULL
                    X <- varnames[2]
                }   
            } else { # length(varnames) > 2
                if (ignore.treat == 0) {
                    D <- varnames[2]
                    X <- varnames[3:length(varnames)]
                } else {
                    D <- NULL
                    X <- varnames[2:length(varnames)]
                }
            }
        }
        else if (is.numeric(Y) == TRUE) { # Y is a number
            ## outcome
            Y <- NULL
            ## treatment indicator and covariates
            if (length(varnames) == 1) { # 1 ~ D/X
                if (ignore.treat == 0) { # 1 ~ D
                    D <- varnames[1]
                    X <- NULL
                } else { # 1 ~ X
                    stop("formula form not allowed")
                }
                # 1 ~ variable, type(miss): not allowed
                if (type == "missing") {
                    stop("formula form not allowed")
                }
            } else if (length(varnames) == 2) { ## 1 ~ D + X
                if (ignore.treat == 0) { # 1 ~ D + X
                    D <- varnames[1]
                    X <- varnames[2]
                } else { # 1 ~ X
                    stop("formula form not allowed")
                }   
            } else { # length(varnames) > 2
                if (ignore.treat == 0) {
                    D <- varnames[1]
                    X <- varnames[2:length(varnames)]
                } else {
                    stop("formula form not allowed")
                }
            }
        }
    
    } else { # no formula
        varnames <- c(Y, D, X)
        if (is.null(D)==TRUE & is.null(X)==TRUE) { # Y="Y", set type = "miss" as default
            if (type == "treat") {
                message("\"type = treat\" not allowed. Plot \"type = missing\" instead.\n")
                type <- "missing"
            }
        }
    }
    

    ## check Incorrect variable names
    for (i in 1:length(varnames)) {
        if(!varnames[i] %in% colnames(data)) {
            stop(paste("Variable \"", varnames[i],"\" is not in the dataset.", sep = ""))
        }
    }

    ## index 
    if (length(index) != 2 | sum(index %in% colnames(data)) != 2) {
        stop("\"index\" option misspecified. Try, for example, index = c(\"unit.id\", \"time\").")
    }
        
    ## index names
    index.id <- index[1]
    index.time <- index[2]


    ## exclude other covariates 
    data <- data[,c(index, Y, D, X)] 

    varV <- nv <- NULL
    if (report.missing == TRUE) {
        ## report missings
        varV <- c(Y, D, X)
        nv <- length(varV)

        ## a nv*2 matrix 
        mis <- matrix(NA, nv, 2)
        for (i in 1:nv) {
            mis[i, 1] <- sum(is.na(data[, varV[i]]))
        }
        mis[, 2] <- round(mis[, 1] / dim(data)[1] * 100, 1) 

        rownames(mis) <- varV
        colnames(mis) <- c("# Missing", "% Missing")

        print(mis)
        cat("\n")

    }


    if (by.cohort == TRUE) {
        leave.gap <- 1
    }

    if (leave.gap == 0) {
        data <- na.omit(data)
    }
    else {
        # if there is a unit that has variable missing across all periods, then we drop this unit
        data$rowmiss <- rowSums(is.na(data))
        data$minrowmiss <- ave(data$rowmiss, list(data[,1]), FUN=min)
        data <- data[!(data$minrowmiss != 0),] #if minrowmiss != 0, drop this unit
        data <- data[1:(ncol(data)-2)]
    }


    #if (na.rm == FALSE & sum(is.na(data)) > 0) {
    #    stop("Missing values in dataset. Try set na.rm = TRUE.\n")
    #}    
    
    # sort data
    data <- data[order(data[,index.id], data[,index.time]), ]


    minmintime <- as.numeric(min(data[, 2], na.rm = TRUE))
    maxmaxtime <- as.numeric(max(data[, 2], na.rm = TRUE))
    timegap <- (maxmaxtime - minmintime)/(length(unique(data[,index.time]))-1)
    inttimegap <- as.integer(timegap)    
    
    data_1 <- transform(data, differencetime = ave(as.numeric(data[, 2]), data[, 1], FUN = function(x) c(NA, diff(x))))
    mintimegap <- min(data_1$differencetime, na.rm = TRUE)
    maxtimegap <- max(data_1$differencetime, na.rm = TRUE)


    if (leave.gap == 0) {
        if (timegap != mintimegap | timegap != inttimegap) {
            message("Time is not evenly distributed (possibly due to missing data).\n")
        }
    }


    if (leave.gap == 1) {
       # expand panel data

        data <- transform(data, differencetime = ave(as.numeric(data[, 2]), data[, 1], FUN = function(x) c(NA, diff(x))))

        mintimegap <- min(data$differencetime, na.rm = TRUE)
        maxtimegap <- max(data$differencetime, na.rm = TRUE)
        divide_differencetime <- maxtimegap / mintimegap

        if (timegap != mintimegap | inttimegap != timegap) {
            #common difference: mintimegap:
            if (mintimegap != maxtimegap & mintimegap != 1 & divide_differencetime == as.integer(divide_differencetime)) {
                #1. Create all combinations of `id` and `year`
                g <- with(data, expand.grid(g.id = unique(data[,index[1]]), 
                        g.time = seq(from = minmintime, to = maxmaxtime, by = mintimegap))) 
                colnames(g)[1] <- colnames(data[1])
                colnames(g)[2] <- colnames(data[2])
                #2. Merge `g` with `data`
                data2 <- merge(g, data, all.x = TRUE)
                data <- data2
                }
        else { #commmon difference = 1 
                #1. Create all combinations of `id` and `year`
                g <- with(data, expand.grid(g.id = unique(data[,index[1]]), 
                        g.time = seq(from = minmintime, to = maxmaxtime))) 
                colnames(g)[1] <- colnames(data[1])
                colnames(g)[2] <- colnames(data[2])
                #2. Merge `g` with `data`
                data2 <- merge(g, data, all.x = TRUE)
                data <- data2
        }
        }
        data <- data[1:(length(data)-1)] #drop the differencetime column
    }



    ## check duplicated observations
    unique_label <- unique(paste(data[,index[1]],"_",data[,index[2]],sep=""))
    if (length(unique_label)!= dim(data)[1]) {
        stop("Unit and time variables do not uniquely identify all observations. Some may be duplicated or Incorrectly marked in the dataset.")
    }

    #if (length(unique(data[,index[1]])) > 1000) {
    #    stop("Please limit your units within 1000 for elegant presentation")
    #}

    if (length(unique(data[,index[1]])) > 300 & gridOff != TRUE & type != "outcome") {
        message("If the number of units is more than 300, we set \"gridOff = TRUE\".\n")
        gridOff <- TRUE
    }
    
    if (length(unique(data[,index[1]])) > 300 & gridOff != TRUE & type == "outcome") {
        gridOff <- TRUE
    }

    if (display.all == FALSE & length(unique(data[,index[1]])) > 500) {
        message("If the number of units is more than 500, we randomly select 500 units to present.
        You can set \"display.all = TRUE\" to show all units.\n")
        set.seed(1346)
        sample_subject_ids = sample(unique(data[,index[1]]), 500)
        data = subset(data, data[,index[1]] %in% sample_subject_ids)
    }

    ##-------------------------------##
    ## Checking Other Parameters
    ##-------------------------------## 

    if (type == "missing") {
        type <- "treat"
        ignore.treat <- 1
    }

    if (by.group == TRUE || type == "outcome") {
        cex.main.top <- cex.main
        cex.main <- cex.main.sub
    }

    if (is.null(cex.axis.x)==TRUE) {
        cex.axis.x <- cex.axis
    }

    if (is.null(cex.axis.y)==TRUE) {
        cex.axis.y <- cex.axis
    }


    ## check plot 
    if (is.null(D)) {
        if (ignore.treat == 0) {
            message("No treatment indicator.\n")
            ignore.treat <- 1
        }
    }

    if (is.null(Y) && (type == "outcome" || type == "bivariate")) {
        stop("No outcomes.\n")
    }

    ## axis.lab
    if (!axis.lab %in% c("both", "unit", "time", "off")) {
        stop("\"axis.lab\" option misspecified. Try, for example, axis.lab = c(\"both\", \"unit\", \"time\", \"off\").") 
    }
    ## time labels gap
    if (sum(axis.lab.gap < 0) > 0) {
        stop("\"gap\" should be equal to or greater than 0.\n")
    }

    ## legend labels
    if (is.null(legend.labs)==FALSE) {
        legend.labs <- as.character(legend.labs)
    }

    ## outcome.type
    if (!outcome.type %in% c("continuous", "discrete")) {
        stop("\"outcome.type\" option misspecified. Try, for example, outcome.type = c(\"continuous\", \"discrete\").")
    }

    ## check treatment indicator
    d.levels <- NULL
    d.bi <- FALSE

    # without ignore.treat:
    if (ignore.treat == 0) {

        if (leave.gap == 0) {
            if (!inherits(data[, D], c("numeric", "integer"))) {
                    ## data[, Dname] <- as.numeric(as.character(data[, Dname]))
                    stop("Treatment indicator should be a numeric value.")
                }
        }

        d.levels <- sort(unique(data[, D]))
        n.levels <- length(d.levels) # n.levels: treatment levels
        d.bi <- d.levels[1] == 0 & d.levels[2] == 1 & n.levels == 2 # d.bi: binary treatment

        if (d.bi == FALSE & by.cohort == TRUE) {
            stop("option \"by.cohort = TRUE\" works only with dummy treatment variable")
        }

        if (outcome.type == "discrete") {
            y.levels <- sort(unique(data[, Y]))
        }

        if (n.levels == 1) {
            message("Only one treatment level...\n")
            ignore.treat <- 1
        } else {
            if (d.bi == FALSE) {
                cat(paste0(n.levels, " treatment levels.\n"))
            }
        }

        if (is.null(treat.type)== FALSE) {
            if (!treat.type %in% c("discrete","continuous")) {
                stop("\"treat.type\" must be \"discrete\" or \"continuous\"")
            }
            if (treat.type == "discrete" & n.levels>=5) {
                message("Too many treatment levels; treat as continuous.")
                treat.type <- "continuous"
            }
            if (treat.type == "continuous" & n.levels<=4) {
                cat("Too few treatment levels; consider setting treat.type = \"discrete\".")                
            }
        } else {
            if (n.levels>5) {
                treat.type <- "continuous"
            } else {
                treat.type <- "discrete"
            }
        }

        ## if (!(1%in%data[, D] & 0%in%data[, D] & length(unique(data[,D])) == 2)) {
        ##     stop(paste("variable \"", D, "\" should contain only 0 and 1.\n"))
        ## }
    }
    else { #ignore.treat == 1
        n.levels <- 0
        treat.type <- "discrete"
    }

    ## shade in the post-treatment period
    if (!inherits(shade.post, c("logical","numeric"))) {
        stop("Incorrect type for option \"shade.post\"")
    }

    

    ## ------------------------ ##
    ## parsing data.            ##
    ## ------------------------ ##

    ## raw id and time
    raw.id <- sort(unique(data[,index[1]]))
    raw.time <- sort(unique(data[,index[2]]))
    N <- length(raw.id) 
    TT <- length(raw.time)

    ## id to be plotted 
    input.id <- NULL
    if (!is.null(id)) {
        if (!is.null(show.id)) {
            message("Using specified id.\n")
        }
        ## check id 
        remove.id <- setdiff(id, raw.id)
        if (length(remove.id) != 0) {
            cat("List of units removed from dataset:", remove.id)
            cat("\n\n")
            input.id <- intersect(sort(id), raw.id)
        } else {
            input.id <- sort(id)
        }
    } else {
        if (!is.null(show.id)) {

            if (length(show.id) > N ) {
                stop("Length of \"show.id\" should not be larger than total number of units. \n")
            }
            if (!inherits(show.id, c("numeric", "integer"))) {
                stop("\"show.id\" option misspecified. Try, for example, show.id = 1:100. \n")
            }
            if (sum(show.id > N) > 0) {
                stop("Some specified units are not in the data.\n")
            }
            if (length(unique(show.id)) != length(show.id)) {
                stop("Repeated values in \"show.id\" option.")
            }

            input.id <- raw.id[show.id]

        } else {
            input.id <- raw.id
        }
    }


    ## store variable names
    data.old <- data
    Yname <- Y
    Dname <- D

    # if any of D/Y/X in data missing, then obs.missing = -200
    data$rowmiss <- rowSums(is.na(data))
    rowmissname <- colnames(data[ncol(data)])

    ## plot a subset of all data
    if (length(input.id) != length(raw.id)) {
        data <- data[which(data[,index.id] %in% input.id),]
        N <- length(input.id)
    }

    id.all <- time.all <- count <- coordin <- data.x <- x.na <- NULL  
    M <- Y <- I <- D <- X <- NULL


    if (leave.gap == 0) {
    ## check balanced panel and fill unbalanced panel
    
    if (dim(data)[1] != TT*N) { # unbalanced panel
        data[,index.id] <- as.numeric(as.factor(data[,index.id])) 
        data[,index.time] <- as.numeric(as.factor(data[,index.time])) 

        if (!is.null(Yname)) {
            Y <- matrix(NA, TT, N)
        }
        
        I <- matrix(0, TT, N) #I: observed(1) and missing(0)
        
        if (ignore.treat == 0) {
            D <- matrix(0, TT, N)
        }

        for (i in 1:dim(data)[1]) {
            if (!is.null(Yname)) {
                Y[data[i,index.time],data[i,index.id]] <- data[i,Yname] 
            }

            if (ignore.treat == 0) {
                D[data[i,index.time],data[i,index.id]] <- data[i,Dname] 
            }

            I[data[i,index.time], data[i,index.id]] <- 1 #I: observed(1) and missing(0)
        }

    } else { # balanced panel
        
        I <- matrix(1, TT, N) 
        if (!is.null(Yname)) {
            Y <- matrix(data[,Yname], TT, N)
        }
        if (ignore.treat == 0) {
            D <- matrix(data[,Dname], TT, N)
        }
    }
    }
    else { # leave.gap == 1: balanced panel
        data[,index.id] <- as.numeric(as.factor(data[,index.id]))
        data[,index.time] <- as.numeric(as.factor(data[,index.time]))

        M <- matrix(0, TT, N)
        for (i in 1:dim(data)[1]) {
            M[data[i,index.time], data[i,index.id]] <- data[i,rowmissname] 
        }

        if (!is.null(Yname)) {
            Y <- matrix(NA, TT, N)
        }
        I <- matrix(0, TT, N) #I: observed(1) and missing(0)
        if (ignore.treat == 0) {
            D <- matrix(0, TT, N)
        } 

        for (i in 1:dim(data)[1]) {
            if (!is.null(Yname)) {
                Y[data[i,index.time],data[i,index.id]] <- data[i,Yname] 
            }
            if (ignore.treat == 0) {  
                D[data[i,index.time],data[i,index.id]] <- data[i,Dname] 
            }
            I[data[i,index.time], data[i,index.id]] <- 1 #I: observed(1) and missing(0)
        }
    }

    if (collapse.history == TRUE) {

        if (is.null(M)) {
            D.f <- rbind(D, I)
        } else {
            D.f <- rbind(D, I, M)
        }

        D.d <- as.data.frame(t(D.f))
        suppressMessages(ff <- as.data.frame(summarise(group_by_all(D.d), COUNT = n())))
        

        D <- t(as.matrix(ff[, 1:TT]))
        I <- t(as.matrix(ff[, (TT+1):(2*TT)]))

        if (is.null(M)) {
            input.id <- ff[, (2 * TT + 1)]
        } else {
            M <- t(as.matrix(ff[, (2 * TT + 1):(3*TT)]))
            input.id <- ff[, (3 * TT + 1)]
        }

        N <- length(input.id)

        ## sort by cohort size 
        D.id <- cbind(1:N, input.id)
        D.id <- D.id[order(D.id[, 2], decreasing = TRUE), ]

        D.id.vec <- D.id[, 1]
        input.id <- D.id[, 2]

        D <- D[, D.id.vec]
        I <- I[, D.id.vec]

        if (!is.null(M)) {
            M <- M[, D.id.vec]
        }

        ## return(obs.missing)

        ## colnames(obs.missing) <- input.id
        ## rownames(obs.missing) <- raw.time

         

        ## cat("ok2")
        ## cat(N)

    }

    D.old <- D ## store the original indicators 


    ## binary treatment indicator 
    if (ignore.treat == FALSE && d.bi == 1) {

        if (length(unique(c(D.old))) > 2) {
            D[which(D > 1)] <- 1 ## set all treatment levels to 1
        }

        ## once treated, always treated!
        D <- apply(D, 2, function(vec){cumsum(coalesce(vec, 0)) + vec*0}) 
        co.total.all <- TT - apply(D, 2, sum)
        D <- ifelse(D > 0, 1, 0)


        ## timing
        tr.pos <- which(D[TT,] == 1) ## which units are treated
        T0 <- apply(D == 0, 2, sum, na.rm = TRUE)[tr.pos]+1 ## first time expose to treatment 

        T1 <- apply(D == 1, 2, sum, na.rm = TRUE)[tr.pos] ## number of periods expose to treatment 
        T1[which(T1 > 1)] <- 0 ## indicate the last dot of treatment status change

        co.total <- co.total.all[tr.pos] ## total number of periods not exposed to treatment 

        DID <- length(unique(T0)) == 1 ## DID type: all treated units are treated at the same time

        ## number of periods after first get treated
        # T1 <- t1 <- NULL ## sort by timing
        # if (by.timing == TRUE) {
            
        #     T1 <- rep(NA, length(tr.pos))            
        #     for (i in 1:length(tr.pos)) {
        #         i.tr <- I[,tr.pos[i]]
        #         d.tr <- D.old[,tr.pos[i]]
        #         t1 <- which(d.tr == 0 & i.tr == 1)
        #         if (length(t1) > 0) {
        #             if (max(t1) <= T0[i]) {
        #                 T1[i] <- 0
        #             } else {
        #                 T1[i] <- TT - min(t1[which(t1 > T0[i])])
        #             }
        #         } else {
        #             T1[i] <- 0
        #         }
        #     }
        # }

        ## check DID mode
        if (sum(abs(D.old[which(I==1)] - D[which(I==1)]), na.rm = TRUE) == 0) {
            staggered <- 1
        } else { ## FE mode, with reversals
            DID <- 0
            if (type == "outcome" || type == "bivariate") {
                message("Treatment has reversals.\n")
            if (by.cohort == TRUE) {
                stop("option \"by.cohort = TRUE\" works only with staggered adoption.")
            }
            }            
            staggered <- 0
        }

    } else {
        DID <- 0
        staggered <- 1
    }

    ## missing matrix 

    ########################################
    ## unified labels:
    ##  -200 for missing
    ##  -1 for control condition (or observed)
    ##   0 for treated pre
    ##   1 for treated post  
    ########################################
    
    obs.missing <- NULL

if (leave.gap == 0) {
    if (ignore.treat == 0 && d.bi == 1) { #  binary, and without ignore.treat 
        con1 <- type == "treat" && pre.post == TRUE
        con2 <- type == "outcome" && by.group == FALSE

        if (staggered == 1 && (con1 || con2)) {  ## DID type data
            
            tr <- D[TT,] == 1     # cross-sectional: treated unit

            id.tr <- which(tr==1)
            id.co <- which(tr==0)

            D.tr <- as.matrix(D[,which(tr==1)])
            I.tr <- as.matrix(I[,which(tr==1)])
            Y.tr <- Y.co <- NULL
            if (type == "outcome") {
                Y.tr <- as.matrix(Y[,which(tr==1)])
                Y.co <- as.matrix(Y[,which(tr==0)])
            }
            

            Ntr <- sum(tr)
            Nco <- N - Ntr

            ## 1. control group: -1
            obs.missing <- matrix(-1, TT, N)
            ## 2. add treated units
            obs.missing[, id.tr] <- D[, id.tr]
            ## 3. set missing values
            obs.missing[which(I==0)] <- -200 ## missing -200; I==0: missings in unbalanced panel

            unit.type <- rep(1, N) ## 1 for control; 2 for treated; 3 for reversal
            unit.type[id.tr] <- 2

        } else {
            unit.type <- rep(NA, N) ## 1 for control; 2 for treated; 3 for reversal


            for (i in 1:N) {
                di <- D.old[, i]
                ii <- I[, i]


                if (length(unique(di[which(ii==1)])) == 1) { ## treated or control
                    if (0 %in% unique(di[which(ii==1)])) {
                        unit.type[i] <- 1 ## always control
                    } else {
                        unit.type[i] <- 2 ## always treated
                    }
                } else {
                    unit.type[i] <- 3 ## control to treated / treated to control
                }
            }
        
            ## 1. using D.old  
            obs.missing <- D.old 
            ## 2. set controls
            obs.missing[which(D.old == 0)] <- -1 ## under control
            ## 3. set missing 
            obs.missing[which(I==0)] <- -200 ## missing
        }
        
        obs.missing.treat <- obs.missing
        if (length(unique(c(D.old))) > 2) {
            obs.missing[which(obs.missing > 1)] <- 1
        }

    } 
    else { # either not binary (>2 treatment levels) or ignore.treat == 1
        if (n.levels > 2 && type == "treat") { ## >2 treatment levels
            obs.missing <- D
            # NA: leave.gap == 0
            obs.missing[which(I == 0)] <- NA             
        } else {
                    obs.missing <- matrix(-1, TT, N) 
                    obs.missing[which(I==0)] <- -200 ## missing
                    ignore.treat <- 1   
        }
    }
}
else if (leave.gap == 1) {
        if (ignore.treat == 0 && d.bi == 1) { #  binary, and without ignore.treat 
        con1 <- type == "treat" && pre.post == TRUE
        con2 <- type == "outcome" && by.group == FALSE

        if (staggered == 1 && (con1 || con2)) {  ## DID type data
            
            tr <- D[TT,] == 1     # cross-sectional: treated unit

            id.tr <- which(tr==1)
            id.co <- which(tr==0)

            D.tr <- as.matrix(D[,which(tr==1)])
            I.tr <- as.matrix(I[,which(tr==1)])
            Y.tr <- Y.co <- NULL

            if (type == "outcome") {
                Y.tr <- as.matrix(Y[,which(tr==1)])
                Y.co <- as.matrix(Y[,which(tr==0)])
            }

            Ntr <- sum(tr)
            Nco <- N - Ntr

            ## 1. control group: -1
            obs.missing <- matrix(-1, TT, N)
            ## 2. add treated units
            obs.missing[, id.tr] <- D[, id.tr]
            ## 3. set missing values
            obs.missing[which(I==0)] <- -200 ## missing -200
            obs.missing[which(M!=0)] <- -200

            unit.type <- rep(1, N) ## 1 for control; 2 for treated; 3 for reversal
            unit.type[id.tr] <- 2

        } else {
            unit.type <- rep(NA, N) ## 1 for control; 2 for treated; 3 for reversal

            for (i in 1:N) {
                di <- D.old[, i]
                ii <- I[, i] ## I: observed or missing

                if (length(unique(di[which(ii==1)])) == 1) { ## treated or control
                    if (0 %in% unique(di[which(ii==1)])) {
                        unit.type[i] <- 1 ## control
                    } else {
                        unit.type[i] <- 2 ## treated
                    }
                } 
                else if ((length(unique(di[which(ii==1)])) == 2) & (NA %in% unique(di[which(ii==1)]))) { # NA 0 / NA 1
                    if (0 %in% unique(di[which(ii==1)])) {
                        unit.type[i] <- 1 ## control
                    } else {
                        unit.type[i] <- 2 ## treated
                    }
                    }
                else {
                    unit.type[i] <- 3 ## control to treated / treated to control / NA 0 1 / NA 1 0
                }
            }
        
            ## 1. using D.old  
            obs.missing <- D.old 
            ## 2. set controls
            obs.missing[which(D.old == 0)] <- -1 ## under control
            ## 3. set missing 
            obs.missing[which(I==0)] <- -200 ## missing
            obs.missing[which(M!=0)] <- -200
        }
        
        obs.missing.treat <- obs.missing
        if (length(unique(c(D.old))) > 2) {
            obs.missing[which(obs.missing > 1)] <- 1
        }

    } else { # either not binary (>2 treatment levels) or ignore.treat == 1
        if (n.levels > 2 && type == "treat") { ## >2 treatment levels (note that if ignore.treat = 1, n.levels = 0)
            obs.missing <- D
            # -200: leave.gap == 1
            obs.missing[which(I == 0)] <- -200
            obs.missing[which(M != 0)] <- -200          
        } 
        else {
            obs.missing <- matrix(-1, TT, N)
            obs.missing[which(I==0)] <- -200
            obs.missing[which(M != 0)] <- -200
            ignore.treat <- 1 
        }
    }
}

    colnames(obs.missing) <- input.id
    rownames(obs.missing) <- raw.time
    
    ## cat("ok")
    ## plot unique treatment histories 
    #if (collapse.history == TRUE) {

    #    obs.missing.d <- as.data.frame(t(obs.missing))
    #    suppressMessages(ff <- as.data.frame(summarise(group_by_all(obs.missing.d), COUNT = n())))

    #    obs.missing <- t(as.matrix(ff[, 1:TT]))
    #    input.id <- ff[, (TT + 1)]

        ## return(obs.missing)

    #    colnames(obs.missing) <- input.id
    #    rownames(obs.missing) <- raw.time

    #    N <- length(input.id) 

        ## cat("ok2")
        ## cat(N)

    #}

    time <- raw.time
    id <- input.id 

    ## ------------------------------------- ##
    ##          part 2: plot
    ## ------------------------------------- ##
    
    outcome <- NULL ## global variable
    treatment <- NULL
    labels1 <- labels2 <- labels3 <- NULL
    
    if (is.null(xlim)==FALSE) {
        if (is.numeric(xlim)==FALSE) {
            stop("Some element in \"xlim\" is not numeric.")
        } else {
            if (length(xlim)!=2) {
                stop("\"xlim\" must be of length 2.")
            }
        }
    }

    if (type != "bivar" & type != "bivariate") {
        if (is.null(ylim)==FALSE) {
            if (is.numeric(ylim)==FALSE) {
                stop("Some element in \"ylim\" is not numeric.")
            } else {
                if (length(ylim)!=2) {
                    stop("\"ylim\" must be of length 2.")
                }
            }
        }
    }

    if (is.null(xlab)==FALSE) {
        if (is.character(xlab) == FALSE) {
            stop("\"xlab\" is not a string.")
        } else {
            xlab <- xlab[1]
        }   
    }
    if (is.null(ylab)==FALSE) {
        if (is.character(ylab) == FALSE) {
            stop("\"ylab\" is not a string.")
        } else {
            ylab <- ylab[1]
        }   
    }

    if (is.logical(legendOff) == FALSE & is.numeric(legendOff)==FALSE) {
        stop("\"legendOff\" is not a logical flag.")
    }

    if (is.logical(gridOff) == FALSE & is.numeric(gridOff)==FALSE) {
        stop("\"gridOff\" is not a logical flag.")
    }

    if (is.null(main)==FALSE) {
        if (is.character(main) == FALSE) {
            stop("\"main\" is not a string.")
        } else {
            main <- main[1]
        }   
    }

    if (is.null(axis.lab.angle)==FALSE) {
        angle <- axis.lab.angle
        x.v <- 1
        x.h <- 1
    } else {
        if (axis.adjust == TRUE) {
            angle <- 45
            x.v <- 1
            x.h <- 1
        } else {
            angle <- 0
            x.v <- 0
            if (type == "treat") {
                x.h <- 0.5
            } else {
                x.h <- 0
            }
        }
    }    
    
    ## type of plots
    if (!is.numeric(time[1])) {
        time <- 1:TT
    }

    ## periods to show
    if (length(xlim) != 0) {
        show <- which(time>=xlim[1] & time<=xlim[2])
    } else {
        show <- 1:length(time)
    }     

    nT <- length(show)
    time.label <- raw.time[show]
    T.b <- 1:length(show)

    ## labels
    N.b <- 1:N
    if (type == "treat") {
        if (axis.lab == "both") {
            if (length(axis.lab.gap)==2) {
                x.gap <- axis.lab.gap[1]
                y.gap <- axis.lab.gap[2] 
            } else {
                x.gap <- y.gap <- axis.lab.gap[1]
            }
        } else {
            x.gap <- y.gap <- axis.lab.gap[1]
        }
        if (y.gap != 0) {
            N.b <- seq(from = N, to = 1, by = -(y.gap + 1))
        }
    } else {
        x.gap <- axis.lab.gap[1]
    }

    if (x.gap != 0) {
        T.b <- seq(from = 1, to = length(show), by = (x.gap + 1))
    }

    ## legend on/off
    if (legendOff == 1) {
        legend.pos <- "none"
    } else {
        legend.pos <- "bottom"
    }

    ###########################
    ## Dispatch to plot functions
    ###########################
    s <- as.list(environment())
    if (type == "outcome") {
        .pv_plot_outcome(s)
    } else if (type == "treat") {
        .pv_plot_treat(s)
    } else if (type == "bivariate") {
        .pv_plot_bivariate(s)
    }
}
