## A pre-view function for tscs data
## 2022-01-03

## missing: plot treatment status and missing data
## outcome: plot raw outcome data

##---------------------------------------------------------------##
## preview of data treatment status, missing values and raw data ##
##---------------------------------------------------------------##

panelview <- function(data, # a data frame (long-form)
                      formula = NULL,
                      Y = NULL,
                      D = NULL,
                      X = NULL,
                      index, # c(unit, time) indicators
                      na.rm = TRUE, # remove missing values
                      ignore.treat = FALSE,
                      type = "treat", ## treat, outcome, or bivar(iate)
                      outcome.type = "continuous", # continuous or discrete
                      treat.type = NULL, # discrete or continuous
                      by.group = FALSE, # (color pre-treatment treated differently)
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
                      pre.post = FALSE, # only used for treat plot 
                      id = NULL,
                      show.id = NULL,
                      color = NULL,
                      axis.adjust = FALSE,
                      axis.lab = "both",
                      axis.lab.gap = c(0, 0),
                      shade.post = TRUE,
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
                      lwd = 0.2
                    ) {  
        
    ## ------------------------- ##
    ## parse variable.           ##
    ## ------------------------- ##
    if (is.data.frame(data) == FALSE || length(class(data)) > 1) {
        data <- as.data.frame(data)        
    }



    if (!is.null(formula)) {
        
        varnames <- all.vars(formula)

        ## outcome
        Y <- varnames[1]

        ## treatment indicator and covariates
        if (length(varnames) == 1) { ## only y
            D <- X <- NULL
            ignore.treat <- 1
        } else if (length(varnames) == 2) {
            if (ignore.treat == 0) {
                D <- varnames[2]
                X <- NULL
            } else {
                D <- NULL
                X <- varnames[2]
            }   
        } else {
            if (ignore.treat == 0) {
                D <- varnames[2]
                X <- varnames[3:length(varnames)]
            } else {
                D <- NULL
                X <- varnames[2:length(varnames)]
            }
        }
    } else {
        varnames <- c(Y, D, X)
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
        
    ## exclude other covariates 
    data <- data[,c(index, Y, D, X)] 

    ## remove missing values
    if (is.logical(na.rm) == FALSE & !na.rm%in%c(0, 1)) {
        stop("na.rm is not a logical flag.")
    } 
    if (na.rm == FALSE & sum(is.na(data)) > 0) {
        stop("Missing values in dataset. Try set na.rm = TRUE.\n")
    } 
    if (na.rm == TRUE) {
        data <- na.omit(data)
    }

    ## check duplicated observations
    unique_label <- unique(paste(data[,index[1]],"_",data[,index[2]],sep=""))
    if (length(unique_label)!= dim(data)[1]) {
        stop("Unit and time variables do not uniquely identify all observations. Some may be duplicated or Incorrectly marked in the dataset.")
    }

    ##-------------------------------##
    ## Checking Other Parameters
    ##-------------------------------## 

    if (!type %in% c("missing", "raw", "treat", "outcome","bivar","bivariate")) {
        stop("\"type\" option misspecified.")
    }
    if (type == "missing") {
        type <- "treat"
    }
    if (type == "raw") {
        type <- "outcome"
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
            warning("No treatment indicator.\n")
            ignore.treat <- 1
        }
    }

    if (is.null(Y) && (type == "outcome" || type == "bivar" || type == "bivariate")) {
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

        if (!(class(data[, D]) %in% c("numeric", "integer"))) {
            ## data[, Dname] <- as.numeric(as.character(data[, Dname]))
            stop("Treatment indicator should be a numeric value.")
        }

        d.levels <- sort(unique(data[, D]))
        n.levels <- length(d.levels) # n.levels: treatment levels
        d.bi <- d.levels[1] == 0 & d.levels[2] == 1 & n.levels == 2 # d.bi: binary treatment

        if (outcome.type == "discrete") {
            y.levels <- sort(unique(data[, Y]))
        }

        if (n.levels == 1) {
            warning("Only one treatment level...\n")
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
            if (treat.type == "discrete" & n.levels>5) {
                warning("Too many treatment levels; treat as continuous.")
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
    else{
        n.levels <- 0
        treat.type <- "discrete"
    }

    ## shade in the post-treatment period
    if (!class(shade.post) %in% c("logical","numeric")) {
        stop("Incorrect type for option \"shade.post\"")
    }

    ## ------------------------ ##
    ## parsing data.            ##
    ## ------------------------ ##

    ## index names
    index.id <- index[1]
    index.time <- index[2]

    ## raw id and time
    raw.id <- sort(unique(data[,index[1]]))
    raw.time <- sort(unique(data[,index[2]]))
    N <- length(raw.id)
    TT <- length(raw.time)

    ## id to be plotted 
    input.id <- NULL
    if (!is.null(id)) {
        if (!is.null(show.id)) {
            warning("Using specified id.\n")
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
            if (!class(show.id) %in% c("numeric", "integer")) {
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
    ## data.old <- data
    Yname <- Y
    Dname <- D
    Xname <- X

    ## plot a subset of all data
    if (length(input.id) != length(raw.id)) {
        data <- data[which(data[,index.id] %in% input.id),]
        N <- length(input.id)
    }

    ## sort data
    data <- data[order(data[,index.id], data[,index.time]), ]

    id.all <- time.all <- count <- coordin <- data.x <- x.na <- NULL
    
    ## check balanced panel and fill unbalanced panel
    Y <- I <- D <- NULL
    if (dim(data)[1] != TT*N) {

        data[,index.id] <- as.numeric(as.factor(data[,index.id]))
        data[,index.time] <- as.numeric(as.factor(data[,index.time]))

        if (!is.null(Yname)) {
            Y <- matrix(NA, TT, N)
        }
        I <- matrix(0, TT, N)
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
            I[data[i,index.time],data[i,index.id]] <- 1 #I: observed(1) and missing(0)
        }

    } else {
        I <- matrix(1, TT, N) 
        if (!is.null(Yname)) {
            Y <- matrix(data[,Yname], TT, N)
        }
        if (ignore.treat == 0) {
            D <- matrix(data[,Dname], TT, N)
        }
    }

    ## binary treatment indicator 
    if (ignore.treat == 0 && d.bi == 1) {

        D.old <- D ## store the original indicators
        if (length(unique(c(D.old))) > 2) {
            D[which(D > 1)] <- 1 ## set all treatment levels to 1
        }

        ## once treated, always treated
        D <- apply(D, 2, function(vec){cumsum(vec)})
        co.total.all <- TT - apply(D, 2, sum)
        D <- ifelse(D > 0, 1, 0)


        ## timing
        tr.pos <- which(D[TT,] == 1) ## which units are treated
        T0 <- apply(D == 0, 2, sum)[tr.pos]+1 ## first time expose to treatment
        T1 <- apply(D == 1, 2, sum)[tr.pos] ## number of periods expose to treatment
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
        if (sum(abs(D.old[which(I==1)] - D[which(I==1)])) == 0) {
            by.group <- by.group
            FEmode <- 0
        } else { ## FE mode
            DID <- 0
            if (by.group == FALSE & (type == "outcome" || type == "bivar" || type == "bivariate")) {
                warning("Treatment has reversals.\n")
            }
            ## by.group <- TRUE
            FEmode <- 1
        }

    } else {
        DID <- 0
        FEmode <- 0
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
    
    if (ignore.treat == 0 && d.bi == 1) { #  binary, and without ignore.treat 
        
        con1 <- type == "treat" && pre.post == TRUE
        con2 <- type == "outcome" && by.group == FALSE

        if (FEmode == 0 && (con1 || con2)) {  ## DID type data
            
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

            unit.type <- rep(1, N) ## 1 for control; 2 for treated; 3 for reversal
            unit.type[id.tr] <- 2

        } else {
            
            unit.type <- rep(NA, N) ## 1 for control; 2 for treated; 3 for reversal
            
            for (i in 1:N) {
                di <- D.old[, i]
                ii <- I[, i]
                if (length(unique(di[which(ii==1)])) == 1) { ## treated or control
                    if (0 %in% unique(di[which(ii==1)])) {
                        unit.type[i] <- 1 ## control
                    } else {
                        unit.type[i] <- 2 ## treated
                    }
                } else {
                    unit.type[i] <- 3 ## reversal
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

    } else { # either not binary (>2 treatment levels) or ignore.treat == 1

        if (n.levels > 0 && type == "treat") { ## >2 treatment levels
            obs.missing <- D
            obs.missing[which(I == 0)] <- NA

        } else { ## ignore.treat == 1

            obs.missing <- matrix(-1, TT, N) 
            obs.missing[which(I==0)] <- -200 ## missing
            ignore.treat <- 1
        }

    }

    colnames(obs.missing) <- input.id
    rownames(obs.missing) <- raw.time
    

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


    ############  START  ###############    
    if (type == "outcome") {
        
        ## axes labels
        if (is.null(xlab)==TRUE) {
            xlab <- index[2]
        } else if (xlab == "") {
            xlab <- NULL
        }
        if (is.null(ylab)==TRUE) {
            ylab <- Yname
        } else if (ylab == "") {
            ylab <- NULL
        }

        ## plot color setting
        raw.color <- NULL

        ## color setting
        if (is.null(color)==TRUE) {
            if (theme.bw == FALSE) {
                if (ignore.treat == 0) {
                    raw.color <- c("#99999950", "#FC8D6280", "red")
                    
                } else {
                    raw.color <- "#99999950"
                }
            } else {
                if (outcome.type == "continuous") {
                    if (ignore.treat == 0) {
                        raw.color <- c("#5e5e5e50", "#4671D565", "#06266F")
                    } else {
                        raw.color <- "#5e5e5e50"
                    }
                } else {
                    if (ignore.treat == 0) {
                        raw.color <- c("#5e5e5e60", "#4671D5", "#06266F")
                    } else {
                        raw.color <- "#5e5e5e60"
                    }
                }
            }
        } else {        
            if (ignore.treat == 0) {
                if (FEmode == 1) {
                    if (length(color) != 2) {
                        stop("Length of \"color\" should be equal to 2.\n")    
                    } else {
                        cat("Specified colors are in the order of \"under treatment\", \"under control\".\n")
                        raw.color <- color[c(2,1)]
                    }
                } else {
                    if (by.group == FALSE & length(color) != 3) {
                        stop("Length of \"color\" should be equal to 3.\n")
                    }
                    else if (by.group == TRUE & length(color) != 2) {
                        stop("Length of \"color\" should be equal to 2.\n")
                    }
                    else if (by.group == FALSE & length(color) == 3) {
                        cat("Specified colors in the order of \"treated (pre)\", \"treated (post)\", \"control\".\n")
                        raw.color <- color[c(3,1,2)]
                    }
                    else {
                        cat("Specified colors in the order of \"under treatment\", \"under control\".\n")
                        raw.color <- color[c(2,1)]
                    }
                }
            } else {
                if (length(color) != 1) {
                    stop("Length of \"color\" should be equal to 1.\n") 
                }
            }
        }

        if (ignore.treat == 1) {

            data <- cbind.data.frame("time" = rep(time[show], N), 
                                     "outcome" = c(Y[show,]),
                                     "type" = rep("co",(N*nT)),
                                     "id" = rep(1:N,each = nT))

            if (outcome.type == "discrete") {
                data <- na.omit(data)
                data$outcome <- factor(data$outcome)
            }

            ## theme
            p <- ggplot(data) + xlab(xlab) +  ylab(ylab)
            
            if (theme.bw == TRUE) {
                p <- p + theme_bw()
            }
            p <- p + theme(legend.position = legend.pos,
             axis.text.x = element_text(angle = angle, hjust=x.h, vjust=x.h),
             plot.title = element_text(size=cex.main, hjust = 0.5, face="bold",margin = margin(8, 0, 8, 0)))

            if (outcome.type == "continuous") {
                ## main
                p <- p + geom_line(aes(time, outcome,
                                       colour = type,
                                       size = type,
                                       linetype = type,
                                       group = id))

                ## legend
                set.limits = "co"
                set.colors = raw.color
                set.linetypes = "solid"
                set.linewidth = 0.5
                if (!is.null(legend.labs)) {
                    if (length(legend.labs) != 1) {
                        warning("Incorrect number of labels in the legend. Using default.\n")
                        set.labels = "Observed"  
                    } else {
                        set.labels <- legend.labs
                    }
                } else {
                    set.labels <- "Observed"
                }
                labels.ncol <- 1

                ## scale
                p <- p + scale_colour_manual(limits = set.limits,
                                             labels = set.labels,
                                             values =set.colors) +
                    scale_linetype_manual(limits = set.limits,
                                          labels = set.labels,
                                          values = set.linetypes) +
                    scale_size_manual(limits = set.limits,
                                      labels = set.labels,
                                      values = set.linewidth) +
                    guides(linetype = guide_legend(title=NULL, ncol=labels.ncol),
                           colour = guide_legend(title=NULL, ncol=labels.ncol),
                           size = guide_legend(title=NULL, ncol=labels.ncol))

            } else { ## categorical data
              
                ## main
                p <- p + geom_jitter(width = 0.15, height = 0.15,
                                     aes(x = time, y = outcome, colour = type, shape = type))

                ## scale
                set.limits = "co"
                set.colors = raw.color
                set.shapes = 1
                if (!is.null(legend.labs)) {
                    if (length(legend.labs) != 1) {
                        warning("Incorrect number of labels in the legend. Using default.\n")
                        set.labels = "Observed"
                    } else {
                        set.labels <- legend.labs
                    }
                } else {                      
                    set.labels = "Observed"
                }
              
                labels.ncol <- 1

                p <- p + scale_colour_manual(limits = set.limits,
                                             labels = set.labels,
                                             values =set.colors) + 
                         scale_shape_manual(limits = set.limits,
                                            labels = set.labels,
                                            values =set.shapes) + 
                         guides(colour = guide_legend(title=NULL, ncol=labels.ncol),
                                shape = guide_legend(title=NULL, ncol=labels.ncol))
            }

            if (!is.numeric(time.label)) {
                p <- p + 
                     scale_x_continuous(expand = c(0, 0), breaks = show[T.b], labels = time.label[T.b])
            }

            ## title
            if (is.null(main) == TRUE) {
                p <- p + ggtitle("Raw Data")
            } else if (main!="") {
                p <- p + ggtitle(main)
            }

            ## ylim
            if (is.null(ylim) == FALSE) {
                p <- p + coord_cartesian(ylim = ylim)
            }
            
            suppressWarnings(print(p))

        }
        else if (ignore.treat == 0 && by.group == FALSE) { ## Mixed units
            
            ## time-line
            if (outcome.type == "continuous") { ## continuous outcome
                
                if (FEmode == 0) { ## DID data

                    time.bf <- time[unique(T0)]
                    pst <- D.tr

                    for (i in 1:Ntr) {
                        pst[T0[i], i] <- 1
                    }

                    time.pst <- c(pst[show,] * time[show])
                    time.pst <- time.pst[which(c(pst[show,])==1)]
                    Y.tr.pst <- c(Y.tr[show,])[which(pst[show,]==1)]
                    id.tr.pst <- matrix(rep(1:Ntr,each=TT),TT,Ntr,byrow=FALSE)[show,]
                    id.tr.pst <- c(id.tr.pst)[which(pst[show,]==1)]
                    T1_0 <- c(T1)[which(T1==0)]
                    T1_1 <- c(T1)[which(T1==1)]
                    N_T1_1 <- sum(T1_1)
                    N_T1_0 <- Nco*nT + Ntr*nT + length(Y.tr.pst) - N_T1_1 
                    

                    data <- cbind.data.frame("time" = c(rep(time[show], N), time.pst),
                                             "outcome" = c(c(Y.tr[show,]),
                                                           c(Y.co[show,]),
                                                           Y.tr.pst),
                                             "type" = c(rep("tr",(Ntr*nT)),
                                                        rep("co",(Nco*nT)),
                                                        rep("tr.pst",length(Y.tr.pst))),
                                            "last_dot" = c(rep("0",N_T1_0),
                                                           rep("1",N_T1_1)),
                                             "id" = c(rep(1:N,each = nT), id.tr.pst*(-1)))

                    idtimes <- sapply(1:length(data$id),function(x)sum(data$id[1:x]==data$id[x]))
                    data <- cbind(data, idtimes)
                    data$idtimes <- ave(data$idtimes, data$id, FUN=max)
                    data$last_dot <- 0
                    data$last_dot[data$idtimes == 1] <- 1
                    #print(data)


                    ## legend 
                    set.limits = c("co", "tr", "tr.pst")
                    set.colors = c(raw.color[1], raw.color[2], raw.color[3])
                    set.linetypes = c("solid","solid","solid")
                    set.linewidth = c(0.5, 0.5, 0.5)
                    if (!is.null(legend.labs)) {
                        if (length(legend.labs) != 3) {
                            warning("Incorrect number of labels in the legend. Using default.\n")
                            set.labels <- c("Controls","Treated (Pre)","Treated (Post)")  
                        } else {
                            set.labels <- legend.labs
                        }
                    } else {
                        set.labels <- c("Controls","Treated (Pre)","Treated (Post)") 
                    }
                    labels.ncol <- 3
                } 
                else { ## FE mode data
                
                    D.plot <- D.old
                    D.plot[which(D.plot == 0)] <- NA
                    D.plot[which(I == 0)] <- NA

                    Y.trt <- Y * D.plot
                    Y.trt.show <- as.matrix(Y.trt[show,])
                    time.trt.show <- time[show]
                    ut.time <- ut.id <- NULL
                    for (i in 1:N) {
                        if (sum(is.na(Y.trt.show[,i])) != nT) {
                            ut.id <- c(ut.id, rep(i, nT - sum(is.na(Y.trt.show[,i]))))
                            ut.time <- c(ut.time, time.trt.show[which(!is.na(Y.trt.show[,i]))])
                        }
                    }
                    T1_0 <- c(T1)[which(T1==0)]
                    T1_1 <- c(T1)[which(T1==1)]
                    N_T1_1 <- sum(T1_1)
                    N_T1_0 <- N*nT + length(ut.id) - N_T1_1

                    data <- cbind.data.frame("time" = c(rep(time[show], N), ut.time),
                                             "outcome" = c(c(Y[show,]),
                                                         c(Y.trt.show[which(!is.na(Y.trt.show))])),
                                             "type" = c(rep("co",(N*nT)),
                                                        rep("tr",length(ut.id))),
                                            "last_dot" = c(rep("0",N_T1_0),
                                                           rep("1",N_T1_1)),
                                             "id" = c(rep(1:N,each = nT), ut.id))

                    idtimes <- sapply(1:length(data$id),function(x)sum(data$id[1:x]==data$id[x]))
                    data <- cbind(data, idtimes)
                    data$idtimes <- ave(data$idtimes, data$id, FUN=max)
                    data$last_dot <- 0
                    data$last_dot[data$idtimes == 1] <- 1

                    ## legend
                    set.limits = c("co","tr")
                    set.colors = raw.color[1:2]
                    set.linetypes = c("solid","solid")
                    set.linewidth = c(0.5, 0.5)
                    if (!is.null(legend.labs)) {
                        if (length(legend.labs) != 2) {
                            warning("Incorrect number of labels in the legend. Using default.\n")
                            set.labels = c("Control", "Treated")  
                        } else {
                            set.labels <- legend.labs
                        }
                    } else {
                        set.labels <- c("Control", "Treated") 
                    }
                    labels.ncol <- 2
                }
            
        
                ## theme
                p <- ggplot(data) + xlab(xlab) +  ylab(ylab)
                if (theme.bw == TRUE) {
                    p <- p + theme_bw()
                }
                p <- p + theme(legend.position = legend.pos,
                    axis.text.x = element_text(angle = angle, hjust=x.h, vjust=x.h),
                    plot.title = element_text(size=cex.main, hjust = 0.5, face="bold",margin = margin(8, 0, 8, 0)))
            
                if (DID == TRUE && Ntr >= 1) {
                    if (time.bf >= min(show) && time.bf <= max(show)) {
                        p <- p + geom_vline(xintercept=time.bf,colour="white",size = 2)
                        if (shade.post == TRUE) {
                            p <- p + annotate("rect", xmin= time.bf, xmax= Inf,
                                     ymin=-Inf, ymax=Inf, alpha = .3) 
                        }                            
                    }
                }
        
                ## main
                p <- p + geom_line(aes(time, outcome,
                                       colour = type,
                                       size = type,
                                       linetype = type,
                                       group = id))

                data1 <- subset(data, data$last_dot==1)                       
                p <- p + geom_point(data = data1,
                                    aes(time, outcome),
                                    colour = raw.color[3],
                                    size = 0.5)
          
            
                p <- p + scale_colour_manual(limits = set.limits,
                                             labels = set.labels,
                                             values =set.colors) +
                    scale_linetype_manual(limits = set.limits,
                                          labels = set.labels,
                                          values = set.linetypes) +
                    scale_size_manual(limits = set.limits,
                                      labels = set.labels,
                                      values = set.linewidth) +
                    guides(linetype = guide_legend(title=NULL, ncol=labels.ncol),
                           colour = guide_legend(title=NULL, ncol=labels.ncol),
                           size = guide_legend(title=NULL, ncol=labels.ncol))
            
            } else { ## categorical data
                
                data <- cbind.data.frame("time" = rep(time[show], N), 
                                         "outcome" = factor(c(Y[show,])),
                                         "type" = c(obs.missing[show,]))

                data <- na.omit(data)
                data$type <- factor(data$type, levels = c(-1, 0, 1), labels = c("co","tr","tr.pst"))

                ## theme
                p <- ggplot(data) + xlab(xlab) +  ylab(ylab)
                if (theme.bw == TRUE) {
                    p <- p + theme_bw()
                }
                p <- p + theme(legend.position = legend.pos,
                    axis.text.x = element_text(angle = angle, hjust=x.h, vjust=x.h),
                    plot.title = element_text(size=cex.main, hjust = 0.5, face="bold",margin = margin(8, 0, 8, 0)))
                
                ## main plot
                p <- p + geom_jitter(width = 0.15, height = 0.15,
                                     aes(x = time, y = outcome, colour = type, shape = type))

                ## legend
                if (FEmode == 0) {
                    
                    time.bf <- time[unique(T0)]
                    set.limits = c("co", "tr", "tr.pst")
                    set.colors = c(raw.color[1], raw.color[2], raw.color[3])
                    set.shapes = c(1, 1, 16)
                    if (!is.null(legend.labs)) {
                        if (length(legend.labs) != 3) {
                            warning("Incorrect number of labels in the legend. Using default.\n")
                            set.labels = c("Controls","Treated (Pre)","Treated (Post)")  
                        } else {
                            set.labels <- legend.labs
                        }
                    } else {
                        set.labels = c("Controls","Treated (Pre)","Treated (Post)") 
                    }
                    labels.ncol <- 3
                
                } else {
                    
                    set.limits = c("co", "tr")
                    set.colors = raw.color[1:2]
                    set.shapes = c(1, 1)
                    if (!is.null(legend.labs)) {
                        if (length(legend.labs) != 2) {
                            warning("Incorrect number of labels in the legend. Using default.\n")
                            set.labels = c("Control", "Treatment")  
                        } else {
                            set.labels <- legend.labs
                        }
                    } else {
                        set.labels = c("Control", "Treatment") 
                    }
                    labels.ncol <- 2
                }

                p <- p + scale_colour_manual(limits = set.limits,
                                             labels = set.labels,
                                             values =set.colors) + 
                         scale_shape_manual(limits = set.limits,
                                            labels = set.labels,
                                            values =set.shapes) + 
                    guides(colour = guide_legend(title=NULL, ncol=labels.ncol),
                           shape = guide_legend(title=NULL, ncol=labels.ncol))                
            } 
        
            if (!is.numeric(time.label)) {
                p <- p + 
                    scale_x_continuous(expand = c(0, 0), breaks = show[T.b], labels = time.label[T.b])
            }

            ## title
            if (is.null(main) == TRUE) {
                p <- p + ggtitle("Raw Data")
            } else if (main!="") {
                p <- p + ggtitle(main)
            }

            ## ylim
            if (is.null(ylim) == FALSE) {
                p <- p + coord_cartesian(ylim = ylim)
            }
            
            suppressWarnings(print(p))
            ## end of raw plot
        
        } else { ## separate plot (by.group==T)
            
            if (is.null(main) == TRUE) {
                main <- "Raw Data"
            }

            if (!is.null(legend.labs)) {
               if (length(legend.labs) != 2) {
                   warning("Incorrect number of labels in the legend. Using default.\n")
                   set.labels = c("Control", "Treatment") 
               } else {
                   set.labels <- legend.labs
               }
            } else {
                set.labels = c("Control", "Treatment") 
            } 


            if (1 %in% unit.type) {
                co.pos <- which(unit.type == 1)
                Nco <- length(co.pos)

                data1 <- cbind.data.frame("time" = c(rep(time[show], Nco)),
                                          "outcome" = c(Y[show, co.pos]),
                                          "type" = c(rep("co", (Nco*nT))),
                                          "id" = c(rep(1:Nco, each = nT)))

                limits1 <- c("co", "tr")
                colors1 <- raw.color[1:2]

                main1 <- "Always Under Control"
            }

            if (2 %in% unit.type) {
                tr.pos <- which(unit.type == 2)
                Ntr <- length(tr.pos)

                data2 <- cbind.data.frame("time" = c(rep(time[show], Ntr)),
                                          "outcome" = c(Y[show, tr.pos]),
                                          "type" = c(rep("tr",(Ntr*nT))),
                                          "id" = c(rep(1:Ntr,each = nT)))

                limits2 <- c("co", "tr")
                colors2 <- raw.color[1:2]
                main2 <- "Always Under Treatment"
            }

            if (3 %in% unit.type) {
                rv.pos <- which(unit.type == 3)
                Nrv <- length(rv.pos)

                if (outcome.type == "continuous") {

                    D.plot <- D.old

                    D.plot[which(D.plot == 0)] <- NA
                    D.plot[which(I == 0)] <- NA

                    D.rv <- as.matrix(D.plot[, rv.pos])
                    Y.rv <- as.matrix(Y[, rv.pos])

                    Y.trt <- Y.rv * D.rv
                    Y.trt.show <- as.matrix(Y.trt[show,])
                    time.trt.show <- time[show]
                    ut.time <- ut.id <- NULL
                    for (i in 1:Nrv) {
                        if (sum(is.na(Y.trt.show[,i])) != nT) {
                            ut.id <- c(ut.id, rep(i, nT - sum(is.na(Y.trt.show[,i]))))
                            ut.time <- c(ut.time, time.trt.show[which(!is.na(Y.trt.show[,i]))])
                        }
                    }

                    T1_0 <- c(T1)[which(T1==0)]
                    T1_1 <- c(T1)[which(T1==1)]
                    N_T1_1 <- sum(T1_1)
                    N_T1_0 <- Nrv*nT + length(ut.id) - N_T1_1

                    data3 <- cbind.data.frame("time" = c(rep(time[show], Nrv), ut.time),
                                              "outcome" = c(c(Y[show, rv.pos]),
                                                          c(Y.trt.show[which(!is.na(Y.trt.show))])),
                                              "type" = c(rep("co",(Nrv*nT)),
                                                       rep("tr",length(ut.id))),
                                              "last_dot" = c(rep("0",N_T1_0),
                                                             rep("1",N_T1_1)),
                                              "id" = c(rep(1:Nrv,each = nT), ut.id))

                    data3_tr <- subset(data3, data3$type=="tr")  
                    data3_co <- subset(data3, data3$type=="co") 

                    idtimes <- sapply(1:length(data3_tr$id),function(x)sum(data3_tr$id[1:x]==data3_tr$id[x]))
                    data3_tr <- cbind(data3_tr, idtimes)
                    data3_tr$idtimes <- ave(data3_tr$idtimes, data3_tr$id, FUN=max)
                    data3_tr$last_dot <- 0
                    data3_tr$last_dot[data3_tr$idtimes == 1] <- 1

                    idtimes <- sapply(1:length(data3_co$id),function(x)sum(data3_co$id[1:x]==data3_co$id[x]))
                    data3_co <- cbind(data3_co, idtimes)
                    data3_co$idtimes <- ave(data3_co$idtimes, data3_co$id, FUN=max)

                    data3 <- rbind(data3_co,data3_tr)
                    #print(data3)


                } else { ## categorical data

                    data3 <- cbind.data.frame("time" = c(rep(time[show], Nrv)),
                                              "outcome" = c(Y[show, rv.pos]),
                                              "type" = c(obs.missing[show,]), ## -1 for control and 1 for treated
                                              "id" = c(rep(1:Nrv,each = nT)))

                    data3$type <- factor(data3$type, levels = c(-1, 1), labels = c("co","tr"))

                }

                limits3 <- c("co", "tr")
                colors3 <- raw.color[1:2]

                main3 <- "Treatment Status Changed"
            }
            
            ## sub-plot function for each type
            subplot <- function (data, limits, labels, colors, main, outcome.type, theme.bw) {
                
                if (outcome.type == "discrete") {
                    data$outcome <- factor(data$outcome)
                    data <- na.omit(data)
                }

                ## theme
                p <- ggplot(data) + xlab(xlab) +  ylab(ylab)

                ## legend
                set.limits = limits
                set.colors = colors
                set.linetypes = rep("solid", length(limits))
                set.linewidth = rep(0.5, length(limits))

                if (theme.bw == TRUE) {
                    p <- p + theme_bw() + 
                             theme(panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank(),
                                   axis.text.x = element_text(angle = angle, hjust=x.h, vjust=x.h),
                                   plot.title = element_text(size=cex.main, hjust = 0.5, face="bold",margin = margin(8, 0, 8, 0)))
                }
                else {
                    p <- p + theme(axis.text.x = element_text(angle = angle, hjust=x.h, vjust=x.h),
                                   plot.title = element_text(size=cex.main, hjust = 0.5, face="bold",margin = margin(8, 0, 8, 0)))
                }
                

                ## main
                if (outcome.type == "continuous") {
                    p <- p + geom_line(aes(time, outcome,
                                           colour = type,
                                           size = type,
                                           linetype = type,
                                           group = id))

                data1 <- subset(data, data$last_dot==1)                       
                p <- p + geom_point(data = data1,
                                    aes(time, outcome),
                                    colour = raw.color[2],
                                    size = 0.5)

                    p <- p + scale_colour_manual(limits = set.limits,
                                                 labels = set.labels,
                                                 values =set.colors) +
                        scale_linetype_manual(limits = set.limits,
                                              labels = set.labels,
                                              values = set.linetypes) +
                        scale_size_manual(limits = set.limits,
                                          labels = set.labels,
                                          values = set.linewidth) +
                        guides(linetype = guide_legend(title=NULL, nrow=1),
                               colour = guide_legend(title=NULL, nrow=1),
                               size = guide_legend(title=NULL, nrow=1))
                } else {
                    p <- p + geom_jitter(width = 0.15, height = 0.15, shape = 1,
                                     aes(x = time, y = outcome, colour = type))

                    p <- p + scale_colour_manual(limits = set.limits,
                                                 labels = set.labels,
                                                 values =set.colors) +
                        guides(linetype = guide_legend(title=NULL, nrow=1),
                               colour = guide_legend(title=NULL, nrow=1),
                               size = guide_legend(title=NULL, nrow=1))

                } 
        
                if (!is.numeric(time.label)) {
                    p <- p + 
                        scale_x_continuous(expand = c(0, 0), breaks = show[T.b], labels = time.label[T.b])
                }

                ## title
                if (is.null(main) == TRUE) {
                    p <- p + ggtitle("Raw Data")
                } else if (main!="") {
                    p <- p + ggtitle(main)
                }

                ## ylim
                if (is.null(ylim) == FALSE) {
                    p <- p + coord_cartesian(ylim = ylim)
                }
                return(p)
            }

            if (length(unique(unit.type)) == 1) {
                
                if (1%in%unit.type) {
                    p1 <- subplot(data1, limits1, labels1, colors1, main1, outcome.type, theme.bw)
                    if (legend.pos != "none") {
                        suppressWarnings(g <- ggplotGrob(p1 + theme(legend.position="bottom"))$grobs)
                        legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
                        suppressWarnings(grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                                         legend, nrow = 2, heights = c (1, 1/5)),
                                         top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2))))
                    } else {
                        suppressWarnings(grid.arrange(p1 + theme(legend.position="none"),
                                         top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2))))
                    }   
                }
                else if (2%in%unit.type) {
                    p2 <- subplot(data2, limits2, labels2, colors2, main2, outcome.type, theme.bw)
                    if (legend.pos != "none") {
                        suppressWarnings(g <- ggplotGrob(p2 + theme(legend.position="bottom"))$grobs)
                        legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
                        suppressWarnings(grid.arrange(arrangeGrob(p2 + theme(legend.position="none"),
                                         legend, nrow = 2, heights = c (1, 1/5)),
                                         top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2)))) 
                    } else {
                        suppressWarnings(grid.arrange(p2 + theme(legend.position="none"),
                                         top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2))))
                    }   
                }
                else if (3%in%unit.type) {
                    p3 <- subplot(data3, limits3, labels3, colors3, main3, outcome.type, theme.bw)
                    if (legend.pos != "none") {
                        suppressWarnings(g <- ggplotGrob(p3 + theme(legend.position="bottom"))$grobs)
                        legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
                        suppressWarnings(grid.arrange(arrangeGrob(p3 + theme(legend.position="none"),
                                         legend, nrow = 2, heights = c (1, 1/5)),
                                         top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2)))) 
                    } else {
                        suppressWarnings(grid.arrange(p3 + theme(legend.position="none"),
                                         top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2))))
                    }  
                }

            }
            else if (length(unique(unit.type))==2) {
                
                if (!1%in%unit.type) {
                    p2 <- subplot(data2, limits2, labels2, colors2, main2, outcome.type, theme.bw)
                    p3 <- subplot(data3, limits3, labels3, colors3, main3, outcome.type, theme.bw)
                    if (legend.pos != "none") {
                        suppressWarnings(g <- ggplotGrob(p2 + theme(legend.position="bottom"))$grobs)
                        legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
                        suppressWarnings(grid.arrange(arrangeGrob(p2 + theme(legend.position="none"), p3 + theme(legend.position="none"),
                                         legend, nrow = 3, heights = c (1, 1, 1/5)),
                                         top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2))))  
                    } else {
                        suppressWarnings(grid.arrange(p2 + theme(legend.position="none"),
                                         p3 + theme(legend.position="none"),
                                         top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2))))
                    }  
                }
                else if (!2%in%unit.type) {
                    p1 <- subplot(data1, limits1, labels1, colors1, main1, outcome.type, theme.bw)
                    p3 <- subplot(data3, limits3, labels3, colors3, main3, outcome.type, theme.bw)
                    if (legend.pos != "none") {
                        suppressWarnings(g <- ggplotGrob(p1 + theme(legend.position="bottom"))$grobs)
                        legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
                        suppressWarnings(grid.arrange(arrangeGrob(p1 + theme(legend.position="none"), p3 + theme(legend.position="none"),
                                         legend, nrow = 3, heights = c (1, 1, 1/5)),
                                         top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2))))  
                    } else {
                        suppressWarnings(grid.arrange(p1 + theme(legend.position="none"),
                                         p3 + theme(legend.position="none"),
                                         top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2))))
                    }  
                }
                else if (!3%in%unit.type) {
                    p1 <- subplot(data1, limits1, labels1, colors1, main1, outcome.type, theme.bw)
                    p2 <- subplot(data2, limits2, labels2, colors2, main2, outcome.type, theme.bw)
                    if (legend.pos != "none") {
                        suppressWarnings(g <- ggplotGrob(p1 + theme(legend.position="bottom"))$grobs)
                        legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
                        suppressWarnings(grid.arrange(arrangeGrob(p1 + theme(legend.position="none"), p2 + theme(legend.position="none"),
                                         legend, nrow = 3, heights = c (1, 1, 1/5)),
                                         top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2))))  
                    } else {
                        suppressWarnings(grid.arrange(p1 + theme(legend.position="none"),
                                         p2 + theme(legend.position="none"),
                                         top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2))))
                    }   
                }

            }
            else {
                
                p1 <- subplot(data1, limits1, labels1, colors1, main1, outcome.type, theme.bw)
                p2 <- subplot(data2, limits2, labels2, colors2, main2, outcome.type, theme.bw)
                p3 <- subplot(data3, limits3, labels3, colors3, main3, outcome.type, theme.bw)
                if (legend.pos != "none") {
                    suppressWarnings(g <- ggplotGrob(p1 + theme(legend.position="bottom"))$grobs)
                    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
                    suppressWarnings(grid.arrange(arrangeGrob(p1 + theme(legend.position="none"), p2 + theme(legend.position="none"),
                                     p3 + theme(legend.position="none"), legend, nrow = 4, heights = c (1, 1, 1, 1/5)),
                                     top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2))))
                } else {
                    suppressWarnings(grid.arrange(p1 + theme(legend.position="none"),
                                         p2 + theme(legend.position="none"),
                                         p3 + theme(legend.position="none"),
                                         top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2))))
                }

            }
        }    ## end of raw plot
    
    }   ############# Treatment Status ###############
    else if (type=="treat") {
        
        if (is.null(xlab)==TRUE) {
            xlab <- index[2]
        } else if (xlab == "") {
            xlab <- NULL
        }
        if (is.null(ylab)==TRUE) {
            ylab <- index[1]
        } else if (ylab == "") {
            ylab <- NULL
        }

        if (is.null(main)==TRUE) {
            if (ignore.treat == 0) {
                main <- "Treatment Status"
            } else {
                main <- "Missing Values"
            }
        } else if (main == "") {
            main <- NULL
        }
        
        ## N <- dim(m)[2]
        units <- rep(rev(1:N), each = TT)
        period <- rep(1:TT, N)


        ## replicate data
        m <- as.matrix(obs.missing[show,])
        all <- unique(na.omit(c(m)))
        col <- breaks <- label <- NULL 

        ## set breaks, colors and labels
        
        if (d.bi == FALSE && ignore.treat == 0) { ## >2 treatment level

            tr.col <- c("#66C2A5","#FC8D62","#8DA0CB","#E78AC3","#A6D854","#FFD92F","#E5C494",
                "#FAFAD2", "#ADFF2F", "#87CEFA", "#1874CD", "#00008B")

            if (treat.type == "discrete") {
                for (i in 1:n.levels) {
                    breaks <- c(breaks, d.levels[i])
                    label <- c(label, paste("Treatment level: ", d.levels[i], sep = ""))
                }
                col <- tr.col[1:n.levels]                

            } else {
                cat("Continuous treatment.\n")
                col <- c("#87CEEB", "#00008B")
                label <- "Treatment Levels"
            }
            # # missing values
            # if (-200 %in% all) {
            #     col <- c(col,"#FFFFFF")
            #     breaks <- c(breaks, -200)
            #     label <- c(label,"Missing")
            # }

        } else { ## binary treatment indicator

            if (0 %in% all) { ## have pre and post: general DID type data
                
                ## control
                if (-1 %in% all) {
                    col <- c(col,"#B0C4DE")
                    breaks <- c(breaks, -1)
                    label <- c(label,"Controls")
                }
                
                ## treated pre
                col <- c(col,"#4671D5")
                breaks <- c(breaks, 0)
                label <- c(label,"Treated (Pre)")
                
                ## treated post
                if (1 %in% all) {
                    col <- c(col,"#06266F")
                    breaks <- c(breaks, 1)
                    label <- c(label,"Treated (Post)")
                }

            } else { # do not have pre and post

                ## control
                if (-1 %in% all) {
                    col <- c(col,"#B0C4DE")
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
                    col <- c(col,"#06266F")
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
                    co.seq <- which(unit.type == 1)
                    tr.seq <- setdiff(1:N, co.seq)
                    dataT0 <- cbind.data.frame(tr.seq, T0, co.total)
                    names(dataT0) <- c("id", "T0", "co.total")
                    dataT0 <- dataT0[order(dataT0[, "T0"], dataT0[, "co.total"]),]
                    tr.seq <- dataT0[,"id"]
                    missing.seq <- c(tr.seq, co.seq)

                    m <- as.matrix(m[,missing.seq])
                    id <- id[missing.seq]

                }
                
            }

        }

        ## user-defined color setting and legend
        if (!is.null(color)) {
            if (treat.type == "discrete") { ## discrete treatment indicator
                if (length(col) == length(color)) {
                    cat(paste("Specified colors in the order of: ", paste(label, collapse = ", "), ".\n", sep = ""))
                    col <- color
                } else {
                    stop(paste("Length of \"color\" should be equal to ",length(col),".\n", sep=""))
                }
            } else {
                if (length(color) != 2) {
                    stop(paste("Length of \"color\" should be equal to ",length(col),".\n", sep=""))
                } else {
                    col <- color
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
            } else {
                if (length(legend.labs) != 1) {
                    warning("The length of label should be equal to 1.\n")
                } else {
                    label <- legend.labs
                }
            }
        } 

        ## start plot 
        res <- c(m)
        data <- cbind.data.frame(units=units, period=period, res=res)

        ## if (treat.type == "discrete") {
        ##     data[,"res"] <- as.factor(data[,"res"])
        ## } else {
            data <- na.omit(data)
        # }
        if (treat.type == "discrete") { 
            data[,"res"] <- as.factor(data[,"res"])
        }
        
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
        } else {
            grid.color <- border.color <- background.color <- legend.color <- "grey90"       
        }


        
        id <- rev(id)
        p <- ggplot(data, aes(x = period, y = units,
                              fill = res), position = "identity") 
        

        if (gridOff == FALSE) {
            p <- p + geom_tile(colour=grid.color, size=0.1, stat="identity")
        } else {
            p <- p + geom_tile(stat="identity")
        }

        p <- p + labs(x = xlab, y = ylab, title=main) + theme_bw() 

        if (treat.type == "discrete") {
            p <- p + scale_fill_manual(NA, breaks = breaks, values = col, labels=label)
        } else {
            p <- p + scale_fill_gradient(low = col[1], high = col[2]) + guides(fill=guide_legend(title= label))
        }

        p <- p +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(fill=NA,color=border.color, size=0.5, linetype="solid"),
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
              legend.margin = margin(c(0, 5, 5, 0)),
              legend.text = element_text(margin = margin(r = 10, unit = "pt"), size = cex.legend),
              plot.title = element_text(size=cex.main, hjust = 0.5,face="bold",margin = margin(8, 0, 8, 0)))
                      

        if (treat.type == "discrete") {
            p <- p + theme(legend.title=element_blank())
        }

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
        
        if(length(all) >= 4 && length(all) < 6) {
            p <- p + guides(fill=guide_legend(nrow=2,byrow=TRUE))
        }
        suppressWarnings(print(p))
        ## end of missing plot




    }############# Treatment Status and Outcome ###############
    else if (type == "bivar" || type == "bivariate") {

        ## line, bar, or connedted line
        if (length(style) == 0) { 
            if (treat.type == "discrete" & outcome.type == "continuous") { 
                ystyle <- "l"
                Dstyle <- "b" 
            }
            else if (treat.type == "discrete" & outcome.type == "discrete") {
                ystyle <- "b"
                Dstyle <- "b"
            }
            else if (treat.type == "continuous" & outcome.type == "discrete") { 
                ystyle <- "b"
                Dstyle <- "l"
            }
            else if (treat.type == "continuous" & outcome.type == "continuous") {
                ystyle <- "l"
                Dstyle <- "l"
            }
        } 
        else {
            if (length(style) == 2) {
                ystyle <- style[1]
                Dstyle <- style[2]
            }
            else if (length(style) == 1) {
                ystyle <- style[1]
                Dstyle <- style[1]
            }
            else if (length(style) > 2) {
                stop("Length of \"style\" should not be larger than 2.\n")
            }
        }


        ## axes labels
        if (is.null(xlab)==TRUE) {
            xlab <- Dname
        } else if (xlab == "") {
            xlab <- NULL
        }
        if (is.null(ylab)==TRUE) {
            ylab <- Yname
        } else if (ylab == "") {
            ylab <- NULL
        }


        ## plot color setting
        raw.color <- NULL
        ## color setting 
        if (is.null(color)==TRUE) { #not indicate color
            if (theme.bw == FALSE) { # not theme.bw (black and white theme)
                    raw.color <- c("dodgerblue4", "lightsalmon2") 
            } 
            else { #  theme.bw
                    raw.color <- c("black","azure4")
            }
        } 
        else { #indicate color    
                if (length(color) != 2) {
                    stop("Length of \"color\" should be equal to 2.\n") 
                }
                else {
                    raw.color <- color[c(1,2)]
                    }
        }

            if (treat.type == "continuous") {
                D.old <- D
            }

            data <- cbind.data.frame("time" = rep(time[show], N),
                                    "outcome" = c(Y[show,]), 
                                    "treatment"= c(D.old[show,]),
                                    "id" = rep(1:N,each = nT),
                                    "input.id" = rep(input.id, each = nT))
            colnames(data) <- c("time","outcome","treatment","id","input.id")



        if (by.unit == FALSE) { #Plot average time series for all units:

            if (Dstyle == "bar" | Dstyle == "b") {
                geom_D <- geom_col(aes(y=(treatment * coeff[2L]) + coeff[1L], fill=xlab),alpha=0.3)
            }
            else if (Dstyle == "line" | Dstyle == "l") {
                geom_D <- geom_line( aes(y=(treatment * coeff[2L]) + coeff[1L], color=xlab), size=lwd)
            }
            else if (Dstyle == "connected" | Dstyle == "c") {
                geom_D <- geom_line( aes(y=(treatment * coeff[2L]) + coeff[1L], color=xlab), size=lwd)
                geom_Dc <- geom_point(aes(y=(treatment * coeff[2L]) + coeff[1L], x=time, color=xlab))
            }

            if (ystyle == "bar" | ystyle == "b") {
                geom_y <- geom_col(aes(y=outcome, fill=ylab),alpha=0.3)
            }
            else if (ystyle == "line" | ystyle == "l") {
                geom_y <- geom_line( aes(y=outcome, color=ylab), size=lwd)
            }
            else if (ystyle == "connected" | ystyle == "c") {
                geom_y <- geom_line( aes(y=outcome, color=ylab), size=lwd)
                geom_yc <- geom_point(aes(y=outcome, x=time, color=ylab))
            }

            data.means <- aggregate(data[, 2:3], list(data$time), mean, na.rm = TRUE)
            colnames(data.means) <- c("time","outcome","treatment")

            p <- ggplot(na.omit(data.means), aes(x=time))
            
            if (theme.bw == TRUE) {
                p <- p + theme_bw()
            }


            p <- p + theme(legend.position = legend.pos, aspect.ratio = 1/2,
                    axis.text.x = element_text(angle = angle, hjust=x.h, vjust=x.h),
                    plot.title = element_text(size=cex.main, hjust = 0.5, face="bold",margin = margin(8, 0, 8, 0)))

                        
            if (is.null(ylim) == TRUE) {
                ylim <- c(min(data.means$outcome,na.rm=TRUE),max(data.means$outcome,na.rm=TRUE))

                coeff <- as.numeric(solve(
                a=matrix(c(1,max(data.means$treatment, na.rm= TRUE),1,min(data.means$treatment, na.rm= TRUE)),nrow=2,ncol=2,byrow=TRUE),
                b=matrix(c(max(data.means$outcome, na.rm= TRUE),min(data.means$outcome, na.rm= TRUE)),ncol=1)))
            }
            else {
                ylim.prim <- ylim[[1]]
                ylim.sec <- ylim[[2]]

                coeff <- as.numeric(solve(
                a=matrix(c(1,max(ylim.sec[2]),1,min(ylim.sec[1])),
                        nrow=2,ncol=2,byrow=TRUE),
                b=matrix(c(max(ylim.prim[2]),min(ylim.prim[1])),ncol=1)))

                ylim <- ylim.prim  
            }
        

        if ((ystyle == "line" | ystyle == "l") & (Dstyle == "line" | Dstyle == "l")) { #ll
                p <- p + geom_y +
                        geom_D + 
                        scale_y_continuous(sec.axis = sec_axis(~(. - coeff[1L]) / coeff[2L], name=xlab),
                        expand = c (0.1, 0.2)) + 
                        scale_colour_manual(values = raw.color) + 
                        labs(y=ylab, x = "", colour = "") 
                }
        else if ((ystyle == "bar" | ystyle == "b") & (Dstyle == "bar" | Dstyle == "b")) { #bb
                p <- p + geom_y +
                        geom_D + 
                        scale_y_continuous(sec.axis = sec_axis(~(. - coeff[1L]) / coeff[2L], name=xlab),
                        expand = c (0.1, 0.2)) + 
                        scale_fill_manual(values = rev(raw.color)) + 
                        labs(y=ylab, x = "", colour = "", fill = "") +
                        coord_cartesian(default = TRUE, ylim=ylim)
                }
        else if ((ystyle == "connected" | ystyle == "c") & (Dstyle == "connected" | Dstyle == "c")) { #cc
                p <- p + geom_y + geom_yc +
                        geom_D + geom_Dc +
                        scale_y_continuous(sec.axis = sec_axis(~(. - coeff[1L]) / coeff[2L], name=xlab),
                        expand = c (0.1, 0.2)) + 
                        scale_colour_manual(values = rev(raw.color)) + 
                        labs(y=ylab, x = "", colour = "") 
                } 
        else if ((ystyle == "line" | ystyle == "l") & (Dstyle == "connected" | Dstyle == "c")) { #lc
                p <- p + geom_y +
                        geom_D + geom_Dc +
                        scale_y_continuous(sec.axis = sec_axis(~(. - coeff[1L]) / coeff[2L], name=xlab),
                        expand = c (0.1, 0.2)) + 
                        scale_colour_manual(values = rev(raw.color)) + 
                        labs(y=ylab, x = "", colour = "") +
                        guides(colour = guide_legend(override.aes = list(shape = c(16, NA))))
                
                }
        else if ((ystyle == "connected" | ystyle == "c") & (Dstyle == "line" | Dstyle == "l")) { #cl
                p <- p + geom_y + geom_yc +
                        geom_D +
                        scale_y_continuous(sec.axis = sec_axis(~(. - coeff[1L]) / coeff[2L], name=xlab),
                        expand = c (0.1, 0.2)) + 
                        scale_colour_manual(NULL, values = rev(raw.color)) + 
                        labs(y=ylab, x = "", colour = "") +
                        guides(colour = guide_legend(override.aes = list(shape = c(NA, 16))))
                }
        else if ((ystyle == "line" | ystyle == "l") & (Dstyle == "bar" | Dstyle == "b")) { #lb
                p <- p + geom_y +
                        geom_D + 
                        scale_y_continuous(sec.axis = sec_axis(~(. - coeff[1L]) / coeff[2L], name=xlab),
                        expand = c (0.1, 0.2)) + 
                        scale_colour_manual(values = raw.color[1]) +
                        scale_fill_manual(values = raw.color[2]) + 
                        labs(y=ylab, x = "", colour = "", fill = "") +
                        coord_cartesian(default = TRUE, ylim=ylim)
                }
        else if ((ystyle == "bar" | ystyle == "b") & (Dstyle == "line" | Dstyle == "l")) { #bl
                p <- p + geom_y +
                        geom_D + 
                        scale_y_continuous(sec.axis = sec_axis(~(. - coeff[1L]) / coeff[2L], name=xlab),
                        expand = c (0.1, 0.2)) + 
                        scale_colour_manual(values = raw.color[1]) +
                        scale_fill_manual(values = raw.color[2]) + 
                        labs(y=ylab, x = "", colour = "", fill = "") +
                        coord_cartesian(default = TRUE, ylim=ylim)
                }
        else if ((ystyle == "bar" | ystyle == "b") & (Dstyle == "connected" | Dstyle == "c")) { #bc
                p <- p + geom_y +
                        geom_D + geom_Dc +
                        scale_y_continuous(sec.axis = sec_axis(~(. - coeff[1L]) / coeff[2L], name=xlab),
                        expand = c (0.1, 0.2)) + 
                        scale_colour_manual(values = raw.color[1]) +
                        scale_fill_manual(values = raw.color[2]) + 
                        labs(y=ylab, x = "", colour = "", fill = "") +
                        coord_cartesian(default = TRUE, ylim=ylim)
                }
        else if ((ystyle == "connected" | ystyle == "c") & (Dstyle == "bar" | Dstyle == "b")) { #cb
                p <- p + geom_y + geom_yc +
                        geom_D + 
                        scale_y_continuous(sec.axis = sec_axis(~(. - coeff[1L]) / coeff[2L], name=xlab),
                        expand = c (0.1, 0.2)) + 
                        scale_colour_manual(values = raw.color[1]) +
                        scale_fill_manual(values = raw.color[2]) + 
                        labs(y=ylab, x = "", colour = "", fill = "") +
                        coord_cartesian(default = TRUE, ylim=ylim)
                }
        }





        else if (by.unit == TRUE) { #Plot by each unit:

            p <- ggplot(na.omit(data), aes(x=time))

            if (theme.bw == TRUE) {
                p <- p + theme_bw()
            }

            p <- p + theme(legend.position = legend.pos,
                    axis.text.x = element_text(angle = 90, hjust=x.h, vjust=0.5),
                    plot.title = element_text(size=cex.main, hjust = 0.5, face="bold",margin = margin(8, 0, 8, 0)))

            if (is.null(ylim) == TRUE) {
                ylim <- c(min(data$outcome,na.rm=TRUE),max(data$outcome,na.rm=TRUE))

                coeff <- as.numeric(solve(
                a=matrix(c(1,max(data$treatment, na.rm= TRUE),1,min(data$treatment, na.rm= TRUE)),
                        nrow=2,ncol=2,byrow=TRUE),
                b=matrix(c(max(ylim[2]),min(ylim[1])),ncol=1)))
            }
            else {
                ylim.prim <- ylim[[1]]
                ylim.sec <- ylim[[2]]

                coeff <- as.numeric(solve(
                a=matrix(c(1,max(ylim.sec[2]),1,min(ylim.sec[1])),
                        nrow=2,ncol=2,byrow=TRUE),
                b=matrix(c(max(ylim.prim[2]),min(ylim.prim[1])),ncol=1)))

                ylim <- ylim.prim  
            }

            width <- (max(time,na.rm=TRUE)-min(time,na.rm=TRUE))/(length(time)-1)


            if (Dstyle == "bar" | Dstyle == "b") {
                geom_D <- geom_col(aes(y=(treatment * coeff[2L]) + coeff[1L], fill=xlab),alpha=0.3,width = width)
            }
            else if (Dstyle == "line" | Dstyle == "l") {
                geom_D <- geom_line( aes(y=(treatment * coeff[2L]) + coeff[1L], color=xlab), size=lwd, alpha=0.9)
            }
            else if (Dstyle == "connected" | Dstyle == "c") {
                geom_D <- geom_line( aes(y=(treatment * coeff[2L]) + coeff[1L], color=xlab), size=lwd, alpha=0.9)
                geom_Dc <- geom_point(aes(y=(treatment * coeff[2L]) + coeff[1L], x=time, color=xlab))
            }

            if (ystyle == "bar" | ystyle == "b") {
                geom_y <- geom_col(aes(y=outcome, fill=ylab),alpha=0.3,width = width)
            }
            else if (ystyle == "line" | ystyle == "l") {
                geom_y <- geom_line( aes(y=outcome, color=ylab), size=lwd, alpha=0.9)
            }
            else if (ystyle == "connected" | ystyle == "c") {
                geom_y <- geom_line( aes(y=outcome, color=ylab), size=lwd, alpha=0.9)
                geom_yc <- geom_point(aes(y=outcome, x=time, color=ylab))
            }


            if (treat.type == "discrete" & outcome.type == "continuous") { 
                scale_y_conti <- scale_y_continuous(sec.axis = sec_axis(~(. - coeff[1L]) / coeff[2L], name=xlab, 
                        breaks = 0:(length(d.levels)-1), labels = d.levels),
                        expand = c (0.1, 0.2))
            }
            else if (treat.type == "discrete" & outcome.type == "discrete") {
                scale_y_conti <- scale_y_continuous(sec.axis = sec_axis(~(. - coeff[1L]) / coeff[2L], name=xlab, 
                        breaks = 0:(length(d.levels)-1), labels = d.levels),
                        breaks = 0:(length(y.levels)-1), labels = y.levels,
                        expand = c (0.1, 0.2))
            }
            else if (treat.type == "continuous" & outcome.type == "discrete") { 
                scale_y_conti <- scale_y_continuous(sec.axis = sec_axis(~(. - coeff[1L]) / coeff[2L], name=xlab),
                        breaks = 0:(length(y.levels)-1), labels = y.levels,
                        expand = c (0.1, 0.2))
            }
            else if (treat.type == "continuous" & outcome.type == "continuous") {
                scale_y_conti <- scale_y_continuous(sec.axis = sec_axis(~(. - coeff[1L]) / coeff[2L], name=xlab),
                        expand = c (0.1, 0.2))
            }
            


            if ((ystyle == "line" | ystyle == "l") & (Dstyle == "line" | Dstyle == "l")) { 
                #ll
                p <- p + geom_y +
                        geom_D + 
                        scale_y_conti + 
                        scale_colour_manual(values = raw.color) +
                        labs(y=ylab, x = "", colour = "") +
                        facet_wrap(~input.id, ncol = 4)
                }
            else if ((ystyle == "bar" | ystyle == "b") & (Dstyle == "bar" | Dstyle == "b")) { 
                #bb
                p <- p + geom_y +
                        geom_D + 
                        scale_y_conti + 
                        scale_fill_manual(values = rev(raw.color)) + 
                        labs(y=ylab, x = "", colour = "", fill = "") +
                        facet_wrap(~input.id, ncol = 4) +
                        coord_cartesian(default = TRUE, ylim=ylim)
                }
            else if ((ystyle == "connected" | ystyle == "c") & (Dstyle == "connected" | Dstyle == "c")) {
                 #cc
                p <- p + geom_y + geom_yc +
                        geom_D + geom_Dc +
                        scale_y_conti + 
                        scale_colour_manual(values = rev(raw.color)) + 
                        labs(y=ylab, x = "", colour = "") +
                        facet_wrap(~input.id, ncol = 4)
                } 
            else if ((ystyle == "line" | ystyle == "l") & (Dstyle == "connected" | Dstyle == "c")) { 
                #lc
                p <- p + geom_y +
                        geom_D + geom_Dc +
                        scale_y_conti + 
                        scale_colour_manual(values = rev(raw.color)) + 
                        labs(y=ylab, x = "", colour = "") +
                        guides(colour = guide_legend(override.aes = list(shape = c(16, NA)))) +
                        facet_wrap(~input.id, ncol = 4)
                
                }
            else if ((ystyle == "connected" | ystyle == "c") & (Dstyle == "line" | Dstyle == "l")) { 
                #cl
                p <- p + geom_y + geom_yc +
                        geom_D +
                        scale_y_conti + 
                        scale_colour_manual(NULL, values = rev(raw.color)) + 
                        labs(y=ylab, x = "", colour = "") +
                        guides(colour = guide_legend(override.aes = list(shape = c(NA, 16)))) +
                        facet_wrap(~input.id, ncol = 4)
                }
                else if ((ystyle == "line" | ystyle == "l") & (Dstyle == "bar" | Dstyle == "b")) { 
                #lb
                p <- p + geom_y +
                        geom_D + 
                        scale_y_conti + 
                        scale_colour_manual(values = raw.color[1]) +
                        scale_fill_manual(values = raw.color[2]) + 
                        labs(y=ylab, x = "", colour = "", fill = "") +
                        facet_wrap(~input.id, ncol = 4) +
                        coord_cartesian(default = TRUE, ylim=ylim)
                }
                else if ((ystyle == "bar" | ystyle == "b") & (Dstyle == "line" | Dstyle == "l")) { 
                #bl
                p <- p + geom_y +
                        geom_D + 
                        scale_y_conti + 
                        scale_colour_manual(values = raw.color[1]) +
                        scale_fill_manual(values = raw.color[2]) + 
                        labs(y=ylab, x = "", colour = "", fill = "") +
                        facet_wrap(~input.id, ncol = 4)
                }
                else if ((ystyle == "bar" | ystyle == "b") & (Dstyle == "connected" | Dstyle == "c")) { 
                #bc
                p <- p + geom_y +
                        geom_D + geom_Dc +
                        scale_y_conti + 
                        scale_colour_manual(values = raw.color[1]) +
                        scale_fill_manual(values = raw.color[2]) + 
                        labs(y=ylab, x = "", colour = "", fill = "") +
                        facet_wrap(~input.id, ncol = 4)
                }
                else if ((ystyle == "connected" | ystyle == "c") & (Dstyle == "bar" | Dstyle == "b")) { 
                #cb
                p <- p + geom_y + geom_yc +
                        geom_D + 
                        scale_y_conti + 
                        scale_colour_manual(values = raw.color[1]) +
                        scale_fill_manual(values = raw.color[2]) + 
                        labs(y=ylab, x = "", colour = "", fill = "") +
                        facet_wrap(~input.id, ncol = 4) +
                        coord_cartesian(default = TRUE, ylim=ylim)
                }
            
        }
        
            if (!is.numeric(time.label)) {
                p <- p + 
                    scale_x_continuous(expand = c(0, 0), breaks = show[T.b], labels = time.label[T.b])
            }
            
            ## title
            if ("main" != "") {
                p <- p + ggtitle(main)
            }

            ## ylim
            if (is.null(ylim) == FALSE) {
                p <- p + coord_cartesian(ylim = ylim)
            }
            
            suppressWarnings(print(p))
        }
    
}