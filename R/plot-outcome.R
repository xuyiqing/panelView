.pv_subplot <- function(data, limits, colors, main, outcome.type, theme.bw,
                        xlab, ylab, angle, x.h, cex.main, raw.color,
                        show, T.b, time.label, ylim, set.labels) {
                time <- outcome <- type <- id <- NULL  # suppress R CMD check note
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

.pv_plot_outcome <- function(s) {
    with(s, {
        
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
        if (is.null(color) == TRUE) {
            if (ignore.treat == FALSE) {
                if (outcome.type == "continuous") {
                    raw.color <- c("#5e5e5e50", "#FC8D62", "red")
                } else {
                    raw.color <- c("#5e5e5e60", "#FC8D62", "red")
                }
                if (type == "outcome" && (staggered == 0 | by.group == TRUE | pre.post == FALSE)) { # two conditions only
                    raw.color <- raw.color[c(1,3)]
                }
            } else { # ignore treat
                raw.color <- "#5e5e5e50"
            }
        } else {    # color is specified  
            if (ignore.treat == FALSE) {
                if (staggered == 0 | pre.post == FALSE) { # with reversals or two groups only
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

        #####################    
        ## prepare to plot
        #####################

        if (ignore.treat == TRUE) { # do not show treatment status

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
            } else if (main != "") {
                p <- p + ggtitle(main)
            }

            ## ylim
            if (is.null(ylim) == FALSE) {
                p <- p + coord_cartesian(ylim = ylim)
            }
            
            suppressWarnings(print(p))

        } # ignore.treat == TRUE over
        else if (ignore.treat == FALSE && by.group == FALSE) { ## Mixed units
            
            ## time-line
            if (outcome.type == "continuous") { ## continuous outcome
                
                if (staggered == 0 || (by.cohort == FALSE && pre.post == FALSE)) { ## with reversals
                
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
                    set.colors = raw.color
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
                else { ## staggered 

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
                                            "id" = c(rep(1:N,each = nT), id.tr.pst + N0)) # post seen as different groups
                    idtimes <- sapply(1:length(data$id),function(x)sum(data$id[1:x]==data$id[x]))
                    data <- cbind(data, idtimes)
                    data$idtimes <- ave(data$idtimes, data$id, FUN=max)
                    data$last_dot <- 0
                    data$last_dot[data$idtimes == 1] <- 1
                     ## legend 
                    set.limits = c("co", "tr", "tr.pst")
                    set.colors = raw.color
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
                    
                    if (by.cohort == TRUE) {
                        # expand to balanced panel:
                        ref <- expand.grid(id = unique(data.old[,index.id]), time = unique(data.old[,index.time]))
                        data.old <- merge(ref, data.old, by = 1:2, all.x = TRUE)
                        colnames(data.old)[4] <- "treatment"
                        data.old$treatment<- ave(data.old$treatment, data.old$id, FUN=function(x) approxfun(x, method = "constant", rule=2)(seq_along(x)))
                        #1. If a person's first follow-up data is missing, then add the value of the next row;
                        #2. If a person's non-first follow-up data is missing, then add the value of the previous row;
                        #3. If multiple consecutive follow-up data are missing, then add the value of the previous non-missing row.

                        data.old$treatment_history <- ave(data.old[,"treatment"], data.old$id, FUN = function(x) paste(x, collapse= "_")) # data.old[,4]: treatment; data.old[,1]: id
                        cat(paste0("Number of unique treatment histories: ", length(unique(data.old$treatment_history))))
                        cat("\n")
                        #print(data.old)
                        #print(unique(data.old$treatment_history))

                        if (length(unique(data.old$treatment_history)) > 20) {
                            stop("\"by.cohort = TRUE\" ignored the number of unique treatment history is more than 20.")
                        }
                        else {
                            data.old$outcomehistorymean <- ave(data.old[,3], data.old$treatment_history, data.old$time, FUN=function(x) mean(x, na.rm=TRUE)) # data.old[,3]: outcome

                            data.old <- data.old[,c("time", "treatment", "treatment_history", "outcomehistorymean")]

                            names(data.old)[names(data.old) == 'outcomehistorymean'] <- 'outcome'
                            names(data.old)[names(data.old) == 'treatment_history'] <- 'id'
                            #data.old <- data.old[!duplicated(data.old), ]
                            N_cohort <- length(sort(unique(data.old$id))) 

                            #group id and time to numeric values:
                            data.old[,3] <- as.numeric(as.factor(data.old[,3]))
                            data.old[,1] <- as.numeric(as.factor(data.old[,1]))

                            Y <- matrix(NA, TT, N_cohort) 
                            for (i in 1:dim(data.old)[1]) {
                                Y[data.old[i,1],data.old[i,3]] <- data.old[i,4] # data.old[,1]: time; data.old[,3]: id; data.old[,4]: outcome
                            }

                            D <- matrix(0, TT, N_cohort)
                            for (i in 1:dim(data.old)[1]) {
                                D[data.old[i,1],data.old[i,3]] <- data.old[i,2] # data.old[,2]: treatment
                            }

                            tr <- D[TT,] == 1
                            Ntr <- sum(tr)
                            Nco <- N_cohort - Ntr
                            Y.tr <- Y.co <- NULL
                            Y.tr <- as.matrix(Y[,which(tr==1)])
                            Y.co <- as.matrix(Y[,which(tr==0)])
                            tr.pos <- which(D[TT,] == 1) ## which units are treated
                            T1 <- apply(D == 1, 2, sum, na.rm = TRUE)[tr.pos] ## number of periods expose to treatment 
                            T1[which(T1 > 1)] <- 0 ## indicate the last dot of treatment status change
                            pst <- as.matrix(D[,which(tr==1)]) # treatment matrix
                                                                              


                            time.pst <- c(pst[show,] * time[show])
                            time.pst <- time.pst[which(c(pst[show,])==1)]
                            Y.tr.pst <- c(Y.tr[show,])[which(pst[show,]==1)]
                            id.tr.pst <- matrix(rep(1:Ntr,each=TT),TT,Ntr,byrow=FALSE)[show,]
                            id.tr.pst <- c(id.tr.pst)[which(pst[show,]==1)]
                            T1_0 <- c(T1)[which(T1==0)] 
                            T1_1 <- c(T1)[which(T1==1)] #last dot of treatment status change
                            N_T1_1 <- sum(T1_1)
                            N_T1_0 <- Nco*nT + Ntr*nT + length(Y.tr.pst) - N_T1_1                        
                        
                            if (pre.post == TRUE) {
                                data <- cbind.data.frame("time" = c(rep(time[show], N_cohort), time.pst),
                                                    "outcome" = c(c(Y.tr[show,]),
                                                                c(Y.co[show,]),
                                                                Y.tr.pst),
                                                    "type" = c(rep("tr",(Ntr*nT)),
                                                                rep("co",(Nco*nT)),
                                                                rep("tr.pst",length(Y.tr.pst))),
                                                    "id" = c(rep(1:N_cohort,each = nT), id.tr.pst + N0))
                            } else {
                                tr.vec <- rep("co", nT * N_cohort)
                                tr.vec[which(c(pst[show,])==1)] <- "tr"
                                data <- cbind.data.frame("time" = c(rep(time[show], N_cohort)),
                                                    "outcome" = c(c(Y.tr[show,]), c(Y.co[show,])),
                                                    "type" = tr.vec,
                                                    "id" = c(rep(1:N_cohort,each = nT)))    
                                ## legend 
                                set.limits = c("co", "tr")
                                set.colors = raw.color[1:2]
                                set.linetypes = c("solid","solid")
                                set.linewidth = c(0.5, 0.5)
                                if (!is.null(legend.labs)) {
                                    if (length(legend.labs) != 2) {
                                        warning("Incorrect number of labels in the legend. Using default.\n")
                                        set.labels <- c("Controls", "Treated")  
                                    } else {
                                        set.labels <- legend.labs
                                    }
                                } else {
                                    set.labels <- c("Controls","Treated") 
                                }
                                labels.ncol <- 2                                                                        
                            }                           
                            # last perioed treated (show using a dot)
                            idtimes <- sapply(1:length(data$id),function(x)sum(data$id[1:x]==data$id[x]))
                            data <- cbind(data, idtimes)
                            data$idtimes <- ave(data$idtimes, data$id, FUN=max)
                            data$last_dot <- 0
                            data$last_dot[data$idtimes == 1] <- 1                            
                        }
                    }
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
                    if (exists("time.bf")) {
                        if (time.bf >= min(show) && time.bf <= max(show)) {
                            p <- p + geom_vline(xintercept=time.bf, colour="white", size = 2)
                            if (shade.post == TRUE) {
                                p <- p + annotate("rect", xmin= time.bf, xmax= Inf,
                                                ymin=-Inf, ymax=Inf, alpha = .3) 
                            }                            
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
                if (staggered == 1 && pre.post == TRUE) {
                    
                    time.bf <- time[unique(T0)]
                    set.limits = c("co", "tr", "tr.pst")
                    set.colors = raw.color 
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
                    set.colors = raw.color
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
        
        } else { ## separate plot (by.group == TRUE)
            
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

            if (by.group.side == FALSE) {
                if (length(unique(unit.type)) == 1) {
                    
                    if (1%in%unit.type) {
                        p1 <- .pv_subplot(data1, limits1, colors1, main1, outcome.type, theme.bw,
            xlab, ylab, angle, x.h, cex.main, raw.color, show, T.b, time.label, ylim, set.labels)
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
                        p2 <- .pv_subplot(data2, limits2, colors2, main2, outcome.type, theme.bw,
            xlab, ylab, angle, x.h, cex.main, raw.color, show, T.b, time.label, ylim, set.labels)
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
                        p3 <- .pv_subplot(data3, limits3, colors3, main3, outcome.type, theme.bw,
            xlab, ylab, angle, x.h, cex.main, raw.color, show, T.b, time.label, ylim, set.labels)
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
                        p2 <- .pv_subplot(data2, limits2, colors2, main2, outcome.type, theme.bw,
            xlab, ylab, angle, x.h, cex.main, raw.color, show, T.b, time.label, ylim, set.labels)
                        p3 <- .pv_subplot(data3, limits3, colors3, main3, outcome.type, theme.bw,
            xlab, ylab, angle, x.h, cex.main, raw.color, show, T.b, time.label, ylim, set.labels)
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
                        p1 <- .pv_subplot(data1, limits1, colors1, main1, outcome.type, theme.bw,
            xlab, ylab, angle, x.h, cex.main, raw.color, show, T.b, time.label, ylim, set.labels)
                        p3 <- .pv_subplot(data3, limits3, colors3, main3, outcome.type, theme.bw,
            xlab, ylab, angle, x.h, cex.main, raw.color, show, T.b, time.label, ylim, set.labels)
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
                        p1 <- .pv_subplot(data1, limits1, colors1, main1, outcome.type, theme.bw,
            xlab, ylab, angle, x.h, cex.main, raw.color, show, T.b, time.label, ylim, set.labels)
                        p2 <- .pv_subplot(data2, limits2, colors2, main2, outcome.type, theme.bw,
            xlab, ylab, angle, x.h, cex.main, raw.color, show, T.b, time.label, ylim, set.labels)
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
                    
                    p1 <- .pv_subplot(data1, limits1, colors1, main1, outcome.type, theme.bw,
            xlab, ylab, angle, x.h, cex.main, raw.color, show, T.b, time.label, ylim, set.labels)
                    p2 <- .pv_subplot(data2, limits2, colors2, main2, outcome.type, theme.bw,
            xlab, ylab, angle, x.h, cex.main, raw.color, show, T.b, time.label, ylim, set.labels)
                    p3 <- .pv_subplot(data3, limits3, colors3, main3, outcome.type, theme.bw,
            xlab, ylab, angle, x.h, cex.main, raw.color, show, T.b, time.label, ylim, set.labels)
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
            }
            else if (by.group.side == TRUE) {

                if (length(unique(unit.type)) == 1) {
                    
                    if (1%in%unit.type) {
                        p1 <- .pv_subplot(data1, limits1, colors1, main1, outcome.type, theme.bw,
            xlab, ylab, angle, x.h, cex.main, raw.color, show, T.b, time.label, ylim, set.labels)
                        if (legend.pos != "none") {
                            suppressWarnings(g <- ggplotGrob(p1 + theme(legend.position="bottom"))$grobs)
                            legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
                            suppressWarnings(grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                                            nrow =1, top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2))),legend,nrow=2,heights=c(1,1/8)))
                        } else {
                            suppressWarnings(grid.arrange(p1 + theme(legend.position="none"), nrow =1,
                                            top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2))))
                        }   
                    }
                    else if (2%in%unit.type) {
                        p2 <- .pv_subplot(data2, limits2, colors2, main2, outcome.type, theme.bw,
            xlab, ylab, angle, x.h, cex.main, raw.color, show, T.b, time.label, ylim, set.labels)
                        if (legend.pos != "none") {
                            suppressWarnings(g <- ggplotGrob(p2 + theme(legend.position="bottom"))$grobs)
                            legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
                            suppressWarnings(grid.arrange(arrangeGrob(p2 + theme(legend.position="none"),
                                            nrow =1, top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2))),legend,nrow=2,heights=c(1,1/8)))
                        } else {
                            suppressWarnings(grid.arrange(p2 + theme(legend.position="none"), nrow =1,
                                            top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2))))
                        }   
                    }
                    else if (3%in%unit.type) {
                        p3 <- .pv_subplot(data3, limits3, colors3, main3, outcome.type, theme.bw,
            xlab, ylab, angle, x.h, cex.main, raw.color, show, T.b, time.label, ylim, set.labels)
                        if (legend.pos != "none") {
                            suppressWarnings(g <- ggplotGrob(p3 + theme(legend.position="bottom"))$grobs)
                            legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
                            suppressWarnings(grid.arrange(arrangeGrob(p3 + theme(legend.position="none"),
                                            nrow =1, top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2))),legend,nrow=2,heights=c(1,1/8)))
                        } else {
                            suppressWarnings(grid.arrange(p3 + theme(legend.position="none"), nrow =1,
                                            top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2))))
                        }  
                    }

                }
                else if (length(unique(unit.type))==2) {
                    
                    if (!1%in%unit.type) {
                        p2 <- .pv_subplot(data2, limits2, colors2, main2, outcome.type, theme.bw,
            xlab, ylab, angle, x.h, cex.main, raw.color, show, T.b, time.label, ylim, set.labels)
                        p3 <- .pv_subplot(data3, limits3, colors3, main3, outcome.type, theme.bw,
            xlab, ylab, angle, x.h, cex.main, raw.color, show, T.b, time.label, ylim, set.labels)
                        if (legend.pos != "none") {
                            suppressWarnings(g <- ggplotGrob(p2 + theme(legend.position="bottom"))$grobs)
                            legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
                            suppressWarnings(grid.arrange(arrangeGrob(p2 + theme(legend.position="none"), p3 + theme(legend.position="none"),
                                            nrow =1, top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2))),legend,nrow=2,heights=c(1,1/8)))
                        } else {
                            suppressWarnings(grid.arrange(p2 + theme(legend.position="none"),
                                            p3 + theme(legend.position="none"), nrow =1,
                                            top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2))))
                        }  
                    }
                    else if (!2%in%unit.type) {
                        p1 <- .pv_subplot(data1, limits1, colors1, main1, outcome.type, theme.bw,
            xlab, ylab, angle, x.h, cex.main, raw.color, show, T.b, time.label, ylim, set.labels)
                        p3 <- .pv_subplot(data3, limits3, colors3, main3, outcome.type, theme.bw,
            xlab, ylab, angle, x.h, cex.main, raw.color, show, T.b, time.label, ylim, set.labels)
                        if (legend.pos != "none") {
                            suppressWarnings(g <- ggplotGrob(p1 + theme(legend.position="bottom"))$grobs)
                            legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
                            suppressWarnings(grid.arrange(arrangeGrob(p1 + theme(legend.position="none"), p3 + theme(legend.position="none"),
                                            nrow =1, top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2))),legend,nrow=2,heights=c(1,1/8)))
                        } else {
                            suppressWarnings(grid.arrange(p1 + theme(legend.position="none"),
                                            p3 + theme(legend.position="none"), nrow =1,
                                            top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2))))
                        }  
                    }
                    else if (!3%in%unit.type) {
                        p1 <- .pv_subplot(data1, limits1, colors1, main1, outcome.type, theme.bw,
            xlab, ylab, angle, x.h, cex.main, raw.color, show, T.b, time.label, ylim, set.labels)
                        p2 <- .pv_subplot(data2, limits2, colors2, main2, outcome.type, theme.bw,
            xlab, ylab, angle, x.h, cex.main, raw.color, show, T.b, time.label, ylim, set.labels)
                        if (legend.pos != "none") {
                            suppressWarnings(g <- ggplotGrob(p1 + theme(legend.position="bottom"))$grobs)
                            legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
                            suppressWarnings(grid.arrange(arrangeGrob(p1 + theme(legend.position="none"), p2 + theme(legend.position="none"),
                                            nrow =1, top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2))),legend,nrow=2,heights=c(1,1/8)))
                        } else {
                            suppressWarnings(grid.arrange(p1 + theme(legend.position="none"),
                                            p2 + theme(legend.position="none"), nrow =1,
                                            top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2))))
                        }   
                    }

                }
                else {
                    p1 <- .pv_subplot(data1, limits1, colors1, main1, outcome.type, theme.bw,
            xlab, ylab, angle, x.h, cex.main, raw.color, show, T.b, time.label, ylim, set.labels)
                    p2 <- .pv_subplot(data2, limits2, colors2, main2, outcome.type, theme.bw,
            xlab, ylab, angle, x.h, cex.main, raw.color, show, T.b, time.label, ylim, set.labels)
                    p3 <- .pv_subplot(data3, limits3, colors3, main3, outcome.type, theme.bw,
            xlab, ylab, angle, x.h, cex.main, raw.color, show, T.b, time.label, ylim, set.labels)

                    if (legend.pos != "none") {
                        suppressWarnings(g <- ggplotGrob(p1 + theme(legend.position="bottom"))$grobs)

                        legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

                        suppressWarnings(grid.arrange(arrangeGrob(p1 + theme(legend.position="none"), p2 + theme(legend.position="none"),
                                        p3 + theme(legend.position="none"), nrow =1,
                                        top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2))),legend,nrow=2,heights=c(1,1/8)))
                    } else {
                        suppressWarnings(grid.arrange(p1 + theme(legend.position="none"),
                                            p2 + theme(legend.position="none"),
                                            p3 + theme(legend.position="none"), nrow =1,
                                            top = textGrob(main, gp = gpar(fontsize = cex.main.top,font=2))))
                    }

                }
            }
        }  ## end of (by.group = TRUE or by.group.side = TRUE); END of outcome plot    


    })
}

