.pv_plot_treat <- function(s) {
    with(s, {


        if (is.null(xlab)==TRUE) {
            xlab <- index[2]
        } else if (xlab == "") {
            xlab <- NULL
        }
        if (is.null(ylab)==TRUE) {
            ylab <- index[1]
            if (collapse.history == TRUE) {
                ylab <- "Number of Units"
            }
        } else if (ylab == "") {
            ylab <- NULL
        }

        if (is.null(main)==TRUE) {
            if (collapse.history == TRUE) {
                main <- "Unique Treatment Histories"
            } else {
                if (ignore.treat == 0) {
                    main <- "Treatment Status"
                } else {
                    main <- "Missing Values"
                }
            }
        } else if (main == "") {
            main <- NULL
        }

        ## cat(N)

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
                    #label <- c(label, paste("Treatment level: ", d.levels[i], sep = ""))
                    label <- c(label, paste(d.levels[i], sep = ""))
                }
                col <- tr.col[1:n.levels]                

            } else {
                cat("Continuous treatment.\n")
                #col <- c("#87CEEB", "#00008B")
                interval <- (max(d.levels)-min(d.levels))/4
                m[m >= min(d.levels) & m < min(d.levels)+interval] <- min(d.levels)
                m[m >= min(d.levels)+interval & m < min(d.levels)+2*interval] <- min(d.levels)+interval
                m[m >= min(d.levels)+2*interval & m < min(d.levels)+3*interval] <- min(d.levels)+2*interval
                m[m >= min(d.levels)+3*interval & m < min(d.levels)+4*interval] <- min(d.levels)+3*interval
                m[m >= max(d.levels)] <- max(d.levels)
                
                breaks <- c(min(d.levels), min(d.levels)+interval, min(d.levels)+2*interval, min(d.levels)+3*interval, max(d.levels))
                col <- c("#c6dbef","#4292c6", "#1f78b4", "#08519c", "#042b53")
                #label <- "Treatment Levels"
                for (i in 1:length(breaks)) {
                    label <- c(label, paste(breaks[i], sep = ""))
                }
                treat.type <- "discrete"
            }

             # missing values
            if (-200 %in% all) {
                col <- c(col,"#FFFFFF")
                breaks <- c(breaks, -200)
                label <- c(label,"Missing")
            }

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
                    co.seq <- which(unit.type == 1) ## unit.type: 1 for control; 2 for treated; 3 for reversal
                    tr.seq <- setdiff(1:N, co.seq)
                    dataT0 <- cbind.data.frame(tr.seq, T0, co.total) 
                    names(dataT0) <- c("id", "T0", "co.total")
                    dataT0 <- dataT0[order(dataT0[, "T0"], dataT0[, "co.total"], dataT0[, "id"]),] ## order of by.timing

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
                } 
                else { 
                    stop(paste("Length of \"color\" should be equal to ",length(col),".\n", sep=""))
                }
            } 
            #else {
            #    if (length(color) != 2) {
             #       stop(paste("Length of \"color\" should be equal to ",length(col),".\n", sep=""))
              #  } else {
               #     col <- color
                #}
            #}
        }       
        
        if (!is.null(legend.labs)) {
            if (treat.type == "discrete") { ## discrete treatment indicator
                if (length(legend.labs) != length(label)) {
                    warning("Incorrect number of labels in the legends. Using default.\n")
                } else {
                    cat(paste("Specified labels in the order of: ", paste(label, collapse = ", "), ".\n", sep = ""))
                    label <- legend.labs
                }
            } 
            #else {
             #   if (length(legend.labs) != 1) {
              #      warning("The length of label should be equal to 1.\n")
               # } else {
                #    label <- legend.labs
                #}
            #}
        } 

        ## start plot 
        #if (treat.type == "continuous" && ignore.treat == 0 && leave.gap == 1) {
        #m2 <- NULL
        #m2 <- m
        #m2 <- replace(m2, m2 == -200, NA) # if NA in the first and last period, then this period will disappear
        #res <- c(m2)
        #}
        #else{        
        res <- c(m)
        #}
        
        data <- cbind.data.frame(units=units, period=period, res=res)
      

        if (leave.gap == 0) {
            data <- na.omit(data)
        }

        #if (treat.type == "discrete") { 
            data[,"res"] <- as.factor(data[,"res"])
        #}
        
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

        #if (treat.type == "discrete") {
            p <- p + scale_fill_manual("Treatment level: ", breaks = breaks, values = col, labels=label)
            if (n.levels < 3) {
                p <- p + theme(legend.title=element_blank())
            }
        #} else {
            #p <- p + scale_fill_gradient(low = col[1], high = col[2], na.value="white") + guides(fill=guide_legend(title= label))
        #}

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
        
        if (length(all) >= 4 && length(all) < 6) {
            p <- p + guides(fill=guide_legend(nrow=2,byrow=TRUE))
        }
        suppressWarnings(print(p))
        ## end of missing plot




    })
}

