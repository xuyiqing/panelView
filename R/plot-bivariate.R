.pv_plot_bivariate <- function(s) {
    with(s, {

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
                ylim <- c(min(data.means$outcome, na.rm = TRUE),max(data.means$outcome, na.rm = TRUE))

                coeff <- as.numeric(solve(
                a=matrix(c(1,max(data.means$treatment, na.rm = TRUE),1,min(data.means$treatment, na.rm = TRUE)),nrow=2,ncol=2,byrow=TRUE),
                b=matrix(c(max(data.means$outcome, na.rm = TRUE),min(data.means$outcome, na.rm = TRUE)),ncol=1)))
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
                ylim <- c(min(data$outcome, na.rm = TRUE),max(data$outcome, na.rm = TRUE))

                coeff <- as.numeric(solve(
                a=matrix(c(1,max(data$treatment, na.rm = TRUE),1,min(data$treatment, na.rm = TRUE)),
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

            width <- (max(time, na.rm = TRUE)-min(time, na.rm = TRUE))/(length(time)-1)


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
    })
}

