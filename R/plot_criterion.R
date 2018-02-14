
#' Plot the response criterion by option position, possibly using a
#' grouping variable (can also be used to plot any other values in
#' dependence of option position and acceptance reluctance)
#' @param tab A data table with rows = criterion by position (row 1:
#'     criterion for option 1) and columns distinguishing different
#'     levels of grouping factor
#' @param ylim Limits of y-axis
#' @param txt.cords A `list`, containing two vectors of x and y
#'     coordinates specifying where the factor level descriptions are
#'     printed
#' @param col A vector of two colors specifying the coloring of two
#'     levels of the factor
#' @param ylab Label of the y-axis
#' @param xlab Label of the x-axis
#' @param labelCriterion boolean value, if `TRUE`, on the right axis the
#'     response criterion is labeled as "conservative", "neutral", and
#'     "liberal"
#' @param add Boolean. Should the data be drawn into an existing plot?
#' @param pch The type of the data points
#' @param lty the type of the lines drawn
#' @param lwd the width of the lines drawn
#' @param plotNeutral Boolean. Should a horizontal line at y=0 be drawn
#'     to illustrate a neutral response criterion?
#' @param cex the size of the dots
#' @param err.bar If the error bars should be plotted, pass a table of
#'     the same dimension as `tab`. Each entry is the length of the
#'     error bar to each side of the point (i.e.: the value should be
#'     half of the width of the confidence interval if a confidence
#'     interval is to be plotted)
#' @param type The type of plot, see `?plot`
#' @param txt A vector of text describing the levels of the grouping
#'     variable
#' @param labelx Vector to label the ticks on the x-axis
#' @param dev A single value indicating the margin by which data points
#'     on the same x-axis point should be separated to better illustrate
#'     different grouping points
#' @param main The caption of the plot
#' @param lwd_arrow The thickness of the arrow illustrating liberal
#'     vs. conservative responses
#' 
#' @export
#' 

plot_criterion <- function(tab, ylim=c(-1, 1),
                          txt.cords =list(c(1, 0.3), c(2.8, 0.35)),
                          col = c("black", "red"),
                          xlab = "Position of answer", 
                          ylab ="Response criterion c",
                          labelCriterion=TRUE, add = FALSE,
                          pch=c(16, 15), lty=1, lwd=1.3,
                          plotNeutral=TRUE,
                          cex=1.4, err.bar = NULL, type="b",
                          txt = paste(colnames(tab)),
                          labelx = TRUE, dev = 0.07, main="",
                          lwd_arrow = 1) {

    levels <- colnames(tab)
    if (!add) {
        # ‘c(bottom, left, top, right)’ for margin
        default.mar <- c(5.1, 4.1, 4.1, 2.1)
        par(mar = default.mar + c(0, 0, -1, 0))
        if (labelCriterion) par(mar = default.mar + c(0, 0, -1, 4))
        
        plot(1:nrow(tab)-dev, tab[,levels[1]], las=1, ylim=ylim,
             col= "transparent", las=1, type=type, ylab=ylab,
             xaxt = "n", xlab=xlab, xlim=c(1-0.2, nrow(tab)+0.2),
             pch=pch[1], cex=cex, lty=lty, lwd=lwd, main=main)
        plot.err.bar(err.bar, tab, dev, col)
        points(1:nrow(tab)-dev, tab[,levels[1]], col=col[1], type=type,
               pch = pch[1], cex=cex, lwd=lwd)
        points(1:nrow(tab)+dev, tab[,levels[2]], col=col[2], type=type,
               pch = pch[2], cex=cex, lwd=lwd)

        axis(side=1, at = 1:nrow(tab), labels = labelx)
        if (labelCriterion) {
            axis(side=4, at = c(ylim[1], 0, ylim[2]),
                 labels=c("liberal", "neutral", "conservative"),
                 las = 1, tick=FALSE)
            # draw arrows next to the the plot:
            normal.xpd <- par()$xpd
            par(xpd=NA)
            arrows(nrow(tab)+0.6, c(0.05, -0.05), nrow(tab)+0.6,
                   c(ylim[2]-0.05, ylim[1]+0.05), lwd=lwd_arrow,
                   length=0.2)
            par(xpd=normal.xpd) # reverse
        }
        if (plotNeutral) abline(h = 0, lwd=1, lty=3)
        text(txt.cords[[1]][1], txt.cords[[1]][2], txt[1], pos = 4,
             col = col[1])
        text(txt.cords[[2]][1], txt.cords[[2]][2], txt[2], pos = 4,
             col = col[2])
        on.exit(par(mar = default.mar))
    } else {
        plot.err.bar(err.bar, tab, dev, col)
        points(1:nrow(tab)-dev, tab[,levels[1]], col=col[1], type=type,
               pch=pch[1], cex=cex, lty=lty, lwd=lwd)
        points(1:nrow(tab)+dev, tab[,levels[2]], col=col[2], type=type,
               pch = pch[2], cex=cex, lty=lty, lwd=lwd)
    }
}

#' Compute standard error
#'
#' @param x a vector of values
#' @param na.rm Boolean. Remove NA values?
#' 
#' @export
#'
se <- function(x, na.rm = FALSE) {
    if (na.rm) x <- x[!is.na(x)]
    return(sd(x)/sqrt(length(x)))
}

#' Compute the half confidence interval of a vector
#'
#' @param ci the confidence level as a natural number (e.g., 95)
#' @param x a vector of values
#' @param na.rm Boolean. Remove NA values?
#' 
#' @export
#'
ci <- function(ci, x, na.rm = FALSE) {
    if (na.rm) x <- x[!is.na(x)]
    alpha <- (100 - ci)/100
    t <- qt(1-alpha/2, length(x)-1)
    return(t*se(x))
}

#' Compute the half of the 95\% confidence interval of a vector
#'
#' @param x a vector of values
#' @param na.rm Boolean. Remove NA values?
#' 
#' @export
#'
ci95 <- function(x, na.rm = FALSE) {
    return(ci(95, x, na.rm))
}

# Draw an error bar to a plot
error.bar <- function(x, y, upper, lower=upper, length=0.1, width = 1.5,
                      col) {
    if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
        stop("vectors must be same length")
    arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, lwd = width, col=col)
}

plot.err.bar <- function(err.bar, tab, dev, cols) {
    if (!is.null(err.bar)) {
        error.bar(1:nrow(tab)-dev, tab[,1], err.bar[,1], col=cols[1],
                  width=1.2, length=0.05)
        error.bar(1:nrow(tab)+dev, tab[,2], err.bar[,2], col=cols[2],
                  width=1.2, length=0.05)
    }
}
