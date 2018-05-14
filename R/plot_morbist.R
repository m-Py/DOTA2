
#' Plot the parameters characterizing the MORBIST model
#'
#' Plots decision variable distribution of targets and and lures
#' (assuming a normal distribution of evidence) for a test-taker; can
#' also illustrate a response criterion vector
#'
#' @param x An accuracy value (d')
#' @param y The response criterion vector
#' @param showCriterion Boolean. Should the criterion location be drawn?
#' @param labelCriterion label response criterion with *c*; only has an
#'     effect if showCriterion is TRUE
#' @param distCritLabel If the criterion label is displayed: how far top
#'     should it be (higher values move the label further away from the
#'     plot)
#' @param cex.axis magnification of the decision variable axis (x-axis)
#'     values
#' @param cex.label controls magnification of text size of x-axis label
#'     and "distractors" and "solutions" text
#' @param cex.label controls magnification the text of the criterion
#'     labels
#' @param cex.acc controls magnification the accuracy label
#' @param caption label for decision variable axis (x-axis)
#' @param sd_sol The standard deviation of the solution distribution
#' @param solutions caption for solution distribution
#' @param distractors caption for distractor distribution
#' @param distLabelDistributions how away from the distribution plots
#'     should the labels for the distributions be (higher values move
#'     the label further away from the plot)
#' @param showAccuracy Boolean - illustrate accuracy d'?
#' @param distAccLabel If the accuracy value is displayed: how far top
#'     should it be (higher values move the label further away from the
#'     plot)
#' @return None
#'
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#'
#' @export
#'
plot_morbist <- function(x, y, showCriterion = TRUE,
                         labelCriterion = FALSE, distCritLabel = 0.425,
                         cex.axis = 1, cex.label = 1, cex.crit = 1,
                         cex.acc = 1, caption = "evidence strength",
                         solutions="solutions", sd_sol = 1,
                         distractors="distractors",
                         distLabelDistributions = 2.3,
                         showAccuracy = TRUE, distAccLabel = 0.46) {

    ## determine the intersection of distractor and solution curve
    intersection <- get_intersection(x, sd_sol)
    
    ## functions to illustrate distractor and solution normal
    ## distribution:
    curve_sol <- function(d) dnorm(d, x, sd_sol)
    curve_dis <- function(d) dnorm(d, 0, 1)

    ## set the area in which the plot is drawn
    width     <- 4
    ## to set the axis of the plot:
    seqTotal  <- seq(-width, width+x, by = 1)
    seqTarget <- seq(-width+x, width+x, by=0.1)
    seqLure   <- seq(-width, width, by=0.1)
   
    ## Draw the plot
    if (showAccuracy) {
        ## make room for accuracy label
        ## ‘c(bottom, left, top, right)’
        def_mar <- c(5.1, 4.1, 4.1, 2.1)
        par(mar=def_mar + c(0, 0, distAccLabel, 0))
    }
    curve(curve_sol, axes=FALSE, yaxs="i",
          xlim =c(-width, width+x), ylim=c(0,0.5), ylab="", xlab=caption,
          cex.lab = cex.label)
    par(new=TRUE)
    curve(curve_dis, add = TRUE)
    axis(1, at=seqTotal, cex=cex.axis)
    
    ## draw criterion location lines and labels
    col_range <- (1:length(y))/(1.5*length(y))+0.25
    colorr <- gray(col_range) # color for lines
    if (showCriterion == TRUE) {       
        crt <- y
        for (i in 1:length(crt)) {
            lines(rep(intersection + crt[i], 2), c(0, 0.41), lwd = 2,
                  col = colorr)
            if (labelCriterion) {
                text(intersection + crt[i], distCritLabel, 
                     bquote(italic("c") [.(i)]), cex=cex.crit)
            }
        }
    }

    ## add text to curves
    text(-distLabelDistributions, 0.25, distractors, cex=cex.label)
    text(x+distLabelDistributions, 0.25, solutions, cex=cex.label)
    ## display accuracy
    if (showAccuracy) {
        normal.xpd <- par()$xpd
        par(xpd=NA)
        heightaccrr <- distAccLabel
        arrows(0, heightaccrr, x, heightaccrr)
        arrows(x, heightaccrr, 0, heightaccrr)
        text(intersection, heightaccrr+0.025, bquote(italic("d'") ~"=" ~.(x)), cex=cex.acc)
        par(xpd=normal.xpd) # reverse
    }
    on.exit(par(mar = def_mar))
}

## determine the intersection of distractor and solution curve
get_intersection <- function(d_prime, sd_sol) {
    f <- function(x) dnorm(x, m=0, sd=1) - dnorm(x, m=d_prime, sd=sd_sol)
    intersection <- uniroot(f, interval=c(0, sd_sol))$root
    return(intersection)
}
