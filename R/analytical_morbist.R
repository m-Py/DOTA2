#' Probability that a MC item is solved correctly given accuracy.
#'
#' This function can be used to analytically determine the expected
#' probability that an MC item is solved correctly, as opposed to the
#' simulation approach used in \code{workMCItem}.
#'
#' @param accuracy Accuracy value of an assumed test taker
#' @param n_options number of answer options in the MC item
#' @param sd_sol Standard deviation of the solution
#'     distribution. Distractor SD is always 1
#'
#' @return The expected probability that the item is solved correctly
#'
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#'
#' @export
mc_analytical <- function(accuracy, n_options=4, sd_sol=1) {
    ## for large accuracy the function is not reliable
    if (accuracy > 15) {
        stop("do not use accuracy values greater than 15")
    } 
    ## for derivation of formula see
    ## http://math.stackexchange.com/questions/232535/probability-that-one-random-number-is-larger-than-other-random-numbers
    ## see fileCompareMultipleNormalDistributions.pdf
    inte <- function(alpha) {
        pre <- exp( (-(alpha - accuracy)^2) / (2*sd_sol^2) )
        prob <- pnorm(alpha)^(n_options-1)
        return(pre*prob)
    }
    moep <- integrate(inte, -Inf, Inf)$value
    return(moep*1/(sqrt(2*pi)*sd_sol))
}

#' Probability that a DOMC item is solved correctly given accuracy and
#' criterion vector.
#'
#' This function can be used to analytically determine the expected
#' probability that a DOMC item is solved correctly, as opposed to the
#' simulation approach used in \code{workDOMCItem}
#'
#' @param accuracy Accuracy value of an assumed test taker
#' @param criterionVector The test taker's criterion vector; pass as in
#'     \code{createTestTaker}
#' @param n_options number of answer options in the MC item; only pass
#'     if item parameter is not specified
#' @param item Can be passed instead of n_options - pass a vector of 0
#'     and 1, which indicate the sequence of answer options, in
#'     particular the solution position
#' @param sd_sol Standard deviation of the solution
#'     distribution. Distractor SD is always 1
#'
#' @return The expected probability that an item is solved correctly
#'
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#'
#' @export

domc_analytical <- function(accuracy, criterionVector, n_options=NULL, 
                            item=NULL, sd_sol=1) {

    if (is.null(n_options) && is.null(item)) {
        stop("Error: one of 'n_options' or 'item' must be given.")
    }
    if (!is.null(n_options) && !is.null(item)) {
        stop("Error: give only one of the parameters 'n_options' and 'item'.")
    }
    ## case: item is given
    if (is.null(n_options)) {
        thresholds <- accuracy/2 + criterionVector
        ## distibution means for all answer options
        dis_mean <- item * accuracy
        ## standard deviations of distributions
        dis_sds  <- item * sd_sol
        dis_sds[dis_sds==0] <- 1
        ## threshold that a single option is accepted
        prob_accepted <- 1-pnorm(thresholds, mean=dis_mean, sd=dis_sds)

        ## compute probability for an error; error can occur on each
        ## option position until the solution is shown
        sol_pos <- which(item==1) 
        prob_error          <- prob_accepted[1:sol_pos]
        prob_error[sol_pos] <- 1 - prob_error[sol_pos]
        prob_no_error       <- 1 - prob_error
        ## computation of probability that an error is committed
        summands <- vector()
        for (i in 1:sol_pos) {
            if (i != 1) {
                tmpVec      <- prob_no_error[1:(i-1)]
                summands[i] <- prod(tmpVec) * prob_error[i]
            } else { 
                summands[i] <- prob_error[i]
            }
        }
        return(1 - sum(summands))

        ## case: n_options is given is given
    } else if (is.null(item)) {
        ## expected DOMC test score is mean of expected test scores for
        ## all solution positions
        item_combinations <- list()
        for (i in 1:n_options) {
            tmpVec                 <- rep(0, n_options)
            tmpVec[i]              <- 1 # vary solution position
            item_combinations[[i]] <- tmpVec
        }
        # get probabilities
        probabilities <- vector(length=n_options)
        for (i in 1:n_options) {
            probabilities[i] <- domc_analytical(accuracy, criterionVector,
                                                item=item_combinations[[i]],
                                                sd_sol=sd_sol)
        }
        return(mean(probabilities))
    }
}

#' Estimate accuracy for a given solving probability
#' 
#' This function is the inverse to \code{mc_analytical} and
#' \code{domc_analytical}: It determines the accuracy parameter that
#' corresponds to a given solving probability. Can be used to infer
#' appropriate accuracy values for empirical data.
#'
#' @param probability probability to solve the item correctly (must not
#'     be 1 or 0)
#' @param n_options How many options does the item have
#' @param sd_sol Standard deviation of the solution
#'     distribution. Distractor always has SD of 1
#' @param type either "mc" or "domc"
#'
#' @return The estimated accuracy
#' 
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#'
#' @export
estimateAccuracy <- function(probability, n_options=NULL, sd_sol = 1, 
                             type, criterionVector=NULL, item=NULL) {
    if (probability == 1 || probability == 0) {
        stop("cannot determine accuracy for a probability of 1 or 0")
    }
    steps    <- 0
    acc      <- 0
    stepSize <- 0.1
    goUp     <- TRUE
    while (steps < 10) {
        if (type == "mc") {
            estimate <- mc_analytical(acc, n_options, sd_sol)
        } else if (type == "domc") {
            estimate <- domc_analytical(accuracy=acc,
                                n_options=n_options,
                                item=item,
                                criterionVector=criterionVector,
                                sd_sol=sd_sol)
        }
        if ( (estimate >= probability && goUp == TRUE) ||
                (estimate <= probability && goUp == FALSE) ) {
            steps    <- steps + 1
            stepSize <-  (-1) * (stepSize/10)
            goUp     <- !goUp
        }
        acc <- acc + stepSize
    }
    return(acc)
}
