
#' Simulate test responses in the MORBIST model
#'
#' Simulates a specified number of test-takers working on a specified
#' number of items in MC or DOMC format. The accuracy values of
#' test-takers can be generated randomly by the function or are passed
#' explicitly.
#'
#' @param item_number How many items are simulated for each test-taker
#' @param option_number How many response options should each item have
#' @param n_respondents How many test-takers are to be simulated
#' @param type Which type of test is be simulated? Must be "mc" or
#'     "domc".
#' @param avg_acc Mean accuracy (d') of the test-takers
#' @param sd_acc Standard deviation of the test-takers' accuracy values
#'     d'.
#' @param accuracies If not NULL, it must be a vector of length
#'     `n_respondents` containing an accuracy value d' for each test
#'     taker. If this argument is passed, it overrides the values of
#'     avg_acc and sd_acc. Defaults to NULL.
#' @param criterion_list A list of criterion vectors of length
#'     `testTaker`.
#' @param sd_sol standard deviation of solution distribution
#' @param sd_dis standard deviation of distractor distribution
#'
#' @return If a DOMC test was simulated: A list containing two data
#'     frames in long format - one stores data on item level, the other
#'     store response data on option level. If a MC test was simulated:
#'     A data.frame in long format storing response data on item level.
#'
#' @examples
#' 
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#' @export 
#'
simulate_morbist <- function(item_number, option_number, n_respondents,
                             type, avg_acc = 1.5, sd_acc = 0.5,
                             accuracies = NULL, criterion_list,
                             sd_sol = 1, sd_dis = 1) {

    ## some renaming due to name change of parameters
    itemNumber   <- item_number
    optionNumber <- option_number
    testTakers   <- n_respondents
    avgAcc <- avg_acc
    sdAcc  <- sd_acc
    accuracyList <- accuracies
    criterionList <- criterion_list
    
    ## Some error handling
    if (type != "mc" && type != "domc") { stop("type must be 'mc' or 'domc'") }

    ## set up data storage as a list
    examData   <- list() # just append raw testData to list
    by_item    <- list() # append data in data frame format by item
    by_option  <- list() # append data in data frame format by option    

    # take the test for all test takers 
    for (i in 1:testTakers) {
        test <- createTest(itemNumber, optionNumber) # this is in loop

        ## determine response criterion vector for the i'th test taker
        criterionVector <- criterionList[[i]]

        ## Determine accuracy 
        ## a) a custom accuracy vector has been given
        if (!is.null(accuracyList)) {
            tmpTestee <- createTestTaker(accuracyList[i], criterionVector)
        ## b) accuracy is drawn from a normal distribution
        } else { 
            tmpTestee <- createTestTaker(rnorm(1, mean=avgAcc, sd=sdAcc),
                                         criterionVector)
        }

        ## Work test
        examData[[i]]     <- workTest(tmpTestee, test, type, 
                                      sd_dis = sd_dis, sd_sol = sd_sol)

        ## Transform data to long data frame format; this iterates over
        ## all items and test-takers once more; bad for performance --
        ## could be prevented if workTest directly returns a data.frame
        ## instead of a list (maybe?)        
        by_item[[i]]      <- response_table_person(examData[[i]], i, by_option=FALSE)
        if (type =="domc") {
            by_option[[i]] <- response_table_person(examData[[i]], i, by_option=TRUE)
        }        
    }

    ## merge all data.frames
    by_item   <- ldply(by_item, data.frame)

    ## determine what is returned
    if (type == "domc") {
        by_option <- ldply(by_option, data.frame)
        ret_list  <- list(by_item=by_item, by_option=by_option)
    } else {
        ## MC test does not store data on item level
        ret_list <- by_item
    }
    return(ret_list)
}

# Simulate one test-takter working on one test
#
# Internal function that is called by `simulate_morbist`. Simulates a
# single test taker working on a DOMC or a MC test as created via
# \code{\link{createTest}}. Accuracy of the test taker is assumed to be
# constant across items.
#
# @param testTaker a \code{list} returned by \code{\link{testTaker}}. Contains 
#     an accuracy vector and a bias list
# @param test returned by \code{\link{createTest}}, has a specified item and 
#     option number
# @param type which type of test is be simulated? Must be "mc" 
#     (multiple-choice) or "domc" (discrete-option multiple-choice)
# @param sd_sol standard deviation of solution distribution (default = 1)
# @param sd_dis standard deviation of distractor distribution (default = 1)
#
# @return a \code{list} containing information on the test and on test taker
#     responses, these items are stored:
# 
#   \item{itemNumber}{how many items were on the test}
#   \item{optionNumber}{how many options did every item have}
#   \item{items}{\code{list} that contains response decisions and outcomes as 
#         returned by \code{\link{workDOMCItem}} or \code{\link{workMCItem}} }
#
# @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#
workTest <- function(testTaker, test, type, sd_dis = 1, sd_sol = 1) {
    ## Some error handling
    if (type != "mc" && type != "domc") {
        stop("argument type must be 'mc' or 'domc'") 
    }

    ## List to be returned:
    testData            <- list()

    numberItems    <- test[["itemNumber"]]
    numberOptions  <- test[["optionNumber"]]
    items          <- test[["items"]]

    ## Store number of items and options as entry in test data list

    for (i in 1:numberItems) { # work on as many items as there are in the test
        if (type == "domc") {
            testData[[paste0("item", i)]] <- workDOMCItem(testTaker, items[[i]], 
                                                          sd_dis = sd_dis, sd_sol = sd_sol)
            ## some additional data storage here:
            testData[[paste0("item", i)]]$d_prime <- testTaker[["accuracy"]]
            testData[[paste0("item", i)]]$criterion_c <- testTaker[["testTakerBias"]]
        } else if (type == "mc") {
            testData[[paste0("item", i)]] <- workMCItem(testTaker, items[[i]],
                                                        sd_dis = sd_dis, sd_sol = sd_sol)
            testData[[paste0("item", i)]]$d_prime <- testTaker[["accuracy"]]
        }
    }
    
    return(testData)
}

# Create a test
#
# Test that is worked on via \code{\link{workTest}}
# 
# @param itemNumber How many items should be in the test
# @param optionNumber How many options each item should have
# 
# @return a \code{list}, which represents the test, these items are stored:
#   \item{itemNumber}{how many items are on the test}
#   \item{optionNumber}{how many options does every item have}
#   \item{items}{A \code{list} of test items as created via 
#       \code{\link{createItem}} }
#
# @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#
createTest <- function(itemNumber, optionNumber) {
    test <- list()
    test[["items"]] <- list()
    test[["itemNumber"]] <- itemNumber
    test[["optionNumber"]] <- optionNumber
    for (i in 1:itemNumber) {
        test[["items"]][[i]] <- createItem(optionNumber)
    }
    class(test) <- append(class(test), "DPM_test")
    return(test)
}

# Simulate response in a single DOMC item
#
# Simulates the response process in a (sequential) yes-no item; 
#    option evaluation and decision process is modelled.
#
# @param testTaker the test taker that is to work on the item, 
#     created via \code{\link{createTestTaker}}
# @param item item that is be to processed, created via 
#     \code{\link{createItem}}
# @param sd_sol standard deviation of solution distribution
# @param sd_dis standard deviation of distractor distribution
#
# @return A \code{list}, which represents response decisions and 
#     outcome for the item
#   \item{decisions}{Vector (of length 1 for 'normal' yes-no item). 
#        For each item option, 1 and 0 indicate whether the option was 
#        chosen or not}
#   \item{hit}{was the response outcome a hit?}
#   \item{falseAlarm}{was the response outcome a false alarm?}
#   \item{miss}{was the response outcome a miss?}
#
# @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#
#
workDOMCItem <- function(testTaker, item, sd_dis = 1, sd_sol = 1) {

    numberOptions <- length(item)

    if (numberOptions != length(testTaker[["bias"]])) {
        stop(paste("Number of answer options in DOMC item", 
                   "and length of criterion vector differ."))
    }
    
    ## to be returned
    itemData <- list()
    itemData[["is_solution"]] <- item # store as: is option solution
    ## store data: 
    itemData[["decisions"]]   <- vector(length = numberOptions)  # all decisions per item
    itemData[["hit"]]         <- 0
    itemData[["falseAlarm"]]  <- 0
    itemData[["miss"]]        <- 0
    itemData[["position_solution"]]  <- which(item == 1)

    ## to track the response process:
    optionAccepted <- 0      # keep track: has an option been accepted
    solutionSeen   <- 0      # keep track: has solution been seen

    # Start working on the item
    for (i in 1:numberOptions) {

        # 1. Collect evidence on i'th option
        if (item[[i]] == 0) {
            decisionStrength <- rnorm(1, mean=0, sd=sd_dis)
        }
        else if (item[[i]] == 1 ) {
            solutionSeen <- 1 # stopping criterion
            decisionStrength <- rnorm(1, mean=testTaker[["accuracy"]], 
                                      sd=sd_sol)
        }
        ## compute threshold for the i'th option
        threshold <- testTaker[["accuracy"]]/2 + testTaker[["bias"]][i]

        ## 3. Decision process -> does i'th option surpass threshold?
        ## response criterion is not surpassed
        if (decisionStrength < threshold) {
            itemData[["decisions"]][i] <- 0 # -> option not selected
        } else if (decisionStrength >= threshold) {
            optionAccepted <- 1             # stopping criterion
            itemData[["decisions"]][i] <- 1 # options selected
        }

        # 4. Evaluate decision (if one was made)
        # 4a. determine item response outcome (hit, FA)
        if (!is.na(itemData[["decisions"]][i])) {
            if (itemData[["decisions"]][i] == 1 && item[[i]] == 1) {
                itemData[["hit"]] <- 1
            }
            else if (itemData[["decisions"]][i] == 1 && item[[i]] == 0) {
                itemData[["falseAlarm"]] <- 1
            }
        }
        # 4b. determine item response outcome (miss)
        if (i == numberOptions) { # check when all options are through
            if (optionAccepted == 0 && solutionSeen == 1) {
                itemData[["miss"]] <- 1
            }
        }

        # 5. Check if item presentation is over
        if (optionAccepted == 1 || solutionSeen == 1) {
            break # do not iterate over more options when item presentation is over
        }
    }
    
    ## some more data storage (to be compatible with the empirical data that I store)
    itemData[["n_options_seen"]] <- sum(!is.na(itemData[["decisions"]]))
    itemData[["seen"]]     <- as.numeric(!is.na(itemData[["decisions"]]))
    itemData[["selected"]] <- itemData[["decisions"]]
    itemData[["hit_by_option"]] <- as.numeric(itemData[["decisions"]] & itemData[["is_solution"]])
    itemData[["false_alarm_by_option"]] <- as.numeric(itemData[["decisions"]] &
                                                      !itemData[["is_solution"]])
    itemData[["option_position"]] <- 1:numberOptions
    itemData[["correct"]]         <-  itemData[["hit"]] 
    itemData[["false_alarm"]]  <- itemData[["falseAlarm"]]

    ## these were only renamed to have consistent data api:
    itemData[["hit"]] <- NULL
    itemData[["falseAlarm"]] <- NULL
    itemData[["decisions"]] <- NULL
    
    return(itemData)
}

# Simulate response in Multiple-Choice item
#
# Simulates the response process in a Multiple-Choice item. Item has at 
#    least 2 options, 1 solution and one distractor.
#
# @param testTaker the test taker that is to work on the item, created via
#    \code{\link{createTestTaker}}
# @param item item that is be to processed, created via 
#    \code{\link{createItem}}
# @param equalVariances Boolean. Are distribution variances for solutions and
#    distractors assumed to be equal or not. If false, distribution variance 
#    for solutions is 1.25.
#
# @return A vector of length 1. 1 indicates the item was solved correctly, 
#    0 indicates it was not solved correctly.
#
# @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#

workMCItem <- function(testTaker, item, sd_dis = 1, sd_sol = 1) {

    highestStrength  <- -Inf # initialize
    item_correct     <- 0

    numberOptions <- length(item)
    for (i in 1:numberOptions) {
        if (item[i] == 0 ) { # distractor is evaluated
            decisionStrength <- rnorm(1, mean=0, sd=sd_dis)
            # tiebreaker: same decision strength, earlier option gets chosen 
            # (this will not happen)
            if (decisionStrength > highestStrength ) { 
                highestStrength <- decisionStrength
                item_correct <- 0
            }
        }
        else if (item[i] == 1 ) { # solution is evaluated
            decisionStrength <- rnorm(1, mean=testTaker[["accuracy"]],
                                      sd=sd_sol)
            if (decisionStrength > highestStrength ) {
                highestStrength <- decisionStrength
                item_correct <- 1
            }
        }
    }
    return(list(correct=item_correct)) # 0 or 1
}

# Create a single test item
#
# Creates an item that can be true-false-like or multiple-choice-like
#
# @param optionNumber How many options should the item have. At least two 
#    options must be given.
#
# @return A vector of length `optionNumber` of values 0 and 1. 1 represents 
#    a solution, and 0 represents a distractor.
#
# @author Martin Papenberg \email{martin.papenberg@@hhu.de}
# @export 
#
createItem <- function(optionNumber) {
    item <- sample(optionNumber)
    item[item!=1] <- 0
    return(item)
}

# Create a test-taker
#
# Creates a test-taker who can work on MC or DOMC items
#
# @param accuracy A single numerical value that determines the ability 
#     of the test taker to discriminate between solutions and distractors.
# @param bias A \code{vector}, determines acceptance threshold for an item 
#     option. Must consist of at least ay many entries as number of answer 
#     options in the to be worked on items
# 
# @return A \code{list} that contains the test taker parameters accuracy and 
#     bias. Bias itself is a \code{list}.
#
# @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#
createTestTaker <- function(accuracy, bias) {
    testTaker <- list()
    testTaker[["accuracy"]] <- accuracy
    testTaker[["bias"]]     <- bias
    return(testTaker)
}
