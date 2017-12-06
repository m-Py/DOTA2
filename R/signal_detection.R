
#' Determine accuracy d'
#'
#' @param long.tab A long table containing DOMC response data by
#'     response option (created by `get_item_table(responseDataSample,
#'     IDs, by_option = TRUE)`)
#' @param loglinear Boolean; should a loglinear correction be applied
#'     (defaults to `TRUE`)
#' @param aggregateBy Must be `c("option", "id")`, `"option"`, or
#'     `"id"`. Determines what variables the yes-rate is aggregated
#'     over; the default is `c("id")` in which case d' is computed for
#'     each test-taker across all decisions.
#' @param colnames Indicates the names of the columns containing the
#'     hits and false alarms in `long.tab`
#' 
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#'
#' @export
#' 

accuracy_d <- function(option.tab, loglinear = TRUE,
                       aggregateBy = c("id"),
                       colnames=c("hit_by_option",
                                  "false_alarm_by_option")
                       ) {
    d <- ( qnorm(relative_yes(option.tab, colnames[1], loglinear, aggregateBy)) -
           qnorm(relative_yes(option.tab, colnames[2], loglinear, aggregateBy)) )
    return(d)
}


#' Determine response criterion c by option position per test-taker
#'
#' @param long.tab A long table containing DOMC response data by
#'     response option (created by `get_item_table(responseDataSample,
#'     IDs, by_option = TRUE)`)
#' @param loglinear Boolean; should a loglinear correction be applied
#'     (defaults to TRUE which is *strongly* recommended to compute
#'     response criteria)
#' @param aggregateBy Must be `c("option_position", "id")`,
#'     `"option_position"`, or `"id"`. Determines what variables the
#'     yes-rate is aggregated over; the default is `c("option_position",
#'     "id")` in which case the response criterion is computed for each
#'     test-taker by option.
#' 
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#'
#' @export
#' 

criterion_c <- function(option.tab, loglinear = TRUE,
                        aggregateBy = c("option_position", "id"),
                        colnames=c("hit_by_option",
                                   "false_alarm_by_option")
                        ) {
    c <- -0.5 * ( qnorm(relative_yes(option.tab, colnames[1], loglinear, aggregateBy)) +
                 qnorm(relative_yes(option.tab, colnames[2], loglinear, aggregateBy)) )
    return(c)
}

# Determine yes-rates by option per test-taker
#
# Can be used to compute response criterion c or d'.
#
# @param long.tab A long table containing DOMC response data by
#     response option (created by `get_item_table(responseDataSample,
#     IDs, by_option = TRUE)`)
# @param score The name of the table column for which the yes rate should be
#     computed.
# @param loglinear Boolean; should a loglinear correction be applied
#     (defaults to TRUE which is *strongly* recommended to compute
#     response criteria)
# @param aggregateBy Must be c("option_position", "id"), "option", or
#     "id". Determines what variables the yes-rate is aggregated over;
#     the default is c("option_position", "id") in which case the yes-rate is
#     computed for each test-taker by option.
# 
# @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#
# 
relative_yes <- function(option.tab, score = "hit_by_option", loglinear=TRUE,
                         aggregateBy = c("option", "id")) {
    if ("option" %in% aggregateBy & "id" %in% aggregateBy) {
        tab <- table(option.tab[[score]], option.tab$option, option.tab$id)
        tab <- apply(tab, 3,  relative_yes_one_tab, loglinear)
    } else if (aggregateBy == "option" | aggregateBy == "id") {
        tab <- table(option.tab[[score]], option.tab[[aggregateBy]])
        tab <- relative_yes_one_tab(tab, loglinear)
    }
    return(t(tab))
}

relative_yes_one_tab <- function(dim2tab, loglinear = TRUE) {
    if (loglinear) {
        c1 <- 0.5
        c2 <- 1
    } else {
        c1 <- 0
        c2 <- 0
    }
    return((dim2tab["1",] + c1)/ (colSums(dim2tab) + c2))
}
