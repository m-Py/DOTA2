
#' Creates a table in long format representing the test response data
#'
#' @param response_data A vector of Unipark-exported test data strings
#' @param test_taker_IDs A vector of unique IDs identifying each each
#'     test-taker (the order must correspond to the order of response
#'     data in `response_data_sample`)
#' @param by_option Boolean. Indicates whether response data should be
#'     returned on item level or - if set to `TRUE` - on option level
#' @param item_IDs A numeric vector corresponding to the IDs of all
#'     items in the test. Do not change the default value `NULL` if
#'     response data should be read for all items (i.e. in most cases).
#' @param item_prefix By default it is assumed that each item has a name
#'     of the type "itemx" where x is the ID of the item. If the prefix
#'     is anything other than "item", change the default value of this
#'     argument; this should not be necessary in most cases!
#'
#' @return A `data.frame` in long format containing test-takers'
#'     response data. Columns include the correctness of responses,
#'     response times, and other information. Each row represents the
#'     processing of an item (if `by_option` was `FALSE`) of the
#'     processing of a single option (if `by_option` was `TRUE`).
#'
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#'
#' @export

get_response_table <- function(response_data, test_taker_IDs,
                               by_option = FALSE, item_IDs = NULL,
                               item_prefix = "item") {
    if (length(response_data_sample) != length(test_taker_IDs)) {
        stop("There are not as many test data strings as there are test-taker ids")
    }
    tmp_list <- list()
    for (i in 1:length(response_data)) {
        tmp_list[[i]] <- response_table_person(response_data[i],
                                               test_taker_IDs[i], by_option,
                                               item_IDs, item_prefix)
    }
    return(ldply(tmp_list, data.frame))
}

# create a long table from responses for one test taker - on item basis
#' @importFrom plyr ldply
response_table_person <- function(response_data_person, id, by_option, item_IDs,
                                  item_prefix) {
    response_data <- get_response_data_person(response_data_person,
                                              item_IDs, item_prefix)
    tmp_list <- list()
    for (i in 1:length(response_data)) {
        if (by_option == FALSE) {
            tmp_list[[i]] <- select_single_values(response_data[[i]])
        } else {
            tmp_list[[i]] <- select_multiple_values(response_data[[i]])
        }
        tmp_list[[i]]$item_id <- names(response_data)[[i]]
    }
    frame <- ldply(tmp_list, data.frame)
    frame$id <- id
    return(frame)
}

# Selects all single value vectors from a list
select_single_values <- function(l) {
    for (i in names(l)) {
        if (length(l[[i]]) > 1) {
            l[[i]] <- NULL
        }
    }
    return(data.frame(l))
}


# Selects all vectors from a list that have multiple values
select_multiple_values <- function(l) {
    for (i in names(l)) {
        if (length(l[[i]]) == 1) {
            l[[i]] <- NULL
        }
    }
    ret_ <- data.frame(l)
    ret_$option_position <- 1:nrow(ret_)
    return(ret_)
}
