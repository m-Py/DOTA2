
#' Creates a table in long format representing the test response data
#'
#' This is the main function that should be called to convert output
#' data from Unipark to an R data.frame in long format.
#'
#' @param response_data A vector of Unipark-exported test data strings
#' @param test_taker_IDs A vector of unique IDs identifying each each
#'     test-taker (the order must correspond to the order of response
#'     data in `response_data`)
#' @param by_option Boolean. Indicates whether response data should be
#'     returned on item level or - if set to `TRUE` - on option level
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

get_response_table <- function(response_data, test_taker_IDs, by_option = FALSE) {
  if (length(response_data) != length(test_taker_IDs)) {
    stop("There are not as many test data strings as there are test-taker ids")
  }
  tmp_list <- list()
  for (i in 1:length(response_data)) {
    ## convert json to R data
    dat_as_list <- get_response_data_person(response_data[i])
    ## convert list to data frame
    tmp_list[[i]] <- response_table_person(dat_as_list, test_taker_IDs[i], by_option)
  }
  return(ldply(tmp_list, data.frame))
}

# create a long table from responses for one test taker
#' @importFrom plyr ldply
response_table_person <- function(response_data, id, by_option) {
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
