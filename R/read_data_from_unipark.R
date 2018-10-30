
# Convert DOMC response data from Unipark output to R list
#
# Converts response data for one test-taker from Unipark JSON String to
# a `list`.
#
# @param JsonString The JSON string, usually stored in the column of an
#     Unipark-exported csv
#
# @return A \code{list} containing the test data for one test taker
#   \item{item1 ... itemx}{Item data for the x test items}
#
# @details Each entry of the returned \code{list} contains data
#     detailing a test-taker's response to one item.
#
# @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#

get_response_data_person <- function(JsonString) {
  jsonList       <- convertJsonToList(JsonString)
  domcItemData   <- getDomcItemData(jsonList)
  return(domcItemData)
}

#' @importFrom jsonlite fromJSON
convertJsonToList <- function(JsonString) {
  # had to convert "" to '' in Unipark, this conversion needs to be reverted:
  JsonString <- gsub("'", "\"", JsonString)
  jsonList <- jsonlite::fromJSON(JsonString, simplifyDataFrame = FALSE)
  return(jsonList)
}

getDomcItemData <- function(jsonList) {
  # select data for DOMC items only
  domcData <- list()
  for (i in 1:length(jsonList)) {
    if (!is.null(jsonList[[i]]$responseData)) {
      itemIdentifier <- jsonList[[i]]$id
      itemData       <- jsonList[[i]]$responseData
      domcData[[itemIdentifier]] <- itemData
    }
  }
  return(domcData)
}
