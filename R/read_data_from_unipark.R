
# Convert DOMC response data from Unipark output to R list
#
# Converts response data for one test-taker from Unipark JSON String to
# a `list`.
#
# @param JsonString The JSON string, usually stored in the column of an
#     Unipark-exported csv
# @param itemIDs A numeric vector corresponding to the IDs of all items
#     to be exported. Change only if only a subset of items is to be
#     returned; otherwise do not change the default value `NULL`.
# @param itemPrefix By default the function assumes that each item is
#     represented in the JSON string by the name "itemx" where x is the
#     ID of the item. If the prefix is anything other than "item", this
#     argument can be used to tell the function this prefix.
#
# @return A \code{list} containing the test data for one test taker
#   \item{item1 ... itemx}{Item data for the x test items}
#
# @details Each entry of the returned \code{list} contains data
#     detailing a test-taker's response to one item.
#
# @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#

get_response_data_person <- function(JsonString, itemIDs = NULL,
                                     itemPrefix = "item") {
    jsonList       <- convertJsonToList(JsonString)
    domcItemData   <- getDomcItemData(jsonList)
    sortedIDs      <- getItemsIDsSorted(domcItemData, itemPrefix)
    selectionIDs   <- selectIDs(sortedIDs, itemIDs)
    sortedItemData <- sortItemData(domcItemData, selectionIDs, itemPrefix)
    return(sortedItemData)
}

#' @importFrom jsonlite fromJSON
convertJsonToList <- function(JsonString) {
    # had to convert "" to '' in Unipark, this conversion needs to be reverted:
    JsonString <- gsub("'", "\"", JsonString)
    jsonList <- jsonlite::fromJSON(JsonString, simplifyDataFrame=FALSE)
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

# return a vector of sorted item IDs
getItemsIDsSorted <- function(rndSeqList, itemPrefix) {
    identifierNumber <- vector(length=length(rndSeqList))
    j <- 1
    for (id in names(rndSeqList)) {
        selector <- gsub(paste0("[", itemPrefix, "]"), "", id)
        identifierNumber[j] <- as.numeric(selector)
        j <- j + 1
    }
    identifierSorted <- sort(identifierNumber)
    return(identifierSorted)
}

selectIDs <- function(sortedIDs, itemIDs) {
    if (is.null(itemIDs)) {
        return(sortedIDs)
    }
    return(intersect(sortedIDs, itemIDs))
}

# given DOMC data and vector of sorted item IDs, return sorted DOMC data 
sortItemData <- function(rndSeqList, sortedIDs, itemPrefix) {
    sortedData <- list()
    sortedIDs <- paste0(itemPrefix, sortedIDs)
    for (id in sortedIDs) {
        sortedData[[id]] <- rndSeqList[[id]] 
    }
    return(sortedData)
}
