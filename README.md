# DOTA2

R package for discrete option test analysis

## Installation

```R
library("devtools") # if not available: install.packages("devtools")
install_github("m-Py/DOTA2")

# load the package via 
library("DOTA2")
```

## Usage

### Retrieve data

The function `get_response_table` can be used to read test data from an
Unipark-exported JSON string:

```R

## `testData` is a vector of Unipark-exported JSON strings; `id` is a
## vector of unique test-taker IDs. 

test_dat <- get_response_table(testData, id)

```

The `by_option` parameter controls whether response data is retrieved on
the level of an item or on the level of each option in each item. It
defaults to `FALSE`, i.e. in the default case we retrieve response data
on item level. If `by_option` is `TRUE` we retrieve response data for
each response option. 

```R

item_data <- get_response_table(testData, id, by_option = FALSE)
option_data <- get_response_table(testData, id, by_option = TRUE)

```

If only information about a subset of the test items is required, the
parameter `item_IDs` can be adjusted. If for example only responses to
item 1 are required, use `get_response_table` like this:

```R

item_data <- get_response_table(testData, id, by_option = FALSE, item_IDs = 1)
option_data <- get_response_table(testData, id, by_option = TRUE, item_IDs = 1)

```

### Data description

`get_response_table` returns a `data.frame` in [long
format](https://en.wikipedia.org/wiki/Wide_and_narrow_data). 

#### Item data

If `by_option` is `FALSE`, each row represents an item, the test-taker
who responded to the item is represented in the column `id`. The column
`item_id` contains the ID representing the item. Additionally (as of the
time of this writing), the following columns are stored: 

- `correct`: was the item answered correctly
- `option_id_selected`: the ID of the response option that was selected
- `position_solution`: Where was the solution shown (1 means it was the
   option at the top right below the stem)
- `item_position`: Informs about the position of the item in the
   presentation of all test items (1 means it was the first item that
   was presented in the test)
- `response_time`: the response time in ms

If the test is a DOMC test, the following information is also stored:

- `false_alarm`: was a distractor incorrectly accepted
- `miss`: was the solution incorrectly rejected
- `n_options_seen`: how many response options did the test-taker see

These columns have a different meaning in a DOMC test:

- `option_id_selected` can be 0, meaning that no option was accepted
- `position_solution`: which option was the solution in the sequential
  presentation order

#### Option data

If `by_option` is `TRUE`, each row represents an option. The test-taker
who responded to the option is represented in the column `id`. The
column `item_id` contains the ID representing the item the option
belongs to. The column `option_id` contains the ID of the response
option. Additionally (as of the time of this writing), the following
columns are stored:

- `selected`: was the option selected (1/0)
- `is_solution`: is the option a solution (1/0)
- `option_position`: the position of the option in the presentation
  order

For DOMC tests, the following additional data is stored:

- `seen`: was this option presented to the test-taker (1/0)
- `hit_by_option`: did a hit occur when processing this option (1/0/NA)
   + 1 means this option was the solution and was accepted
   + 0 means this option was the solution and was rejected
   + NA means this option was not a solution or was not seen
- `false_alarm_by_option`: did a false alarm occur when processing this
  option (1/0/NA)
    + 1 means this option was a distractor and was accepted
    + 0 means this option was a distractor and was rejected
    + NA means this option was not a distractor or was not processed
- `response_time_by_option`: the response time for the option in ms

### Merging option and item data

Option and item response data can be joined if that is desired (it does
however make the interpretation of column names more ambiguous):

```R

merge(item_data, option_data, by = c("id", "item_id"))

```
