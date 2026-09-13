## Suppress R CMD check NOTEs about undefined global variables.
## - `group` is used as a column name in dplyr pipelines (tibble).
## - `.` is used in magrittr/dplyr pipe expressions.
utils::globalVariables(c("group", "."))
