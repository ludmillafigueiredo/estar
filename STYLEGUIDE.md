Style guide for development and issue reporting.

# Development

## R syntax
Follow R's [Tidyverse Style Guide](https://style.tidyverse.org/).

## Variable/Argument naming
Object's names are composed of abbreviations indicating to which scenario they refer to, 
and suffixes indicating the type of object they are(not necessarily R types):
- `v` state variable.
- `t` time.
- `d` refers to 'disturbed' system, usually the focus of analysis.
- `udb` refers to 'undisturbed' system, usually the baseline.
- `b` baseline.
- `_i` input vector or column name of **input**.
- `_c` column name in **internal** data frame.
- `_data` input dataframe.
- `_v` internal vector, built to facilitate processing.
- `_df` **internal** dataframe, built to facilitate processing. 
Input dataframes do not get this suffix because their data-related status is more important.

## Git messages
Follow [Udacity's Git Commit Message Style Guide](http://udacity.github.io/git-styleguide/).

## Color blind safe palette

Tol_muted <- c('#88CCEE', '#44AA99', '#117733', '#332288', '#DDCC77', '#999933','#CC6677', '#882255', '#AA4499', '#DDDDDD')

getrgb <- function(x) paste(as.vector(col2rgb(x)), collapse = " ")
sapply(Tol_muted, getrgb)


