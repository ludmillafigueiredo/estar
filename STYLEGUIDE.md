Style guide for development and issue reporting.

# Development

## R syntax
Follow R's [Tidyverse Style Guide](https://style.tidyverse.org/).

## Variable/Argument naming
Object's names are composed of abbreviations indicating to which scenario they refer to, 
and suffixes indicating the type of object they are(not necessarily R types):
- `sv` state variable.
- `t` time.
- `db` refers to 'disturbed' system, usually the focus of analysis.
- `udb` refers to 'undisturbed' system, usually the baseline.
- `bl` baseline.
- `_i` input vector or column name of **input**.
- `_c` column name in **internal** data frame.
- `_data` input dataframe.
- `_v` internal vector, built to facilitate processing.
- `_df` **internal** dataframe, built to facilitate processing. 
Input dataframes do not get this suffix because their data-related status is more important.

## Git messages
Follow [Udacity's Git Commit Message Style Guide](http://udacity.github.io/git-styleguide/).

