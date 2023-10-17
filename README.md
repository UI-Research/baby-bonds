# baby-bonds
Data analysis for the Baby Bonds project and estimation of models for DYNASIM.

# NLSY Data
It is assumed that an R project file is in the repo's root folder and that the Box drive is mounted in '~/..' (this is true on Windows but may not be on Mac, in which case modify `nlsy_data_dir` in common.R.

## NLSY Folder

* `make_nlsy.R`: Run it once to create a local RDS file.
* `explore_nlsy.Rmd`: RMarkdown file that explores NLSY data.
* [`explore_nlsy.html`][https://ui-research.github.io/baby-bonds/]: Rendering of the above file.
