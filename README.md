# baby-bonds
Data analysis for the Baby Bonds project and estimation of models for DYNASIM.

## Root Folder
* [`modeling_college_ed.html`](modeling_college_ed.html): Notebook for modeling college education.


# NLSY Data
It is assumed that an R project file is in the repo's root folder and that the Box drive is mounted in '~/..' (this is true on Windows but may not be on Mac, in which case modify `nlsy_data_dir` in common.R.

## NLSY Folder

* `make_nlsy.R`: Run it once to create a local RDS file.
* `nlsy_explore.Rmd`: RMarkdown file that explores NLSY data.
* [`nlsy_explore.html`](NLSY/nlsy_explore.html): Rendering of the above file.

## Data Folder on Box
* `College-finance2-value-labels.do`: Stata script for reading the sample.
* `College-finance2.NLSY97`: NLSY "tagset" contains a list of variables in the sample.
* `College-finance2.R`: R script for reading the sample.
* `College-finance2.cdb`: Codebook.
* `College-finance2.dat`: Data sample.
* `College-finance2.dct`: 
* `College-finance2.sas`: SAS script for reading the sample.
* `College-finance2.sdf`: Mapping of question names to variable descriptions and variable numbers.
* `customweight_nlsy97_651b190a18af1d9d269.dat`: Weights for use in longitudinal analysis.
* `customweight_nlsy97_651b190a18af1d9d269.readme`: 
