# baby-bonds
Data analysis for the Baby Bonds project and estimation of models for DYNASIM.

# Root Folder
* [`modeling_college_ed.html`](modeling_college_ed.html): Notebook for modeling college education.
* [`dynasim_reweighting.html`](dynasim_reweighting.html): Notebook for reweighting the DYNASIM sample.

## Analysis Folder
### Education

* `DYNASIM-college.qmd`: Quarto report on the effects of baby bonds on college education.
* [`DYNASIM-college.html`](Analysis/Education/DYNASIM-college.html): Rendering of the above file.
* [`DYNASIM-college-2024-05-06.html`](Analysis/Education/DYNASIM-college-2024-05-06.html): Rendering of the above file.

# NLSY Data
It is assumed that an R project file is in the repo's root folder and that the Box drive is mounted in '~/..' (this is true on Windows but may not be on Mac, in which case modify `nlsy_data_dir` in common.R.

## NLSY Folder

* `make_nlsy.R`: Run it once to create a local RDS file.
* `nlsy_explore.Rmd`: RMarkdown file that explores NLSY data.
* [`nlsy_explore.html`](NLSY/nlsy_explore.html): Rendering of the above file.
* `nlsy_imputation.qmd`: Quarto file that imputes missing values in NLSY data.
* [`nlsy_imputation.html`](NLSY/nlsy_imputation.html): Rendering of the above file.
* nlsy_lib.R: library of functions dealing with NLSY data.

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

# Student Loan Data
## Student Loan Folder
* `student_debt_distr.qmd`: Imputes and plots student loan data.
* [`student_debt_distr.html`](student_loans/student_debt_distr.html): Rendering of the above file.

# Presentations

