#Packages
librarian::shelf(dplyr, mice)

# Read data
nlsydf = readRDS(paste0(here::here(), "/NLSY/NLSY-college-finance.rds"))

test <- nlsydf %>% #Add additional variables that should be considered during multiple imputation
    select(id = "PUBID_1997",
           age0 = "CV_AGE_12/31/96_1997", # numeric
           sex =  "KEY_SEX_1997", # un-ordered categorical
           hisp = "KEY_ETHNICITY_1997",
           race = "KEY_RACE_1997",
           mom_educ = "CV_HGC_RES_MOM_1997", # categorical - excluded dad education to avoid multi-collinearity
           citizenship = "CV_CITIZEN_CURRENT_2001", # categorical
           pincome = "CV_INCOME_GROSS_YR_1997",
           pnetworth = "CV_HH_NET_WORTH_P_1997",
    ) %>%
    mutate(race_eth = ifelse(hisp == "Yes", "Hispanic", race), # Creating a joint race_ethnicity column
           race_eth = ifelse(race_eth==2, "White", race_eth),
           race_eth = ifelse(race_eth==3, "Black or African American", race_eth),
           race_eth = ifelse(race_eth==4, "American Indian, Eskimo, or Aleut", race_eth),
           race_eth = ifelse(race_eth==5, "Asian American or Pacific Islander", race_eth),
           race_eth = ifelse(race_eth==6, "Other", race_eth)) %>%
    select(-hisp, -race, -citizenship, -mom_educ)

# Checking for missings in the predictor variables
sapply(test, function(x) sum(is.na(x)))

# Paper that uses multiple imputation: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6049093/
# Multiple imputation in R reference: https://library.virginia.edu/data/articles/getting-started-with-multiple-imputation-in-r
init = mice(test, maxit=10)
meth = init$method
predM = init$predictorMatrix

# Set distributions
num <- c("age0") # numerical variables
#poly <- c("mom_educ") # ordered categorical variables
polyreg <- c("sex", "race_eth") # un-ordered categorical variables

meth[num] = "norm" # numerical variables
#meth[poly] = "polr" # ordered categorical variables
meth[polyreg] = "polyreg" # un-ordered categorical variables

# Assign Variables to be imputed
predM[1L,] <- 0 # not using as a predictor variable
meth[c("race_eth")]= ""

# Income and Wealth imputation
set.seed(25)
imputed = mice(test, method=meth, predictorMatrix=predM, m=10)
imputed <- complete(imputed) %>%
    rename(pincome_imp = pincome, pnetworth_imp = pnetworth) %>%
    select(id, pincome_imp, pnetworth_imp)

# Join back with original data set
nlsydf <- nlsydf %>%
    rename(id = PUBID_1997)

nlsydf_imp <- merge(x=nlsydf,y=imputed,
                    by="id", all.x=TRUE)

# Save imputed data
write.csv(nlsydf_imp, 'NLSY/NLSY-college-finance_imp.csv')
