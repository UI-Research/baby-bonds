#Packages
librarian::shelf(dplyr, mice, car, corrplot, caret)

# Read data
nlsydf = readRDS(paste0(here::here(), "/NLSY/NLSY-college-finance.rds"))

test <- nlsydf %>% #Add additional variables that should be considered during multiple imputation
    select(id = "PUBID_1997",
           mom_age_birth = "CV_BIO_MOM_AGE_YOUTH_1997", # numeric
           age_resp =  "CV_AGE_12/31/96_1997",
           hispanic = "KEY_ETHNICITY_1997", # binary
           race = "KEY_RACE_1997", # categorical
           mom_education = "CV_HGC_RES_MOM_1997", # categorical - highest grade completed in 1997 by mom
           dad_education = "CV_HGC_RES_DAD_1997", # categorical - highest grade completed in 1997 by dad
           savings = "P5-130_1997", #binary - yes/no retirement savings
           home = "P5-101_1997", # binary - homeownership or renting
           both_parents = "YOUTH_BOTHBIO.01_1997",
           par1_deceased = "YOUTH_NONR1DEAD.01_1997",
           par2_deceased = "YOUTH_NONR2DEAD.01_1997",
           pincome = "CV_INCOME_GROSS_YR_1997",
           pnetworth = "CV_HH_NET_WORTH_P_1997",
           # Also, it seems that this represents the wealth of the household in which a respondent lives. That means that for a respondent who lives with only one parent, the net-worth variable represents the wealth of that parent.
           # In the case of an independent respondent, contains respondent's, rather than parents', wealth.
    ) %>%
    mutate(mom_age = mom_age_birth + age_resp)

# Making dummy variables
test$hisp <- ifelse(test$hispanic == "Yes", 1, 0)
test$ret_sav <- ifelse(test$savings == "YES", 1, 0)
test$hown <- ifelse(test$home == "OWNS OR IS BUYING; LAND CONTRACT", 1, 0)
test$both_pars <- ifelse(test$both_parents == "Yes", 1, 0)
test$par_dec <- ifelse(test$par1_deceased == "Yes" | test$par2_deceased == "Yes", 1, 0) # Either parent deceased
test$mom_educ_hs <- ifelse(test$mom_education == "12TH GRADE" | test$mom_education == "1ST YEAR COLLEGE" | test$mom_education == "2ND YEAR COLLEGE" |  test$mom_education == "3RD YEAR COLLEGE" |  test$mom_education == "4TH YEAR COLLEGE" |  test$mom_education == "5TH YEAR COLLEGE" |  test$mom_education == "6TH YEAR COLLEGE" |  test$mom_education == "7TH YEAR COLLEGE" |  test$mom_education == "8TH YEAR COLLEGE", 1, 0)
test$dad_educ_hs <- ifelse(test$dad_education == "12TH GRADE" | test$dad_education == "1ST YEAR COLLEGE" | test$dad_education == "2ND YEAR COLLEGE" |  test$dad_education == "3RD YEAR COLLEGE" |  test$mom_education == "4TH YEAR COLLEGE" |  test$mom_education == "5TH YEAR COLLEGE" |  test$mom_education == "6TH YEAR COLLEGE" |  test$mom_education == "7TH YEAR COLLEGE" |  test$mom_education == "8TH YEAR COLLEGE", 1, 0)
test$race_w <- ifelse(test$race == "White", 1, 0)
test$race_b <- ifelse(test$race == "Black or African American", 1, 0)

# Looking for significance with non-missing income and wealth
non_missing <- test %>%
    filter(!is.na(pincome),
           !is.na(pnetworth))

summary(model <- lm(pincome ~ mom_age + hisp + race_w + race_b + mom_educ_hs + dad_educ_hs + ret_sav + hown + both_pars + par_dec, data = non_missing))
#Non-significant: race_w and par_dec - dropping for now

# Checking for multi-collinearity
testing_cor <- test %>%
    select(-id, -hispanic, -race, -mom_education, -dad_education, -savings, -home, -both_parents, -par1_deceased, -par2_deceased, - mom_age_birth, -pincome, -pnetworth, -race_w, -par_dec) %>%
    drop_na()

M <- cor(testing_cor)
corrplot(M, type = "upper")

summary(model <- lm(pincome ~ mom_age, data = test)) #Adj R2 = 0.057
summary(model <- lm(pincome ~ mom_age + hisp, data = test)) #Adj R2 = 0.09
car::vif(model)
summary(model <- lm(pincome ~ mom_age + hisp + race_b, data = test)) # #Adj R2 = 0.154
car::vif(model)

summary(model <- lm(pincome ~ mom_age + hisp + race_b + mom_educ_hs, data = test)) #Adj R2 = 0.1767
car::vif(model)

summary(model <- lm(pincome ~ mom_age + hisp + race_b + mom_educ_hs + dad_educ_hs, data = test)) #Adj R2 = 0.1336
car::vif(model)

summary(model <- lm(pincome ~ mom_age + hisp + race_b + mom_educ_hs + dad_educ_hs + ret_sav, data = test)) #Adj R2 = 0.2047 - biggest R2 jump
car::vif(model)

summary(model <- lm(pincome ~ mom_age + hisp + race_b + mom_educ_hs + dad_educ_hs + ret_sav + hown, data = test)) #Adj R2 = 0.2165
car::vif(model)

summary(model <- lm(pincome ~ mom_age + hisp + race_b + mom_educ_hs + dad_educ_hs + ret_sav + hown + both_pars, data = test)) #Adj R2 = 0.2178
car::vif(model)

# Making df for imputation
impute <- test %>%
    select(-hispanic, -race, -mom_education, -dad_education, -savings, -home, -both_parents, -par1_deceased, -par2_deceased, - mom_age_birth, -race_w, -par_dec, -age_resp)
t <- nearZeroVar(impute) # no variables are found to have near zero variance

# Checking for missings in the predictor variables
sapply(impute, function(x) sum(is.na(x)))

# Paper that uses multiple imputation: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6049093/
# Multiple imputation in R reference: https://library.virginia.edu/data/articles/getting-started-with-multiple-imputation-in-r
init = mice(impute, maxit=10)
meth = init$method
predM = init$predictorMatrix

# Set distributions
num <- c("mom_age") # numerical variables
log <- c("hisp", "race_b", "mom_educ_hs", "dad_educ_hs", "ret_sav", "hown", "both_pars") # binary variables

meth[num] = "norm" # numerical variables
meth[log] = "logreg" # binary variables

# Assign Variables to be imputed
predM[,c("id")] <- 0 # not using as a predictor variable
predM[c("id"),] <- 0 # not using as a predictor variable
predM[,c("hisp", "mom_age", "ret_sav", "hown", "mom_educ_hs", "dad_educ_hs", "race_b", "both_pars")] <- 0 # not imputing

meth[c("hisp", "mom_age", "ret_sav", "hown", "mom_educ_hs", "dad_educ_hs", "race_b", "both_pars")]= "" #not imputing missing values, just using as a predictor

# Income and Wealth imputation
set.seed(25)
imputed = mice(impute, method=meth, predictorMatrix=predM, m=10)

imputed <- complete(imputed, action = "repeated", include = FALSE)

final <- imputed %>%
    rename(id = id.1,
           mom_age = mom_age.1,
           ret_sav = ret_sav.1,
           hown = hown.1,
           mom_educ_hs = mom_educ_hs.1,
           dad_educ_hs = dad_educ_hs.1,
           race_b = race_b.1,
           hisp = hisp.1,
           both_pars = both_pars.1) %>%
    select(-starts_with("id."), -starts_with("mom_age."), -starts_with("ret_sav."), -starts_with("hown."), -starts_with("mom_educ_hs."), -starts_with("dad_educ_hs."), -starts_with("race_b."), -starts_with("both_pars."), -starts_with("hisp."))

# Join back with original income and wealth
og_values <- impute %>%
    select(id, pincome, pnetworth)

final <- merge(x=final,y=og_values,
                    by="id", all.x=TRUE)

income <- final %>%
    select(id, starts_with("pincome"))

wealth <- final %>%
    select(id, starts_with("pnetworth"))

pincome <- tidyr::gather(income, key = "imputation", value ="pincome", matches("pincome"), -id)
pnetworth <- tidyr::gather(wealth, key = "imputation", value ="pnetworth", matches("pnetworth"), -id)

# Plots for comparing distribution for imputed and non-imputed
ggplot(pincome, aes(x=pincome, fill= imputation)) +
    geom_density(alpha = 0.5) +
    facet_wrap(~imputation, scales="free")

ggplot(pnetworth, aes(x=pnetworth, fill= imputation)) +
    geom_density(alpha = 0.5) +
    facet_wrap(~imputation, scales="free")

# A scatter plot of these two variables jointly would be useful.
pincome$imputation <- gsub("pincome.", "", pincome$imputation)
pincome$imputation <- ifelse(pincome$imputation == "pincome", 0, pincome$imputation)

pnetworth$imputation <- gsub("pnetworth.", "", pnetworth$imputation)
pnetworth$imputation <- ifelse(pnetworth$imputation == "pnetworth", 0, pnetworth$imputation)

merge <- merge(pincome, pnetworth, by = c("id", "imputation"))

ggplot(merge, aes(x=pincome, y=pnetworth, color = imputation)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~imputation, scales="free")

# Save imputed data
#write.csv(nlsydf_imp, 'NLSY/NLSY-college-finance_imp.csv')
