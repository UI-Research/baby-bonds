# This file contains functions for manipulating the NLSY97 dataset.

# Read the raw data
nlsydf = readRDS(paste0(here::here(), "/NLSY/NLSY-college-finance.rds"))

#' Returns the NLSY dataframe with basic demographic information
#'
nlsy_get_base_df = function()
{
    set.seed(8984)

    basedf = nlsydf |>
        select(
            id =        "PUBID_1997",
            age0 =      "CV_AGE_12/31/96_1997",
            sex =       "KEY_SEX_1997",
            bdate_m =   "KEY_BDATE_M_1997",
            bdate_y =   "KEY_BDATE_Y_1997",
            hisp =      "KEY_ETHNICITY_1997",
            race =      "KEY_RACE_1997",
            pincome =   "CV_INCOME_GROSS_YR_1997",
            pnetworth = "CV_HH_NET_WORTH_P_1997",
            has_retsav= "P5-130_1997",
            retsav1 =   "P5-131_1997",
            retsav2 =   "P5-132_1997",
            wt
        )

    basedf = basedf |>
        mutate(
            race = droplevels(race),
            hisp = fct_recode(
                hisp,
                'Hispanic'     = 'Yes',
                'Non-Hispanic' = 'No'
            ),
            has_retsav = case_when(
                has_retsav=='YES' ~ 1,
                has_retsav=='NO'  ~ 0,
                TRUE ~ NA
            )
        )

    basedf = basedf |>
        mutate(
            retsav3 = case_when(
                retsav2 == "A.  $1               -       $5,000" ~ runif(n(), 1, 5000),
                retsav2 == "B.   $5,001      -     $10,000"      ~ runif(n(), 5001, 10000),
                retsav2 == "C.   $10,001    -     $25,000"       ~ runif(n(), 10001, 25000),
                retsav2 == "D.   $25,001    -     $50,000"       ~ runif(n(), 25001, 50000),
                retsav2 == "E.    $50,001   -    $100,000"       ~ runif(n(), 50001, 100000),
                retsav2 == "F.    $100,001       $250,000"       ~ runif(n(), 100001, 250000),
                retsav2 == "G.    More than $250,000"            ~ runif(n(), 250001, 500000),
                TRUE ~ NA
            )
        )

    basedf = basedf |>
        mutate(
            retsav = case_when(
                !is.na(retsav1) ~ retsav1,
                !is.na(retsav3) ~ retsav3,
                TRUE ~ NA
            )
        )

    # Check that we have retsav for all people who have retirement savings and
    # whose networth is not missing
    stopifnot(
        dim(
            filter(
                basedf,
                !is.na(pnetworth) & !is.na(has_retsav) & has_retsav & is.na(retsav)
            )
        )[1] == 0
    )

    return(basedf)
}

#' Recodes race
nlsy_recode_race = function(data)
{
    data = data |>
        mutate(
            race = fct_recode(
                race,
                'Other'  = "Something else? (SPECIFY)",
                'Black'  = "Black or African American",
                'Other'  = "Asian or Pacific Islander",
                'Other' = "American Indian, Eskimo, or Aleut"
            )
        )
    return(data)
}

#' Recodes race and ethnicity
nlsy_recode_race_and_ethn = function(data)
{
    data = data |>
        mutate(
            race = case_when(
                hisp == 'Hispanic'                          ~ 'Hispanic',
                race == 'Black or African American'         ~ 'Black',
                race == 'Asian or Pacific Islander'         ~ 'Other',
                race == 'American Indian, Eskimo, or Aleut' ~ 'Other',
                race == 'White'                             ~ 'White',
                race == 'Something else? (SPECIFY)'         ~ 'Other',
                TRUE                                        ~ race
            )
        )

    #    stopifnot(all(!is.na(data$race)))
    return(data)
}


#' Returns data with college enrollment status in survey years
nlsy_get_col_stat_annual_df = function()
{
    data = nlsydf |>
        select(
            id =        "PUBID_1997",
            starts_with('CV_ENROLLSTAT')
        ) |>
        rename_with(~gsub('_EDT_', '_', .x)) |>
        pivot_longer(
            starts_with('CV_ENROLLSTAT'),
            names_to='year',
            names_prefix='CV_ENROLLSTAT_') |>
        mutate(year=as.integer(year)) |>
        mutate(colenr = case_when(
            (value=="Enrolled in a 2-year college" |
                 value=="Enrolled in a 4-year college") ~ 1,
            TRUE ~ 0
        )
        )
    return(data)
}

#' Returns college enrollment status in the fall semester
nlsy_get_col_stat_fall_df = function()
{
    data = nlsydf |>
        select(
            id =        "PUBID_1997",
            starts_with('SCH_COLLEGE_STATUS_')
        ) |>
        pivot_longer(
            starts_with('SCH_COLLEGE_STATUS_'),
            names_to=c('year', 'month'),
            names_pattern='SCH_COLLEGE_STATUS_(\\d\\d\\d\\d)\\.(\\d\\d)_XRND',
            values_to='college_status'
        ) |>
        mutate(
            year=as.integer(year),
            month=as.integer(month),
            in_college=as.integer(college_status %in%
                                      c("Enrolled in 2-year college",
                                        "Enrolled in 4-year college")
            ),
            in_gradsch=as.integer(college_status == 'Enrolled in Graduate program')
        )

    data = data |>
        # We are interested in new academic year that starts in August
        filter(month >= 8) |>
        group_by(id, year) |>
        summarise(
            # All months are missing
            allna = all(is.na(college_status)),
            # Enrolled during any month
            enrolled=any(in_college, na.rm=TRUE)
        ) |>
        filter(!allna) |>
        select(-allna)

    return(data)
}

#' Returns data with the highest grade completed
nlsy_get_highest_grade_completed_df = function()
{
    data = nlsydf |>
        select(
            id =        "PUBID_1997",
            matches('CV_HGC_EVER_1997'),
            matches('CV_HGC_EVER_EDT_\\d\\d\\d\\d'),
        ) |>
        rename_with(~gsub('_EDT_', '_', .x)) |>
        pivot_longer(
            starts_with('CV_HGC_EVER_'),
            names_to='year',
            names_prefix='CV_HGC_EVER_',
            values_to='hgc'
        ) |>
        mutate(
            year=as.integer(year),
            completed_hs = as.integer(hgc=='12TH GRADE'),
            # Highest college year completed
            hcyc=case_when(
                hgc=='1ST YEAR COLLEGE'         ~ 1,
                hgc=='2ND YEAR COLLEGE'         ~ 2,
                hgc=='3RD YEAR COLLEGE'         ~ 3,
                hgc=='4TH YEAR COLLEGE'         ~ 4,
                hgc=='5TH YEAR COLLEGE'         ~ 5,
                hgc=='6TH YEAR COLLEGE'         ~ 6,
                hgc=='7TH YEAR COLLEGE'         ~ 7,
                hgc=='8TH YEAR COLLEGE OR MORE' ~ 8,
                TRUE                            ~ NA
            )) |>
        group_by(id) |>
        # Add years between surveys
        complete(year=1997:2019) |>
        # Fill missing values with the previous year
        fill(hcyc) |>
        mutate(
            completed_hs = cumsum(completed_hs),
            hs_comp_year = case_when(
                completed_hs == 1 ~ year,
                TRUE ~ NA
            ),
            hs_comp_year = min(hs_comp_year, na.rm=TRUE),
            hcyc = case_when(
                # Try to avoid increases by more than 1, but not in 2019 because lead() creates NA
                lead(hcyc)-hcyc>1 ~ hcyc+1,
                # Some people jump from 12th grade to 2nd year of college. Try to avoid it.
                lead(hcyc)==2 & hgc=='12TH GRADE' & lag(hgc)=='12TH GRADE' ~ 1,
                lead(hcyc)==2 & hcyc==2 & lag(hgc)=='12TH GRADE' ~ 1,
                TRUE ~ hcyc
            )
        ) |>
        filter(!is.na(hs_comp_year))

    return(data)
}

# Returns data with relations to other family members
nlsy_get_famrel_df = function()
{
    withpardf = nlsydf |>
        select(
            id =        "PUBID_1997",
            matches('^HHI2*_RELY.*')
        ) |>
        rename_with(~gsub('HHI2_', 'HHI_',.x), starts_with('HHI2_')) |>
        pivot_longer(
            starts_with('HHI_'),
            names_to=c('memberID', 'year'),
            names_pattern='HHI_RELY\\.(\\d\\d)_(\\d\\d\\d\\d)',
            names_transform = list(year=as.integer)
        )

    return(withpardf)
}

#' Returns data with the amount of student loan debt by year
nlsy_get_student_loans_df = function()
{
    sloandf = nlsydf |>
        select(
            id =        "PUBID_1997",
            starts_with('YSCH-25700')
        ) |>
        left_join(evercoldf, by='id') |>
        filter(evercol==1) |>
        select(-"YSCH-25700.01_1997", -evercol) |>
        pivot_longer(
            starts_with('YSCH-25700'),
            names_to=c('college', 'term', 'year'),
            names_pattern='YSCH-25700\\.(\\d\\d)\\.(\\d\\d)\\_(\\d\\d\\d\\d)'
        ) |>
        group_by(id, year) |>
        summarise(debt=sum(value, na.rm=TRUE)) |>
        mutate(hasdebt=ifelse(debt>0, 1, 0))
    return(sloandf)
}
