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
      completed_hs = if_else(hgc=='12TH GRADE', 1, 0, missing=0),
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
    fill(completed_hs) |>
    mutate(
      completed_hs = cumsum(completed_hs),
      hs_grad_year = case_when(
          completed_hs == 1 ~ year,
          TRUE ~ Inf
      ),
      hs_grad_year = min(hs_grad_year, na.rm=TRUE),
      hs_grad_year = if_else(hs_grad_year==Inf, NA, hs_grad_year),
      hcyc = case_when(
        # Try to avoid increases by more than 1, but not in 2019 because lead() creates NA
        #lead(hcyc)-hcyc>1 ~ hcyc+1,
        # Some people jump from 12th grade to 2nd year of college. Try to avoid it.
        lead(hcyc)==2 & hgc=='12TH GRADE' & lag(hgc)=='12TH GRADE' ~ 1,
        lead(hcyc)==2 & hcyc==2 & lag(hgc)=='12TH GRADE' ~ 1,
        TRUE ~ hcyc
      )
    ) |>
    filter(!is.na(hs_grad_year))

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

# Encode education variable
nlsy_encode_educ4 = function(var)
{
    return( case_when(
        {{var}} %in% c(
            "12TH GRADE"
        ) ~ "High-school graduate",
        {{var}} %in% c(
            "1ST YEAR COLLEGE",
            "2ND YEAR COLLEGE",
            "3RD YEAR COLLEGE"
        ) ~ "Some college",
        {{var}} %in% c(
            "4TH YEAR COLLEGE",
            "5TH YEAR COLLEGE"
        ) ~ "College degree",
        {{var}} %in% c(
            "6TH YEAR COLLEGE",
            "7TH YEAR COLLEGE",
            "8TH YEAR COLLEGE"
        ) ~ "Graduate degree"
    )
    )
}

#' Adds college graduation year to the input dataset
#'
#'  @param data: data frame with hcyc, year
nlsy_add_colgradyr = function(data)
{
    data = data |>
        group_by(id) |>
        mutate(
            # Create college graduation year (4 years of college)
            colgrad = as.integer(!is.na(hcyc) & hcyc==4),
            colgradcum = cumsum(colgrad),
            colgradyr = if_else(colgradcum==1, year, Inf, NA),
            # The calendar year in which a student enrolls in their final year
            colgradyr = min(colgradyr, na.rm=NA),
            colgradyr = if_else(colgradyr==Inf, NA, colgradyr, NA),
            maxhcyc = max(hcyc, na.rm=TRUE)
        )

    # Test
    testdf = data |>
        filter(maxhcyc>=4 & is.na(colgradyr))
    if(dim(testdf)[1] > 0) {
        warning("No colgradyr for graduates ", paste(unique(testdf$id), collapse=", "))
    }

    data = data |>
        select(-colgrad, -colgradcum, -maxhcyc) |>
        ungroup()

    return(data)
}


#' Makes a dataframe with spells of college non-enrollment and enrollment and transitions between them
#'
#'      'id' (integer)          : person ID
#'      tEnroll (integer)       : 1 for transition from non-enrollment to enrollment, 0 when non-enrolled, NA otehrwise
#'      tDrop (integer)         : 1 for transition from enrollment to non-enrollment, 0 when enrolled, NA otherwise
#'      tGrad (integer)         : 1 in the graduation year, 0 when enrolled, NA otherwise
#'      spEnroll (integer)      : number of non-enrollment spell when non-enrolled or in enrollment year, NA when enrolled
#'      spDrop (integer)        : number of enrollment spell when enrolled or in year when dropped, NA otherwise
#'      spGrad (integer)        : number of enrollment spell when enrolled or in graduation year, NA otherwise
#'      timeEnroll (integer)    : number of years since HS graduation or dropping out of college
#'      timeDrop (integer)      : number of years since enrolling into college (for dropping-out-of-college model)
#'      timeGrad (integer)      : number of years since enrolling into college (for graduation model)
#'
#' @param spell_type string Type of spell: `'all'`, `'enroll'`, `'drop'`, or `'grad'`. Defaults to `'all'`
#'
nlsy_make_spell_df = function(spell_type='all')
{
    colstdf = left_join(
        nlsy_get_col_stat_fall_df(),
        nlsy_get_highest_grade_completed_df(),
        by=c('id', 'year')
    )

    # Drop people whose information about college education is inconsistent
    # We also don't allow people who graduated in less than 4 years
    drop_ids = c(
         174, 188, 300, 306, 380, 400, 418, 506, 598, 735,
         741, 763, 951, 994, 999,1161,1902,1906,1681,1988,
        2035,2052,2610,2705,2783,2848,2926,3131,3538,3615,
        3737,3782,3846,4134,4240,4258,4419,4710,4762,5334,
        5379,5680,5796,5949,5966,6074,6093,6486,6798,6885,
        7584,7846,8006,8226,8259,8260,8402,8784,8912,8914
        )

    # Fix some observations
    colstdf = colstdf |>
        filter(!(id %in% drop_ids)) |>
        mutate(
            enrolled = case_when(
                id== 706 & year==2013   ~ TRUE,
                id==1294 & year==2004   ~ TRUE,
                id==2107 & year==2000   ~ TRUE,
                id==2107 & year==2001   ~ TRUE,
                id==2716 & year==2005   ~ TRUE,
                id==2716 & year==2006   ~ TRUE,
                id==2888 & year==2002   ~ TRUE,
                id==2888 & year==2003   ~ TRUE,
                id==4719 & year==1998   ~ TRUE,
                id==4719 & year==1999   ~ TRUE,
                id==4844 & year==2002   ~ TRUE,
                id==4844 & year==2003   ~ TRUE,
                id==4844 & year==2005   ~ TRUE,
                id==6164 & year==2016   ~ TRUE,
                id==9005 & year==2003   ~ TRUE,
                id==9005 & year==2004   ~ TRUE,
                TRUE                    ~ enrolled
                ),
            hcyc = case_when(
                id==   9 & year==2004   ~ 4,
                id==  98 & year==2006   ~ 4,
                id==  98 & year>=2007   ~ 6,
                id== 132 & year>=2005   ~ 4,
                id== 211 & year==2003   ~ 4,
                id== 279 & year==2000   ~ 1,
                id== 279 & year==2002   ~ 3,
                id== 279 & year==2003   ~ 4,
                id== 279 & year>=2004   ~ 6,
                id== 301 & year==2001   ~ 1,
                id== 301 & year==2002   ~ 2,
                id== 301 & year==2003   ~ 3,
                id== 301 & year==2004   ~ 3,
                id== 340 & year==2004   ~ 4,
                id== 391 & year==2007   ~ 4,
                id== 412 & year==2004   ~ 4,
                id== 546 & year==2002   ~ 4,
                id== 565 & year==2014   ~ 3,
                id== 565 & year==2015   ~ 4,
                id== 565 & year==2016   ~ 5,
                id== 609 & year==2005   ~ 4,
                id== 650 & year>=2007   ~ 4,
                id== 706 & year==2014   ~ 1,
                id== 745 & year>=2004   ~ 4,
                id== 762 & year==2003   ~ 4,
                id== 762 & year>=2004   ~ 5,
                id== 977 & year==2004   ~ 4,
                id== 977 & year==2005   ~ 5,
                id== 990 & year==2016   ~ 4,
                id==1009 & year>=2014   ~ 4,
                id==1019 & year==2005   ~ 4,
                id==1223 & year==2016   ~ 4,
                id==1245 & year==2002   ~ 3,
                id==1245 & year==2003   ~ 3,
                id==1245 & year==2004   ~ 4,
                id==1257 & year==2016   ~ 4,
                id==1288 & year==2006   ~ 4,
                id==1290 & year==2003   ~ 4,
                id==1294 & year==2005   ~ 5,
                id==1295 & year==2002   ~ 4,
                id==1330 & year==2004   ~ 4,
                id==1361 & year==2005   ~ 4,
                id==1549 & year==2007   ~ 4,
                id==1551 & year==2005   ~ 4,
                id==1551 & year==2006   ~ 6,
                id==1651 & year==2005   ~ 4,
                id==1655 & year==2005   ~ 4,
                id==1712 & year==2005   ~ 4,
                id==1752 & year==2005   ~ 4,
                id==1805 & year==2012   ~ 4,
                id==1874 & year==2004   ~ 4,
                id==1938 & year==2010   ~ 4,
                id==1972 & year==2014   ~ 4,
                id==2107 & year==2001   ~ 1,
                id==2308 & year==2008   ~ 4,
                id==2597 & year==2005   ~ 4,
                id==2654 & year==2005   ~ 4,
                id==2708 & year==2012   ~ 4,
                id==2716 & year==2006   ~ 1,
                id==2744 & year==2013   ~ 4,
                id==2744 & year==2014   ~ 5,
                id==2888 & year==2004   ~ 2,
                id==2888 & year==2005   ~ 3,
                id==2932 & year==2012   ~ 3,
                id==2932 & year==2013   ~ 4,
                id==2951 & year==2006   ~ 4,
                id==3096 & year==2014   ~ 4,
                id==3188 & year==2003   ~ 4,
                id==3188 & year==2005   ~ 5,
                id==3281 & year==2014   ~ 4,
                id==3318 & year==2012   ~ 4,
                id==3318 & year==2013   ~ 5,
                id==3415 & year==2016   ~ 4,
                id==3464 & year==2006   ~ 4,
                id==3553 & year==2006   ~ 4,
                id==3754 & year==2004   ~ 4,
                id==3754 & year>=2005   ~ 5,
                id==3855 & year==2005   ~ 4,
                id==4026 & year==2004   ~ 4,
                id==4026 & year==2005   ~ 5,
                id==4169 & year==2003   ~ 4,
                id==4169 & year>=2004   ~ 5,
                id==4214 & year==2005   ~ 4,
                id==4296 & year==2005   ~ 4,
                id==4391 & year==2018   ~ 4,
                id==4600 & year==2002   ~ 4,
                id==4673 & year==2012   ~ 4,
                id==4694 & year==2012   ~ 4,
                id==4698 & year==2004   ~ 3,
                id==4698 & year==2005   ~ 4,
                id==4698 & year>=2006   ~ 5,
                id==4719 & year==1999   ~ 1,
                id==4788 & year==2002   ~ 4,
                id==4832 & year==2009   ~ 4,
                id==4842 & year==2006   ~ 4,
                id==4844 & year==2003   ~ 1,
                id==4844 & year==2004   ~ 2,
                id==4844 & year==2005   ~ 3,
                id==4882 & year==2002   ~ 2,
                id==4882 & year==2003   ~ 3,
                id==4882 & year==2004   ~ 4,
                id==4882 & year==2005   ~ 5,
                id==4890 & year==2006   ~ 4,
                id==5634 & year==2016   ~ 4,
                id==5671 & year==2007   ~ 4,
                id==5725 & year==2004   ~ 4,
                id==5746 & year==2012   ~ 4,
                id==5758 & year==2003   ~ 4,
                id==5778 & year==2010   ~ 4,
                id==5783 & year==2014   ~ 4,
                id==6164 & year==2017   ~ 4,
                id==6164 & year==2018   ~ 5,
                id==6248 & year==2005   ~ 4,
                id==6313 & year==2016   ~ 4,
                id==6392 & year==2018   ~ 4,
                id==6421 & year==2006   ~ 4,
                id==6440 & year==2015   ~ 1,
                id==6440 & year==2016   ~ 2,
                id==6440 & year==2017   ~ 3,
                id==6440 & year==2018   ~ 4,
                id==6440 & year==2019   ~ 5,
                id==6561 & year==2014   ~ 4,
                id==6626 & year==2004   ~ 4,
                id==6730 & year==2008   ~ 4,
                id==6745 & year==2005   ~ 4,
                id==6808 & year==2012   ~ 4,
                id==6861 & year==2004   ~ 4,
                id==6951 & year==2012   ~ 3,
                id==6951 & year==2013   ~ 4,
                id==6951 & year==2014   ~ 5,
                id==6952 & year==2006   ~ 4,
                id==6998 & year==2005   ~ 4,
                id==7082 & year==2002   ~ 4,
                id==7111 & year==2016   ~ 4,
                id==7158 & year==2003   ~ 4,
                id==7182 & year==2002   ~ 4,
                id==7193 & year==2001   ~ 4,
                id==7432 & year==2004   ~ 4,
                id==7457 & year==2004   ~ 4,
                id==7457 & year==2005   ~ 5,
                id==7491 & year==2005   ~ 4,
                id==7601 & year==2011   ~ 4,
                id==7601 & year==2012   ~ 5,
                id==7795 & year==2018   ~ 4,
                id==8049 & year==2006   ~ 4,
                id==8168 & year==2006   ~ 4,
                id==8168 & year==2007   ~ 5,
                id==8168 & year==2008   ~ 6,
                id==8168 & year==2009   ~ 7,
                id==8453 & year==2012   ~ 4,
                id==8453 & year==2013   ~ 4,
                id==8453 & year==2014   ~ 5,
                id==8550 & year==2006   ~ 4,
                id==8645 & year==2005   ~ 4,
                id==8887 & year==2007   ~ 3,
                id==8887 & year==2008   ~ 4,
                id==8965 & year==2013   ~ 4,
                id==8965 & year==2014   ~ 5,
                id==8987 & year==2007   ~ 4,
                id==9005 & year==2004   ~ 1,
                TRUE                    ~ hcyc
            )
        )

    colenrdf = colstdf |>
        filter(year>=hs_grad_year) |>
        nlsy_add_colgradyr() |>
        group_by(id) |>
        filter(is.na(colgradyr) | colgradyr>=year) |>
        mutate(
            enrolled = if_else(year==colgradyr, TRUE, enrolled, missing=enrolled),
            enrolled = if_else(hgc=="12TH GRADE" & lead(hgc)=="1ST YEAR COLLEGE", TRUE, enrolled, missing=enrolled),
            cumenr   = cumsum(enrolled),
            tEnroll  = if_else(enrolled==TRUE  & (lag(enrolled)==FALSE | is.na(lag(enrolled))), 1, 0, missing=0),
            tDrop    = if_else(enrolled==FALSE & (lag(enrolled)==TRUE), 1, 0, missing=0),
            tGrad    = if_else(year==colgradyr, 1, 0, missing=0),
            spEnroll = if_else((enrolled==FALSE | tEnroll==1), cumsum(tEnroll) + 1 - tEnroll, NA),
            spDrop   = if_else(enrolled==TRUE   | tDrop==1,   cumsum(tEnroll), NA),
            spGrad   = if_else(cumenr>=4 & enrolled==TRUE, cumsum(tEnroll), NA),
            timeGrad = if_else(cumenr>=4 & enrolled==TRUE, cumenr-4, NA, NA),
            colenryr = if_else(tEnroll==1 & spEnroll==1, year, 0, missing=0),
            colenryr = max(colenryr)
        ) |>
        select(-cumenr) |>
        group_by(id, spEnroll) |>
        mutate(
            yrDrop      = if_else(tDrop==1, year, 0, missing=0),
            yrDrop      = max(yrDrop),
            timeEnroll  = case_when(
                spEnroll==1             ~ year-hs_grad_year,
                spEnroll>1 & tDrop==0   ~ year-yrDrop,
                TRUE                    ~ NA
            ),
            spEnroll    = if_else(tDrop==1, NA, spEnroll, missing=spEnroll)
        ) |>
        group_by(id, spDrop) |>
        mutate(
            yrEnroll    = if_else(tEnroll==1, year, 0, missing=0),
            yrEnroll    = max(yrEnroll),
            timeDrop    = if_else(!is.na(spDrop), year-yrEnroll, NA)
        ) |>
        ungroup()

    keep_vars = c('id')
    if(spell_type=='enroll') {
        colenrdf = colenrdf |>
            select(all_of(keep_vars), matches('.+Enroll')) |>
            filter(!is.na(spEnroll))
    }
    else if(spell_type=='drop') {
        colenrdf = colenrdf |>
            select(all_of(keep_vars), matches('.+Drop')) |>
            filter(!is.na(spDrop))
    }
    else if(spell_type=='grad') {
        colenrdf = colenrdf |>
            select(all_of(keep_vars), matches('.+Grad')) |>
            filter(!is.na(spGrad))
    }

    return(colenrdf)
}

