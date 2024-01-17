# This file contains functions for manipulating the NLSY97 dataset.

# Read the raw data
nlsydf = readRDS(paste0(here::here(), "/NLSY/NLSY-college-finance.rds"))

# Source scripts for interpolation and extrapolation
source('interp_extrap_lib.R')

#' Returns the NLSY dataframe with basic demographic information
#'
nlsy_get_base_df = function()
{
    set.seed(8984)

    basedf = nlsydf |>
        select(
            id              = "PUBID_1997",
            age_resp        = "CV_AGE_12/31/96_1997",
            sex             = "KEY_SEX_1997",
            bdate_m         = "KEY_BDATE_M_1997",
            bdate_y         = "KEY_BDATE_Y_1997",
            hisp            = "KEY_ETHNICITY_1997",
            race            = "KEY_RACE_1997",
            pincome         = "CV_INCOME_GROSS_YR_1997",
            pnetworth       = "CV_HH_NET_WORTH_P_1997",
            has_retsav      = "P5-130_1997",
            retsav1         = "P5-131_1997",
            retsav2         = "P5-132_1997",
            owns_home       = "P5-101_1997",
            both_parents    = "YOUTH_BOTHBIO.01_1997",
            par1_deceased   = "YOUTH_NONR1DEAD.01_1997",
            par2_deceased   = "YOUTH_NONR2DEAD.01_1997",
            dad_bio_ed      = "CV_HGC_BIO_DAD_1997",
            dad_res_ed      = "CV_HGC_RES_DAD_1997",
            mom_bio_ed      = "CV_HGC_BIO_MOM_1997",
            mom_res_ed      = "CV_HGC_RES_MOM_1997",
            mom_age_birth   = "CV_BIO_MOM_AGE_YOUTH_1997",
            wt
        )

    basedf = basedf |>
        mutate(
            race = droplevels(race),
            has_retsav = case_when(
                has_retsav=='YES' ~ 1,
                has_retsav=='NO'  ~ 0,
                TRUE ~ NA
            ),
            owns_home = if_else(
                owns_home == "OWNS OR IS BUYING; LAND CONTRACT", 1, 0
                ),
            both_parents = if_else(both_parents=="Yes", 1, 0),
            # Limit mother's age at birth
            mom_age_birth = pmax(mom_age_birth, 16),
            mom_age_birth = pmin(mom_age_birth, 45),
            mom_age = mom_age_birth + age_resp,
            # Limit parents' income
            pincome = pmax(0, pincome)
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

#' Recodes race and ethnicity
nlsy_recode_race_and_eth5 = function(race, hisp)
{
    return(case_when(
        {{hisp}} == 'Yes'                               ~ 'Hispanic',
        {{race}} == 'Black or African American'         ~ 'Black',
        {{race}} == 'Asian or Pacific Islander'         ~ 'Asian',
        {{race}} == 'American Indian, Eskimo, or Aleut' ~ 'Other',
        {{race}} == 'White'                             ~ 'White',
        {{race}} == 'Something else? (SPECIFY)'         ~ 'Other',
        TRUE                                            ~ {{race}}
        )
    )
}


nlsy_recode_race_and_eth10 = function(race, hisp)
{
    return(case_when(
        {{hisp}} == "Yes" & {{race}} == "White"                             ~ "White Hispanic",
        {{hisp}} == "No"  & {{race}} == "White"                             ~ "White NonHispanic",
        {{hisp}} == "Yes" & {{race}} == "Black or African American"         ~ "Black Hispanic",
        {{hisp}} == "No"  & {{race}} == "Black or African American"         ~ "Black NonHispanic",
        {{hisp}} == "Yes" & {{race}} == "American Indian, Eskimo, or Aleut" ~ "AIAN Hispanic",
        {{hisp}} == "Yes" & {{race}} == "Asian or Pacific Islander"         ~ "AAPI Hispanic",
        {{hisp}} == "Yes" & {{race}} == "Something else? (SPECIFY)"         ~ "Other Race Hispanic",
        {{hisp}} == "No"  & {{race}} == "American Indian, Eskimo, or Aleut" ~ "AIAN NonHispanic",
        {{hisp}} == "No"  & {{race}} == "Asian or Pacific Islander"         ~ "AAPI NonHispanic",
        {{hisp}} == "No"  & {{race}} == "Something else? (SPECIFY)"         ~ "Other Race NonHispanic"
        )
    )
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

#' Returns marriage status the last month of a given year
nlsy_get_marstat_df = function()
{
    data = nlsydf |>
        select(
            id =        "PUBID_1997",
            starts_with('MAR_STATUS_')
        ) |>
        pivot_longer(
            starts_with('MAR_STATUS_'),
            names_to=c('year', 'month'),
            names_pattern='MAR_STATUS_(\\d\\d\\d\\d)\\.(\\d\\d)_XRND',
            values_to='mar_status'
        ) |>
        mutate(
            year=as.integer(year),
            month=as.integer(month)
        ) |>
        group_by(id,year) |>
        fill(mar_status, .direction = "downup") |>
        filter(month == 12, year > 1996) |>
        group_by(id) |>
        arrange(id, year) |>
        complete(year=1997:2019) |>
        fill(mar_status, .direction = "downup") |>
    #Counting missings:sum(is.na(data))/12 = 181.25 out of 8984 in nlsydf
        mutate(mar_status = case_when(
            is.na(mar_status) ~ 'Never Married, Not Cohabitating',
            TRUE ~ mar_status)
         ) |>
        mutate(
            single=as.integer(mar_status == 'Never Married, Not Cohabitating'),
            cohabitating=as.integer(mar_status == 'Never Married, Cohabiting'),
            married=as.integer(mar_status == 'Married'),
            divorced=as.integer(mar_status == 'Divorced'),
            widowed=as.integer(mar_status == 'Widowed')
        )|>
        select(-month)

    return(data)
}

#' Returns number of biological children in and out of household
nlsy_get_biochild_df = function()
{
    data1 = nlsydf |>
        select(
            id =        "PUBID_1997",
            starts_with('CV_BIO_CHILD_HH'),
            -contains('U18') #Dropping variables with only children under 18

        ) |>
        pivot_longer(
            starts_with('CV_BIO_CHILD_HH'),
            names_to='year',
            names_pattern='CV_BIO_CHILD_HH_(\\d\\d\\d\\d)',
            values_to='bio_child_hh',
            values_transform = list(bio_child_hh = as.integer)
        )

    data2 = nlsydf |>
        select(
            id =        "PUBID_1997",
            starts_with('CV_BIO_CHILD_NR'),
            -contains('U18') #Dropping variables with only children under 18

        ) |>
        pivot_longer(
            starts_with('CV_BIO_CHILD_NR'),
            names_to='year',
            names_pattern='CV_BIO_CHILD_NR_(\\d\\d\\d\\d)',
            values_to='bio_child_nr',
            values_transform = list(bio_child_nr = as.integer)
        )

    data = left_join(data1, data2, by=c('id', 'year'))|>
        mutate(year = as.integer(year))|>
        group_by(id) |>
        complete(year=1997:2019) |>
        fill(bio_child_hh, bio_child_nr)|>
        mutate(bio_child_hh = case_when(
                   is.na(bio_child_hh) ~ 0, # Assuming zeroes are part of the survey auto skips
                   TRUE ~ bio_child_hh),
               bio_child_nr = case_when(
                   is.na(bio_child_nr) ~ 0, # Assuming zeroes are part of the survey auto skips
                   TRUE ~ bio_child_nr)
        )|>
        mutate(bio_child = bio_child_hh + bio_child_nr)|>
        select(-bio_child_nr, -bio_child_hh, -bio_child)

return(data)
}


#' Adds longitudinal income and wealth vars to the input dataset
nlsy_get_income_wealth_df = function(){
    income <- nlsydf |>
        select(id = PUBID_1997, income_source = CV_HH_INCOME_SOURCE_1997, matches('CV_INCOME_GROSS_YR|CV_INCOME_FAMILY')) |>
        pivot_longer(-c('id', 'income_source'),
                     names_to = 'year', values_to = 'income') |>
        mutate(year = as.numeric(str_sub(year, start=-4)))
    networth_97_03 <- nlsydf |>
        select(id = PUBID_1997, matches('CV_HH_NET_WORTH_Y')) |>
        pivot_longer(-id,
                     names_to = 'year', values_to = 'networth') |>
        mutate(year = as.numeric(str_sub(year, start=-4)))

    networth_5yr <- nlsydf |>
        select(id = PUBID_1997, age_resp = "CV_AGE_12/31/96_1997", matches('CVC_HH_NET_WORTH_\\d{2}')) |>
        pivot_longer(-c('id', 'age_resp'),
                     names_to = 'year', values_to = 'networth') |>
        mutate(year = as.numeric(str_sub(year, start=-7, end=-6)), # Get age for 5-yr wealth vars
               year = 1997 + (year - age_resp)) # Add correct number of years based on resp's starting age

    networth_merged <- tidylog::full_join(networth_97_03, networth_5yr, by = c('id', 'year', 'networth')) |>
        select(-age_resp)

    dups <- count(networth_merged, id, year) |> filter(n>1)
    dups_rm_na <- networth_merged |> inner_join(dups, by=c('id', 'year')) |> filter(!is.na(networth)) |> select(-n)
    networth_merged2 <- networth_merged |>
        anti_join(dups, by=c('id', 'year')) |> # Remove duplicate id-years
        bind_rows(dups_rm_na) |> # Add back in the de-duplicated id-years
        arrange(id, year)


    income_networth <- tidylog::full_join(income, networth_merged2, by = c('id', 'year')) |>
        group_by(id) |>
        complete(year = 1997:2019) |>
        filter(year <= 2019) |>
        ungroup()

    return(income_networth)

}



#' Interpolates income and wealth
#' @param data: data frame with income, wealth by ID and year
#' @param interp_type: Type of interpolation, either approx for linear or spline for nonlinear
nlsy_interp_inc_nw <- function(data, interp_type=approx){
    # Can't interpolate for these IDs
    missing_ids_income <- data |> summarize(income_na = sum(is.na(income)), .by='id') |> filter(income_na >= 22) |> pull(id)
    missing_ids_nw <- data |> summarize(networth_na = sum(is.na(networth)), .by='id') |> filter(networth_na >= 22) |> pull(id)

    interp_income_nw <- full_join(
        data |>
            filter(!(id %in% missing_ids_income)) |>
            group_by(id) |>
            do(grouped_interpolate(., variable='income', interp_type=interp_type)) |>
            ungroup(),
        data |>
            filter(!(id %in% missing_ids_nw)) |>
            group_by(id) |>
            do(grouped_interpolate(., variable='networth', interp_type=interp_type)) |>
            ungroup(),
        by = c('id', 'year')
    )
    return(interp_income_nw)
}


#' Extrapolates income and wealth
#' #' @param data: data frame with income, wealth interpolated by ID and year
nlsy_extrap_inc_nw <- function(data){
    by_id <- data |>
        group_by(id) |>
        nest()
    by_id_extrap <- by_id |>
        mutate(start_inc = map(data, function(df) df |> filter(!is.na(income)) |> head(1) |> pull(year)),
               end_inc = map(data, function(df) df |> filter(!is.na(income)) |> tail(1) |> pull(year)),
               start_nw = map(data, function(df) df |> filter(!is.na(networth)) |> head(1) |> pull(year)),
               end_nw = map(data, function(df) df |> filter(!is.na(networth)) |> tail(1) |> pull(year)),
               ts_income = map(data, ~create_ts_by_id(.x, 'income')),
               ts_networth = map(data, ~create_ts_by_id(.x, 'networth')),
               income_extrap = pmap(list(ts_income, start_inc, end_inc, id), extrap_ts, variable='income'),
               networth_extrap = pmap(list(ts_networth, start_nw, end_nw, id), extrap_ts, variable='networth')
               ) |>
        select(id, income_extrap, networth_extrap) |>
        unnest(-id) |>
        ungroup()
    return(by_id_extrap |>
                bind_cols(year = rep(1997:2019, length(unique(by_id_extrap$id)))))
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

nlsy_get_educ5_levels = function()
{
    return(c(
        "Less than high school",
        "High-school graduate",
        "Some college",
        "College degree",
        "Graduate degree"
    ))
}

#' Encodes education into 5 levels
nlsy_encode_educ5 = function(var, factorize=FALSE)
{

    edlevels = nlsy_get_educ5_levels()

    x = case_when(
        {{var}} %in% c(
            "1ST GRADE",
            "2ND GRADE",
            "3RD GRADE",
            "4TH GRADE",
            "5TH GRADE",
            "6TH GRADE",
            "7TH GRADE",
            "8TH GRADE",
            "9TH GRADE",
            "10TH GRADE",
            "11TH GRADE"
        ) ~ edlevels[1],
        {{var}} %in% c(
            "12TH GRADE"
        ) ~ edlevels[2],
        {{var}} %in% c(
            "1ST YEAR COLLEGE",
            "2ND YEAR COLLEGE",
            "3RD YEAR COLLEGE"
        ) ~ edlevels[3],
        {{var}} %in% c(
            "4TH YEAR COLLEGE",
            "5TH YEAR COLLEGE"
        ) ~ edlevels[4],
        {{var}} %in% c(
            "6TH YEAR COLLEGE",
            "7TH YEAR COLLEGE",
            "8TH YEAR COLLEGE"
        ) ~ edlevels[5]
    )

    if(factorize) {
        x = factor(x, levels=edlevels, ordered=TRUE)
    }

    return(x)
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
    colstdf =  left_join(nlsy_get_col_stat_fall_df(), nlsy_get_col_stat_fall_df(), by=c('id', 'year')) %>%
        left_join(., nlsy_get_marstat_df(), by=c('id', 'year')) %>%
        left_join(., nlsy_get_biochild_df(), by=c('id', 'year')
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

