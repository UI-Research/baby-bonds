


sum_by_aser_time = function(data, timevar, agevar)
{
    # By age, time, race, and sex
    tmpdf1 = data |>
        count({{agevar}}, sex, race, {{timevar}}, gradecat, dataset, wt=weight) |>
        group_by(dataset, {{timevar}}, sex, race, {{agevar}}) |>
        complete(gradecat=grades, fill=list(n=0)) |>
        arrange(dataset, {{timevar}}, sex, race, {{agevar}}, desc(gradecat)) |>
        mutate(
            cumn     = cumsum(n),
            share    = n/sum(n),
            cumshare = cumsum(share)
        )
    # By age, time, "all" sexes, and race
    tmpdf2 = tmpdf1 |>
        group_by(dataset, {{timevar}}, race, {{agevar}}, gradecat) |>
        summarise(n = sum(n), cumn = sum(cumn)) |>
        arrange(dataset, {{timevar}}, race, {{agevar}}, desc(gradecat)) |>
        mutate(
            sex="All",
            share    = n/sum(n),
            cumshare = cumsum(share)
        )

    tmpdf2b = bind_rows(tmpdf1, tmpdf2)

    # By age, time, sex, and "all" races
    tmpdf3 = tmpdf2b |>
        group_by(dataset, {{timevar}}, sex, {{agevar}}, gradecat) |>
        summarise(n = sum(n), cumn = sum(cumn)) |>
        arrange(dataset, {{timevar}}, sex, {{agevar}}, desc(gradecat)) |>
        mutate(
            race="All",
            share    = n/sum(n),
            cumshare = cumsum(share)
        )

    res = bind_rows(tmpdf2b, tmpdf3) |>
        ungroup()

    stopifnot(dim(res |>
                      arrange(dataset, {{timevar}}, sex, race, {{agevar}}, desc(gradecat)) |>
                      group_by(dataset, {{timevar}}, sex, race, {{agevar}}) |>
                      mutate(testcumn=cumsum(n), diff=testcumn-cumn) |>
                      filter(diff>0))[1] == 0
    )

    return(res)
}
