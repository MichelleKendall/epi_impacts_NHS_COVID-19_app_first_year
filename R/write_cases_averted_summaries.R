write_cases_averted_summaries <- function(wave1, wave2, wave3) {
    
    national.averted <- tibble(
      "wave" = c("pre-alpha", "alpha", "delta"),
      "cases.averted" = c(wave1$total_averted, wave2$total_averted, wave3$total_averted),
      "cases.averted_upper" = c(wave1$total_averted_upper, wave2$total_averted_upper, wave3$total_averted_upper),
      "cases.averted_lower" = c(wave1$total_averted_lower, wave2$total_averted_lower, wave3$total_averted_lower),
      "CHR" = c(0.098, 0.05, 0.027), # case hospitalisation rate per wave
      "CFR" = c(0.015, 0.019, 0.002) # case fatality rate per wave
    ) %>%
      mutate("hospitalisations.averted" = cases.averted * CHR,
             "hospitalisations.averted_upper" = cases.averted_upper * CHR,
             "hospitalisations.averted_lower" = cases.averted_lower * CHR,
             "deaths.averted" = cases.averted * CFR,
             "deaths.averted_upper" = cases.averted_upper * CFR,
             "deaths.averted_lower" = cases.averted_lower * CFR)

    if (resave.results) {   write_csv(national.averted, file=glue("results/national.averted.per.wave.csv")) }

    # totals across the waves. Write to file, rounded to 2 significant figures for an easy summary.
    total.cases.averted <- sum(national.averted$cases.averted)
    total.cases.averted_upper <- sum(national.averted$cases.averted_upper)
    total.cases.averted_lower <- sum(national.averted$cases.averted_lower)
    total.hospitalisations.averted <- sum(national.averted$hospitalisations.averted)
    total.hospitalisations.averted_upper <- sum(national.averted$hospitalisations.averted_upper)
    total.hospitalisations.averted_lower <- sum(national.averted$hospitalisations.averted_lower)
    total.deaths.averted <- sum(national.averted$deaths.averted)
    total.deaths.averted_upper <- sum(national.averted$deaths.averted_upper)
    total.deaths.averted_lower <- sum(national.averted$deaths.averted_lower)

    results.summary <- (signif(bind_rows(
      "cases averted" = c("central" = total.cases.averted, "upper" = total.cases.averted_upper, "lower" = total.cases.averted_lower),
      "hospitalisations averted" = c("central" = total.hospitalisations.averted, "upper" = total.hospitalisations.averted_upper, "lower" = total.hospitalisations.averted_lower),
      "deaths averted" = c("central" = total.deaths.averted, "upper" = total.deaths.averted_upper, "lower" = total.deaths.averted_lower)
    ),2))

    if (resave.results) {   write_csv(results.summary, file=glue("results/national.averted.totals.csv")) }
}