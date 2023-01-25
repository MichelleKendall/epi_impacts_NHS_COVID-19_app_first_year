compute_cases_averted_ltlas <- function(app.and.case.data, recalculate = FALSE) {
    
    if(!file.exists("results/averted.first.year.by.ltla.csv")||recalculate){
        
       by.la <- sapply(sort(unique(app.and.case.data$ltla_name)), function(area) {
            
            area.app.and.case.data <- app.and.case.data %>% filter(ltla_name == area)
  
            wave1 <- compute_cases_averted_per_wave(wave.start.date=as.Date("2020-09-24"), 
                                                           wave.end.date=as.Date("2021-05-17"), 
                                                           app.and.case.data = area.app.and.case.data, 
                                                           wave="pre.alpha",
                                                           other_quarantine_reduction = risky.contact.reduction.factor.pre.alpha,
                                                           max.proportion.who.know.infected = max.omega.pre.alpha)
  
            wave2 <- compute_cases_averted_per_wave(wave.start.date=as.Date("2020-09-24"), 
                                                           wave.end.date=as.Date("2021-05-17"), 
                                                           app.and.case.data = area.app.and.case.data, 
                                                           wave="alpha",
                                                           other_quarantine_reduction = risky.contact.reduction.factor.alpha,
                                                           max.proportion.who.know.infected = max.omega.alpha)
  
            wave3 <- compute_cases_averted_per_wave(wave.start.date=as.Date("2021-05-18"), 
                                                           wave.end.date=as.Date("2021-09-24"), 
                                                           app.and.case.data = area.app.and.case.data, 
                                                           wave="delta",
                                                           other_quarantine_reduction = risky.contact.reduction.factor.delta,
                                                           max.proportion.who.know.infected = max.omega.delta)
            
            wave1$total_averted + wave2$total_averted + wave3$total_averted
  
        })

        #stopifnot(signif(sum(by.la),2) == signif(total.cases.averted, 2))
        #signif(by.la,3)

        by.la.tibble <- tibble(
          "ltla_name" = names(by.la),
          "averted" = by.la
        )
        
        # join with actual cases data - useful for plotting later 
        by.la.tibble <- left_join(by.la.tibble, 
                                  app.and.case.data %>%
                                    filter(date >= as.Date("2020-09-24")) %>%
                                    filter(date <= as.Date("2021-09-24")) %>%
                                    group_by(ltla_name) %>%
                                    summarise("total.actual.cases" = sum(cases)), 
                                  by="ltla_name") %>%
        mutate("averted.as.perc.of.actual" = averted / total.actual.cases * 100,  # how many more cases there would have been as a percent of actual
               "averted.as.perc.of.counterfactual" = averted / (averted + total.actual.cases) * 100)


        write_csv(by.la.tibble, file="results/averted.first.year.by.ltla.csv") 
    
    } # end if
    
} # end function


# BY REGION - NOT USED

#by.region <- sapply(sort(unique(app.and.case.data$Region)), function(area) {
#  
#  region.app.and.case.data <- app.and.case.data %>% filter(Region == area)
#  
#  wave1 <- get_total_cases_averted_per_wave_with_realising(wave.start.date=as.Date("2020-09-24"), 
#                                                           wave.end.date=as.Date("2021-05-17"), 
#                                                           app.and.case.data = region.app.and.case.data, 
#                                                           wave="pre.alpha",
#                                                           other_quarantine_reduction = risky.contact.reduction.factor.pre.alpha,
#                                                           max.proportion.who.know.infected = max.omega.pre.alpha)
#  
#  wave2 <- get_total_cases_averted_per_wave_with_realising(wave.start.date=as.Date("2020-09-24"), 
#                                                           wave.end.date=as.Date("2021-05-17"), 
#                                                           app.and.case.data = region.app.and.case.data, 
#                                                           wave="alpha",
#                                                           other_quarantine_reduction = risky.contact.reduction.factor.alpha,
#                                                           max.proportion.who.know.infected = max.omega.alpha)
#  
#  wave3 <- get_total_cases_averted_per_wave_with_realising(wave.start.date=as.Date("2021-05-18"), 
#                                                           wave.end.date=as.Date("2021-09-24"), 
#                                                           app.and.case.data = region.app.and.case.data, 
#                                                           wave="delta",
#                                                           other_quarantine_reduction = risky.contact.reduction.factor.delta,
#                                                           max.proportion.who.know.infected = max.omega.delta)
#  
#  wave1$total_averted + wave2$total_averted + wave3$total_averted
#})
#
#stopifnot(signif(sum(by.region),2) == signif(total.cases.averted, 2))
#signif(by.region,3)
#
#by.region.tibble <- tibble(
#  "region" = names(by.region),
#  "averted" = by.region
#)
#
#write_csv(by.region.tibble, file="data/averted.first.year.by.region.csv")

