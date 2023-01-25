get_TPAEN_context_plot <- function(national.data, first.date.to.plot, last.date.to.plot) {
  
  # we use Table 1j from https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata
  # and age structure from mid 2020 from  https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
  
  prevalence_age <- read_csv("data/ons_prevalence_age.csv", show_col_types=F)
  ons_age_structure <- read_csv("data/ons_age_structure.csv", show_col_types=F) 
  
  age_weighting_denominator <- sum(ons_age_structure %>%
                                     filter(age.lower >=20) %>%
                                     select(population)) +
                                (ons_age_structure  %>% 
                                  filter(age.lower >= 15) %>% 
                                  filter(age.upper <= 19) %>%
                                  select(population)) * 4/5
                              
  population.15_24 <- ons_age_structure %>% 
                        filter(age.lower >= 15) %>% 
                        filter(age.upper <= 24)
  
  weight.16_24 <- (population.15_24$population[[1]]*4/5 + population.15_24$population[[2]])/age_weighting_denominator
                        
  
  weight.25_34 <- sum(ons_age_structure %>% 
                        filter(age.lower >= 25) %>% 
                        filter(age.upper <= 34) %>%
                        select(population))/age_weighting_denominator
  
  weight.35_49 <- sum(ons_age_structure %>% 
                        filter(age.lower >= 35) %>% 
                        filter(age.upper <= 49) %>%
                        select(population))/age_weighting_denominator
  
  weight.50_69 <- sum(ons_age_structure %>% 
                        filter(age.lower >= 50) %>% 
                        filter(age.upper <= 69) %>%
                        select(population))/age_weighting_denominator
  
  weight.70plus <- sum(ons_age_structure %>% 
                        filter(age.lower >= 70) %>% 
                        select(population))/age_weighting_denominator
    
  
  prevalence.16plus <- prevalence_age %>% 
    #filter(date >= as.Date("2021-02-01")) %>%
    mutate("weight.16_24" = as.numeric(weight.16_24),
           "weight.25_34" = as.numeric(weight.25_34),
           "weight.35_49" = as.numeric(weight.35_49),
           "weight.50_69" = as.numeric(weight.50_69),
           "weight.70plus" = as.numeric(weight.70plus)) %>%
    mutate("weighted.prevalence.16plus" = prevalence.16_24 * weight.16_24 +
             prevalence.25_34 * weight.25_34 +
             prevalence.35_49 * weight.35_49 + 
             prevalence.50_69 * weight.50_69 +
             prevalence.70plus * weight.70plus) %>%
    mutate("lower.weighted.prevalence.16plus" = lower.prevalence.16_24 * weight.16_24 +
             lower.prevalence.25_34 * weight.25_34 +
             lower.prevalence.35_49 * weight.35_49 + 
             lower.prevalence.50_69 * weight.50_69 +
             lower.prevalence.70plus * weight.70plus) %>%
    mutate("upper.weighted.prevalence.16plus" = upper.prevalence.16_24 * weight.16_24 +
             upper.prevalence.25_34 * weight.25_34 +
             upper.prevalence.35_49 * weight.35_49 + 
             upper.prevalence.50_69 * weight.50_69 +
             upper.prevalence.70plus * weight.70plus)
  
  TPAEN.rolling <- national.data %>%
    select(date, TPAEN) %>%
    #filter(date >= as.Date("2021-02-01")) %>%
    mutate("TPAEN.percent" = TPAEN*100) %>%
    mutate("TPAEN.rolling" = frollmean(TPAEN.percent, n=14, align="center", fill=6.02))
  
  TPAEN.context <- left_join(prevalence.16plus %>% select(date, weighted.prevalence.16plus, lower.weighted.prevalence.16plus, upper.weighted.prevalence.16plus), TPAEN.rolling) %>%
    mutate("times.more.likely.infected.if.pinged" = TPAEN.rolling/weighted.prevalence.16plus)  %>%
    mutate("times.more.likely.infected.if.pinged.lower" = TPAEN.rolling/upper.weighted.prevalence.16plus)  %>%
    mutate("times.more.likely.infected.if.pinged.upper" = TPAEN.rolling/lower.weighted.prevalence.16plus) %>% 
    ungroup()
  
  if (resave.results) {     
      # save figure data

      Fig_5d_data <- TPAEN.context  %>%
        filter(date >= first.date.to.plot) %>%
        filter(date <= last.date.to.plot) %>%
        select(date,
               "times_more_likely_infected_if_notified" = times.more.likely.infected.if.pinged,
               "times_more_likely_infected_if_notified_lower" = times.more.likely.infected.if.pinged.lower,
               "times_more_likely_infected_if_notified_upper" = times.more.likely.infected.if.pinged.upper)

      write_csv(Fig_5d_data, file="results/Fig_5d.csv")
  }    
  
  date.labels <- seq.Date(as.Date("2020-10-01"), as.Date("2021-09-24"), by="month")
  
  line.height <- 55
  plot_ly(TPAEN.context) %>%
    filter(date >= first.date.to.plot) %>%
    filter(date <= last.date.to.plot) %>%
    add_lines(x=~date, y=~times.more.likely.infected.if.pinged, showlegend=FALSE,
              line=list(width=5), color=I("#b73779"), name="Whole population") %>%
    add_ribbons(x=~date, ymin=~times.more.likely.infected.if.pinged.lower, ymax=~times.more.likely.infected.if.pinged.upper,
                color=I("#b73779"), opacity=0.5,
                showlegend=FALSE) %>%
    add_lines(x=~date, y=1, color=I("black"), line=list(dash="dash", width=5), showlegend=FALSE) %>%
    layout(
      xaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="",
        titlefont=f1,
        tickfont=f1,
        tickvals = date.labels,
        ticktext = format(date.labels, "%b %y")
      ),
      yaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="Daily probability of testing positive when notified\nby the app relative to the general population",
        tickfont=f1,
        titlefont=f4,
        type="log",
        tickvals=c(1, 2,3,4, 5, 10, 20, 30, 40, 50),
        range=c(-0.3,1.7)
      )
    )

}
