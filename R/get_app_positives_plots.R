get_app_positives_plots <- function(cleaned.app.data) {
  
  # NB these were originally manually downloaded from the .gov dashboard.
  english.case.data <- read_csv("data/cases_by_specimen_date_England.csv", show_col_types=F)
  welsh.case.data <- read_csv("data/cases_by_specimen_date_Wales.csv", show_col_types=F)
  
  daily.app.positives <- cleaned.app.data %>% 
    group_by(date) %>%
    summarise("app_positives" = sum(test_positive), "app_positives_after_EN" = sum(positive_after_EN))
  
  daily.app.positives <- daily.app.positives %>%
    mutate("Rolling_app_positives" = frollmean(app_positives, n=7, fill=NA, align="center")) %>%
    mutate("Rolling_app_positives_after_EN" = frollmean(app_positives_after_EN, n=7, fill=NA, align="center"))
  
  
  engwales.case.data <- bind_rows(english.case.data, welsh.case.data) %>%
    filter(date > as.Date("2020-09-24")) %>%
    filter(date < max(english.case.data$date)) %>% # right censoring one day
    group_by(date) %>%
    summarise("Cases" = sum(newCasesBySpecimenDate)) %>%
    mutate("Rolling_cases" = frollmean(Cases, n=7, fill=NA, align="center")) # rolling 7-day mean
  
  app.positives.as.percentage.of.total <- left_join(daily.app.positives, engwales.case.data) %>%
    mutate("perc_through_app" = app_positives / Cases * 100) %>%
    mutate("Rolling_perc_through_app" = Rolling_app_positives / Rolling_cases * 100) %>%
    mutate("perc_positive_after_EN" = app_positives_after_EN / app_positives * 100) %>%
    mutate("Rolling_perc_positive_after_EN" = Rolling_app_positives_after_EN / Rolling_app_positives * 100) 
  
  CBA_EngWales <- read_csv("data/cases_by_age_England_and_Wales.csv", show_col_types=F)
  
  cases_16_to_19 <-  CBA_EngWales %>%
    ungroup() %>%
    filter(age_format == "15-19") %>%
    mutate("cases_16_to_19" = cases * 4/5) %>%
    select(date, "cases"= cases_16_to_19)
  
  cases_over_19 <- CBA_EngWales %>%
    ungroup() %>%
    filter(!age_format %in% c("0-4", "5-9", "10-14", "15-19"))
  
  cases_over_16 <- bind_rows(cases_16_to_19, cases_over_19) %>%
    group_by(date) %>%
    filter(date < max(CBA_EngWales$date)) %>% # right censoring one day
    summarise(cases_over_16 = sum(cases)) %>%
    mutate("rolling_cases_over_16" = frollmean(cases_over_16, n=7, fill=NA, align="center"))
  
  
  app.positives.as.percentage.of.total <- left_join(app.positives.as.percentage.of.total, cases_over_16) %>%
    mutate("perc_over_16_through_app" = app_positives / cases_over_16 * 100) %>%
    mutate("Rolling_perc_over_16_through_app" = Rolling_app_positives / rolling_cases_over_16 * 100)
    
  date.labels <- seq.Date(from=as.Date("2020-09-01"), to=as.Date("2021-09-30"), by="month")
  
  plots <- list()
  
  line.height <- 100
  plots$p1 <- plot_ly(app.positives.as.percentage.of.total) %>%
    filter(date >= first.date.to.plot) %>%
    filter(date <= last.date.to.plot) %>%
    add_lines(x=~date, y=~perc_through_app, name="raw",
              line=list(width=3), color=I("#1f77b4")) %>%
    add_lines(x=~date, y=~Rolling_perc_through_app, name="rolling 7-day", 
              line=list(width=5), color=I("#ff7f0e")) %>%
    layout(
      xaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="",
        titlefont=f1,
        tickfont=f1,
        range=c(first.date.to.plot, last.date.to.plot),
        tickvals = date.labels,
        ticktext = format(date.labels, "%b %y")
      ),
      yaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="Daily percentage of all national cases on\ngovernment dashboard reported through the app",
        titlefont=f1,
        tickfont=f1,
        range=c(0,line.height)
      ),
      legend=list(
        font=f1
      )
    )
  
    
  line.height <- 100
  plots$p2 <- plot_ly(app.positives.as.percentage.of.total) %>%
    filter(date >= first.date.to.plot) %>%
    filter(date <= last.date.to.plot) %>%
    add_lines(x=~date, y=~perc_positive_after_EN, name="raw", 
              line=list(width=5), color=I("#1f77b4")) %>%
    add_lines(x=~date, y=~Rolling_perc_positive_after_EN, name="rolling 7-day", 
              line=list(width=5), color=I("#ff7f0e")) %>%
    layout(
      xaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="",
        titlefont=f1,
        tickfont=f1,
        range=c(first.date.to.plot, last.date.to.plot),
        tickvals = date.labels,
        ticktext = format(date.labels, "%b %y")
      ),
      yaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="Daily percentage of app-reported cases which\nfollowed an exposure notification",
        titlefont=f4,
        tickfont=f1,
        range=c(0,20)
      ),
      legend=list(
        font=f1
      )
    )
  
  line.height <- 100
  plots$p3 <- plot_ly(app.positives.as.percentage.of.total) %>%
    filter(date >= first.date.to.plot) %>%
    filter(date <= last.date.to.plot) %>%
    add_lines(x=~date, y=~perc_over_16_through_app, showlegend=FALSE, # name="raw",
              line=list(width=3), color=I("#1f77b4")) %>%
    add_lines(x=~date, y=~Rolling_perc_over_16_through_app, showlegend=FALSE, #, name="rolling 7-day", 
              line=list(width=5), color=I("#ff7f0e")) %>%
    layout(
      xaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="",
        titlefont=f1,
        tickfont=f1,
        range=c(first.date.to.plot, last.date.to.plot),
        tickvals = date.labels,
        ticktext = format(date.labels, "%b %y")
      ),
      yaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="Daily percentage of all national cases aged 16+ on\ngovernment dashboard reported through the app",
        titlefont=f1,
        tickfont=f1,
        range=c(0,line.height)
      ),
      #showlegend=FALSE
      legend=list(
        font=f1
      )
    )
  
  
  line.height <- 5000
  plots$p4 <- plot_ly(daily.app.positives) %>%
    filter(date >= first.date.to.plot) %>%
    filter(date <= last.date.to.plot) %>%
   add_lines(x=~date, y=~app_positives_after_EN, showlegend=FALSE,# name="raw", 
            line=list(width=5), color=I("#1f77b4")) %>%
    add_lines(x=~date, y=~Rolling_app_positives_after_EN, showlegend=FALSE,# name="rolling 7-day", 
              line=list(width=5), color=I("#ff7f0e")) %>%
    layout(
      xaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="",
        titlefont=f1,
        tickfont=f1,
        range=c(first.date.to.plot, last.date.to.plot),
        tickvals = date.labels,
        ticktext = format(date.labels, "%b %y")
      ),
      yaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="Daily app-reported cases which\nfollowed an exposure notification",
        titlefont=f4,
        tickfont=f1,
        range=c(0,line.height)
      ),
      legend=list(
        font=f1
      )
    )
 
 if (resave.results) {   
         # save figure data

         Fig_3a_data <- app.positives.as.percentage.of.total %>%
           select(date,
                  "Percentage_national_tests_through_app" = perc_through_app,
                  "Rolling_percentage_national_tests_through_app" = Rolling_perc_through_app)

         write_csv(Fig_3a_data, file="results/Fig_3a.csv")  

         Fig_3b_data <- app.positives.as.percentage.of.total %>%
           select(date,
                  "Percentage_national_tests_over_16_through_app" = perc_over_16_through_app,
                  "Rolling_percentage_national_tests_over_16_through_app" = Rolling_perc_over_16_through_app)

         write_csv(Fig_3b_data, file="results/Fig_3b.csv")  


         Fig_5a_data <- daily.app.positives %>%
           select(date,
                  "App_positives_which_follow_exposure_notification" = app_positives_after_EN,
                  "Rolling_app_positives_which_follow_exposure_notification" = Rolling_app_positives_after_EN)

         write_csv(Fig_5a_data, file="results/Fig_5a.csv")  

         Fig_5b_data <- app.positives.as.percentage.of.total %>%
           select(date,
                  "Percentage_of_app_positives_which_follow_exposure_notification" = perc_positive_after_EN,
                  "Rolling_percentage_of_app_positives_which_follow_exposure_notification" = Rolling_perc_positive_after_EN)

         write_csv(Fig_5b_data, file="results/Fig_5b.csv")  
     }
 
 plots
}
