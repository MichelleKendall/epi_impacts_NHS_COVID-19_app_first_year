get_comparing_to_manual_plot <- function(cleaned.app.data){
   
  manual_tracing <- read_csv("data/manual_tracing.csv", show_col_types = F) # data originally from https://www.gov.uk/government/publications/weekly-statistics-for-nhs-test-and-trace-england-31-march-to-6-april-2022
  
  manual_tracing <- manual_tracing %>% # report for middle of the week
    mutate("date" = date + 3)
  
  weekly_enpic <- cleaned.app.data %>% 
    filter(date > first.date.to.plot) %>%
    filter(date <= last.date.to.plot) %>%
    group_by(date) %>%
    summarise("N_approx" = sum(notifications_approx), 
              "N" = sum(notifications), 
              "P" = sum(test_positive)) %>%
    mutate("enpic_approx" = N_approx / P,
           "enpic" = N / P,
           "weekly_enpic_approx" = frollmean(enpic_approx, n=7, fill=NA, align="center"),
           "weekly_enpic" = frollmean(enpic, n=7, fill=NA, align="center"))
  
  comparing_to_manual <- left_join(manual_tracing, weekly_enpic) %>%
    #filter(date >= as.Date("2020-12-24")) %>%
    mutate("manual_npic" = manual_contacts / manual_cases,
           "manual_npic_household" = manual_contacts_household / manual_cases,
           "manual_npic_non_household" = manual_contacts_non_household / manual_cases) 
  
  # weekly_enpic typically has another weekly value available after the CTAS timeseries ends, add it!
  comparing_to_manual <- bind_rows(
    comparing_to_manual,
    weekly_enpic %>% filter(date == max(comparing_to_manual$date) + 7)
  )
  
  if (resave.results) {   
      # save figure data

      Fig_4b_data <- comparing_to_manual %>%
        filter(date >= first.date.to.plot) %>%
        filter(date <= last.date.to.plot) %>%
        select(date,
               "app_notifications_per_app_positive_test" = weekly_enpic,
               "CTAS_notifications_per_CTAS_positive_test" = manual_npic,
               "CTAS_notifications_household_per_CTAS_positive_test" = manual_npic_household,
               "CTAS_notifications_non_household_per_CTAS_positive_test" = manual_npic_non_household
              )

      write_csv(Fig_4b_data, file="results/Fig_4b.csv") 
  }
  
  date.labels <- sort(
    c(seq(first.date.to.plot+9, last.date.to.plot, by=14),
      first.date.to.plot)
  )
  
  rows.when.notification.field.works <- which(comparing_to_manual$date >= as.Date("2020-12-17"))
  
  line.height <- 8
  plot_ly(comparing_to_manual) %>%
    filter(date <= last.date.to.plot) %>%
    add_lines(x=as.Date("2021-08-16"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-08-02"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-07-19"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-05-17"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-04-12"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-03-29"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-03-08"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=~date[1:(min(rows.when.notification.field.works) - 1)], y=~weekly_enpic_approx[1:(min(rows.when.notification.field.works) - 1)], line=list(width=5, dash="dash"), color=I("#d62728"), showlegend=FALSE) %>%
    add_lines(x=~date[rows.when.notification.field.works], y=~weekly_enpic[rows.when.notification.field.works], line=list(width=5), name="App notifications\nper index case", color=I("#d62728")) %>%
    add_lines(x=~date, y=~manual_npic, line=list(width=5), name="CTAS notifications\nper index case", color=I("#573b70")) %>%
    add_lines(x=~date, y=~manual_npic_household, line=list(width=3), name="CTAS household contacts\nper index case", color=I("#9467bd")) %>%
    add_lines(x=~date, y=~manual_npic_non_household, line=list(width=3), name="CTAS non-household contacts\nper index case", color=I("#7f7f7f")) %>%
    layout(
      xaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="",
        tickfont=f1,
        range=c(first.date.to.plot, last.date.to.plot),
        tickvals = date.labels,
        ticktext = format(date.labels, "%b %d")
      ),
      yaxis=list(
        gridcolor = toRGB("darkgrey"),
        titlefont=f1,
        tickfont=f1,
        title="Weekly average contacts\nnotified per index case",
        range=c(0,line.height)
      ),
      legend=list(
        font=f2
        )
    )
  
}

