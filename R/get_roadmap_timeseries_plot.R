get_roadmap_plot <- function(cleaned.app.data, first.date.to.plot, last.date.to.plot) {
  
  app.data.summary <- cleaned.app.data %>% 
    filter(date > first.date.to.plot) %>%
    filter(date <= last.date.to.plot) %>%
    group_by(date) %>%
    summarise("N_approx" = sum(notifications_approx), "N" = sum(notifications), "P" = sum(test_positive)) %>%
    #mutate("enpic_unscaled" = N/P) %>%
    mutate("rolling_N_approx" = frollmean(N_approx,n=7,align='center',fill=NA))  %>%
    mutate("rolling_N" = frollmean(N,n=7,align='center',fill=NA))  %>%
    mutate("rolling_P" = frollmean(P,n=7,align='center',fill=NA)) #%>%
    #mutate("rolling_enpic_unscaled" = rolling_N / rolling_P)
  
  if (resave.results) {     
      # save figure data

      Fig_4a_data <- app.data.summary %>%
        select(date,
               "rolling_7_day_mean_notifications" = rolling_N,
               "rolling_7_day_mean_notifications_estimated_pre_version_4.1" = rolling_N_approx,
               "rolling_7_day_mean_positive_tests" = rolling_P)

      write_csv(Fig_4a_data, file="results/Fig_4a.csv")  
  }
  
  row.when.notification.field.works <- which(app.data.summary$date == as.Date("2020-12-17"))
  
  date.labels <- seq.Date(from=as.Date("2020-10-01"), to=as.Date("2021-09-30"), by="month")
  
  line.height <- 130000
  plot_ly(app.data.summary) %>%
    add_lines(x=as.Date("2022-02-24"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
   add_annotations(x=as.Date("2022-02-18"), y=3.2, text="End of\nlegal\nrestrictions",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2022-01-27"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2022-01-27"), y=3.5, text="End of\nPlan B",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2022-01-11"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2022-01-11"), y=3.2, text="LFD role\nchanges",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-11-27"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-11-27"), y=3.5, text="First\nmeasures\nagainst\nOmicron",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-08-16"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-08-16"), y=3.0, text="Advice\nchange",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-08-02"), y=c(0,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-08-02"), y=3.5, text="Logic\nchange",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-07-19"), y=c(0,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-07-19"), y=4.5, text="Step 4",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-05-17"), y=c(0,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-05-17"), y=4.8, text="Step 3",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-04-12"), y=c(0,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
      add_annotations(x=as.Date("2021-04-12"), y=4.5, text="Step 2",
                      font=f2,
                      xref = "x",
                      yref = "y",
                      showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-03-29"), y=c(0,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
      add_annotations(x=as.Date("2021-03-29"), y=4.8, text="Step 1b",
                      font=f2,
                      xref = "x",
                      yref = "y",
                      showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-03-08"), y=c(0,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
      add_annotations(x=as.Date("2021-03-08"), y=4.5, text="Step 1a",
                      font=f2,
                      xref = "x",
                      yref = "y",
                      showarrow = FALSE) %>%
    add_polygons(x=c(as.Date("2021-09-28"), as.Date("2021-09-28"),
                     as.Date("2021-10-15"), as.Date("2021-10-15")),
                 y=c(0, line.height, line.height, 0),
                 color=I("darkgrey"), showlegend=FALSE) %>%
    add_annotations(x = as.Date("2021-10-06"),
                    y = 4.8,
                    text = "Fewer\ntests\nreported",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    xanchor = 'center',
                    showarrow = FALSE) %>%
    #add_lines(x=last.enpic.date.plotted.in.last.report - 3, y=c(0,0,line.height), color=I("black"), line=list(dash="dot"), showlegend=FALSE) %>%
    add_lines(x=~date[1:row.when.notification.field.works-1], y=~rolling_N_approx[1:row.when.notification.field.works-1],
              line=list(width=4, dash="dash"), showlegend=FALSE, color=I("#1f77b4")) %>%
    add_lines(x=~date[row.when.notification.field.works:nrow(app.data.summary)], y=~rolling_N[row.when.notification.field.works:nrow(app.data.summary)],
              line=list(width=4), name="App notifications", color=I("#1f77b4")) %>%
    add_lines(x=~date, y=~rolling_P,
              line=list(width=4), name="App-reported\ncases", color=I("#ff7f0e")) %>%
    layout(
      xaxis=list(
        gridcolor = toRGB("darkgrey"),
        tickfont=f1,
        range=c(first.date.to.plot, last.date.to.plot - 2),
        tickvals=date.labels,
        ticktext=format(date.labels, "%b %y"),
        title=""
      ),
      yaxis=list(
        titlefont=f1,
        title="Rolling weekly average\n(log scale)",
        tickfont=f1,
        type="log",
        tickvals=c(10^3, 10^4, 10^5),
        ticktext=c("1k", "10k", "100k"),
        range=c(2.5,log10(line.height))
      ),
      legend=list(
        font=f1
      )
    )
  
}
