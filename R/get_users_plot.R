get_users_plot <- function(national.data) {
    
  # load app usage data, originally from https://www.gov.uk/government/publications/nhs-covid-19-app-statistics  
  users.data <- read_csv("data/public_app_data_national_uptake.csv", show_col_types=FALSE) %>%
    select(date,
           "active_users" = `Users with app installed (Defnyddwyr gyda ap wedi'i osod)`,
           "contact_tracing_enabled" = `Users with contact tracing enabled (Defnyddwyr ag olrhain cyswllt wedi'u galluogi)`) 
    
  date.labels <- seq.Date(as.Date("2020-10-01"), as.Date("2021-09-24"), by="month")

  if (resave.results) {   write_csv(users.data, file="results/Fig_1a.csv") }
  
  line.height <- 20500000
  users.plot <- plot_ly(users.data) %>%  
    filter(date >= first.date.to.plot) %>%
    filter(date <= last.date.to.plot) %>%
    add_lines(x=as.Date("2021-08-16"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-08-16"), y=7500000, text="Advice\nchange",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-08-02"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-08-02"), y=5000000, text="Logic\nchange",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-07-19"), y=c(0,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-07-19"), y=7500000, text="Step 4",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-05-17"), y=c(0,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-05-17"), y=5000000, text="Step 3",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-04-12"), y=c(0,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-04-12"), y=7500000, text="Step 2",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-03-29"), y=c(0,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-03-29"), y=5000000, text="Step 1b",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-03-08"), y=c(0,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-03-08"), y=7500000, text="Step 1a",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2020-12-17"), y=c(0,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2020-12-17"), y=7500000, text="Version 4.1\nreleased",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=~date, y=~active_users, name="Active users", #color=I("#1f77b4"), 
              line=list(width=5, opacity=1)) %>%
    add_lines(x=~date, y=~contact_tracing_enabled, name="Contact tracing\nenabled", #color=I("#1f77b4"), 
              line=list(width=5, opacity=1)) %>%
    layout(
      xaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="",
        tickfont=f1,
        range=c(first.date.to.plot, last.date.to.plot),
        tickvals = date.labels,
        ticktext = format(date.labels, "%b %y")
      ),
      yaxis=list(
        gridcolor = toRGB("darkgrey"),
        title= "Daily total estimators of app usage",
        titlefont=f1,
        tickfont=f1,
        range=c(0,line.height)
      ),
      legend=list(
          font=f2
      )
    )
  

  users.plot
}
