get_public_app_data_plot <- function() {
    
    # load data
    public.app.data <- read_csv("data/covid19_app_country_specific_dataset_Feb2022.csv", show_col_types=F) # data originally from https://www.gov.uk/government/publications/nhs-covid-19-app-statistics

    # tidy data
    public.app.data.cleaned <- public.app.data %>%
      select("date" = 'Week starting (Wythnos yn dechrau)',
             "checkins" = 'Check-ins (Cofrestriadau)',
             "symptoms" = 'Symptoms reported (Symptomau a adroddwyd)',
             "positives" = 'Positive test results linked to app (Canlyniadau prawf positif)') %>%
      filter(date < as.Date("2021-09-30")) %>%
      group_by(date) %>%
      summarise("checkins" = sum(checkins),
                "symptoms" = sum(symptoms),
                "positives" = sum(positives)) %>%
      mutate("midweek.date" = date + 3) 
    
    if (resave.results) {   write_csv(public.app.data.cleaned, file="results/Fig_2.csv") }


    # plots
    line.height <- 155000
    p_positives <- plot_ly(public.app.data.cleaned) %>%
      add_lines(x=as.Date("2021-08-16"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
      add_annotations(x=as.Date("2021-08-16"), y=line.height*0.9, text="Advice\nchange",
                  font=f1,
                  xref = "x",
                  yref = "y",
                  showarrow = FALSE) %>%
      add_lines(x=as.Date("2021-08-02"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
      add_annotations(x=as.Date("2021-08-02"), y=line.height*0.2, text="Logic\nchange",
                  font=f1,
                  xref = "x",
                  yref = "y",
                  showarrow = FALSE) %>%
      add_lines(x=as.Date("2021-07-19"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
      add_annotations(x=as.Date("2021-07-19"), y=line.height*0.4, text="Step 4",
                  font=f1,
                  xref = "x",
                  yref = "y",
                  showarrow = FALSE) %>%
      add_lines(x=as.Date("2021-05-17"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
      add_annotations(x=as.Date("2021-05-17"), y=line.height*0.9, text="Step 3",
                  font=f1,
                  xref = "x",
                  yref = "y",
                  showarrow = FALSE) %>%
      add_lines(x=as.Date("2021-04-12"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
      add_annotations(x=as.Date("2021-04-12"), y=line.height*0.9, text="Step 2",
                  font=f1,
                  xref = "x",
                  yref = "y",
                  showarrow = FALSE) %>%
      add_lines(x=as.Date("2021-03-29"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
      add_annotations(x=as.Date("2021-03-29"), y=line.height*0.7, text="Step 1b",
                  font=f1,
                  xref = "x",
                  yref = "y",
                  showarrow = FALSE) %>%
      add_lines(x=as.Date("2021-03-08"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
      add_annotations(x=as.Date("2021-03-08"), y=line.height*0.9, text="Step 1a",
                  font=f1,
                  xref = "x",
                  yref = "y",
                  showarrow = FALSE) %>%
      add_lines(x=~midweek.date, y=~positives, name="Positives", line=list(width=6), color=I("#ff7f0e")) %>%
      layout(
        xaxis=list(
          gridcolor = toRGB("darkgrey"),
          title="",
          tickfont=f1,
          tickvals = date.labels,
          ticktext = format(date.labels, "%b %y")
        ),
        yaxis=list(
          gridcolor = toRGB("darkgrey"),
          title= "Weekly total\napp-reported cases",
          titlefont=f1,
          tickfont=f1,
          range=c(0,line.height)
        ),
        showlegend=FALSE
      )
    
    line.height <- 120000
    p_symptoms <- plot_ly(public.app.data.cleaned) %>%
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
      add_lines(x=~midweek.date, y=~symptoms, name="Symptoms", line=list(width=6), color=I("#e377c2")) %>%
      layout(
        xaxis=list(
          gridcolor = toRGB("darkgrey"),
          title="",
          tickfont=f1,
          tickvals = date.labels,
          ticktext = format(date.labels, "%b %y")
        ),
        yaxis=list(
          gridcolor = toRGB("darkgrey"),
          title= "Weekly total\nsymptoms reported",
          titlefont=f1,
          tickfont=f1
        ),
        showlegend=FALSE
      )
    
    line.height <- 16500000
    p_checkins <- plot_ly(public.app.data.cleaned) %>%
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
      add_lines(x=~midweek.date, y=~checkins, name="Check-ins", line=list(width=6), color=I("#2ca02c")) %>%
      layout(
        xaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="",
        tickfont=f1,
        tickvals = date.labels,
        ticktext = format(date.labels, "%b %y")
      ),
      yaxis=list(
        gridcolor = toRGB("darkgrey"),
        title= "Weekly total\ncheck-ins",
        titlefont=f1,
        tickfont=f1,
        range=c(0, line.height)
      ),
      showlegend=FALSE
      )


    subplot(p_positives,
            p_symptoms, 
            p_checkins,
            nrows = 3,
            titleY=T,
            margin=0.03)
    
    
}