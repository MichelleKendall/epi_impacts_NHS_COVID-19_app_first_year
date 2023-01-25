get_TPAEN_plot <- function(national.data, first.date.to.plot, last.date.to.plot) {
  
  date.labels <- seq.Date(as.Date("2020-10-01"), as.Date("2021-09-24"), by="month")
  
  line.height <- 9
  
  if (resave.results) {     
      Fig_5c_data <- national.data %>%
        mutate("TPAEN" = TPAEN*100,
               "TPAEN_lower" = lower.TPAEN*100,
               "TPAEN_upper" = upper.TPAEN*100) %>%
        select(date, TPAEN, TPAEN_lower, TPAEN_upper)

      write_csv(Fig_5c_data, file="results/Fig_5c.csv")  
  }    
  
  plot_ly(national.data) %>%  
    filter(date <= last.date.to.plot) %>%
    add_lines(x=~date, y=~TPAEN*100, color=I("orange"), showlegend=FALSE,
              line=list(width=5, opacity=1)) %>%
    add_ribbons(x=~date, ymin=~lower.TPAEN*100, ymax=~upper.TPAEN*100, color=I("orange"), opacity=0.6, showlegend=FALSE)  %>% 
    layout(
      xaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="",
        titlefont=f1,
        tickfont=f1,
        range=c(first.date.to.plot, last.date.to.plot),
        tickvals = c(as.Date("2021-02-01"),date.labels),
        ticktext = format(c(as.Date("2021-02-01"),date.labels), "%b %y")
      ),
      yaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="Daily percentage of notified users who\ngo on to test positive (TPAEN)",
        titlefont=f4,
        tickfont=f1,
        range=c(0,line.height)
      )
    )
}
