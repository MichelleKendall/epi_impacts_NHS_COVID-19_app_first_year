get_key_sharing_plot <- function(national.data, first.date.to.plot, last.date.to.plot) {
  
  date.labels <- seq.Date(as.Date("2020-10-01"), as.Date("2021-09-24"), by="month")
  
  line.height <- 100
  
  if (resave.results) {     
      Fig_S1_data <- national.data %>%
        select(date, "key_sharing" = k)

      write_csv(Fig_S1_data, file="results/Fig_S1.csv")  
  }
  
  key.sharing.is.estimated.rows <- which(national.data$date < as.Date("2021-06-11"))  
  key.sharing.is.recorded.rows <- which(national.data$date >= as.Date("2021-06-11"))  
    
  plot_ly(national.data) %>%  
    filter(date <= last.date.to.plot)  %>%
    add_lines(x=~date[key.sharing.is.estimated.rows], y=~k[key.sharing.is.estimated.rows], showlegend=FALSE,
              line=list(width=5, opacity=1, dash="dash"), color=I("#1f77b4")) %>%
    add_lines(x=~date[key.sharing.is.recorded.rows], y=~k[key.sharing.is.recorded.rows], showlegend=FALSE,
              line=list(width=5, opacity=1), color=I("#1f77b4")) %>%
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
        title="Daily percentage of app users\ntesting positive who share keys",
        titlefont=f4,
        tickfont=f1,
        range=c(0,line.height)
      )
    )
}
