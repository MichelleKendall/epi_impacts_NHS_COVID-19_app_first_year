get_variant_weighting_plot <- function(national.data) {
  
  alpha.weighting <- tibble(
    "date" = unique(national.data$date),
    "alpha.weighting" = 1/( 1+exp(as.double(-0.08*difftime(unique(national.data$date),as.Date('2020-12-21'),units="day")) ) ),
    "pre.alpha.weighting" = 1 - 1/( 1+exp(as.double(-0.08*difftime(unique(national.data$date),as.Date('2020-12-21'),units="day")) ) )
  ) %>%
    mutate("check" = alpha.weighting + pre.alpha.weighting)
  
  date.labels <- seq.Date(as.Date("2020-10-01"), as.Date("2021-09-24"), by="month")
  
  if (resave.results) {   write_csv(alpha.weighting, file="results/Supp_fig_variant_weighting.csv") }
  
  plot_ly(alpha.weighting) %>%
    filter(date <= as.Date("2021-05-17")) %>%
    add_lines(x=~date, y=~pre.alpha.weighting, line=list(width=5), name="pre-Alpha") %>%
    add_lines(x=~date, y=~alpha.weighting, line=list(width=5), name="Alpha") %>%
    add_lines(x=c(as.Date("2021-05-17"), last.date.to.plot), y=1, line=list(width=5), name="Delta") %>%
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
        title="Modelled variant prevalence",
        titlefont=f1,
        tickfont=f1,
        range=c(-0.05,1.05)
      ),
      legend=list(
        font=f2
      )
    )
}