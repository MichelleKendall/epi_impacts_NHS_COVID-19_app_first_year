get_cumulative_cases_averted_plots <- function(wave1, wave2, wave3,
                                               CHR.pre.alpha, CHR.alpha, CHR.delta,
                                               CFR.pre.alpha, CFR.alpha, CFR.delta) {

    CHR_daily <- c(
      rep(CHR.pre.alpha, length(wave1$dates)),
      rep(CHR.alpha, length(wave2$dates)),
      rep(CHR.delta, length(wave3$dates))
    )

    CFR_daily <- c(
      rep(CFR.pre.alpha, length(wave1$dates)),
      rep(CFR.alpha, length(wave2$dates)),
      rep(CFR.delta, length(wave3$dates))
    )
    
    total_averted_cumulative <- tibble(
      "date" = c(wave1$dates, wave2$dates, wave3$dates),
      "wave" = c(rep("wave1", length(wave1$dates)), rep("wave2", length(wave2$dates)), rep("wave3", length(wave3$dates))),
      CHR_daily,
      CFR_daily,
      "averted_per_wave" = c(wave1$daily_averted, wave2$daily_averted, wave3$daily_averted),
      "averted_per_wave_upper" = c(wave1$daily_averted_upper, wave2$daily_averted_upper, wave3$daily_averted_upper),
      "averted_per_wave_lower" = c(wave1$daily_averted_lower, wave2$daily_averted_lower, wave3$daily_averted_lower),
      "hospitalisations_averted_per_wave" = averted_per_wave * CHR_daily,
      "hospitalisations_averted_per_wave_upper" = averted_per_wave_upper * CHR_daily,
      "hospitalisations_averted_per_wave_lower" = averted_per_wave_lower * CHR_daily,
      "deaths_averted_per_wave" = averted_per_wave * CFR_daily,
      "deaths_averted_per_wave_upper" = averted_per_wave_upper * CFR_daily,
      "deaths_averted_per_wave_lower" = averted_per_wave_lower * CFR_daily
    ) %>%
      group_by(date) %>%
      summarise("averted_total" = sum(averted_per_wave),
            "averted_total_upper" = sum(averted_per_wave_upper),
            "averted_total_lower" = sum(averted_per_wave_lower),
            "hospitalisations_averted_total" = sum(hospitalisations_averted_per_wave),
            "hospitalisations_averted_total_upper" = sum(hospitalisations_averted_per_wave_upper),
            "hospitalisations_averted_total_lower" = sum(hospitalisations_averted_per_wave_lower),
            "deaths_averted_total" = sum(deaths_averted_per_wave),
            "deaths_averted_total_upper" = sum(deaths_averted_per_wave_upper),
            "deaths_averted_total_lower" = sum(deaths_averted_per_wave_lower)) %>%
      mutate("averted_cumulative" = cumsum(averted_total),
         "averted_cumulative_upper" = cumsum(averted_total_upper),
         "averted_cumulative_lower" = cumsum(averted_total_lower),
         "hospitalisations_averted_cumulative" = cumsum(hospitalisations_averted_total),
         "hospitalisations_averted_cumulative_upper" = cumsum(hospitalisations_averted_total_upper),
         "hospitalisations_averted_cumulative_lower" = cumsum(hospitalisations_averted_total_lower),
         "deaths_averted_cumulative" = cumsum(deaths_averted_total),
         "deaths_averted_cumulative_upper" = cumsum(deaths_averted_total_upper),
         "deaths_averted_cumulative_lower" = cumsum(deaths_averted_total_lower))
    
    if (resave.results) {   
        # save figure data
        write_csv(total_averted_cumulative %>%
                      select(date,
                             averted_cumulative,
                             averted_cumulative_upper,
                             averted_cumulative_lower,
                             hospitalisations_averted_cumulative,
                             hospitalisations_averted_cumulative_upper,
                             hospitalisations_averted_cumulative_lower,
                             deaths_averted_cumulative,
                             deaths_averted_cumulative_upper,
                             deaths_averted_cumulative_lower
                      ),
                   file=glue("results/Fig_6abc.csv"))
    }
        

    cases_cumulative_plot <- plot_ly(total_averted_cumulative) %>%
      add_lines(x=~date, y=~averted_cumulative, line=list(width=5)) %>%
      add_ribbons(x=~date, ymin=~averted_cumulative_lower, ymax=~averted_cumulative_upper,
              color=I("#1f77b4"), opacity=0.6) %>%
      layout(
        xaxis=list(
          gridcolor = "#a2a2a2",
          title="",
          tickfont=list(size=32),
          tickvals = seq.Date(as.Date("2020-09-01"), as.Date("2021-09-24"), by="month"),
          ticktext = format(seq.Date(as.Date("2020-09-01"), as.Date("2021-09-24"), by="month"), "%b %y")
        ),
        yaxis=list(
          gridcolor = "#a2a2a2",
          title="Estimated cases averted as a\nresult of app exposure notifications",
          tickfont=list(size=32),
          titlefont=list(size=32)
        ),
          annotations=list(
              x = as.Date("2021-09-10"), y = 1100000,
              text = paste0(signif(total_averted_cumulative$averted_cumulative[[366]]/1000000,3), " M"),
              showarrow = F, font=f1 
              ),
        showlegend=FALSE
    )

    hospitalisations_cumulative_plot <- plot_ly(total_averted_cumulative) %>%
      add_lines(x=~date, y=~hospitalisations_averted_cumulative, line=list(width=5)) %>%
      add_ribbons(x=~date, ymin=~hospitalisations_averted_cumulative_lower, ymax=~hospitalisations_averted_cumulative_upper,
              color=I("#1f77b4"), opacity=0.6) %>%
      layout(
        xaxis=list(
          gridcolor = "#a2a2a2",
          title="",
          tickfont=list(size=32),
          tickvals = seq.Date(as.Date("2020-09-01"), as.Date("2021-09-24"), by="month"),
          ticktext = format(seq.Date(as.Date("2020-09-01"), as.Date("2021-09-24"), by="month"), "%b %y")
        ),
        yaxis=list(
          gridcolor = "#a2a2a2",
          title="Hospitalisations averted",
          tickfont=list(size=40),
          titlefont=list(size=40)
        ),
              annotations=list(
                x = as.Date("2021-09-10"), y = 48000, 
                text = paste0(signif(total_averted_cumulative$hospitalisations_averted_cumulative[[366]]/1000,3), " k"), 
                          showarrow = F, font=f1 
              ),
    showlegend=FALSE
  )

    deaths_cumulative_plot <- plot_ly(total_averted_cumulative) %>%
      add_lines(x=~date, y=~deaths_averted_cumulative, line=list(width=5)) %>%
      add_ribbons(x=~date, ymin=~deaths_averted_cumulative_lower, ymax=~deaths_averted_cumulative_upper,
              color=I("#1f77b4"), opacity=0.6) %>%
      layout(
        xaxis=list(
          gridcolor = "#a2a2a2",
          title="",
          tickfont=list(size=32),
          tickvals = seq.Date(as.Date("2020-09-01"), as.Date("2021-09-24"), by="month"),
          ticktext = format(seq.Date(as.Date("2020-09-01"), as.Date("2021-09-24"), by="month"), "%b %y")
        ),
        yaxis=list(
          gridcolor = "#a2a2a2",
          title="Deaths averted",
          tickfont=list(size=40),
          titlefont=list(size=40)
        ),
              annotations=list(
                x = as.Date("2021-09-10"), y = 10500, 
                text = paste0(signif(total_averted_cumulative$deaths_averted_cumulative[[366]]/1000,3), " k"), 
                          showarrow = F, font=f1 
              ),
    showlegend=FALSE
    )
    
  plots <- list()
  plots$p1 <- cases_cumulative_plot
  plots$p2 <- hospitalisations_cumulative_plot
  plots$p3 <- deaths_cumulative_plot 

  plots

    
}