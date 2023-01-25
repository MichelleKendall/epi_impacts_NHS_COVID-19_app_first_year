get_hosp_cases_sensitivity_analysis_plot <- function(r.min, r.max, r.step,
                                                     max.omega.min, max.omega.max, max.omega.step,
                                                     CHR.pre.alpha, CHR.alpha, CHR.delta) {
    
    
    r.values <- seq(r.min, r.max, by=r.step)
    max.omega.values <- seq(max.omega.min, max.omega.max, by=max.omega.step)

    CHR_daily <- c(
      rep(CHR.pre.alpha, length(wave1$dates)),
      rep(CHR.alpha, length(wave2$dates)),
      rep(CHR.delta, length(wave3$dates))
    )

    SA.plot <- subplot(lapply(r.values, function(r) {
    
        subplot(lapply(max.omega.values, function(max.omega) {
      
          risky.contact.reduction.factor.pre.alpha <- r
          risky.contact.reduction.factor.alpha <- r
          risky.contact.reduction.factor.delta <- r
      
          max.omega.pre.alpha <- max.omega - 0.2
          max.omega.alpha <- max.omega - 0.2
          max.omega.delta <- max.omega
      
          # split into waves:
      
          wave1 <- compute_cases_averted_per_wave(wave.start.date=as.Date("2020-09-24"), 
                                                               wave.end.date=as.Date("2021-05-17"), 
                                                               app.and.case.data = app.and.case.data, 
                                                               wave="pre.alpha",
                                                               other_quarantine_reduction = risky.contact.reduction.factor.pre.alpha,
                                                               max.proportion.who.know.infected = max.omega.pre.alpha)
      
          wave2 <- compute_cases_averted_per_wave(wave.start.date=as.Date("2020-09-24"), 
                                                               wave.end.date=as.Date("2021-05-17"), 
                                                               app.and.case.data = app.and.case.data, 
                                                               wave="alpha",
                                                               other_quarantine_reduction = risky.contact.reduction.factor.alpha,
                                                               max.proportion.who.know.infected = max.omega.alpha)
      
          wave3 <- compute_cases_averted_per_wave(wave.start.date=as.Date("2021-05-18"), 
                                                               wave.end.date=as.Date("2021-09-24"), 
                                                               app.and.case.data = app.and.case.data, 
                                                               wave="delta",
                                                               other_quarantine_reduction = risky.contact.reduction.factor.delta,
                                                               max.proportion.who.know.infected = max.omega.delta)
      
      
          total_averted_cumulative <- tibble(
            "date" = c(wave1$dates, wave2$dates, wave3$dates),
            "wave" = c(rep("wave1", length(wave1$dates)), rep("wave2", length(wave2$dates)), rep("wave3", length(wave3$dates))),
            "averted_per_wave" = c(wave1$daily_averted, wave2$daily_averted, wave3$daily_averted),
            "averted_per_wave_upper" = c(wave1$daily_averted_upper, wave2$daily_averted_upper, wave3$daily_averted_upper),
            "averted_per_wave_lower" = c(wave1$daily_averted_lower, wave2$daily_averted_lower, wave3$daily_averted_lower),
            "hospitalisations_averted_per_wave" = averted_per_wave * CHR_daily,
            "hospitalisations_averted_per_wave_upper" = averted_per_wave_upper * CHR_daily,
            "hospitalisations_averted_per_wave_lower" = averted_per_wave_lower * CHR_daily
          ) %>%
            group_by(date) %>%
            summarise("hospitalisations_averted_total" = sum(hospitalisations_averted_per_wave),
                      "hospitalisations_averted_total_upper" = sum(hospitalisations_averted_per_wave_upper),
                      "hospitalisations_averted_total_lower" = sum(hospitalisations_averted_per_wave_lower)
                     ) %>% 
            mutate("hospitalisations_averted_cumulative" = cumsum(hospitalisations_averted_total),
                   "hospitalisations_averted_cumulative_upper" = cumsum(hospitalisations_averted_total_upper),
                   "hospitalisations_averted_cumulative_lower" = cumsum(hospitalisations_averted_total_lower)) 
      
          plot_ly(total_averted_cumulative) %>%
            add_lines(x=~date, y=~hospitalisations_averted_cumulative, line=list(width=5), color=I("#1f77b4")) %>%
            add_ribbons(x=~date, ymin=~hospitalisations_averted_cumulative_lower, ymax=~hospitalisations_averted_cumulative_upper,
                    color=I("#1f77b4"), opacity=0.6) %>%
            plotly::layout(
              xaxis=list(
                gridcolor = "#a2a2a2",
                title=paste0("Omega_delta = ", max.omega),
                titlefont=list(size=32),
                tickfont=list(size=32),
                tickvals = seq.Date(as.Date("2020-09-01"), as.Date("2021-09-24"), by="month"),
                ticktext = format(seq.Date(as.Date("2020-09-01"), as.Date("2021-09-24"), by="month"), "%b %y")
              ),
              yaxis=list(
                gridcolor = "#a2a2a2",
                title=paste0("r = ", r),
                tickfont=list(size=32),
                titlefont=list(size=32),
                range=c(0,90000)
              ),
              annotations=list(
                x = date.labels[[11]] , y = 60000, 
                text = paste0(signif(total_averted_cumulative$hospitalisations_averted_cumulative[[366]]/1000,3), " k"), 
                          showarrow = F, font=f1 
              ),
              showlegend=FALSE
            )
      
        }), 
        nrows=1, shareY=TRUE, titleY = TRUE
        ) # end subplot

    
      }), 
      nrows=length(r.values), shareX=TRUE, titleX=TRUE
    )

SA.plot

}