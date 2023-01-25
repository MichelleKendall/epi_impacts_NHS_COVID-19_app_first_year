get_manual_timings_plot <- function() {
    
    # load data
    timings_data <- read_csv("data/CTAS_timings_data.csv", show_col_types=F) # data originally from https://www.gov.uk/government/publications/weekly-statistics-for-nhs-test-and-trace-england-31-march-to-6-april-2022

    timings_data <- timings_data %>%
      filter(date >= as.Date("2020-09-24")) %>%
      filter(date <= as.Date("2021-09-24")) %>%
      select(date,
         "symp_to_trace" = "Median time taken (hours) from index symptoms to contacts being traced",
         "test_results_regional" = "Median time taken (hours) to receive test results for tests conducted by regional test sites" ,  
         "test_results_local" = "Median time taken (hours) to receive test results for tests conducted by local test sites" ,     
         "test_results_mobile" = "Median time taken (hours) to receive test results for tests conducted by mobile testing units"  ,
         "test_results_satellite" = "Median time taken (hours) to receive test results for tests conducted by satellite test centres",
         "test_results_home" = "Median time taken (hours) to receive test results for tests conducted by home testing kits"  
         )

    plot_ly(timings_data) %>%
      add_lines(x=~date, y=~symp_to_trace, line=list(width=6), name="From index symptoms\nto contacts traced (non-app)") %>% 
      add_lines(x=~date, y=~test_results_home, line=list(width=6, dash="dash"), name="\n<b>From taking test\nto receiving result\nfor tests conducted by:</b>\nhome testing kits" ) %>%
      add_lines(x=~date, y=~test_results_satellite, line=list(width=6, dash="dash"), name="satellite test centres") %>%
      add_lines(x=~date, y=~test_results_local, line=list(width=6, dash="dash"), name="local test sites") %>%
      add_lines(x=~date, y=~test_results_regional, line=list(width=6, dash="dash"), name="regional test sites")  %>%
      add_lines(x=~date, y=~test_results_mobile, line=list(width=6, dash="dash"), name="mobile testing units") %>%
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
          title= "Median time taken (hours)",
          titlefont=f1,
          tickfont=f1
        ),
        legend=list(
          font=f1
        )
      )
 
}