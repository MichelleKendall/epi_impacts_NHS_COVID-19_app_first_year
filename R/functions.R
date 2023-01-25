# data manipulation and analysis functions
source("R/get_national_data.R")
source("R/compute_TPAEN.R")
source("R/compute_cases_averted_ltlas.R")
source("R/compute_cases_averted_per_wave.R")
source("R/compute_cases_averted_sensitivity_analysis.R")
source("R/compute_hosp_cases_averted_sensitivity_analysis.R")
source("R/compute_deaths_averted_sensitivity_analysis.R")
source("R/prepare_data_for_calculating_cases_averted.R")
source("R/write_cases_averted_summaries.R")

# plotting aesthetic functions
f1 <- list(
  family = "Arial, sans-serif",
  size = 32,
  color = "black"
)

f2 <- list(
  family = "Arial, sans-serif",
  size = 22,
  color = "black"
)

f3 <- list(
  family = "Arial, sans-serif",
  size = 16,
  color = "black"
)

f4 <- list(
  family = "Arial, sans-serif",
  size = 28,
  color = "black"
)

date.labels <- seq.Date(as.Date("2020-10-01"), as.Date("2021-09-24"), by="month")

# making plots from data functions
source("R/get_app_positives_plots.R")
source("R/get_comparing_to_manual_plot.R")
source("R/get_cumulative_cases_averted_plots.R")
source("R/get_infectiousness_diagram.R")
source("R/get_key_sharing_plot.R")
source("R/get_manual_timings_plot.R")
source("R/get_map_plots.R")
source("R/get_public_app_data_plot.R")
source("R/get_relative_incidence_plots.R")
source("R/get_roadmap_timeseries_plot.R")
source("R/get_TPAEN_context_plot.R")
source("R/get_TPAEN_plot.R")
source("R/get_users_plot.R")
source("R/get_variant_weighting_plot.R")