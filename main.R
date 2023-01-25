# This script prepares the findings and figures for the paper "Epidemiological impacts of the NHS COVID-19 app throughout its first year" (ADD LINK).

# It uses a mixture of publicly available data (included) and private app data which is available on request (ADD DETAILS). 
# If you would like to run it and you don't have access to the private data then you may use the csvs from the "dummyprivate" folder. 
# These are dummy versions of the private datasets needed to run the code simply so that it can be sense-checked, though it will give nonsensical results.
# Change the name of the "dummyprivate" folder to "private" to run the code.
# The "private" folder is in .gitignore so this should not cause conflicts.

########
# SETUP
########

# It may not be desirable to re-save results and plots, particularly when working with dummy data.
resave.results <- FALSE
resave.plots <- FALSE # saving plots also relies on successful kaleido and webshot installation (see below)

# helper function for installing (where necessary) and loading packages
using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    suppressWarnings(lapply(need,require,character.only=TRUE))
  }
}

# alongside each package is a note of the version used for the original analysis

using("tidyverse", # v1.3.2
      "plotly", # v4.10.0
      "viridis", # v0.6.2
      "glue", #v.1.6.2
      "here", #v.1.0.1
      "data.table", #v.1.14.2
      "leaflet", #v.2.1.1
      "leaflet.extras", #v.1.0.0
      "sf", #v.1.0.7
      "mapview") #v.2.11.0

# NB If you wish to recalculate TPAEN (see Data Loading and Prep section below) then you also need the packages "rstan" and "splines", which will be 
# installed and loaded as appropriate within that code. Note that "rstan" may require some additional installation steps using RTools. 

setwd(here())

# source the relevant functions
source("R/functions.R")

# date range to plot: the app's first year, 24 Sept 2020 to 24 Sept 2021
first.date.to.plot <- as.Date("2020-09-24") 
last.date.to.plot <- as.Date("2021-09-24") 

########################
# DATA LOADING AND PREP
########################

# load the clean app timeseries which also contains ONS population size data. This is private data available on request.
cleaned.app.data <- read_csv("data/private/app_data_timeseries.csv", show_col_types = FALSE)

# summarise to national data, adding the national key sharing data and TPAEN estimates.
# Our estimates of TPAEN are distributed with the private data to save time on recalculation, but the original code is available
# in R/compute_TPAEN.R and can be triggered by using "recalculate.TPAEN = TRUE"
# but please note that it can take a long time (hours) especially if `parallel::detectCores()` is 1.
# For full details see file "R/compute_national_data.R"
national.data <- get_national_data(cleaned.app.data = cleaned.app.data, recalculate.TPAEN = FALSE) 

# prepare data
app.and.case.data <- prepare_data_for_calculating_cases_averted(cleaned.app.data = cleaned.app.data)

##############
# Assumptions
##############

# Case Hospitalisation Rate, per wave
CHR.pre.alpha <- 0.098
CHR.alpha <- 0.05
CHR.delta <- 0.027

# Case Fatality Rate, per wave
CFR.pre.alpha <- 0.015
CFR.alpha <- 0.019
CFR.delta <- 0.002

# Fractional risky contact reduction, per wave
risky.contact.reduction.factor.pre.alpha <- 0.4
risky.contact.reduction.factor.alpha <- 0.4
risky.contact.reduction.factor.delta <- 0.4

# Fraction of infected individuals who realise they are infected by the end of their infection, per wave
max.omega.pre.alpha <- 0.8
max.omega.alpha <- 0.8
max.omega.delta <- 1

###########
# ANALYSIS
###########

# compute national cases averted per wave
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

# write results summaries to the "results" folder; output to screen the overall cases averted figure
write_cases_averted_summaries(wave1 = wave1, wave2 = wave2, wave3 = wave3)
paste0("Total cases averted in first year was ",signif(wave1$total_averted + wave2$total_averted + wave3$total_averted, 7))

# Cases averted per LTLA
# This takes a while to calculate (approx 8 mins) so set "recalculate = TRUE" if you want to over-ride it simply re-using an old computation
compute_cases_averted_ltlas(app.and.case.data = app.and.case.data, recalculate = FALSE)

###########
# Plotting
###########

# get plot of users over time, Fig 1a
users_plot <- get_users_plot(national.data = national.data)

# get multipanel plot of public app data, Fig 2
public_app_data_plot <- get_public_app_data_plot()

# get plots about positives through the app for Figs 3a, 3b, 5a and 5b
app.positives.plots <- get_app_positives_plots(cleaned.app.data = cleaned.app.data)
paper_figure_3 <- subplot(app.positives.plots$p1, 
                          app.positives.plots$p3,
                          titleY=T,
                          nrows=1,
                          margin = 0.04)

# get plot of notifications and positives over time, with roadmap etc. timepoints annotated, Fig 4a
roadmap_timeseries_plot <- get_roadmap_plot(cleaned.app.data = cleaned.app.data, first.date.to.plot = first.date.to.plot, last.date.to.plot = last.date.to.plot)

# get plot comparing notifications per index case from app and manual tracing, Fig 4b
comparing_to_manual_plot <- get_comparing_to_manual_plot(cleaned.app.data = cleaned.app.data)
paper_figure_4 <- subplot(roadmap_timeseries_plot,
                          comparing_to_manual_plot,
                          nrows=2, 
                          shareX=T, 
                          titleY=T)

# get plot of proportion Testing Positive After Exposure Notification, Fig 5c
TPAEN.plot <- get_TPAEN_plot(national.data = national.data, 
                             first.date.to.plot = first.date.to.plot, last.date.to.plot = last.date.to.plot)

# get plot of TPAEN compared to ONS prevalence, Fig 5d
TPAEN.context.plot <- get_TPAEN_context_plot(national.data = national.data, first.date.to.plot = first.date.to.plot, last.date.to.plot = last.date.to.plot)

# get plots of relative incidence between app notified and app not notified users, Fig 5e and 5f
relative_incidence_plots <- get_relative_incidence_plots(last.date.to.plot = last.date.to.plot)

# compile multiplanel Fig 5
paper_figure_5 <- subplot(app.positives.plots$p4, app.positives.plots$p2,
                          TPAEN.plot, TPAEN.context.plot,
                          relative_incidence_plots$p1, relative_incidence_plots$p2,
                          nrows=3,
                          shareX=T,
                          titleY=T,
                          margin = 0.05)

# get plots of cumulative estimated cases, hospitalisations and deaths averted, Fig 6a, 6b and 6c
cumulative_plots <- get_cumulative_cases_averted_plots(wave1 = wave1, wave2 = wave2, wave3 = wave3,
                                                       CHR.pre.alpha = CHR.pre.alpha, CHR.alpha = CHR.alpha, CHR.delta = CHR.delta,
                                                       CFR.pre.alpha = CFR.pre.alpha, CFR.alpha = CFR.alpha, CFR.delta = CFR.delta)

# get map plots of uptake and cases averted by LTLA, Figs 1b, 6d, 6e
ltla_map_plots <- get_map_plots(cleaned.app.data = cleaned.app.data)

# get map of proportion consenting to contact tracing, Fig S1
key_sharing_plot <- get_key_sharing_plot(national.data = national.data, 
                                         first.date.to.plot = first.date.to.plot, last.date.to.plot = last.date.to.plot)

# get plot of how we weighted the cases and notifications to each wave, Fig S2
variant_weighting_plot <- get_variant_weighting_plot(national.data = national.data)

# get plot of timings for test results and manual contact tracing, Fig S3
manual_timings_plot <- get_manual_timings_plot()

# get diagram of how infectiousness is modified by interventions, Fig S4
infectiousness_diagram <- get_infectiousness_diagram()

# Perform sensitivity analyses, producing the plots as we go.
# There is redundancy in the recalculations of these sensitivity analyses across cases, hospitalisations and deaths
# which could be improved, but they don't take too long as they are.
# It's also not ideal to be looping and re-calculating every time - this should be refined if the analysis is expected to be repeated often!
cases_sensitivity_analysis_plot <- get_cases_sensitivity_analysis_plot(r.min=0.2, r.max=1, r.step=0.2,
                                                                       max.omega.min=0.2, max.omega.max=1, max.omega.step=0.2)

hosp_cases_sensitivity_analysis_plot <- get_hosp_cases_sensitivity_analysis_plot(r.min=0.2, r.max=1, r.step=0.2,
                                                                                 max.omega.min=0.2, max.omega.max=1, max.omega.step=0.2,
                                                                                 CHR.pre.alpha = CHR.pre.alpha, CHR.alpha = CHR.alpha, CHR.delta = CHR.delta)

deaths_sensitivity_analysis_plot <- get_deaths_sensitivity_analysis_plot(r.min=0.2, r.max=1, r.step=0.2,
                                                                         max.omega.min=0.2, max.omega.max=1, max.omega.step=0.2,
                                                                         CFR.pre.alpha = CFR.pre.alpha, CFR.alpha = CFR.alpha, CFR.delta = CFR.delta)




#################
# Saving plots
#################

if (resave.plots) {
    
    # NB the following code to save images will throw an error if you don't have kaleido and webshot installed. Try:
    #using('reticulate')
    #reticulate::install_miniconda()
    #reticulate::conda_install('r-reticulate', 'python-kaleido')
    #reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
    #reticulate::use_miniconda('r-reticulate')
    #reticulate::py_run_string("import sys") 
    # If that fails, you can (at the time of writing) still use the deprecated function "orca" instead of "save_image".
    # For the map plots you may also need to run:
    # webshot::install_phantomjs()

    save_image(users_plot, file=glue("plots/Fig_1a_uptake_plot.png"), width=1400, height=800)
    save_image(users_plot, file=glue("plots/Fig_1a_uptake_plot.pdf"), width=1400, height=800)

    save_image(public_app_data_plot, file="plots/Fig_2_public_app_data.png", height=1200, width=1800)
    save_image(public_app_data_plot, file="plots/Fig_2_public_app_data.pdf", height=1200, width=1800)

    save_image(paper_figure_3, file=glue("plots/Fig_3_app_positives_panel.png"), width=2200, height=1000)
    save_image(paper_figure_3, file=glue("plots/Fig_3_app_positives_panel.pdf"), width=2200, height=1000)

    save_image(paper_figure_4, file=glue("plots/Fig_4_N_and_P.png"), width=1800, height=1000)
    save_image(paper_figure_4, file=glue("plots/Fig_4_N_and_P.pdf"), width=1800, height=1000)

    save_image(paper_figure_5, file=glue("plots/Fig_5_TPAEN_and_variations.png"), width=2200, height=2200)
    save_image(paper_figure_5, file=glue("plots/Fig_5_TPAEN_and_variations.pdf"), width=2200, height=2200)

    save_image(cumulative_plots$p1, file="plots/Fig_6a_cumulative_cases.png", width=1600, height=800)
    save_image(cumulative_plots$p1, file="plots/Fig_6a_cumulative_cases.pdf", width=1600, height=800)

    save_image(cumulative_plots$p2, file="plots/Fig_6b_cumulative_hospitalisations.png", width=1600, height=800)
    save_image(cumulative_plots$p2, file="plots/Fig_6b_cumulative_hospitalisations.pdf", width=1600, height=800)

    save_image(cumulative_plots$p3, file="plots/Fig_6c_cumulative_deaths.png", width=1600, height=800)
    save_image(cumulative_plots$p3, file="plots/Fig_6c_cumulative_deaths.pdf", width=1600, height=800)

    save_image(key_sharing_plot, file=glue("plots/Fig_S1_key_sharing.png"), width=1400, height=600)
    save_image(key_sharing_plot, file=glue("plots/Fig_S1_key_sharing.pdf"), width=1400, height=600)

    save_image(variant_weighting_plot, file=glue("plots/Fig_S2_variant_weighting.png"), width=1100, height=700)
    save_image(variant_weighting_plot, file=glue("plots/Fig_S2_variant_weighting.pdf"), width=1100, height=700)

    save_image(manual_timings_plot, file=glue("plots/Fig_S3_manual_timings.png"), width=2500, height=1200)
    save_image(manual_timings_plot, file=glue("plots/Fig_S3_manual_timings.pdf"), width=2500, height=1200)

    save_image(infectiousness_diagram, file=glue("plots/Fig_S4_infectiousness_diagram.pdf"), width=1200, height=500)
    save_image(infectiousness_diagram, file=glue("plots/Fig_S4_infectiousness_diagram.png"), width=1200, height=500)

    save_image(cases_sensitivity_analysis_plot, file=glue("plots/Fig_S6_cases_sensitivity_analysis.png"), width=2500, height=2000)
    save_image(cases_sensitivity_analysis_plot, file=glue("plots/Fig_S6_cases_sensitivity_analysis.pdf"), width=2500, height=2000)

    save_image(hosp_cases_sensitivity_analysis_plot, file=glue("plots/Fig_S7_hosp_cases_sensitivity_analysis.png"), width=2500, height=2000)
    save_image(hosp_cases_sensitivity_analysis_plot, file=glue("plots/Fig_S7_hosp_cases_sensitivity_analysis.pdf"), width=2500, height=2000)

    save_image(deaths_sensitivity_analysis_plot, file=glue("plots/Fig_S8_deaths_sensitivity_analysis.png"), width=2500, height=2000)
    save_image(deaths_sensitivity_analysis_plot, file=glue("plots/Fig_S8_deaths_sensitivity_analysis.pdf"), width=2500, height=2000)

    mapshot(ltla_map_plots$l0, file="plots/Fig_1b_uptake_map.pdf", vwidth=440, vheight=450, selfcontained=FALSE)
    mapshot(ltla_map_plots$l0, file="plots/Fig_1b_uptake_map.png", vwidth=440, vheight=450)

    mapshot(ltla_map_plots$l1, file="plots/Fig_6_map_averted_raw.pdf", vwidth=600, vheight=450)
    mapshot(ltla_map_plots$l1, file="plots/Fig_6_map_averted_raw.png", vwidth=600, vheight=450)

    mapshot(ltla_map_plots$l2, file="plots/Fig_6_map_averted.pdf", vwidth=440, vheight=450)
    mapshot(ltla_map_plots$l2, file="plots/Fig_6_map_averted_perc.png", vwidth=440, vheight=450)

    # zip the plot files ready for easy exporting / downloading
    zip(zipfile = glue("plots/plots"), files = 'plots/')
}



