prepare_data_for_calculating_cases_averted <- function(cleaned.app.data = cleaned.app.data) {
    
    # load cases by specimen data by LTLA
    case.data <- read_csv("data/cases_by_specimen_date_ltlas.csv", show_col_types=F )
  
    # load national TPAEN estimates
    TPAEN.data <- read_csv(glue("data/private/TPAEN.csv"), show_col_types=F) %>% 
      select(date, TPAEN) 

    # get the region data and add it
   region.data <- read_csv("data/Merged_PD_Demo_Geo_2021-01-21_v2.csv", show_col_types=F)
    region.data <- region.data %>%
      mutate("ltla_name" = local_authority)  %>%
      mutate("Region" = region) %>%  
    select(c(ltla_name, Region))
    region.data <- unique(region.data)
    region.data <- bind_rows(
      region.data,
      c("ltla_name" = "Hackney and City of London", "Region" = "London") # to match up with the data sets we need to join it to
    )
    
    # clean up data so that all the datasets align on ltlas
    cleaned.app.data$ltla_name[cleaned.app.data$ltla_name=="Hackney"]<-"Hackney and City of London"
    cleaned.app.data$ltla_name[cleaned.app.data$ltla_name=="City of London"]<-"Hackney and City of London"
    cleaned.app.data$ltla_name[cleaned.app.data$ltla_name=="Cornwall"]<-"Cornwall and Isles of Scilly"

    Bucks.case.data <- case.data %>%
      filter(areaName %in% c( "Aylesbury Vale", "Chiltern", "South Bucks", "Wycombe")) %>%
      group_by(date) %>%
      summarise("areaCode" = NA, 
                "areaName" = "Buckinghamshire", 
                "areaType" = "ltla", 
                newCasesBySpecimenDate = sum(newCasesBySpecimenDate))

    case.data <- bind_rows(
      case.data %>%
        filter(!areaName %in% c( "Aylesbury Vale", "Chiltern", "South Bucks", "Wycombe")),
      Bucks.case.data
    )

    ltlas.app<-sort(unique(cleaned.app.data$ltla_name))
    ltlas.cases<- sort(unique(case.data$areaName))
    stopifnot(length(setdiff(ltlas.cases, ltlas.app)) == 0)
    stopifnot(length(setdiff(ltlas.app, ltlas.cases)) == 0)

  
    # tidy to only the relevant columns and join
    app.data <- cleaned.app.data %>%
          group_by(date,ltla_name) %>%
          summarise("users" = sum(users, na.rm=T), 
                    "cases_app" = sum(test_positive, na.rm=T),
                    "uptake" = mean(uptake, na.rm=T),
                    "ping" = sum(notifications, na.rm=T)
                    )

    case.data <- case.data %>% 
          select(date, 
                 "ltla_name" = areaName, 
                 "cases" = newCasesBySpecimenDate
                )

    case.data <- left_join(case.data, region.data)

    app.and.case.data <- left_join(app.data, case.data)
    app.and.case.data <- left_join(app.and.case.data, TPAEN.data)
    
    # introducing the weighting so we can approximately distinguish between pre-alpha and alpha cases
    alpha.weighting <- tibble(
      "date" = unique(app.and.case.data$date),
      "alpha.weighting" = 1/( 1+exp(as.double(-0.08*difftime(unique(app.and.case.data$date),as.Date('2020-12-21'),units="day")) ) ),
      "pre.alpha.weighting" = 1 - 1/( 1+exp(as.double(-0.08*difftime(unique(app.and.case.data$date),as.Date('2020-12-21'),units="day")) ) )
    ) %>%
      mutate("check" = alpha.weighting + pre.alpha.weighting)

    app.and.case.data <- left_join(app.and.case.data, alpha.weighting)

    app.and.case.data <- app.and.case.data %>%
      mutate("pre.alpha.cases" = cases*pre.alpha.weighting,
             "alpha.cases" = cases*alpha.weighting,
             "pre.alpha.ping" = ping*pre.alpha.weighting,
             "alpha.ping" = ping*alpha.weighting)
    
    app.and.case.data
}

