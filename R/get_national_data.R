get_national_data <- function(cleaned.app.data, recalculate.TPAEN = FALSE) {
  
  # cleaned.app.data has a row per combination of date and area.
  # summarise into a "national" dataframe with one row per date, showing total positive tests and total notifications 
  national.data <- cleaned.app.data %>%
    group_by(date) %>% 
    summarise(
        test_positive=sum(test_positive,na.rm=T),
        notifications=sum(notifications,na.rm=T)
    )
  
  if (recalculate.TPAEN) {
      print("Recalculating TPAEN. Warning: this can take multiple hours. Timing will depend on how many cores are available.")
      compute_TPAEN(cleaned.app.data)
  } 
  
  # add TPAEN estimates and key sharing data
  TPAEN.data <- read_csv(glue("data/private/TPAEN.csv"), show_col_types=F )
  national.data <- left_join(national.data, TPAEN.data, by="date") 
  
  key.sharing.data <- read_csv(glue("data/private/key_sharing.csv"), show_col_types=F )
  national.data <- left_join(national.data, key.sharing.data, by="date") 

  return(national.data) 
}