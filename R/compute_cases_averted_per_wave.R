compute_cases_averted_per_wave <- function(wave.start.date, wave.end.date, app.and.case.data, wave, 
                                             other_quarantine_reduction = other_quarantine_reduction,
                                             max.proportion.who.know.infected
) {
  
    # function for calculating the moving average - would be better to use frollmean from data.table 
    runmean <- function(x,k){ 
      k<-floor(k/2)
      n<-length(x)
      isna<-is.na(x)
      x[isna]<-0
      cxs<-cumsum(x)
      cxn<-cumsum(1-isna)
      cs<-c(rep(0,k+1),cxs,rep(cxs[n],k))
      cn<-c(rep(0,k+1),cxn,rep(cxn[n],k))
      ( cs[2*k+1+1:n]-cs[1:n] ) / ( cn[2*k+1+1:n]-cn[1:n] )
    }
    
    wave.app.and.case.data <- app.and.case.data %>%
        filter(date >= wave.start.date) %>%
        filter(date <= wave.end.date) 
  
    if (wave == "pre.alpha") {
        
        # totcases_w is a matrix where columns are LTLAs and each row is a date, entries are rolling weekly means of cases
        my_totcases<- wave.app.and.case.data %>%
          select(date, ltla_name, pre.alpha.cases) %>%
          arrange(ltla_name) %>%
          pivot_wider(names_from = ltla_name, values_from = pre.alpha.cases) %>%
          ungroup() %>%
          select(-date)
          
        my_totcases_w <- suppressWarnings((apply(my_totcases,2,function(x){runmean(x,7)})))
        
        # partial_w is a matrix where columns are LTLAs and each row is a date, entries are rolling weekly means of number of isolation notifications
    
        my_partial <- wave.app.and.case.data %>%
          select(date, ltla_name, pre.alpha.ping) %>%
          pivot_wider(names_from = ltla_name, values_from = pre.alpha.ping) %>%
          ungroup() %>%
          select(-c(date))
        
        my_partial_w <- suppressWarnings((apply(my_partial,2,function(x){runmean(x,7)})))
    
    } else if (wave == "alpha") {
        # totcases_w is a matrix where columns are LTLAs and each row is a date, entries are rolling weekly means of cases
        my_totcases <- wave.app.and.case.data %>%
          select(date, ltla_name, alpha.cases) %>%
          arrange(ltla_name) %>%
          pivot_wider(names_from = ltla_name, values_from = alpha.cases) %>%
          ungroup() %>%
          select(-date)
    
        my_totcases_w <- suppressWarnings((apply(my_totcases,2,function(x){runmean(x,7)})))
    
        # partial_w is a matrix where columns are LTLAs and each row is a date, entries are rolling weekly means of number of isolation notifications
    
        my_partial <- wave.app.and.case.data %>%
          select(date, ltla_name, alpha.ping) %>%
          pivot_wider(names_from = ltla_name, values_from = alpha.ping) %>%
          ungroup() %>%
          select(-c(date))
        
        my_partial_w <- suppressWarnings((apply(my_partial,2,function(x){runmean(x,7)})))
    
    } else {
        # totcases_w is a matrix where columns are LTLAs and each row is a date, entries are rolling weekly means of cases
        my_totcases <- wave.app.and.case.data %>%
          select(date, ltla_name, cases) %>%
          arrange(ltla_name) %>%
          pivot_wider(names_from = ltla_name, values_from = cases) %>%
          ungroup() %>%
          select(-date)
      
        my_totcases_w <- suppressWarnings((apply(my_totcases,2,function(x){runmean(x,7)})))
      
        # partial_w is a matrix where columns are LTLAs and each row is a date, entries are rolling weekly means of number of isolation notifications
      
        my_partial <- wave.app.and.case.data %>%
          select(date, ltla_name, ping) %>%
          pivot_wider(names_from = ltla_name, values_from = ping) %>%
          ungroup() %>%
          select(-c(date))
        
        my_partial_w <- suppressWarnings((apply(my_partial,2,function(x){runmean(x,7)})))
      
    }
  

  # mean generation time from Ferretti, Ledda et al 2020
  generation_time<-integrate(function(x){dweibull(x,shape=3.2862,scale=6.1244)*x},0,Inf)$value
  
  step.size <- 0.001
  infectious.end.time <- 12
  
  # logistic distribution to describe the proportion of infected people who "know" 
  # (or have enough reason to think they are, to change their behaviour by reducing their risky contacts to other_quarantine_reduction of previous levels)
  proportion.who.know <- function(t, max.proportion.who.know.infected) {
    max.proportion.who.know.infected/(exp(-t+infectious.end.time/2) + 1)
  }
  
  # relative infectiousness profile in time, based on the above generation time
  infectiousness<-function(t){
    sapply(1:dim(t)[1],
           function(j){
             (mean(1 - pweibull(qnorm(seq(0,1,step.size),t[j,1],t[j,2]),shape=3.2862,scale=6.1244) -
                    other_quarantine_reduction * 
                    proportion.who.know(seq(0,infectious.end.time,step.size*infectious.end.time), max.proportion.who.know.infected)))
             
           })
    }
  
    # get delay data 
  delay_quarantine <- read_csv("data/private/delays_exposure_to_notification.csv", show_col_types=F)  
  
  # fraction of infectious period remaining after notification
  #infectiousness_after_notification <- infectiousness(cbind.data.frame(delay_quarantine$meanENdelay, delay_quarantine$sdENdelay))
  
  if (wave == "delta") {
    app_quarantine_reduction_pessimistic <- 0.2
    app_quarantine_reduction_optimistic <- 0.6
    app_quarantine_reduction_central <- 0.4
  } else { # pre-alpha or alpha
    # reduction in transmission due to app notification
    full_app_quarantine_reduction<-0.975
    # pessimistic estimates for mean, 1st and 3rd quartile, and lower/upper 95% credibility intervals
    app_quarantine_reduction_pessimistic <- 0.11*full_app_quarantine_reduction+(0.65-0.11)*0.5 # approx 0.38
    app_quarantine_reduction_optimistic <- 0.8*full_app_quarantine_reduction+(1-0.8-0.12)*0.5 # 0.82
    app_quarantine_reduction_central <- (app_quarantine_reduction_optimistic + app_quarantine_reduction_pessimistic)/2 # 0.6
  }
  
  
  delay_quarantine <- delay_quarantine %>%
    mutate("infectiousness_after_notification_central" = infectiousness(cbind.data.frame(delay_quarantine$meanENdelay, delay_quarantine$sdENdelay)) * app_quarantine_reduction_central,
           "infectiousness_after_notification_optimistic" = infectiousness(cbind.data.frame(delay_quarantine$meanENdelay, delay_quarantine$sdENdelay)) * app_quarantine_reduction_optimistic,
           "infectiousness_after_notification_pessimistic" = infectiousness(cbind.data.frame(delay_quarantine$meanENdelay, delay_quarantine$sdENdelay)) * app_quarantine_reduction_pessimistic
    ) %>%
    filter(date >= wave.start.date) %>%
    filter(date <= wave.end.date)
  
  one.ltla <- unique(wave.app.and.case.data$ltla_name)[[1]]
  
  
  infxTPAEN_central <- delay_quarantine$infectiousness_after_notification_central * (wave.app.and.case.data %>% filter(ltla_name == one.ltla))$TPAEN  # NB all ltla's have the same TPAEN in this version
  infxTPAEN_optimistic <-  delay_quarantine$infectiousness_after_notification_optimistic * (wave.app.and.case.data %>% filter(ltla_name == one.ltla))$TPAEN
  infxTPAEN_pessimistic <- delay_quarantine$infectiousness_after_notification_pessimistic * (wave.app.and.case.data %>% filter(ltla_name == one.ltla))$TPAEN
  
  my_averted_central <- my_totcases_w*(apply(sweep(my_partial_w/my_totcases_w,1,infxTPAEN_central,"*"),2,function(x){cumsum(x)}))/generation_time 
  my_averted_optimistic <- my_totcases_w*(apply(sweep(my_partial_w/my_totcases_w,1,infxTPAEN_optimistic,"*"),2,function(x){cumsum(x)}))/generation_time 
  my_averted_pessimistic <- my_totcases_w*(apply(sweep(my_partial_w/my_totcases_w,1,infxTPAEN_pessimistic,"*"),2,function(x){cumsum(x)}))/generation_time 
  
  # remove any "Infs"
  my_averted_central[which(!is.finite(my_averted_central))] <- 0 
  my_averted_optimistic[which(!is.finite(my_averted_optimistic))] <- 0 
  my_averted_pessimistic[which(!is.finite(my_averted_pessimistic))] <- 0 
  
  my_daily_averted <- rowSums(my_averted_central,na.rm=T)
  my_daily_averted_upper <- rowSums(my_averted_optimistic, na.rm = T)
  my_daily_averted_lower <- rowSums(my_averted_pessimistic, na.rm = T)
  
  my_tot_averted <- sum(my_daily_averted)
  my_tot_averted_upper <- sum(my_daily_averted_upper)
  my_tot_averted_lower <- sum(my_daily_averted_lower)
  
  my_tot_averted
  
  return(
    list(
      "total_averted" = my_tot_averted,
      "total_averted_upper" = my_tot_averted_upper,
      "total_averted_lower" = my_tot_averted_lower,
      "daily_averted" = my_daily_averted,
      "daily_averted_upper" = my_daily_averted_upper,
      "daily_averted_lower" = my_daily_averted_lower,
      "dates" = seq.Date(wave.start.date, wave.end.date, by=1)
    )
  )
  
}

