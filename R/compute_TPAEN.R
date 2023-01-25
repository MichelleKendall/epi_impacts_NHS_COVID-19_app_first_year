# This code calculates TPAEN.
# Please note that the calculation can take a long time, especially if `parallel::detectCores()` is 1.

compute_TPAEN <- function(cleaned.app.data){

  using("rstan", #v.2.21.5
  "splines", #v.4.0.4
  "Rcpp") #v.1.0.9
  
  # R code to call Stan 
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  sm<-stan_model(file = "R/compute_TPAEN.stan")
  
  df.discrete <- read_csv("data/private/df.discrete.csv", show_col_types = FALSE)
  
  delaydistr <- df.discrete$pmf[1:31]
  delaydistr <- delaydistr/sum(delaydistr)
  reverse_delaydistr <- rev(delaydistr[1:31])
  
  data.orig <- cleaned.app.data
  
  # Select columns of interest and aggregate to national
  data <- data.orig %>%
    group_by(date) %>%
    summarise(
      users = sum(users, na.rm=TRUE),
      received_exposure_notification = sum(notifications, na.rm=TRUE),
      test_positive = sum(test_positive, na.rm=TRUE),
      test_positive_after_exposure = sum(positive_after_EN, na.rm=TRUE)
    ) %>%
    ungroup() %>%
    drop_na()
  
  num.days <- nrow(distinct(data, date))
  data$day.as.int <- 1:num.days
  
  N <- data$received_exposure_notification # notified
  P <- data$test_positive_after_exposure # notified and positive
  
  stopifnot(sort(data$day.as.int)==seq(from=min(data$day.as.int), to=max(data$day.as.int), by=1))
  
  X <- data$day.as.int # generating inputs
  knots.dates <- rev(seq(from=max(X)-1, to=min(X), by=-7))
  
  B <- t(ns(X, knots=knots.dates, intercept = TRUE)) # creating the natural B-splines
  num_data <- length(X); num_basis <- nrow(B)
  #Y_true <- as.vector(a0*X + a%*%B) # generating the output
  n_ref <- mean(N) # typical value
  s_ref <- mean(P)/n_ref # typical value
  data_for_stan<-list(
    num_data = num_data, 
    num_basis = num_basis, 
    n_ref = n_ref,
    s_ref = s_ref,
    P = P,
    N = N, 
    X = X, 
    reverse_delaydistr = reverse_delaydistr,
    B = B
  )
  
  fit<-sampling(sm, data=data_for_stan, iter=2000,control = list(adapt_delta=0.95))
  
  fit_summary<-summary(fit)$summary
  sampled_values<-extract(fit)
  
  
  tpaen.df <- fit_summary[grepl("S_",rownames(fit_summary)),]
  tpaen.df <- as_tibble(tpaen.df)
  tpaen.df$date <- data$date
    
  tpaen.df <- tpaen.df %>%
    select(
        date,
        "TPAEN" = mean, 
        "lower.TPAEN" = `2.5%`, 
        "upper.TPAEN" = `97.5%`
    )
    
  write_csv(tpaen.df, "data/private/TPAEN.csv")
}




