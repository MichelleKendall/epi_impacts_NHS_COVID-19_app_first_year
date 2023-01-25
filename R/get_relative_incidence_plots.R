get_relative_incidence_plots <- function(last.date.to.plot) {
  
  # N.B the two datasets used in this function are the results of calculations on raw data.
  # Although the raw data cannot be shared for privacy reasons, the code for calculating them can be found below as a comment.
  
  relative.incidence.df <- read_csv("data/private/relative_incidence.csv", show_col_types=F)
    
  # left-censor a few days to allow for installations
  relative.incidence.df <- relative.incidence.df %>% 
    filter(date >= as.Date("2020-10-01")) 
  
  incidence.notified <- relative.incidence.df %>%
    filter(notified == TRUE) %>%
    mutate("notified.incidence" = incidence*1e5) %>%
    select(date, notified.incidence)
    
  incidence.not.notified <- relative.incidence.df %>%
    filter(notified == FALSE) %>%
    mutate("not.notified.incidence" = incidence*1e5) %>%
    select(date, not.notified.incidence)
    
  incidence.by.notification.status <- left_join(incidence.notified, incidence.not.notified)
  
  date.labels <- seq.Date(from=as.Date("2020-09-01"), to=as.Date("2021-09-30"), by="month")
  
  plots <- list()
  
  plots$p1 <- plot_ly(incidence.by.notification.status) %>%
    filter(date >= as.Date("2020-10-07")) %>% # skip the NAs
    filter(date <= last.date.to.plot) %>%
    add_lines(x=~date, y=~notified.incidence, name="Recently\nnotified",
              line=list(width=5), color=I("#21918c")) %>%
    add_lines(x=~date, y=~not.notified.incidence, name="Not recently\nnotified",
              line=list(width=5), color=I("#440154")) %>%
    layout(
      xaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="",
        titlefont=f1,
        tickfont=f1,
        tickvals=date.labels,
        ticktext = format(date.labels, "%b %y")
      ),
      yaxis=list(
        type="log",
        titlefont=f4,
        tickfont=f1,
        title="Daily incidence of cases per 100,000 individuals"
      ),
      legend=list(
        font=f1
      )
    )
  
  if (resave.results) {   
      # save figure data

      Fig_5e_data <- incidence.by.notification.status  %>%
        filter(date >= as.Date("2020-10-07")) %>%
        filter(date <= last.date.to.plot)

      write_csv(Fig_5e_data, file="results/Fig_5e.csv")
  }
  
    
  df_glm <- read_csv("data/private/relative_incidence_glm.csv", show_col_types=F)
  
  plots$p2 <- plot_ly(df_glm)  %>%
    add_ribbons(x=~date, ymin=~value_exp_lower,
                ymax =~value_exp_upper,
               opacity=0.5, color=I("#b73779"), showlegend=FALSE) %>%
    add_lines(x=~date, y=~value_exp,
              line=list(width=5), color=I("#b73779"), showlegend=FALSE) %>%
    add_lines(x=~date, y=1, color=I("black"), line=list(dash="dash", width=5), showlegend=FALSE) %>%
    layout(
      xaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="",
        titlefont=f1,
        tickfont=f1,
        tickvals=date.labels,
        ticktext = format(date.labels, "%b %y")
      ),
      yaxis=list(
        gridcolor = toRGB("darkgrey"),
        type="log",
        tickvals=c(1,2,3,5,10, 20, 30, 50, 100),
        ticktext=c(1,2,3,5,10, 20, 30, 50, 100),
        range=c(log10(0.7),log10(110)),
        titlefont=f4,
        tickfont=f1,
        title="Daily odds for testing positive in recently notified app users\nrelative to not-recently-notified app users"
      ),
      legend=list(
        font=f1
      )
    )
  plots$p2
  
  if (resave.results) {   
      # save figure data

      Fig_5f_data <- df_glm  %>%
        select(date, value_exp, value_exp_lower, value_exp_upper)

      write_csv(Fig_5f_data, file="results/Fig_5f.csv")
  }
  
  plots
}


# Code for calculating the relative incidence datasets

#  Query the app analytics data.
# Count the number of people each day disaggregated by whether they have been
# notified recently or not, whether they test positive that day or not, and
# whether they have an iphone or not.


# query_string <- paste0("
# SELECT COUNT(*) AS count,
#   CAST(startdate AS DATE) AS date,
#   /*localAuthority AS ltla,*/
#   CASE WHEN deviceModel LIKE 'iPhone%' THEN 1 ELSE 0 END AS iphone,
# 	CASE WHEN receivedPositiveTestResult > 0 THEN 1 ELSE 0 END AS positive,
# 	CASE WHEN receivedriskycontactnotification > 0 OR hashadriskycontactbackgroundtick > 0 THEN 1 ELSE 0 END AS notified
# FROM [LIVE].[XXX] # Note that the table name has been redacted for privacy reasons
# WHERE CAST(startdate AS DATE) >= '2020-12-17' /*'2021-06-09'*/
# 	AND CAST(startdate AS DATE) < '", Sys.Date(), "'
# GROUP BY CAST(startdate AS DATE),
#   CAST(startdate AS DATE),
#   /*localAuthority,*/
#   CASE WHEN deviceModel LIKE 'iPhone%' THEN 1 ELSE 0 END,
# 	CASE WHEN receivedPositiveTestResult > 0 THEN 1 ELSE 0 END,
#   CASE WHEN receivedriskycontactnotification > 0 OR hashadriskycontactbackgroundtick > 0 THEN 1 ELSE 0 END
# ")
# query <- dbSendQuery(db_connection, query_string)
# df <- dbFetch(query)
# dbClearResult(query)
# df <- df %>%
#   as_tibble() %>%
#   mutate(iphone = as.logical(iphone),
#          positive = as.logical(positive),
#          notified = as.logical(notified),
#          date = as.Date(date))
# 
# # Make implicit missing data explicit (make absent rows present, with count zero)
# df <- df %>%
#   complete(date, iphone, positive, notified)
# 
# # Add rows corresponding to either iOS or Android (i.e. the sum of both)
# df <- bind_rows(df %>%
#                   rename(os = iphone) %>%
#                   mutate(os = if_else(os, "iOS", "Android")),
#                 df %>%
#                   group_by(date, positive, notified) %>%
#                   summarise(count = sum(count), .groups = "drop") %>%
#                   mutate(os = "either"))
# 
# # Calculate daily incidence by OS and notification status...
# df_ <- df %>%
#   group_by(date, os, notified) %>%
#   summarise(incidence = unique(count[positive]) / sum(count), .groups = "drop")
# 
# write_csv(df %>% filter(os=="either"), file="data/private/relative_incidence.csv")
# 
# # For each OS and date, fit a logistic regression to get max-likelihood values
# # and confidence intervals for the effect of being notified on the log odds
# # for testing positive
# df_glm <- df %>%
#   split(list(.$os, .$date)) %>%
#   map(function(df_) {    
#     glm(data = df_,
#         formula = positive ~ notified,
#         family = binomial(link = 'logit'),
#         weights = count)}) %>%
#   map(function(glm_) {
#     inner_join(summary(glm_)$coefficients %>% as_tibble(rownames = "param"),
#                confint(glm_) %>% as_tibble(rownames = "param"),
#                by = "param")}) %>%
#   bind_rows(.id = "os.date") %>%
#   tidyr::separate(col = os.date, into = c("os", "date"), sep = "\\.") %>%
#   rename(value = Estimate,
#          value_lower = `2.5 %`,
#          value_upper = `97.5 %`) %>%
#   mutate(value_exp = exp(value),
#          value_exp_lower = exp(value_lower),
#          value_exp_upper = exp(value_upper))
# 
# write_csv(df_glm %>% 
#             filter(param != "(Intercept)",
#                    os == "either",
#                    date >= as.Date("2020-10-16"),
#                    date <= last.date.to.plot) %>%
#             select(date, value_exp, value_exp_lower, value_exp_upper),
#           file="relative_incidence_glm.csv")


