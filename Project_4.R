#####Libraries
library(tidyverse)
library(dplyr)
library(lubridate)
library(purrr)
library(ggplot2)
library(frenchdata)
library(xts)
library(PerformanceAnalytics)
library(CVXR)
library(quantmod)
library(readxl)
library(openxlsx)

#############################################
######### Pick one of the two cases #########
#############################################


#### Load the events/earnings data for Alnylam

events <- as.data.frame(read_excel("Copy of earnings_Alnylam.xlsx"))

# Convert the 'Ann_Date' column to Date format
events$Ann_Date <- as.Date(events$Ann_Date, format = "%m/%d/%Y")

### Load the events/announcements data for Alnylam

events <- tribble(
  ~Ann_Date, ~Event, ~News,
  "2019-03-06", "Positive Phase 3 results for Givosiran (ENVISION study)", "Good",
  "2019-04-08", "Collaboration with Regeneron ($800M investment)", "Good",
  "2019-06-05", "NDA submission for Givosiran completed", "Good",
  "2019-08-05", "FDA grants Priority Review to Givosiran NDA", "Good",
  "2019-11-20", "FDA approval of Givlaari™ (givosiran)", "Good",
  "2020-04-13", "$2B Strategic financing with Blackstone", "Good",
  "2020-10-30", "Fitusiran trials paused for safety", "Bad",
  "2020-11-23", "FDA approval of Oxlumo® (lumasiran)", "Good",
  "2020-12-10", "Fitusiran dosing resumed with modifications", "Good",
  "2020-12-18", "FDA rejects inclisiran (Complete Response Letter)", "Bad",
  "2021-01-07", "Positive Phase 3 results for Vutrisiran (HELIOS-A study)", "Good",
  "2021-10-28", "Founding CEO announces resignation", "Bad",
  "2021-12-22", "FDA approval of Leqvio® (inclisiran)", "Good",
  "2022-03-17", "Patent lawsuits over mRNA vaccine technology", "Neutral",
  "2022-06-13", "FDA approval of Amvuttra™ (vutrisiran)", "Good",
  "2022-08-03", "Positive Phase 3 results for Patisiran (APOLLO-B study)", "Good",
  "2023-07-24", "Partnership with Roche on Zilebesiran (hypertension)", "Good",
  "2023-09-07", "Positive Phase 2 results for Zilebesiran (KARDIA-1 study)", "Good",
  "2023-09-13", "FDA Advisory Committee supports Patisiran in cardiomyopathy", "Good",
  "2023-10-09", "FDA rejects Patisiran for cardiomyopathy (CRL)", "Bad",
  "2024-06-24", "Positive Phase 3 results for Vutrisiran (HELIOS-B study)", "Good",
  "2024-11-25", "FDA accepts sNDA for Vutrisiran in cardiomyopathy", "Good",
)

# Convert the 'Ann_Date' column to Date format
events$Ann_Date <- as.Date(events$Ann_Date, format = "%Y-%m-%d")

###################################################################################



### Load the price data for Alnylam and caluclate daily returns

ALNY_PX <- read.xlsx("Daily_Data_Case_1.xlsx", sheet = 2) 
ALNY_PX$Date <- as.Date(ALNY_PX$Date,origin = "1899-12-30")

ALNY_PX <- ALNY_PX[which(ALNY_PX$Date > as.Date("2014-01-01")),]
na <- which(is.na(ALNY_PX$Last.Price))
ALNY_PX$Last.Price[na] <- (ALNY_PX$Last.Price[na-1] + ALNY_PX$Last.Price[na+1]) * 0.5

ALNY_PX_xts <- xts(ALNY_PX$Last.Price, order.by = as.Date(ALNY_PX$Date))

ALNY_Ret_xts <- periodReturn(ALNY_PX_xts, period = "daily", type = "arithmetic")

ALNY_Ret <- data.frame(date = index(ALNY_Ret_xts), coredata(ALNY_Ret_xts))[-1,]


### Use Fama Frenche Data for Mrkt. Excess Return and Riskfree Rate

start_date <- "2014-01-01"
end_date <- "2024-12-31"

# Download the Fama-French 3 Factors data (both monthly and daily)
factors_ff_daily_raw <- download_french_data("Fama/French 3 Factors [Daily]")


mkt_rf_ff_daily <- factors_ff_daily_raw$subsets$data[[1]] %>%
  mutate(
    # Convert the date string to a Date object
    date = ymd(date),
    # Convert percentage figures to decimals
    across(c(RF, `Mkt-RF`, SMB, HML), ~ as.numeric(.) / 100),
    # Drop any other columns
    .keep = "none"
  ) %>%
  # Rename columns to lower case for readability
  rename_with(str_to_lower) %>%
  # Rename 'mkt-rf' column to 'mkt_excess'
  rename(mkt_excess = `mkt-rf`) %>%
  # Filter for the desired time frame
  # Rearranging columns if needed
  select(date, mkt_excess, rf)

# Display the first few rows of the daily data
head(mkt_rf_ff_daily)


### Merge 

Returns <- left_join(ALNY_Ret, mkt_rf_ff_daily, by = "date")

Returns <- Returns %>% 
  mutate(
    ALNY_exc = daily.returns - rf
  )


#### Event Study

MM_t <- function(ann_date) {
  
  # 1) Regression estimation window: the -220 to -21 days prior to Ann_date
  
  sub <- Returns %>%
    filter(date >= ann_date - 220, date <= ann_date - 21) %>%
    select(date, ALNY_exc, mkt_excess,rf) %>%
    arrange(date)
  
  # 3) Determine event window (T1 days after announcement)
  row_num_for_date  <- which(Returns$date == ann_date) + 5
  ann_date_plus_t1  <- Returns$date[row_num_for_date]
  
  sub_event_window <- Returns %>%
    filter(date <= ann_date_plus_t1) %>%
    select(date, ALNY_exc, mkt_excess,rf) %>%
    arrange(date)
  sub_event_window <- tail(sub_event_window, 11)
  
  # 4) Rename columns for simpler formula
  colnames(sub) <- c("date", "dep", "ind","rf")
  
  # 5) If fewer than 100 non-NA observations, return NA
  if (length(na.omit(sub$dep)) < 100) {
    return(NA)
  }
  
  # 6) OLS
  model <- lm(dep ~ ind, data = sub)
  res   <- var(residuals(model))
  alpha <- coef(model)[1]
  beta  <- coef(model)[2]
  
  # 7) Estimate normal excess returns, compute abnormal returns
  estimate <- alpha + beta * sub_event_window[["mkt_excess"]]
  abnormal_returns <- sub_event_window[["ALNY_exc"]] - estimate
  cum_abnormal_returns_55  <- cumsum(abnormal_returns)
  cum_abnormal_returns_51  <- c(cumsum(abnormal_returns[1:5]),rep(NA, 6))
  cum_abnormal_returns_05  <- c(rep(NA,4),cumsum(abnormal_returns[5:11]))
  
  c(rep(NA, 3), vec, rep(NA, 3))
  # 8) Output
  output <- data.frame(
    Date = sub_event_window$date,
    Abnormal_Return = abnormal_returns,
    Cumulative_Abnormal_Returns_55 = cum_abnormal_returns_55,
    Cumulative_Abnormal_Returns_51 = cum_abnormal_returns_51,
    Cumulative_Abnormal_Returns_05 = cum_abnormal_returns_05,
    Actual_Return = sub_event_window[["ALNY_exc"]] + sub_event_window[["rf"]],
    Estimated_Return = estimate
  )
  
  return(output)
}



results <- data.frame(Event = as.Date(character()),
                      News = character(),
                      Output = I(list()))


results <- list()

# Fill them with results
for (i in seq_len(nrow(events))) {
  # For each event, run each T1
  
  print(events$Ann_Date[i])
  # Market Model
  event_name <- as.character.Date(events$Ann_Date[i])
  mm_result <- MM_t(ann_date = events$Ann_Date[i])
  
  #chris solution
  results[[event_name]] <- mm_result
  
  print(paste0("Finished ", events$Ann_Date[i]))
}

# Save results[["2019-03-06"]] as an excel
write.xlsx(results[["2019-03-06"]], file = "2019-03-06.xlsx", sheetName = "Sheet1", rowNames = FALSE)

# Obtain the t-values of the AR and CARs for each event
t_values_events <- lapply(results, function(x) {
  t_test_ar <- t.test(x$Abnormal_Return, mu = 0)
  t_test_car_55 <- t.test(x$Cumulative_Abnormal_Returns_55, mu = 0)
  t_test_car_51 <- t.test(x$Cumulative_Abnormal_Returns_51, mu = 0)
  t_test_car_05 <- t.test(x$Cumulative_Abnormal_Returns_05, mu = 0)
  t_value <- data.frame(
    t_AR = t_test_ar$statistic,
    p_AR = t_test_ar$p.value,
    significant_AR = ifelse(t_test_ar$p.value < 0.05, TRUE, FALSE),
    
    t_CAR_55 = t_test_car_55$statistic,
    p_CAR_55 = t_test_car_55$p.value,
    significant_CAR_55 = ifelse(t_test_car_55$p.value < 0.05, TRUE, FALSE),
    
    t_CAR_51 = t_test_car_51$statistic,
    p_CAR_51 = t_test_car_51$p.value,
    significant_CAR_51 = ifelse(t_test_car_51$p.value < 0.05, TRUE, FALSE),
    
    t_CAR_05 = t_test_car_05$statistic,
    p_CAR_05 = t_test_car_05$p.value,
    significant_CAR_05 = ifelse(t_test_car_05$p.value < 0.05, TRUE, FALSE)
  )
  return(t_value)
})

# Save results[["2019-03-06"]] as an excel
write.xlsx(t_values_events[["2019-03-06"]], file = "2019-03-06_t.xlsx", sheetName = "Sheet1", rowNames = FALSE)

######################################### 18 ###################################
# Aggregate each event AR and CARs into a single data frame by taking the average

# Create a data frame to store the aggregated results
aggregated_results_good_news <- data.frame(
  days = -5:5,
  AAR = rep(0, 11),
  CAAR_55 = rep(0, 11),
  CAAR_51 = rep(0, 11),
  CAAR_05 = rep(0, 11)
)

aggregated_results_neutral_news <- data.frame(
  days = -5:5,
  AAR = rep(0, 11),
  CAAR_55 = rep(0, 11),
  CAAR_51 = rep(0, 11),
  CAAR_05 = rep(0, 11)
)

aggregated_results_bad_news <- data.frame(
  days = -5:5,
  AAR = rep(0, 11),
  CAAR_55 = rep(0, 11),
  CAAR_51 = rep(0, 11),
  CAAR_05 = rep(0, 11)
)

# Keep track of the amount of good, bad, and neutral news
good_counter <- 0
neutral_counter <- 0
bad_counter <- 0

# Loop through each event and calculate the average AR and CARs
for (i in seq_along(results)) {
  event_data <- results[[i]]
  event_date <- event_data$Date[6]
  
  # print(event_date)

  # Determine the news type by finding the row of the event_date in the events data frame and checking the News column
  event_row <- which(events$Ann_Date == event_date)
  news_type <- events$News[event_row]
  # print(news_type)
  
  # Calculate the average AR and CARs for each day
  if (news_type == "Good") {
    good_counter <- good_counter + 1
    aggregated_results_good_news$AAR <- aggregated_results_good_news$AAR + event_data$Abnormal_Return
    aggregated_results_good_news$CAAR_55 <- aggregated_results_good_news$CAAR_55 + event_data$Cumulative_Abnormal_Returns_55
    aggregated_results_good_news$CAAR_51 <- aggregated_results_good_news$CAAR_51 + event_data$Cumulative_Abnormal_Returns_51
    aggregated_results_good_news$CAAR_05 <- aggregated_results_good_news$CAAR_05 + event_data$Cumulative_Abnormal_Returns_05
  } else if (news_type == "Neutral") {
    neutral_counter <- neutral_counter + 1
    aggregated_results_neutral_news$AAR <- aggregated_results_neutral_news$AAR + event_data$Abnormal_Return
    aggregated_results_neutral_news$CAAR_55 <- aggregated_results_neutral_news$CAAR_55 + event_data$Cumulative_Abnormal_Returns_55
    aggregated_results_neutral_news$CAAR_51 <- aggregated_results_neutral_news$CAAR_51 + event_data$Cumulative_Abnormal_Returns_51
    aggregated_results_neutral_news$CAAR_05 <- aggregated_results_neutral_news$CAAR_05 + event_data$Cumulative_Abnormal_Returns_05
  } else if (news_type == "Bad") {
    bad_counter <- bad_counter + 1
    aggregated_results_bad_news$AAR <- aggregated_results_bad_news$AAR + event_data$Abnormal_Return
    aggregated_results_bad_news$CAAR_55 <- aggregated_results_bad_news$CAAR_55 + event_data$Cumulative_Abnormal_Returns_55
    aggregated_results_bad_news$CAAR_51 <- aggregated_results_bad_news$CAAR_51 + event_data$Cumulative_Abnormal_Returns_51
    aggregated_results_bad_news$CAAR_05 <- aggregated_results_bad_news$CAAR_05 + event_data$Cumulative_Abnormal_Returns_05
  }
}

# Divide the accumulated values by the number of events for each type of news
aggregated_results_good_news$AAR <- aggregated_results_good_news$AAR / good_counter
aggregated_results_good_news$CAAR_55 <- aggregated_results_good_news$CAAR_55 / good_counter
aggregated_results_good_news$CAAR_51 <- aggregated_results_good_news$CAAR_51 / good_counter
aggregated_results_good_news$CAAR_05 <- aggregated_results_good_news$CAAR_05 / good_counter

aggregated_results_neutral_news$AAR <- aggregated_results_neutral_news$AAR / neutral_counter
aggregated_results_neutral_news$CAAR_55 <- aggregated_results_neutral_news$CAAR_55 / neutral_counter
aggregated_results_neutral_news$CAAR_51 <- aggregated_results_neutral_news$CAAR_51 / neutral_counter
aggregated_results_neutral_news$CAAR_05 <- aggregated_results_neutral_news$CAAR_05 / neutral_counter

aggregated_results_bad_news$AAR <- aggregated_results_bad_news$AAR / bad_counter
aggregated_results_bad_news$CAAR_55 <- aggregated_results_bad_news$CAAR_55 / bad_counter
aggregated_results_bad_news$CAAR_51 <- aggregated_results_bad_news$CAAR_51 / bad_counter
aggregated_results_bad_news$CAAR_05 <- aggregated_results_bad_news$CAAR_05 / bad_counter

combined_aggregated_results <- data.frame(
  Event_Day = aggregated_results_good_news$days,
  
  AAR_Good     = aggregated_results_good_news$AAR,
  AAR_Neutral  = aggregated_results_neutral_news$AAR,
  AAR_Bad      = aggregated_results_bad_news$AAR,
  
  CAAR_55_Good    = aggregated_results_good_news$CAAR_55,
  CAAR_55_Neutral = aggregated_results_neutral_news$CAAR_55,
  CAAR_55_Bad     = aggregated_results_bad_news$CAAR_55,
  
  CAAR_51_Good    = aggregated_results_good_news$CAAR_51,
  CAAR_51_Neutral = aggregated_results_neutral_news$CAAR_51,
  CAAR_51_Bad     = aggregated_results_bad_news$CAAR_51,
  
  CAAR_05_Good    = aggregated_results_good_news$CAAR_05,
  CAAR_05_Neutral = aggregated_results_neutral_news$CAAR_05,
  CAAR_05_Bad     = aggregated_results_bad_news$CAAR_05
)

write.xlsx(combined_aggregated_results, file = "agg_results.xlsx", sheetName = "Sheet1", rowNames = FALSE)

# Calculate the t-values for the AAR and CAARs
t_values_good_news <- t.test(aggregated_results_good_news$AAR, mu = 0)
t_values_neutral_news <- t.test(aggregated_results_neutral_news$AAR, mu = 0)
t_values_bad_news <- t.test(aggregated_results_bad_news$AAR, mu = 0)
t_values_good_news_55 <- t.test(aggregated_results_good_news$CAAR_55, mu = 0)
t_values_neutral_news_55 <- t.test(aggregated_results_neutral_news$CAAR_55, mu = 0)
t_values_bad_news_55 <- t.test(aggregated_results_bad_news$CAAR_55, mu = 0)
t_values_good_news_51 <- t.test(aggregated_results_good_news$CAAR_51, mu = 0)
t_values_neutral_news_51 <- t.test(aggregated_results_neutral_news$CAAR_51, mu = 0)
t_values_bad_news_51 <- t.test(aggregated_results_bad_news$CAAR_51, mu = 0)
t_values_good_news_05 <- t.test(aggregated_results_good_news$CAAR_05, mu = 0)
t_values_neutral_news_05 <- t.test(aggregated_results_neutral_news$CAAR_05, mu = 0)
t_values_bad_news_05 <- t.test(aggregated_results_bad_news$CAAR_05, mu = 0)

# Create a data frame to store the t-values
t_values_agg_res <- data.frame(
  News_Type = c("Good", "Neutral", "Bad"),
  AAR_t_value = c(t_values_good_news$statistic, t_values_neutral_news$statistic, t_values_bad_news$statistic),
  AAR_p_value = c(t_values_good_news$p.value, t_values_neutral_news$p.value, t_values_bad_news$p.value),
  AAR_significant = c(t_values_good_news$p.value < 0.05, t_values_neutral_news$p.value < 0.05, t_values_bad_news$p.value < 0.05),
  
  CAAR_55_t_value = c(t_values_good_news_55$statistic, t_values_neutral_news_55$statistic, t_values_bad_news_55$statistic),
  CAAR_55_p_value = c(t_values_good_news_55$p.value, t_values_neutral_news_55$p.value, t_values_bad_news_55$p.value),
  CAAR_55_significant = c(t_values_good_news_55$p.value < 0.05, t_values_neutral_news_55$p.value < 0.05, t_values_bad_news_55$p.value < 0.05),
  
  CAAR_51_t_value = c(t_values_good_news_51$statistic, t_values_neutral_news_51$statistic, t_values_bad_news_51$statistic),
  CAAR_51_p_value = c(t_values_good_news_51$p.value, t_values_neutral_news_51$p.value, t_values_bad_news_51$p.value),
  CAAR_51_significant = c(t_values_good_news_51$p.value < 0.05, t_values_neutral_news_51$p.value < 0.05, t_values_bad_news_51$p.value < 0.05),
  
  CAAR_05_t_value = c(t_values_good_news_05$statistic, t_values_neutral_news_05$statistic, t_values_bad_news_05$statistic),
  CAAR_05_p_value = c(t_values_good_news_05$p.value, t_values_neutral_news_05$p.value, t_values_bad_news_05$p.value),
  CAAR_05_significant = c(t_values_good_news_05$p.value < 0.05, t_values_neutral_news_05$p.value < 0.05, t_values_bad_news_05$p.value < 0.05)
)


write.xlsx(t_values_agg_res, file = "t_agg_results.xlsx", sheetName = "Sheet1", rowNames = FALSE)

ggplot(combined_aggregated_results, aes(x = Event_Day)) +
  geom_line(aes(y = AAR_Good, color = "Good News"), linewidth = 1) +
  geom_line(aes(y = AAR_Neutral, color = "Neutral News"), linewidth = 1, linetype = "dashed") +
  geom_line(aes(y = AAR_Bad, color = "Bad News"), linewidth = 1, linetype = "dotted") +
  labs(
    title = "Average Abnormal Returns by News Category",
    x = "Event Day",
    y = "AAR"
  ) +
  scale_color_manual(values = c("Good News" = "darkgreen", "Neutral News" = "black", "Bad News" = "red")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

ggplot(combined_aggregated_results, aes(x = Event_Day)) +
  geom_line(aes(y = CAAR_55_Good, color = "Good News"), linewidth = 1) +
  geom_line(aes(y = CAAR_55_Neutral, color = "Neutral News"), linewidth = 1, linetype = "dashed") +
  geom_line(aes(y = CAAR_55_Bad, color = "Bad News"), linewidth = 1, linetype = "dotted") +
  labs(
    title = "Cumulative Average Abnormal Returns for the -5 to 5 Interval by News Category",
    x = "Event Day",
    y = "CAAR"
  ) +
  scale_color_manual(values = c("Good News" = "darkgreen", "Neutral News" = "black", "Bad News" = "red")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

ggplot(combined_aggregated_results[0:5, ], aes(x = Event_Day)) +
  geom_line(aes(y = CAAR_51_Good, color = "Good News"), linewidth = 1) +
  geom_line(aes(y = CAAR_51_Neutral, color = "Neutral News"), linewidth = 1, linetype = "dashed") +
  geom_line(aes(y = CAAR_51_Bad, color = "Bad News"), linewidth = 1, linetype = "dotted") +
  labs(
    title = "Cumulative Average Abnormal Returns for the -5 to -1 Interval by News Category",
    x = "Event Day",
    y = "CAAR"
  ) +
  scale_color_manual(values = c("Good News" = "darkgreen", "Neutral News" = "black", "Bad News" = "red")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

ggplot(combined_aggregated_results[6:11, ], aes(x = Event_Day)) +
  geom_line(aes(y = CAAR_05_Good, color = "Good News"), linewidth = 1) +
  geom_line(aes(y = CAAR_05_Neutral, color = "Neutral News"), linewidth = 1, linetype = "dashed") +
  geom_line(aes(y = CAAR_05_Bad, color = "Bad News"), linewidth = 1, linetype = "dotted") +
  labs(
    title = "Cumulative Average Abnormal Returns for the 0 to 5 Interval by News Category",
    x = "Event Day",
    y = "CAAR"
  ) +
  scale_color_manual(values = c("Good News" = "darkgreen", "Neutral News" = "black", "Bad News" = "red")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )


