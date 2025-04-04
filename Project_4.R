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


#### Load the events

events <- as.data.frame(read_excel("Copy of earnings_Alnylam.xlsx"))

# Convert the 'Ann_Date' column to Date format
events$Ann_Date <- as.Date(events$Ann_Date, format = "%m/%d/%Y")

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
  
  # Market Model
  event_name <- as.character.Date(events$Ann_Date[i])
  mm_result <- MM_t(ann_date = events$Ann_Date[i])
  
  #chris solution
  results[[event_name]] <- mm_result
  
  print(paste0("Finished ", events$Ann_Date[i]))
}


