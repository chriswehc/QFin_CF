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
  filter(date >= start_date & date <= end_date) %>%
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

MM_t <- function(company, ann_date, ret = all_data, T1, mp = "Mkt_lr") {
  
  # 1) Identify the log-return column for this company
  company_lr <- paste0(gsub(" ", "_", company), "_lr")
  
  # 2) Regression estimation window: the 250 days prior to ann_date
  #    We keep the last (250 - T1) rows, then keep the first 250 of that subset
  #    This effectively handles "shifting" if T1 != 10
  sub <- ret %>%
    filter(date <= ann_date) %>%
    select(date, all_of(company_lr), all_of(mp)) %>%
    arrange(date)
  sub <- tail(sub, 250 - T1)
  sub <- head(sub, 250)
  
  # 3) Determine event window (T1 days after announcement)
  row_num_for_date  <- which(all_data$date == ann_date) + T1
  ann_date_plus_t1  <- ret$date[row_num_for_date]
  
  sub_event_window <- ret %>%
    filter(date <= ann_date_plus_t1) %>%
    select(date, all_of(company_lr), all_of(mp)) %>%
    arrange(date)
  sub_event_window <- tail(sub_event_window, T1*2 + 1)
  
  # 4) Rename columns for simpler formula
  colnames(sub) <- c("date", "dep", "ind")
  
  # 5) If fewer than 100 non-NA observations, return NA
  if (length(na.omit(sub$dep)) < 100) {
    return(NA)
  }
  
  # 6) OLS
  model <- lm(dep ~ ind, data = sub)
  res   <- var(residuals(model))
  alpha <- coef(model)[1]
  beta  <- coef(model)[2]
  
  # 7) Estimate normal returns, compute abnormal returns
  estimate <- alpha + beta * sub_event_window[[mp]]
  abnormal_returns      <- sub_event_window[[company_lr]] - estimate
  cum_abnormal_returns  <- cumsum(abnormal_returns)
  
  # 8) Output
  output <- data.frame(
    Date = sub_event_window$date,
    Company = rep(company, T1*2 + 1),
    Abnormal_Log_Return = abnormal_returns,
    Cumulative_Abnormal_Log_Returns = cum_abnormal_returns,
    Actual_Log_Return = sub_event_window[[company_lr]],
    Estimated_Log_Return = estimate,
    Alpha = rep(alpha, T1*2 + 1),
    Beta = rep(beta, T1*2 + 1),
    Residuals = rep(res, T1*2 + 1)
  )
  
  return(output)
}

intervals <- c(10, 5, 2, 1, 0)



# Initialize empty data frames for each approach + interval
for (j in intervals) {
  # Market Model
  assign(paste0("MM_list_", j),
         data.frame(date = as.Date(character()),
                    Company = character(),
                    News = character(),
                    Output = I(list())) )
}

# Fill them with results
for (i in seq_len(nrow(events_df))) {
  # For each event, run each T1
  for (j in intervals) {
    
    # Market Model
    interval_name_MM <- paste0("MM_list_", j)
    mm_result <- MM_t(
      company  = events_df$Ticker[i],
      ann_date = events_df$Ann_Date[i],
      T1       = j
    )
    current_df_mm <- get(interval_name_MM)
    current_df_mm <- rbind(
      current_df_mm,
      data.frame(date = events_df$Ann_Date[i],
                 Company = events_df$Ticker[i],
                 News = events_df$news[i],
                 Output = I(list(mm_result)))
    )
    assign(interval_name_MM, current_df_mm)
    
  }
  print(paste0("Finished ", events_df$Ticker[i], " at ", events_df$Ann_Date[i]))
}







