

# 1. 
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(lubridate)) install.packages("lubridate")
if(!require(ggplot2)) install.packages("ggplot2")

library(tidyverse)
library(lubridate)
library(ggplot2)

# 2. 
reports_raw <- read_csv("D:/StHimarkData/mc1-reports-data.csv")

# 3. 
reports_clean <- reports_raw %>%
  mutate(
    # Parse time using multiple formats; if parsing fails, returns NA.
    time_parsed = parse_date_time(
      time,
      orders = c(
        "ymd HMS", "ymd HM", "ymd",
        "mdy HMS", "mdy HM", "mdy",
        "dmy HMS", "dmy HM", "dmy"
      ),
      tz = "UTC"  # Set to "" if you do not want UTC.
    ),
    # Convert numeric columns
    shake_intensity   = as.numeric(shake_intensity),
    sewer_and_water   = as.numeric(sewer_and_water),
    power             = as.numeric(power),
    roads_and_bridges = as.numeric(roads_and_bridges),
    medical           = as.numeric(medical),
    buildings         = as.numeric(buildings)
  ) %>%
  mutate(shake_intensity = ifelse(is.na(shake_intensity), 0, shake_intensity))


na_count <- sum(is.na(reports_clean$time_parsed))
cat("Number of unparseable time rows:", na_count, "\n")

# Overwrite the time column with the parsed result, remove time_parsed,
# and preserve the original row order.
reports_clean <- reports_clean %>%
  mutate(time = format(time_parsed, "%Y-%m-%d %H:%M:%S")) %>%
  select(-time_parsed)

# 4.
write_csv(reports_clean, "D:/StHimarkData/reports_clean.csv")
cat("\nCleaned data (preserving original order) has been output to: D:/StHimarkData/reports_clean.csv\n")

# 5. Create an analysis copy: sort and aggregate by (time_5min, location) --------
#    a. Convert time to POSIXct (stored in time_dt)
#    b. Sort by time_dt (ascending) and location (ascending)
#    c. Floor the time to 5-minute intervals (time_5min)
analysis_df <- reports_clean %>%
  mutate(time_dt = ymd_hms(time)) %>%
  filter(!is.na(time_dt)) %>%       # Exclude records with unparseable time
  arrange(time_dt, location) %>%    # Sort in ascending order by time and location
  mutate(time_5min = floor_date(time_dt, "5 minutes"))

# 5.1 Aggregate data with the same (time_5min, location)
#     For each group, compute the mean of numeric columns (or other aggregation)
#     and record the number of merged records (count).
analysis_agg <- analysis_df %>%
  group_by(time_5min, location) %>%
  summarise(
    sewer_and_water   = mean(sewer_and_water, na.rm = TRUE),
    power             = mean(power, na.rm = TRUE),
    roads_and_bridges = mean(roads_and_bridges, na.rm = TRUE),
    medical           = mean(medical, na.rm = TRUE),
    buildings         = mean(buildings, na.rm = TRUE),
    shake_intensity   = mean(shake_intensity, na.rm = TRUE),
    count             = n(),  # Record the number of merged original records
    .groups           = "drop"
  ) %>%
  arrange(time_5min, location)

# Output the aggregated data for inspection
write_csv(analysis_agg, "D:/StHimarkData/analysis_agg.csv")
cat("Aggregated data has been output to: D:/StHimarkData/analysis_agg.csv\n")

# 6. Build a global time series based on the aggregated data --------------------
#    Aggregate across all locations by time_5min to get overall avg_shake and total report count.
time_series <- analysis_agg %>%
  group_by(time_5min) %>%
  summarise(
    avg_shake = mean(shake_intensity, na.rm = TRUE),  # You can also use max/min if desired.
    report_count = sum(count),  # Total report count across all locations.
    .groups = "drop"
  ) %>%
  arrange(time_5min)

# 6.1 Plot the time series for trend observation
p1 <- ggplot(time_series, aes(x = time_5min, y = report_count)) +
  geom_line() +
  labs(title = "Aggregated: Report Count per 5-Min Interval", x = "Time", y = "Report Count")

p2 <- ggplot(time_series, aes(x = time_5min, y = avg_shake)) +
  geom_line(color = "red") +
  labs(title = "Aggregated: Average Shake per 5-Min Interval", x = "Time", y = "Average Shake")

print(p1)
print(p2)

# 7. Earthquake detection (Threshold-based method) -----------------------------
# 7.1 Method A: Based on an absolute threshold of avg_shake
threshold_level <- 1.0  # Adjust this threshold based on your data distribution
cat("\n--- Method A: Detect the earliest time when avg_shake >= ", threshold_level, " ---\n")
idx_methodA <- which(time_series$avg_shake >= threshold_level)
if(length(idx_methodA) == 0) {
  quake_time_A <- NA
  cat("Method A: No avg_shake values found above the threshold\n")
} else {
  quake_time_A <- time_series$time_5min[min(idx_methodA)]
  cat("Method A detected earthquake time:", quake_time_A, "\n")
}

# 7.2 Method B: Based on a threshold on diff_avg_shake
threshold_diff <- 1.0  # Adjust this threshold as needed
cat("\n--- Method B: Detect the earliest time when diff_avg_shake >= ", threshold_diff, " ---\n")
time_series <- time_series %>%
  mutate(diff_avg_shake = c(NA, diff(avg_shake)))
idx_methodB <- which(time_series$diff_avg_shake >= threshold_diff)
if(length(idx_methodB) == 0) {
  quake_time_B <- NA
  cat("Method B: No diff_avg_shake values found above the threshold\n")
} else {
  first_spike <- min(idx_methodB)
  if(first_spike == nrow(time_series)) {
    quake_time_B <- time_series$time_5min[first_spike]
  } else {
    quake_time_B <- time_series$time_5min[first_spike + 1]
  }
  cat("Method B detected earthquake time:", quake_time_B, "\n")
}

# 7.3 Choose the final earthquake time (adjust logic as needed)
if(!is.na(quake_time_A)) {
  final_quake_time <- quake_time_A
} else {
  final_quake_time <- quake_time_B
}
cat("\n=== Final detected earthquake time:", final_quake_time, "===\n")

# 8. Label each record in the aggregated dataset as pre-quake or post-quake ------
#    Based on the earthquake time, add a new column "phase" to analysis_agg.
analysis_agg <- analysis_agg %>%
  mutate(phase = if_else(time_5min < final_quake_time, "pre-quake", "post-quake"))

# Extract pre-quake and post-quake datasets separately.
reports_prequake <- analysis_agg %>% filter(phase == "pre-quake")
reports_postquake <- analysis_agg %>% filter(phase == "post-quake")

# 9. Output the pre-quake/post-quake aggregated data ----------------------------
#    Adjust the columns as needed (e.g., remove auxiliary columns such as count if not required)
write_csv(reports_prequake,  "D:/StHimarkData/reports_prequake.csv")
write_csv(reports_postquake, "D:/StHimarkData/reports_postquake.csv")

cat("\n=== Data processing complete. Output files:\n")
cat("1) data/reports_clean.csv    -- Cleaned data in original order (not aggregated)\n")
cat("2) data/analysis_agg.csv     -- Aggregated data (grouped by time_5min and location)\n")
cat("3) data/reports_prequake.csv -- Aggregated pre-quake data (with phase label)\n")
cat("4) data/reports_postquake.csv-- Aggregated post-quake data (with phase label)\n")
cat("=== Final detected earthquake time:", final_quake_time, "===\n")
