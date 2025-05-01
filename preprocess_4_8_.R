# -----------------------------------------
# data_processing.R
# -----------------------------------------
# This script performs the following tasks:
#   1. Reads the CSV file ("mc1-reports-data.csv") with a header.
#   2. Parses the time field using multiple potential formats.
#   3. Removes records that do not include specific time information.
#   4. Keeps only April 8 data (the entire day).
#   5. Cleans the numeric columns, replacing NA and negative values with 0.
#   6. Aggregates the data by grouping on location and time:
#         - For shake_intensity: take the maximum value.
#         - For the other columns (sewer_and_water, power, roads_and_bridges,
#           medical, buildings): take the sum.
#   7. Writes the final processed data to "reports_4_8.csv".
# -----------------------------------------

library(tidyverse)
library(lubridate)

# 1. Read the CSV file (assumes a header and comma-separated values)
raw_data <- read_csv("D:/StHimarkData/CityVisualization/mc1-reports-data.csv")

# 2. Parse the 'time' column using multiple possible date-time formats
parsed_data <- raw_data %>%
  mutate(
    time_parsed = parse_date_time(
      time,
      orders = c(
        "ymd HMS", "ymd HM", "ymd",
        "mdy HMS", "mdy HM", "mdy",
        "dmy HMS", "dmy HM", "dmy"
      ),
      tz = "UTC"
    )
  ) %>%
  # Remove any rows where parsing failed (i.e., missing specific time)
  filter(!is.na(time_parsed)) %>%
  select(-time) %>%             # Drop the original 'time' column
  rename(time = time_parsed)    # Rename 'time_parsed' to 'time'

# 3. Convert key columns to numeric and clean NAs/negatives to 0
processed_data <- parsed_data %>%
  mutate(across(
    c(shake_intensity, sewer_and_water, power,
      roads_and_bridges, medical, buildings),
    as.numeric
  )) %>%
  mutate(across(
    c(shake_intensity, sewer_and_water, power,
      roads_and_bridges, medical, buildings),
    ~ if_else(is.na(.x) | .x < 0, 0, .x)
  ))

# 4. Keep only records for April 8 (all day)
data_4_8 <- processed_data %>%
  filter(date(time) == ymd("2020-04-08"))

# 5. Aggregate by location and time:
#    - shake_intensity: maximum
#    - other columns: sum
aggregated_4_8 <- data_4_8 %>%
  group_by(location, time) %>%
  summarise(
    shake_intensity   = max(shake_intensity,   na.rm = TRUE),
    sewer_and_water   = sum(sewer_and_water,   na.rm = TRUE),
    power             = sum(power,             na.rm = TRUE),
    roads_and_bridges = sum(roads_and_bridges, na.rm = TRUE),
    medical           = sum(medical,           na.rm = TRUE),
    buildings         = sum(buildings,         na.rm = TRUE),
    .groups = "drop"
  )

# 6. Rename columns to desired capitalization and write out the final CSV
final_data <- aggregated_4_8 %>%
  rename(
    ShakeIntensity   = shake_intensity,
    SewerAndWater    = sewer_and_water,
    Power            = power,
    RoadsAndBridges  = roads_and_bridges,
    Medical          = medical,
    Buildings        = buildings
  )

write_csv(final_data, "D:/StHimarkData/CityVisualization/reports48.csv")
cat("Preprocessing done! Output => reports48.csv\n")
