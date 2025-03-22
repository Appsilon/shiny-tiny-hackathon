library(data.table)

# Set new total number of reports
n_scaled <- 301797
scale_factor <- n_scaled / 30179725

# Function to scale counts and adjust to sum exactly to n_scaled
adjust_counts <- function(counts, n_target) {
  scaled <- round(counts * scale_factor)
  # diff <- n_target - sum(scaled)
  # # Adjust the last element to ensure the total matches exactly
  # if(diff != 0) scaled[length(scaled)] <- scaled[length(scaled)] + diff
  return(scaled)
}

## ------------------------
## 1. Create the "Year" column
## ------------------------
year_levels <- 1968:2024
year_counts <- c(642, 32598, 107682, 78936, 57822, 68262, 55518, 63690, 64926,
                 82446, 56610, 49458, 77472, 87000, 142230, 192126, 195738, 232794,
                 335568, 328554, 306828, 364092, 451206, 451740, 635724, 745188,
                 774732, 850002, 1033224, 1189404, 959340, 1346178, 1198794,
                 1219248, 1109274, 1351404, 1636986, 1931274, 2014020, 2179950,
                 2638026, 2942448, 4035840, 4689762, 5581074, 6411558, 7190598,
                 10318746, 10100238, 10830060, 12840894, 13051704, 13216884,
                 13971540, 14022594, 12926730, 12250974)
year_counts_scaled <- adjust_counts(year_counts, n_scaled)
years <- rep(year_levels, times = year_counts_scaled)

## ------------------------
## 2. Create the "Quarter" column
## ------------------------
# Assign quarters uniformly at random
set.seed(123)  # for reproducibility
quarter <- sample(c("Q1", "Q2", "Q3", "Q4"), size = n_scaled, replace = TRUE)

## ------------------------
## 3. Create the "Age Group" column
## ------------------------
age_levels <- c("Not Specified", "0-1 Month", "2 Months-2 Years", "3-11 Years",
                "12-17 Years", "18-64 Years", "65-85 Years", "More than 85 Years")
age_counts <- c(11623602, 82051, 162459, 394501, 468958, 10690879, 6218174, 539101)
age_counts_scaled <- adjust_counts(age_counts, n_scaled)
age_group <- rep(age_levels, times = age_counts_scaled)

## ------------------------
## 4. Create the "Report Type" column
## ------------------------
rt_levels <- c("Expedited", "Non-Expedited", "Direct", "BSR")
rt_counts <- c(16526850, 12340068, 1311944, 863)
rt_counts_scaled <- adjust_counts(rt_counts, n_scaled)
report_type <- rep(rt_levels, times = rt_counts_scaled)

## ------------------------
## 5. Create the "Reporter" column
## ------------------------
rep_levels <- c("Not Specified", "Consumer", "Healthcare Professional", "Other")
rep_counts <- c(1017299, 14472340, 14689879, 207)
rep_counts_scaled <- adjust_counts(rep_counts, n_scaled)
reporter <- rep(rep_levels, times = rep_counts_scaled)

## ------------------------
## 6. Create the "Reporter Region" column
## ------------------------
rr_levels <- c("Domestic", "Foreign", "Not Specified")
rr_counts <- c(20718828, 9416520, 44377)
rr_counts_scaled <- adjust_counts(rr_counts, n_scaled)
reporter_region <- rep(rr_levels, times = rr_counts_scaled)

## ------------------------
## 7. Create the "Seriousness" column
## ------------------------
serious_levels <- c("Serious", "Death", "Non-Serious")
serious_counts <- c(16664479, 2722806, 10792440)
serious_counts_scaled <- adjust_counts(serious_counts, n_scaled)
seriousness <- rep(serious_levels, times = serious_counts_scaled)

## ------------------------
## 8. Create the "Sex" column
## ------------------------
sex_levels <- c("Not Specified", "Female", "Male")
sex_counts <- c(3570073, 16029968, 10579684)
sex_counts_scaled <- adjust_counts(sex_counts, n_scaled)
sex <- rep(sex_levels, times = sex_counts_scaled)

## ------------------------
## 9. Ensure each vector has exactly n_scaled elements
## ------------------------
# In case rounding leads to slight differences, we force each vector to have exactly n_scaled elements.
years <- sample(years, n_scaled)
age_group <- sample(age_group, n_scaled)
report_type <- sample(report_type, n_scaled)
reporter <- sample(reporter, n_scaled)
reporter_region <- sample(reporter_region, n_scaled)
seriousness <- sample(seriousness, n_scaled)
sex <- sample(sex, n_scaled)

## ------------------------
## 10. Combine all columns into a data frame
## ------------------------
df_scaled <- data.frame(Year = years,
                        Quarter = quarter,
                        Report_Type = report_type,
                        Reporter_Region = reporter_region,
                        Sex = sex,
                        Age_Group = age_group,
                        Seriousness = seriousness,
                        Reporter = reporter,
                        stringsAsFactors = FALSE)

## Optionally, shuffle the data frame rows for additional randomness
# set.seed(123)
# df_scaled <- df_scaled[sample(nrow(df_scaled)), ]
rownames(df_scaled) <- NULL

## Preview the first few rows
head(df_scaled)

setDT(df_scaled)

setorder(df_scaled, Year, Quarter)

df_scaled

fwrite(df_scaled, "data2.csv")
