# 3 wards
# patient start and end dates
# calculate los
# have outlier
# by ward
# visulaise distributions
#
# join beds table
# calculate who in bed at end of each month
# calculate bed occupancy
# look at occupancy over time
# create spc of bed occupancy
#
# look at correlation between los and occupancy
#
library(ggpubr)
library(tidyverse)
ward <- c("bulbasaur", "squirtle", "charmander")
no_beds <- c(25, 4, 2)

beds_data <- data.frame(
  ward,
  no_beds
)


ward <- sample(
  c(
    "bulbasaur", "bulbasaur", "squirtle",
    "squirtle", "charmander"
  ),
  300,
  replace = TRUE
)

patient <- seq(1, 300)

start_date <- as.Date("2025-01-01")
end_date <- as.Date("2025-12-31")

all_dates <- seq(from = start_date, to = end_date, by = "day")

set.seed(1234) # Set seed for reproducibility

random_dates_df <- data.frame(admission_date = all_dates) %>%
  mutate(
    Year = year(admission_date),
    Month = month(admission_date, label = TRUE, abbr = FALSE) # Returns full month name
  ) %>%
  group_by(Year, Month) %>%
  slice_sample(n = 35, replace = TRUE) %>%
  arrange(Year, Month, admission_date) %>% # Sort chronologically
  ungroup() |>
  slice_sample(n = 300, replace = TRUE) |>
  select(admission_date)

ward_data <- bind_cols(
  ward = ward,
  patient = patient,
  random_dates_df
)

ward_data <- ward_data |>
  rowwise() |>
  mutate(discharge_date = case_when(ward == "bulbasaur" ~ admission_date + days(as.integer(abs(rnorm(1, 40, 5)))),
    ward == "squirtle" ~ admission_date + days(as.integer(abs(rexp(1, 1 / 3)))),
    ward == "charmander" ~ admission_date + days(as.integer(rexp(1, 1 / 5))),
    .default = admission_date
  )) |>
  ungroup()

# manually add an outlier
ward_data$discharge_date[ward_data$patient == 152] <- as.Date("2027-12-25")

all_obs <- ls()

del_obs <- all_obs[!all_obs %in% c("beds_data", "ward_data")]

rm(list = del_obs)

rm(del_obs)

rm(all_obs)










#####################
# Start of analysis #
#####################

# load in a couple of libraries we are going to use
library(ggpubr)
library(tidyverse)

# look at our data

ward_data

beds_data

# order our data

ward_data <- ward_data |>
  arrange(
    ward,
    admission_date
  )

# calc los per patient

ward_data <- ward_data |>
  mutate(los = as.numeric(discharge_date - admission_date))

# visualise

ward_data |>
  ggplot() +
  aes(x = los) +
  geom_histogram() +
  theme_minimal()

summary(ward_data$los)


# we seem to have a big outlier - lets remove and replot

ward_data |>
  filter(los < 100) |>
  ggplot() +
  aes(x = los) +
  geom_histogram() +
  theme_minimal()


# visualise across each ward as histogram

ward_data |>
  filter(los < 100) |>
  ggplot() +
  aes(x = los) +
  geom_histogram() +
  facet_wrap(~ward) +
  theme_minimal()

# or could as a density plot
# far easier to see shapes of distribution
# bulbasaur = normal, charmander & squirtle expodential

ward_data |>
  filter(los < 100) |>
  ggdensity(
    x = "los",
    add = "mean",
    facet.by = "ward",
    rug = TRUE,
    color = "ward",
    fill = "ward"
  )

# boxplot with comparison of mean and medians

ward_data |>
  filter(los < 100) |>
  ggplot() +
  aes(
    x = los,
    y = ward
  ) +
  geom_boxplot() +
  stat_summary(
    fun = mean,
    geom = "point", shape = "+", size = 5, color = "red"
  ) +
  theme_minimal()

# or if we are interested in seeing patients box plot a jitter is pretty good

ward_data |>
  filter(los < 100) |>
  ggplot() +
  aes(
    x = los,
    y = ward
  ) +
  geom_boxplot() +
  geom_jitter() +
  stat_summary(fun = mean, geom = "point", shape = "+", size = 5, color = "red") +
  theme_minimal()

# lets make a new dataframe - a bit  like a pivot table
# we will have an average by each month and ward in year 2025

# mutate all our discharges to the start(floor) of the month 
# and then we can group them together

ward_data_average <- ward_data |>
  filter(discharge_date < '2026-01-01') |>
  mutate(discharge_month = floor_date(discharge_date, 
                                      "month")) |>    # convert to 1st of mth
  summarise(
    mean_av = mean(los),
    .by = c(
      ward,
      discharge_month     # calc mean by ward and updated month 
    )
  ) |>
  arrange(
    ward,
    discharge_month     # arrange by ward and month
  )

# lets have a quick look by plotting

ward_data_average |>
  ggplot() +
  aes(
    x = discharge_month,
    y = mean_av,
    colour = ward
  ) +
  geom_line(size = 1) +
  theme_minimal()

# add median?

ward_data_average <- ward_data |>
  filter(discharge_date < '2026-01-01') |>
  mutate(discharge_month = floor_date(discharge_date, 
                                      "month")) |>    # convert to 1st of mth
  summarise(
    mean_av = mean(los),
    median_av = median(los),     # simply add a median function
    .by = c(
      ward,
      discharge_month
    )
  ) |>
  arrange(
    ward,
    discharge_month
  )

# plot the median and mean

ward_data_average |>
  ggplot() +
  geom_line(aes(
    x = discharge_month,
    y = median_av,
    colour = ward,
  ) ,
  size = 1) +
  geom_line(aes(
    x = discharge_month,
    y = mean_av,
    colour = ward
 
  ) ,
  size = 1,
  linetype = 'dashed') +
  facet_wrap(~ward) +
  theme_minimal()


# lets do some more data wrangling

# calculate occupancy

# create a dummy list of end dates to do a cross join
# basically we want to know for each patient which months 
# they we in a bed for at the end of the month


month_ends <- tibble(month_end = seq(
  from = as.Date('2025-01-01'),
  to = as.Date('2025-12-31'),
  by = "1 month"
) |>
  ceiling_date("month") - days(1))

# join and count patients present at each month-end
# 
# a cross join that is like a lookup 
# and duplicates our patient rows for each month end
# 
monthly_ward_counts <- month_ends |>
  inner_join(
    ward_data,
    join_by(between(
      month_end,
      admission_date,
      discharge_date
    ))
  ) 

# we can repeat that process but now add a simple count of each month
monthly_ward_counts <- month_ends |>
  inner_join(
    ward_data,
    join_by(between(
      month_end,
      admission_date,
      discharge_date
    ))
  ) |>
  summarise(
    patient_count = n(),
    .by = c(
      ward,
      month_end
    )
  )

# now add on the number of beds and calculate % occupancy

monthly_ward_counts <- monthly_ward_counts |>
  left_join(beds_data) |>
  mutate(perc_occ = patient_count / no_beds)

monthly_ward_counts |>
  ggplot() +
  aes(
    x = month_end,
    y = perc_occ,
    colour = ward
  ) +
  geom_line(size = 1) +
  facet_wrap(~ward) +
  theme_minimal()

library(NHSRplotthedots)

monthly_ward_counts |>
  ptd_spc(
    date_field = month_end,
    value_field = perc_occ,
    facet_field = ward
  )

# add 90% occupancy target for squirtle
# and 80% for others 

monthly_ward_counts |>
  ptd_spc(
    date_field = month_end,
    value_field = perc_occ,
    facet_field = ward,
    improvement_direction = 'decrease',
    target = ptd_target('bulbasaur' = 0.8,
                        'charmander' = 0.8,
                        'squirtle' = 0.9)
  ) |>
  ptd_create_ggplot(percentage_y_axis = TRUE)


# explore correlation between bed occupancy and los
ward_data_joined <- ward_data_average |>
  mutate(discharge_month = ceiling_date(discharge_month, 'month') - days(1)) |>
  left_join(monthly_ward_counts,
    by = c("ward" = "ward", 
           "discharge_month" = "month_end")
  ) |>
  filter(median_av < 500)


# plot linear correlation across each plot 
ward_data_joined |>
  filter(median_av < 500) |>
  ggplot() +
  aes(
    x = median_av,
    y = perc_occ
  ) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  stat_cor(
    method = "pearson",
    p.accuracy = 0.001,
    r.accuracy = 0.01
  ) +
  facet_wrap(~ward,
    scales = "free_x"
  ) +
  theme_minimal()