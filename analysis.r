library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)

cars <- read_csv("data/fastest_german_cars_2005_2025.csv")

glimpse(cars)
head(cars)

cars_clean <- cars %>%
  filter(!is.na(top_speed_kmh), !is.na(zero_to_100_kmh_s), !is.na(power_hp)) %>%
  mutate(
    pwr_weight = power_hp / weight_kg * 1000,
    decade = case_when(
      year >= 2020 ~ "2020s",
      year >= 2010 ~ "2010s", 
      year >= 2000 ~ "2000s",
      TRUE ~ "Other"
    )
  )

print(paste("got", nrow(cars_clean), "cars"))

# fastest ones
fast <- cars_clean %>%
  arrange(desc(top_speed_kmh)) %>%
  select(maker, model, year, top_speed_kmh, power_hp) %>%
  head(10)
print(fast)

# quick acceleration 
quick <- cars_clean %>%
  arrange(zero_to_100_kmh_s) %>%
  select(maker, model, year, zero_to_100_kmh_s, power_hp) %>%
  head(8)
print(quick)

# by maker
makers <- cars_clean %>%
  group_by(maker) %>%
  summarise(
    n = n(),
    avg_speed = round(mean(top_speed_kmh)),
    max_speed = max(top_speed_kmh),
    avg_hp = round(mean(power_hp))
  ) %>%
  arrange(desc(avg_speed))

print(makers)

# drivetrain stuff
dt_stats <- cars_clean %>%
  group_by(drivetrain) %>%
  summarise(
    count = n(),
    avg_speed = round(mean(top_speed_kmh)),
    avg_accel = round(mean(zero_to_100_kmh_s), 1)
  )

print(dt_stats)


# correlations
cor1 = cor(cars_clean$power_hp, cars_clean$top_speed_kmh)
print(paste("power vs speed:", round(cor1, 2)))

cor2 = cor(cars_clean$power_hp, cars_clean$zero_to_100_kmh_s)  
print(paste("power vs accel:", round(cor2, 2)))

# power to weight leaders
pw_leaders <- cars_clean %>%
  arrange(desc(pwr_weight)) %>%
  select(maker, model, year, pwr_weight, power_hp, weight_kg) %>%
  head(6)
print(pw_leaders)

# decade trends
by_decade <- cars_clean %>%
  group_by(decade) %>%
  summarise(
    count = n(),
    avg_speed = round(mean(top_speed_kmh)),
    avg_hp = round(mean(power_hp))
  )

print(by_decade)

# electronic limiters
limited = sum(cars_clean$electronically_limited == "Yes", na.rm=T)
print(paste("limited cars:", limited))

# hybrids
cars_clean$hybrid = str_detect(cars_clean$engine, "hybrid")
hybrid_stats <- cars_clean %>%
  group_by(hybrid) %>%
  summarise(
    n = n(),
    avg_speed = round(mean(top_speed_kmh)),
    avg_hp = round(mean(power_hp))
  )
print(hybrid_stats)

write_csv(cars_clean, "data/german_cars_cleaned.csv")
