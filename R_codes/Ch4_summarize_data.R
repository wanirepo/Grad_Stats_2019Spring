library(pander)
library(cowplot)

options(digits = 2)

# load the NHANES data library ----
library(NHANES)

# drop duplicated IDs within the NHANES dataset
NHANES <- 
  NHANES %>% 
  distinct(ID, .keep_all = TRUE)

# open the help page for the dataset
# help(NHANES)

# summarize physical activity data ----

PhysActive_table <- NHANES %>%
  dplyr::select(PhysActive) %>%
  group_by(PhysActive) %>%
  summarize(AbsoluteFrequency = n())

pander(PhysActive_table)

# summarize physical activity data after dropping NA values using drop_na() ----

NHANES %>%
  drop_na(PhysActive) %>%
  dplyr::select(PhysActive) %>%
  group_by(PhysActive) %>%
  summarize(AbsoluteFrequency = n()) %>%
  pander()

# compute relative frequency of physical activity categories ----

NHANES %>%
  drop_na(PhysActive) %>%
  dplyr::select(PhysActive) %>%
  group_by(PhysActive) %>%
  summarize(AbsoluteFrequency = n()) %>%
  mutate(RelativeFrequency = AbsoluteFrequency / sum(AbsoluteFrequency)) %>%
  pander()

# compute percentages for physical activity categories ----

PhysActive_table_filtered <- NHANES %>%
  drop_na(PhysActive) %>%
  dplyr::select(PhysActive) %>%
  group_by(PhysActive) %>%
  summarize(AbsoluteFrequency = n()) %>%
  mutate(
    RelativeFrequency = AbsoluteFrequency / sum(AbsoluteFrequency),
    Percentage = RelativeFrequency * 100
  )

pander(PhysActive_table_filtered)

# create summary table for relative frequency of different ----
# values of SleepHrsNight 

NHANES %>%
  drop_na(SleepHrsNight) %>%
  dplyr::select(SleepHrsNight) %>%
  group_by(SleepHrsNight) %>%
  summarize(AbsoluteFrequency = n()) %>%
  mutate(
    RelativeFrequency = AbsoluteFrequency / sum(AbsoluteFrequency),
    Percentage = RelativeFrequency * 100
  ) %>% 
  pander()

# setup breaks for sleep variable ----
scalex <- 
  scale_x_continuous(
    breaks = c(
      min(NHANES$SleepHrsNight, na.rm = TRUE):max(NHANES$SleepHrsNight, na.rm = TRUE)
    )
  ) # set the break points in the graph 

p1 <- SleepHrsNight_data_filtered %>% 
  ggplot(aes(SleepHrsNight)) +
  geom_histogram(binwidth = 1) +
  scalex

p2 <- SleepHrsNight_data_filtered %>% 
  ggplot(aes(SleepHrsNight)) +
  geom_histogram(aes(y = ..density..), binwidth = 1) +
  scalex

plot_grid(p1,p2)

# create cumulative frequency distribution of SleepHrsNight data ----

SleepHrsNight_cumulative <- 
  NHANES %>%
  drop_na(SleepHrsNight) %>%
  dplyr::select(SleepHrsNight) %>%
  group_by(SleepHrsNight) %>%
  summarize(AbsoluteFrequency = n()) %>%
  mutate(CumulativeFrequency = cumsum(AbsoluteFrequency))

pander(SleepHrsNight_cumulative)

SleepHrsNight_data_filtered <- 
  NHANES %>%
  drop_na(SleepHrsNight) %>%
  dplyr::select(SleepHrsNight)


# draw cumulative plots ----

p1 <- SleepHrsNight_cumulative %>% 
  ggplot(aes(SleepHrsNight, AbsoluteFrequency)) +
  geom_line(color = "red", size = 1.25) +
  geom_line(
    aes(SleepHrsNight, CumulativeFrequency), 
    color = "blue", 
    size = 1.25
  ) +
  scalex +
  labs(y = "Frequency")

SleepHrsNight_cumulative <- 
  NHANES %>%
  drop_na(SleepHrsNight) %>%
  dplyr::select(SleepHrsNight) %>%
  group_by(SleepHrsNight) %>%
  summarize(AbsoluteFrequency = n()) %>%
  mutate(
    RelativeFrequency = AbsoluteFrequency / sum(AbsoluteFrequency),
    CumulativeDensity = cumsum(RelativeFrequency)
  )

p2 <- SleepHrsNight_cumulative %>% 
  ggplot(aes(SleepHrsNight, RelativeFrequency)) +
  geom_line(color = "red", size = 1.25) +
  geom_line(
    aes(SleepHrsNight, CumulativeDensity), 
    color = "blue", 
    size = 1.25) +
  scalex +
  labs(
    y = "Proportion"
  )

plot_grid(p1,p2)


# take a slice of a few values from the full data frame ----
NHANES_adult <-
  NHANES %>% 
  drop_na(Age, Height) %>%
  dplyr::filter(Age > 17)

NHANES_adult %>%
  dplyr::select(Height) %>%
  slice(45:50) %>%
  pander()