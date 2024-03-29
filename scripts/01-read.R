# query details

# daily alert counts where >= 1
# https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?startMonth=january&graphOnly=true&geography=15919&geography=33622&geography=33177&datasource=va_hosp&startDate=5May2023&medicalGroupingSystem=essencesyndromes&userId=4887&multiStratVal=ccddCategory&alertCountFilterOperator=gte&stratVal=hospitalGrouping&endDate=5May2023&percentParam=noPercent&admissionTypeCategory=e&graphOptions=multiplesmall&aqtTarget=TimeSeries&geographySystem=hospital&detector=probrepswitch&alertCountFilter=1&removeZeroSeries=true&timeResolution=daily&hasBeenE=1

# package libraries 
library(here)
library(tidyverse)
library(Rnssp)
library(janitor)
library(lubridate)

# set Rnnsp credentials
myProfile <- Credentials$new(
  username = askme("Enter your username: "),
  password = askme()
)

# function to query the essence api for counts
query_fnct <- function(x){
  # set url 
  url <- x
  
  # query ESSENCE 
  api_data <- get_api_data(url)
  
  # check variable names 
  names(api_data)
  
  # create a tidy data table 
  api_data_cts <- api_data$timeSeriesData %>%
    clean_names() %>%
    as_tibble() %>%
    mutate(
      date = ymd(date)
    )
}

df <- query_fnct(x = "https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?startMonth=january&graphOnly=true&geography=15919&geography=33622&geography=33177&datasource=va_hosp&startDate=5May2022&medicalGroupingSystem=essencesyndromes&userId=4887&multiStratVal=ccddCategory&alertCountFilterOperator=gte&stratVal=hospitalGrouping&endDate=5Aug2022&percentParam=noPercent&admissionTypeCategory=e&graphOptions=multiplesmall&aqtTarget=TimeSeries&geographySystem=hospital&detector=probrepswitch&alertCountFilter=1&removeZeroSeries=true&timeResolution=daily&hasBeenE=1")

glimpse(df)

list_hosp <- unique(df$hospital_grouping_display)
list_ccdd <- unique(df$ccdd_category_display)

sample_fnct <- function(x){
  df %>%
    filter(hospital_grouping_display == as.character(!!sym(x)))
}

df %>%
  map(
    .x = list_hosp,
    .f = ~sample_fnct(.x)
)
 
df %>%
  filter(hospital_grouping_display == "AZ-Banner Page Hospital") %>%
  ggplot(mapping = aes(
    x = date,
    y = count
  )) +
  geom_line() +
  facet_wrap(
    facets = ~ccdd_category_display,
    ncol = 4
  )

df %>%
  filter(hospital_grouping_display == "AZ-Flagstaff Medical Center") %>%
  ggplot(mapping = aes(
    x = date,
    y = count
  )) +
  geom_line() +
  geom_vline(
    xintercept = as.Date("2022-06-12"),
    color = "red",
    size = 2,
    alpha = 3/7
  ) +
  facet_wrap(
    facets = ~ccdd_category_display
  )

df_counts <- df %>%
  filter(hospital_grouping_display == "AZ-Flagstaff Medical Center") %>%
  group_by(date) %>%
  summarise(
    count_total = sum(count)
  )

df %>%
  filter(hospital_grouping_display == "AZ-Flagstaff Medical Center") %>%
  left_join(
    y = df_counts
  ) %>%
  mutate(
    percent_total = count / count_total
  ) %>%
  glimpse()
