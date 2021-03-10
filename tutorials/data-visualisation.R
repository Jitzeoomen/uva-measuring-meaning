# Data Visualisation

library(tidyverse)

apple_data <- read_csv("data/apple_mobility_data.csv")

long_apple_data <- gather(apple_data, key=day, value=mobility_data, `2020-01-13`:`2020-08-20`)

country_avg <- long_apple_data %>%
                filter(transportation_type == "walking") %>%
                group_by(country) %>%
                summarize(walking_avg=mean(mobility_data, na.rm = TRUE)) %>%
                filter(is.na(country))

ggplot(country_avg, aes(y = reorder(country, walking_avg), weight=walking_avg)) + 
    geom_bar(fill = "blue") +
    xlab("Relative rate of walking distance request") +
    ylab("Country") +
    theme_minimal()
