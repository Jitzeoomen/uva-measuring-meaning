# Data "Wrangling"

# If you haven't already; 
# install.package('tidyverse')

# Im
library(tidyverse)

load("data/apple_mobility_data.Rdata")

# Show the first 6 rows of the list
head(apple_data)

# Subsetting the dataset (dplyr)
# Create a new list where only Brazil is included.
brazil_data <- filter(apple_data, region == "Brazil")

regions <- select(apple_data, region)

# Creates a tibble
transport_types <- count(apple_data, transportation_type)

alpha_order <- arrange(apple_data, region)

# Transform the dataset from a "wide" set to a "long" set.
long_apple_data <- gather(apple_data, key=day, value=mobility_data, `2020-01-13`:`2020-08-20`)

# To find the minimum 'mobility' one can either;
min(long_apple_data$mobility_data, na.rm = TRUE)

# Or, to follow the tutorial more closely;
arrange(long_apple_data, mobility_data)
