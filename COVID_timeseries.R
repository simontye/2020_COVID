#########################################
### Time-series of COVID-19 cases in the United States
### 2020.03.27
### Simon Tye
#########################################

# Reset environment
rm(list = ls())

# Install packages
#install.packages(c("readr", "tidyverse", "lubridate", "ggplot2", "gganimate", "sf", "rnaturalearth", "housingData", "scales"))

# Load packages
library(readr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(gganimate)
library(maps)
library(sf)
library(rnaturalearth)
library(housingData)
library(scales)
library(spData)

#########################################
### COVID-19 data

# Download COVID-19 data from the New York Times
covid_nyt = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

# Load COVID-19 data
covid <- read_csv(url(covid_nyt))

# Create column with state and county data
covid$ID <- tolower(with(covid, paste0(state, ",", county)))

# Remove periods from county names
covid$ID <- gsub("[.]", "", covid$ID)

#########################################
### Census data

# Download US Census Bureau data
pop_census = "https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv"

# Load census data
census <- read_csv(url(pop_census))

# Create fips column from state and county data
census$fips <- with(census, paste0(STATE, COUNTY))

# Subset 2019 census data
census <- subset(census, select = c("STNAME", "CTYNAME", "POPESTIMATE2019", "fips"))

# Reformat county names to match other datasets
census <- separate(data = census, col = CTYNAME, into = c("county", "name"), sep = "County")
census <- separate(data = census, col = county, into = c("county", "name"), sep = "Parish")

# Change county name with tilde
census$county[1835] <- "Dona Ana"

# Create column with state and county data
census$ID <- tolower(with(census, paste0(STNAME, ",", county)))

# Remove trailing whitespace after county names
census$ID <- trimws(census$ID, which = c("right"))

# Remove periods from county names
census$ID <- gsub("[.]", "", census$ID)

#########################################
### Map data

# Load US counties map data
counties <- map_data("county")
counties2 <- st_as_sf(map("county", plot = FALSE, fill = TRUE))

# Load centroids of US counties data
counties.cent <- geoCounty
counties.cent$fips <- as.character(counties.cent$fips)

# Load US state map data
#states <- map_data("state")
states <- us_states

# Load US map data
world <- ne_countries(scale = "medium", returnclass = "sf")

#########################################
### Merge dataframes

# Merge COVID-19 and county centroid data
covid.counties <- full_join(x = covid, y = counties.cent, by = "fips")

# Merge COVID-19, county, and census data
covid.final <- full_join(x = covid.counties, y = census, by = "fips")

# Subset final dataframe
covid.final <- subset(covid.final, select = c("date", "fips", "cases", "lon", "lat", "POPESTIMATE2019"))

# Create column for cases by county population size 
covid.final$cases_pop <- covid.final$cases / covid.final$POPESTIMATE2019

# Remove old dataframes
rm(census, counties, counties.cent, counties2, covid, covid.counties, world)

#########################################
### Map of COVID-19 cases by county in the United States
map.covid.cases <- ggplot() +
  geom_sf(data = states, fill = "rosybrown1", color = "white", size = 3, alpha = 0.8, mapping = aes(geometry = geometry)) + 
  geom_point(data = covid.final, shape = 16, alpha = 0.4, color = "red3", aes(x = lon, y = lat, size = cases)) +
  scale_size(range = c(6, 150)) +
  coord_sf(xlim = c(-128, -65), ylim = c(23, 52), expand = FALSE) +
  theme(plot.title = element_text(hjust = 0.5, face =, "bold", size = 120, margin = margin(0, 0, 15, 0), color = "black", family = "Times"),
        plot.subtitle = element_text(hjust = 0.5, size = 110, margin = margin(0, 0, -5, 0), color = "black", family = "Times"),
        plot.background = element_rect(fill = 'white', color = 'white'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "white", fill = "NA"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none") +
  labs(title = "Confirmed cases of COVID-19 by county in the United States", subtitle = "{current_frame}") +
  transition_manual(date, cumulative = TRUE) +
  ease_aes('linear') +
  enter_fade() +
  exit_fade()

# Set animation settings
animate(map.covid.cases, duration = 15, fps = 5, end_pause = 10, height = 2100, width = 4000)

# Save animation
anim_save("covid_county_20200327.gif", animation = last_animation())

# Map of COVID-19 cases by county population size in the United States
map.covid.pop <- ggplot() +
  geom_sf(data = states, fill = "rosybrown1", color = "white", size = 3, alpha = 0.8, mapping = aes(geometry = geometry)) + 
  geom_point(data = covid.final, shape = 16, alpha = 0.4, color = "red3", aes(x = lon, y = lat, size = cases_pop)) +
  scale_size(range = c(3, 40)) +
  coord_sf(xlim = c(-128, -65), ylim = c(23, 52), expand = FALSE) +
  theme(plot.title = element_text(hjust = 0.5, face =, "bold", size = 120, margin = margin(0, 0, 15, 0), color = "black", family = "Times"),
        plot.subtitle = element_text(hjust = 0.5, size = 110, margin = margin(0, 0, -5, 0), color = "black", family = "Times"),
        plot.background = element_rect(fill = 'white', color = 'white'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "white", fill = "NA"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none") +
  labs(title = "Confirmed cases of COVID-19 by county population size in the United States", subtitle = "{current_frame}\n Data from the New York Times.") +
  transition_manual(date, cumulative = TRUE) +
  ease_aes('linear') +
  enter_fade() +
  exit_fade()

# Set animation settings
animate(map.covid.pop, duration = 15, fps = 5, end_pause = 10, height = 2100, width = 4000)

# Save animation
anim_save("covid_population_20200328.gif", animation = last_animation())


