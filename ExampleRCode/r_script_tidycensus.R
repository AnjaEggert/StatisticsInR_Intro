# Load the tidycensus package into your R session
library("tidycensus")
library("tidyverse")

# Define your Census API key and set it with census_api_key()
api_key <- "2c61c8e46075c14baaf5ebc3f84ccefb68ed543b"
census_api_key(api_key)

# Check your API key
Sys.getenv("CENSUS_API_KEY")

# get_decennial, which grants access to the 1990, 2000, and 2010 decennial US Census APIs
m90 <- get_decennial(geography = "state", variables = "H043A001", year = 1990)
head(m90)

# The function returns a tibble with four columns by default:
# GEOID, which is an identifier for the geographical unit associated with the row;
# NAME, which is a descriptive name of the geographical unit;
# variable, which is the Census variable represented in the row;
# and value, which is the value of the variable for that unit.
# By default, tidycensus functions return tidy data frames in which rows represent
# unit-variable combinations;
# for a wide data frame with Census variable names in the columns,
# set output = "wide" in the function call.

m90 %>%
  ggplot(aes(x = value, y = reorder(NAME, value))) + 
  geom_point()

# Searching for variables
v15 <- load_variables(2016, "acs5", cache = TRUE)

View(v15)

# Working with ACS data
vt <- get_acs(geography = "county", 
              variables = c(medincome = "B19013_001"), 
              state = "VT")

vt

vt %>%
  mutate(NAME = gsub(" County, Vermont", "", NAME)) %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Household income by county in Vermont",
       subtitle = "2012-2016 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)")

# Spatial data in tidycensus
options(tigris_use_cache = TRUE)

orange <- get_acs(state = "CA", county = "Orange", geography = "tract", 
                  variables = "B19013_001", geometry = TRUE)

head(orange)

library(viridis)

orange %>%
  ggplot(aes(fill = estimate, color = estimate)) + 
  geom_sf() + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis(option = "magma") + 
  scale_color_viridis(option = "magma")


# Faceted mapping
racevars <- c(White = "P005003", 
              Black = "P005004", 
              Asian = "P005006", 
              Hispanic = "P004003")

harris <- get_decennial(geography = "tract", variables = racevars, 
                        state = "TX", county = "Harris County", geometry = TRUE,
                        summary_var = "P001001") 

head(harris)

harris %>%
  mutate(pct = 100 * (value / summary_value)) %>%
  ggplot(aes(fill = pct, color = pct)) +
  facet_wrap(~variable) +
  geom_sf() +
  coord_sf(crs = 26915) + 
  scale_fill_viridis() +
  scale_color_viridis()

# Detailed shoreline mapping with tidycensus

library(mapview)
options(tigris_use_cache = TRUE)

ny <- get_acs(geography = "tract", 
              variables = "B19013_001", 
              state = "NY", 
              county = "New York", 
              geometry = TRUE)

mapview(ny, zcol = "estimate", legend = TRUE)

ny2 <- get_acs(geography = "tract", 
               variables = "B19013_001", 
               state = "NY", 
               county = "New York", 
               geometry = TRUE, 
               cb = FALSE)

library(sf)
library(tigris)

st_erase <- function(x, y) {
  st_difference(x, st_union(st_combine(y)))
}

ny_water <- area_water("NY", "New York", class = "sf")

ny_erase <- st_erase(ny2, ny_water)

mapview(ny_erase, zcol = "estimate", legend = TRUE)

# Writing to shapefiles
library(sf)
st_write(orange, "orange.shp")
