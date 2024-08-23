# Cleaning file for STAT 454

# library
library(dplyr)
library(tidyverse)
library(sf)
library(readxl)
library(CARBayes)
library(RColorBrewer)
library(spdep)
library(GGally)
library(coda)
library(mapview)
library(leafsync)
library(INLA)
library(ggpubr)

select <- dplyr::select
# import data 
code_book <- read_excel("Gecon40_post_final.xls", sheet = "definitions")
used_def <- code_book[c(2,3,4,5,7,8,13,14,20,25,30,34,35,41,44,45),]
used_var <- used_def$Variable

data_demo <- read_excel("Gecon40_post_final.xls") 

us_data <- data_demo %>% filter(COUNTRY == "United States") %>% select(used_var) %>% filter(POPGPW_2005_40 !=0, MER2005_40 !=0) %>% 
  mutate(dis_to_water = pmin(D1,D2,DIS_LAKE, DIS_MAJOR_RIVER, na.rm = TRUE)) %>% mutate(log(POPGPW_2005_40),log(MER2005_40)) %>% 
  select(-c(D1,D2,DIS_LAKE, DIS_MAJOR_RIVER)) 

full_data <- data_demo %>% select(used_var) %>% na.omit()


##### Check linearity

linear_graph <- function(x){
  us_data %>%  ggplot(aes(x = {{x}}, y = `log(MER2005_40)`))+ 
    geom_point()+
    geom_smooth()+
    geom_smooth(method = "lm", color = "red")
}

linear_graph(dis_to_water) # closest Distance to coast , major navigable lake/ river (km) 
linear_graph(`log(POPGPW_2005_40)`) # very strong positive trend, log(population) vs log(GCT)
linear_graph(D3)
linear_graph(TEMPAV_8008)
# non_linear, spline is better but not for car model (linear model)
linear_graph(PRECAVNEW80_08)+
  geom_smooth(method = "lm", color = "green",formula = y~poly(x,2))



#### sf data for us
points_us_sf <- us_data %>% mutate(lat = LAT, long = LONGITUDE) %>% 
  st_as_sf(coords = c("LONGITUDE", "LAT"), crs = 4326)

expanded_us_grid <- st_make_grid(points_us_sf, cellsize = c(1, 1))

index_us <- which(lengths(st_intersects(expanded_us_grid, points_us_sf)) > 0)

# Filter out only the intersecting grid cells
fishnet_us <- expanded_us_grid[index_us]

# Convert the grid cells to sf, create unique IDs, and join with points
joined_us_data <- fishnet_us %>%
  st_as_sf() %>%
  mutate(grid_id = row_number()) %>%
  st_join(points_us_sf) %>% 
  na.omit()

simple_data_A <- joined_us_data %>% filter(lat > 0 & lat < 50  & long > -130 & long < -110) %>% na.omit()

