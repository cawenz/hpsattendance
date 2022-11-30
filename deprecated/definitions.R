library(readr)
library(dplyr)
library(geojsonsf)
library(leaflet)
library(sf)
library(openrouteservice)
library(tibble)
library(tidygeocoder)

# Set open route service key
orskey <- ("5b3ce3597851110001cf6248afeeb9d139b3473fb179bd6160078feb")

# JUST USE THE api_key argument in ors_directions

#Load data
schgrades <- read_csv("data/schgrades.csv")

schgeos <- schgrades %>%
    group_by(school)%>%
    slice(1)

zones <- 
    geojson_sf("data/attzones.geojson")%>%
    select(-file)%>%
    rename(zone=NAME)

zs <- st_transform(zones, 4326)%>%
    mutate(col.id=row_number())

holyokeborder <-
    geojson_sf("data/holyokeborder.geojson")

# Make the icons
schicon <- makeIcon("www/school-dark.png",iconWidth=18,iconHeight=18)
houseicon <- makeIcon("www/red_home.png", iconWidth=18, iconHeight=18)

# Define the colors
USAFAblue      <- "#1B547E"
HoneyYellow    <- "#F6AE2D"
Jazzberry      <- "#950952"          
DartmouthGreen <- "#136E3B" 
GeraniumRed    <- "#DD282C"
MangoTango     <- "#ED7D3A"
# Purply Options
Ruby           <- "#D81E5B"       
Darkpurple     <- "#48233C"
RoyalPurple    <- "#724E91"

zonecolors <- c(GeraniumRed,  #DON
                MangoTango,   #ENW
                Jazzberry,     #KLY
                DartmouthGreen,#MCM
                HoneyYellow,   #MOR
                USAFAblue,     #LAW
                Darkpurple) 