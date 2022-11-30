library(tidygeocoder)
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# Load the necessary data
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# get the data on the school/grade etc. 
schgrades <- read_sheet("https://docs.google.com/spreadsheets/d/1HCQFnDyiBTu0c-0Ruv9yqJLL8PLuqtZZGG4CigOE1Ng/edit#gid=1573472191")

# get the geometries of all of the attendance zones and the holyoke border
zones <- 
    geojson_sf("data/attzones.geojson")%>%
    select(-file)%>%
    rename(zone=NAME)
# %>%
#     left_join(schgrades, by="zone")
holyokeborder <-
    geojson_sf("data/holyokeborder.geojson")

# These columns will need to be created with the user input in the app.

useraddr <- tibble(useraddress = c(
    paste0("320 Main Street", ", Holyoke, MA, 01040")
), 
grade="Grade 8"
)
usergeo <- useraddr %>%
    geocode(address=useraddress , method="osm", verbose=F)

usercoords <- usergeo %>%
    select(long, lat)%>%
    rename(usrLong=long,
           usrLat=lat
    )

user_sf <- st_as_sf(usergeo, coords=c("long","lat"), crs=4326)

zs <- st_transform(zones, 4326)%>%
    mutate(col.id=row_number())

user_zone <- st_intersects(user_sf, zs) %>%
    as.data.frame(Value)%>%
    bind_cols(useraddr)%>%
    left_join(zs, by=c("col.id"))%>%
    left_join(schgrades, by=c("zone", "grade"))%>%
    bind_cols(usercoords)%>%
    relocate(usrLong:usrLat, .after=useraddress)%>%
    relocate(usrLat, .after=useraddress)  
    

# Now make a dataframe to get the directions (This will have to be in a separate
# observe event)
direct_coords <- data.frame(
lon= c(usergeo$long, user_zone$long),
lat = c(usergeo$lat, user_zone$lat))

user_directions <- ors_directions(direct_coords, format="geojson", 
                                  api_key = orskey )
