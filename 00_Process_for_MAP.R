#figure out process for geocoding 

# Step 1: Get the geocode from the user input

usercode <- ors_geocode("32 River Terrace, Holyoke, MA, 01040", 
                        size=1
                        )

# make an sf object
usercodesf <- geojson_sf(usercode)

# make a geojson

usercodejson <- FROM_GeoJson(url_file_string = usercode)


# Step 2: Find the attendance zone
library(tidyverse)
zs <- st_transform(zones, 4326)%>%
    mutate(col.id=row_number())%>%
    select(-file)

match <- st_intersects(usercodesf, zs)


st_crs(usercodesf)
st_crs(zs)




FROM_GeoJson(url_file_string = usercode)

usercoord <- tibble(usercode$features[[1L]]$geometry$coordinates[1])


st_as_sf()


leaflet()%>%
    addProviderTiles(providers$CartoDB.Positron)%>%
    setView(lng=-72.630551, lat=42.208839, zoom=12.25)%>%
    addPolygons(data=zones, group="Holyoke Border", color="#042A2B",
                weight=1, opacity=0.9, fillOpacity = 0)%>%
    addPolygons(data=zones, group="Attendance Zones", 
                fillColor=zonecolors, 
                color="#84949A",
                fillOpacity = 0.2,weight=1)%>%
    addMarkers(data=subset(geosall,group=="school" & 
                               !grepl("High School|Metcalf", 
                                      description)),
               lat=~lat, lng=~lon,
               group="HPS Schools",
               icon=schicon,
               popup=~description
    )%>%
    addGeoJSON(user_directions, color=GeraniumRed, fillOpacity = 0, opacity = 1)%>%
    fitBBox(user_directions$bbox)

