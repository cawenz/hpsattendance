#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# Start of the reactive chain from user input
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Create a reactiveValue "usergeo" that will get over-written by the eventReactive
# based on rungeo

ug <- reactiveVal(value=NULL, label="usergeocode")
# NEXT: use an observeEvent to set the value of ug OR display an error message

uz <- eventReactive(input$rungeo, {
    useraddr <- tibble(useraddress = c(
        paste0(input$address, ", Holyoke, MA, 01040")
    ), 
    grade=input$gradeselect
    )
    usergeo <- useraddr %>%
        geocode(address=useraddress , method="osm", verbose=F)
    
    usercoords <- usergeo %>%
        select(long, lat)%>%
        rename(usrLong=long,
               usrLat=lat
        )
    # Use the value of usercoords to determine the value of uz
    # If is.na(usercoords$usrLong) then ug == "ERROR"
    # Then I can use observeEvent to render 3 different versions of the next
    #
    user_sf <- st_as_sf(usergeo, coords=c("long","lat"), crs=4326)
    
    zs <- st_transform(zones, 4326)%>%
        mutate(col.id=row_number())
    user_zone <- st_intersects(user_sf, zs) %>%
        as.data.frame(Value)%>%
        bind_cols(useraddr)%>%
        left_join(zs, by=c("col.id"))%>%
        left_join(schgrades, by=c("zone", "grade"))%>%
        bind_cols(usercoords)%>%
        relocate(usrLong:usrLat, .after=useraddress)
    user_zone
}
)

observeEvent(input$rungeo, {
    direct_coords <- data.frame(
        lon= c(uz()$usrLong, uz()$long),
        lat = c(uz()$usrLat, uz()$lat))
    
    user_directions <- ors_directions(direct_coords, format="geojson", api_key=orskey)
    
    leafletProxy("zonemap", session)%>%
        clearGeoJSON()%>%
        clearMarkers()%>%
        addMarkers(data=schgeos,
                   lat=~lat, lng=~long,
                   group="HPS Schools",
                   icon=schicon,
                   popup=~school)%>%
        addMarkers(data= uz(),
                   lat=~usrLat, 
                   lng=~usrLong, 
                   icon=houseicon
        )%>%
        addGeoJSON(user_directions, fill=NULL)%>%
        fitBBox(user_directions$bbox)
    
})