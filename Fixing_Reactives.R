

useraddr <- tibble(useraddress = c(
    paste0("57 Suffolk Street", ", Holyoke, MA, 01040")
), 
grade="Kindergarten"
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
    relocate(usrLong:usrLat, .after=useraddress)
user_zone