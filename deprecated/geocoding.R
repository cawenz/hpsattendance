library(tidygeocoder)
library(tibble)
librar
geocode(
    tribble(
        ~street, ~cty, ~st,
        "123 Main Street", "Holyoke", "MA"
    ),
    method="osm"
)

addrmatch <- tibble(singlelineaddress = c("123 Main Street, Holyoke, MA"))

coded <- addrmatch %>%
geocode(address=singlelineaddress, method="osm", verbose=T)

st_as_sf(coded, coords=c("long", "lat"), crs=4326)
