library(shiny)
library(bslib)
library(imola)
pagetitle <- "HPS Attendance Zone Locator"
# Define UI for application that draws a histogram
shinyUI(fluidPage(
    theme= bs_theme(
        bootswatch="lumen",
        bg="#FFFFFF",
        fg="#1b547e",
        primary="#1b547e",
        secondary="#dd282c",
        base_font = font_google("Noto Sans"),
        font_scale = 0.9
    ),
    # Application title
    titlePanel(windowTitle = pagetitle,
               title =
                   div(
                       img(
                           src = "hps_horizontal.png",
                           height = 55,
                           width = 300,
                           style = "margin:10px 10px 0px 0px"
                       )
                   )),
    div(style="width: 100%; height:20px; background-color: #1b547e; margin-bottom:10px"),
    mainPanel(width=12,
              # Add a 
              fluidRow(
                  column(3,
                         h5("Step 1:"),
                         textInput("address", "Enter your street address", placeholder = "123 Main Street",value=NULL )),
                  column(4,
                         conditionalPanel(condition="input.address",
                                          h5(strong("Step 2:")),
                                          selectInput("gradeselect", 
                                                      "Select the grade of your child", 
                                                      choices=c("Kindergarten", "Grade 1", "Grade 2", 
                                                                "Grade 3", "Grade 4","Grade 5", "Grade 6", 
                                                                "Grade 7", "Grade 8", 
                                                                selected=NULL, multiple=F))
                         )),
                  column(3,
                         conditionalPanel(condition= "input.address",
                                          h5("Step 3:"),
                                          actionButton("rungeo", "Find school")
                         ))
              ),
              fluidRow(
                  column(width=12,
                         div(style="width: 100%; height:20px; background-color: #1b547e; margin-bottom:10px; margin-top:10px")
                  )),
              flexPanel(
                  direction="row", 
                  template = "two-row",
                  div(leafletOutput("zonemap",
                                    width="90vh", 
                                    height="50vh"
                  )),
                  div(
                      uiOutput("results")
                  )
              )
    )
)
)

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# Server side
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

library(shiny)
library(leaflet)
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    # Basic Map    
    output$zonemap <- renderLeaflet(
        leaflet()%>%
            addProviderTiles(providers$CartoDB.Positron)%>%
            setView(lng=-72.630551, lat=42.208839, zoom=12.25)%>%
            addPolygons(data=zones, group="Zone Borders", color="#042A2B",weight=1, opacity=0.9, fillOpacity = 0)%>%
            addPolygons(data=zones, group="Attendance Zones", fillColor=zonecolors, 
                        color="#84949A",
                        fillOpacity = 0.25,weight=1)%>%
            addMarkers(data=schgeos,
                       lat=~lat, lng=~long,
                       group="HPS Schools",
                       icon=schicon,
                       popup=~school)
    )
    
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
    
    output$schoolmessage <- renderUI(
        h3("Your School is....")
    )
}
)