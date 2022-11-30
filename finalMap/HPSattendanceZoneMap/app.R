library(shiny)
library(imola)
library(bslib)
library(leaflet)
library(tidyverse)
library(shinyjs)
library(shinycssloaders)

source("definitions.R")

pagetitle <- "HPS Attendance Zone Locator"
# Define UI for application that draws a histogram
ui <- fluidPage(
    theme= bs_theme(
        bootswatch="lumen",
        bg="#FFFFFF",
        fg="#1b547e",
        primary="#1b547e",
        secondary="#dd282c",
        base_font = font_google("Noto Sans"),
        font_scale = 0.9
    ),
    useShinyjs(),
    # Create a "Grayed out style"
    tags$head(tags$style(
        '
    .grey-out {
        background-color: #eee;
        opacity: 0.2;
    }
    '
    )),
    # Application title
    titlePanel(windowTitle = pagetitle,
               title =
                   div(
                       img(
                           src = "hps_horizontal.png",
                           height = 55,
                           width = 300,
                           style = "margin:10px 10px 0px 0px"
                       ),
                           # h5(style="float:right;",
                           #    "Translate this page"),
                       span(style="float:right;",
                            HTML("<!DOCTYPE html><html lang='en-US'>
     <body>
     <div id='google_translate_element'>
     </div></div><script type='text/javascript'>function googleTranslateElementInit() 
     {new google.translate.TranslateElement({pageLanguage: 'en'}, 'google_translate_element');}
     </script>
     <script type='text/javascript' src='//translate.google.com/translate_a/element.js?cb=googleTranslateElementInit'></script>
     </body></html>")))
    ),
    fluidRow(
        column(width=12,
               div(style="width: 100%; height:20px;
                             background-color: #1b547e;
                             margin-bottom:10px;
                             margin-top:10px")
        )),
    mainPanel(
    flexPanel(
              direction="row",
              template="three-row",
    # Area 1 (far left top row) input address
                div(id="area1",
                h5(strong("Step 1:")),
                textInput("address", "Enter your street address", 
                placeholder = "Enter only street number and street name.",value=NULL),
                         conditionalPanel("input.address",
                         actionButton("usrgeocode", "Find your address")
                         )
                         ),
     # Area 2 (middle top row) use this to dynamically render a message
                div(id="area2",
              # Use this as an area to play with renderingUI
                uiOutput("UIarea2")
                )
                
    )
    ),
    fluidRow(
        column(width=12,
               div(style="width: 100%; height:20px; 
                             background-color: #1b547e; 
                             margin-bottom:10px; 
                             margin-top:10px")
        )),

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# Lower Panels
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              flexPanel(
                  direction="row",
                  template = "two-row",
                  align_items = "top",
                  # gap="10px",
                  div(shinycssloaders::withSpinner(leafletOutput("zonemap",
                                    # width="100%"
                                    height="50vh"
                  ))),
                  div(
                      id="resultsarea",
                      style="padding-left:20px;",
                      wellPanel(style="width:75%",
                      h4(strong("School Information"),
                         # style="text-align:center;"
                         ),
                      withSpinner(uiOutput("SchoolResults"))
                      )
                  )
              )
              )

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# 
#               SERVER SIDE
#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

server <- function(input, output, session) {
    
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
    ug <- eventReactive(input$usrgeocode, {
        useraddr <- tibble(useraddress = c(
            paste0(input$address, ", Holyoke, MA, 01040")
        )
        )
        usergeo <- useraddr %>%
            geocode(address=useraddress , method="osm", verbose=F)
        
        usercoords <- usergeo %>%
            select(long, lat,useraddress)%>%
            rename(usrLong=long,
                   usrLat=lat
            )
        usercoords
    })
    # Make Panel 2
    output$UIarea2 <- renderUI(
        if(input$usrgeocode & is.na(ug()$usrLat)){
         div(
             h4(strong("ERROR!", style="color:#DD282C")),
             hr(),
             p("Uh oh! We could not find your address. Please try again. 
                  You do not need to enter apartment numbers, or the city and state.
                  For example, if your address is:"),
             p(strong("1001 Main Street Apt 2, Holyoke, MA")),
             p("you ONLY should enter:"),
             p(strong("1001 Main Street"))
         )
        }
        else if(input$usrgeocode & !is.na(ug()$usrLat)){
            div(
                h5(strong("Step 2:")),
                selectInput("gradeselect",
                        "Select the grade of your child",
                        choices=c("Kindergarten", "Grade 1", "Grade 2",
                                  "Grade 3", "Grade 4","Grade 5", "Grade 6",
                                  "Grade 7", "Grade 8",
                                  selected=NULL)),
                actionButton("findschool", "Find your child's school")
            )
        }
        else{
            "Enter a valid address."
        }
    )
    
    observeEvent(input$usrgeocode && is.na(ug()$usrLat),{
        leafletProxy("zonemap", session)%>%
            clearPopups()%>%
            clearGeoJSON()%>%
            clearMarkers()%>%
            addMarkers(data=schgeos,
                       lat=~lat, lng=~long,
                       group="HPS Schools",
                       icon=schicon,
                       popup=~school)%>%
            setView(lng=-72.630551, lat=42.208839, zoom=12.25)
})
    
# Make an observeEvent to move the map to the user-selected address
    observeEvent(input$usrgeocode && !is.na(ug()$usrLat),{
        leafletProxy("zonemap", session)%>%
            clearGeoJSON()%>%
            clearPopups()%>%
            clearMarkers()%>%
            addMarkers(data=schgeos,
                       lat=~lat, lng=~long,
                       group="HPS Schools",
                       icon=schicon,
                       popup=~school)%>%
            addMarkers(
                data=ug(),
                lat=~usrLat, 
                lng=~usrLong,
                icon=houseicon
            )%>%
            setView(lng=ug()$usrLong, lat=ug()$usrLat, zoom=15)
    })
    

    # Trigger identifying school
    uz <- eventReactive(input$findschool, {
    
    userdefined <- ug() %>%
        mutate(grade=input$gradeselect)
    
    user_sf <- st_as_sf(userdefined, coords=c("usrLong","usrLat"), crs=4326)
    
    zs <- st_transform(zones, 4326)%>%
            mutate(col.id=row_number())
    
      user_zone <- st_intersects(user_sf, zs) %>%
        as.data.frame(Value)%>%
        bind_cols(userdefined)%>%
        left_join(zs, by=c("col.id"))%>%
        left_join(schgrades, by=c("zone", "grade"))%>%
        relocate(usrLong:usrLat, .after=useraddress)
      user_zone}
    )
    
    # Write an observeEvent to reset the map to the initial state if
    # input$geocode => 0 & there is an NA value of usr coordinates
    
    observeEvent(input$findschool, {
        direct_coords <- data.frame(
            lon= c(uz()$usrLong, uz()$long),
            lat = c(uz()$usrLat, uz()$lat))
        # shinyjs::disable("usrgeocode")
        
        user_directions <- ors_directions(direct_coords, format="geojson", api_key=orskey)
        
        uzpopup <- 
        
        leafletProxy("zonemap", session)%>%
            clearGeoJSON()%>%
            clearMarkers()%>%
            clearPopups()%>%
            addMarkers(
                        # data=subset(schgeos, zone != unique(uz()$zone)),
                       data=schgeos, 
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
            fitBounds(uz()$usrLong, uz()$usrLat, 
                      uz()$long,uz()$lat, 
                      options = list(padding = c(100,100))
                      )%>%
                addPopups(data=uz(), ~long, ~lat, ~school)
        
    })
    
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# Display the school zone information based on the input
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
    
# What are the conditions I need to deal with in the results panel    

# 1.The "find school" button has not been pressed -> empty
    
# 2. The "find school" button has been pressed and the user coordinates 
    # are NA (equivalent to the resetting of the map that 
    # happens when a new address is entered and it isn't a valid geocode. 
    
output$SchoolResults <- renderUI({
       div(
        hr(),
        div(
        img(src=uz()$img,   
            height = 100,
            width = 100,
            style = "margin:0px 10px 0px 0px; float:left"),
        h5(strong(uz()$school)),
        p(uz()$address),
        p(uz()$pone)
        ),
        div(style="padding:5px 0px 0px 0px",
        br(),
        p(strong("Grades served:"), uz()$grades),
        p(strong("Principal:"),uz()$principal),
        p(strong("Principal email:"),uz()$principal_email),
        hr(),
        p(a("Link to Department of Education School Profile", href=uz()$deseurl, 
            style="color:#DD282C")
          ), 
        p(a("Link to HPS Website", href=uz()$hpsurl,style="color:#DD282C"))
       )
       )
})
}

# Run the application 
shinyApp(ui = ui, server = server)
