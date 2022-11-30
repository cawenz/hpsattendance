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