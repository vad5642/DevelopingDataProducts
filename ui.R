

library(shiny)
require(markdown)


# Define UI for slider demo application
shinyUI(navbarPage(
    
    #  Application title
    "Kelly Blue Book resale data for 2005 model year GM cars",
    
    tabPanel("Explore the Data",
    sidebarLayout(
        sidebarPanel(
            uiOutput("Mileage"),
            uiOutput("Price"),         
            uiOutput("Brand"), 
            uiOutput("Body"), 
            uiOutput("Cylinder"), 
            uiOutput("Doors"), 
            uiOutput("Cruise"),           
            uiOutput("Sound"), 
            uiOutput("Leather")
        ),
         
        # Show a table summarizing the values entered
        mainPanel(
           tableOutput("values"),
           textOutput("msg")
        )
    )
    ),
    
    tabPanel("About",
             mainPanel(
                 includeMarkdown("about.md")
             )
    )
))
