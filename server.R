

library(shiny)
library(caret)
library(dplyr)

data(cars)

branded <- cars
branded$Brand <- NA
branded$Body <- NA
txt <- "Init ..."

# Prepare data
branded <- mutate(branded, Brand = ifelse(Cadillac ==1, "Cadillac", 
                               ifelse(Buick == 1, "Buick", 
                                      ifelse(Chevy == 1, "Chevy",
                                             ifelse(Pontiac==1, "Pontiac",
                                                    ifelse(Saab==1, "Saab",
                                                           ifelse(Saturn == 1, "Saturn", NA)))))))

branded <- mutate(branded, Body = ifelse(convertible ==1, "convertible", 
                               ifelse(coupe == 1, "coupe", 
                                      ifelse(hatchback == 1, "hatchback",
                                             ifelse(sedan==1, "sedan",
                                                    ifelse(wagon==1, "wagon", NA))))))

drop <- names(cars)[8:18]
branded <- branded[,!(names(branded) %in% drop)]

yes_no = function(x) ifelse(x == 1, "Yes", "No")
branded <- mutate(branded, Cruise = yes_no(Cruise), 
                           Sound = yes_no(Sound),
                           Leather = yes_no(Leather))

branded$Body <- as.factor(branded$Body)
branded$Brand <- as.factor(branded$Brand)
branded$Cruise <- as.factor(branded$Cruise)
branded$Sound <- as.factor(branded$Sound)
branded$Leather <- as.factor(branded$Leather)
branded$Doors <- as.factor(branded$Doors)
branded$Cylinder <- as.factor(branded$Cylinder)

# Define server logic for slider examples
shinyServer(function(input, output) {
    
    
    max_price <- max(branded$Price)
    max_mileage <- max(branded$Mileage)
    min_price <- min(branded$Price)
    min_mileage <- min(branded$Mileage)
    
    output$Mileage <- renderUI({
        sliderInput("Mileage", "Mileage:",
            min=min_mileage, max=max_mileage, value = c(min_mileage, max_mileage))
    })
    
    output$Price <- renderUI({
        sliderInput("Price", "Price:",
                    min=min_price, max=max_price, value = c(min_price, max_price))
    })
    
    output$Brand <- renderUI({
        selectInput("Brand", 
                label = "Brand",
                choices = c("All", levels(branded$Brand)),
                selected = "All")
    })
    
    output$Body <- renderUI({
        checkboxGroupInput("Body", 
                       label = "Car body configuration",
                       choices = levels(branded$Body),
                       selected = "sedan")
    })
    
    output$Cylinder <- renderUI({
        checkboxGroupInput("Cylinder", "Cylinder", choices = levels(branded$Cylinder), 
                       inline=T, selected=levels(branded$Cylinder))
    })
    
    output$Doors <- renderUI({
        checkboxGroupInput("Doors", "Doors", levels(branded$Doors), 
                       inline=T, selected=levels(branded$Doors))
    })
    
    output$Cruise <- renderUI({
        checkboxGroupInput("Cruise", "Cruise", choices = c("Yes","No"), 
                       inline=T, selected=c("Yes","No"))
    })
    
    output$Sound <- renderUI({
        checkboxGroupInput("Sound", "Sound", choices = c("Yes","No"), 
                       inline=T, selected=c("Yes","No"))
    })
    
    output$Leather <- renderUI({
        checkboxGroupInput("Leather", "Leather", choices = c("Yes","No"), 
                       inline=T, selected=c("Yes","No"))
    })
    
    
    # Reactive expression to compose a data frame containing all of
    # the input values
    carValues <- reactive({
        
        if (length(input$Doors) == 0)
            doors <- levels(branded$Doors)
        else
            doors <- unlist(strsplit(input$Doors, split=" "))

        if (length(input$Sound) == 0)
            sound <- levels(branded$Sound)
        else
            sound <- unlist(strsplit(input$Sound, split=" "))
        
        if (length(input$Cylinder) == 0)
            cylinder <- levels(branded$Cylinder)
        else
            cylinder <- unlist(strsplit(input$Cylinder, split=" "))
        
        if (length(input$Leather) == 0)
            leather <- levels(branded$Leather)
        else
            leather <- unlist(strsplit(input$Leather, split=" "))
        
        if (length(input$Body) == 0)
            body <- levels(branded$Body)
        else
            body <- unlist(strsplit(input$Body, split=" "))
        
        if (length(input$Cruise) == 0)
            cruise <- levels(branded$Cruise)
        else
            cruise <- unlist(strsplit(input$Cruise, split=" "))

        if (length(input$Brand) != 0)
            if (input$Brand == "All")
               brand <- levels(branded$Brand) 
            else
               brand <- input$Brand
        else
            brand <- levels(branded$Brand)
        
        # Compose data frame
        tryCatch(
        filter(branded, Price >= input$Price[1] & 
                        Price <= input$Price[2] &
                        Mileage >= input$Mileage[1] &
                        Mileage <= input$Mileage[2] &
                        Doors %in% doors &
                        Brand %in% brand &
                        Cylinder %in% cylinder &
                        Sound %in% sound &
                        Leather %in% leather &
                        Cruise %in% cruise &
                        Body %in% body),
        error = function(e){}
        )
    }) 
    
    # Show the values using an HTML table
    output$values <- renderTable({
        
        tbl <- carValues()
        if (length(tbl) != 0)
        {
            if (nrow(tbl)!= 0)
            {
                output$msg <- renderText({ 
                    ""
                })
               tbl
            } else
            {
                output$msg <- renderText({ 
                    "Your search did not find any results"
                })
                return()
            }
        }
    })
    
    output$msg <- renderText({ 
        txt
    })
})


