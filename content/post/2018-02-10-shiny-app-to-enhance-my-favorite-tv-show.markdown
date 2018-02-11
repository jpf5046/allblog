---
title: Shiny App to Enhance my Favorite TV Show
author: John F
date: '2018-02-10'
slug: shiny-app-to-enhance-my-favorite-tv-show
categories:
  - Shiny
tags:
  - Shiny
---


## Enhancing Shark Tank -- Net Worth Compare Tool

At some point in an episode of Shark Tank I end up thinking: "Well Mark Cuban is worth 3 billion so this deal is the equivalent of me investing 15 dollars." I use estimates and never fully know what each Shark is worth. It also depends on how much the end user is worth. If my parents were to have the same thought, a 200 thousand dollar investment from Cuban, might be similar to a 30 dollar investment from my parents due to higher net worth. 

## Must be a better way 

To quickly allow myself to compare different deals while I'm watching Shark Tank I created the following Shiny app. It pulls Net Worth information from the Sharks and allows end user to put in his or her net worth. A quick calulcation is added and the Shiny app spits out what a 300 thousand dollar investment from Robert is compared to someones 450 thousand dollar home equity. 

# Code 

Here is the code I used to write the app:


```r
# I did some research finding out Shark's net worth and placed in a csv 
x <- read.csv('NetWorth of Famous People.csv')
list.of.people <- as.list(x$Name)

library(shiny)
library(dplyr)


ui <- fluidPage(
   
  # Title 
  titlePanel("Tool for watching Shark Tank"),
  
  # this is the sidebar on the left when you run 
  sidebarLayout(
    sidebarPanel(
      helpText( "Enter in the following information:"),
      
      # investment asking input            
      numericInput("investmentAmount",
                   h3("Seeking investment amount:"),
                   value = 75000),
      
      
      # selection of names
      selectInput("people",
                  label = "Which Shark is involved?",
                  choices = list.of.people),
      
      # user input            
      numericInput("userNetWorth",
                   h3("End User's Net Worth:"),
                   value = 10000),
      
      # action button
      actionButton("calculate", "Calculate")
              
            ), 
      
                  
    mainPanel(
      helpText("Press calculate to refresh."),
      textOutput("textsummary")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  # this allows the app to use the calculate button 
  
  vals <- reactiveValues()
  observe({
    vals$people <- input$people
    vals$investmentAmount <- input$investmentAmount
    vals$userNetWorth <- input$userNetWorth
    vals$personnetworth <- x %>% filter(Name == input$people) %>% pull(Networth)
    vals$comparison <-  round( vals$investmentAmount/ vals$personnetworth * vals$userNetWorth, 2)
    vals$networthprint <- x %>% filter(Name == input$people) %>% pull(Networthprint)
    
  })
  
observeEvent(

  input$calculate, {
    
    # I had a bit of an issue figuring this out, check out 
    # this stackoverflow post to get the details     
    # https://stackoverflow.com/questions/48244457/repetitive-output-for-shiny-rendertext
  output$textsummary <- renderText({
                          paste("Based on seeking an investment for  ", vals$investmentAmount, 
                                " from Shark ", vals$people, " (worth ", vals$networthprint,
                                ") would be the equilivent of someone worth ",
                                vals$userNetWorth, " investing $", vals$comparison)
                                   })
  
                    }
            )

}

# Run the application 
shinyApp(ui = ui, server = server)
```


# That's it

Okay so that's all the difficult stuff. Let's see th app in action!


 <div class="iframe_container">
 
 
  <iframe width="800" height="600" src="https://jpf5046.shinyapps.io/NetWorthCompareTool/" frameborder="1" allowfullscreen></iframe>
</div>
 


