library(plotly)
library(shiny)
library(ggplot2)
library(dplyr)


AllPrviousHeisman <- read.csv('AllPrviousHeisman.csv')
lookupTableHeisman <- read.csv('lookupTableHeisman.csv')


# user interface
ui <- fluidPage(
  titlePanel("Compare Between Previous Heisman Winners and Heisman Hopefuls"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
 
      selectizeInput('Heismancompare1',
                     label = "Select (multiple) Heisman Winners or Heisman Hopefuls",
                     choices = unique(AllPrviousHeisman$player),
                     multiple = T,
                     selected = c('Johnny Manziel', 'Cameron Newton', 'Baker Mayfield')),
      
      # br() element to introduce extra vertical spacing ----
      br(), 
      
      # conditional panel for Stat Sheet tab
      conditionalPanel(
        'input.tabs === "3rd Down Efficiency"', helpText("Third down Efficiency is calculated by number of times the player had the ball on a 3rd down play and got a first down divided by the number of times the player had the ball on a third down play regardless of outcome."),
                                      helpText(" For Example: if Matt Leinart hands the ball off to Reggie Bush, that is considered a 3rd down play for only Reggie Bush"),
                                      helpText(" But if Matt Leinart throws a pass to Reggie Bush on 3rd down, that play gets counted in both players stats."),
                                      helpText("TotalPlays3rdDown = Number of plays for that game were the player selected had an the ball (not including a handoff) on third down"),
                                      helpText("Numberof3rdDownConverstions = Number of 1st Downs made by player when having the ball on third down"),
                                      helpText("Player3rdDownEfficiency = Numberof3rdDownconversions / TotalPlays3rdDown")
                                                         
      )
      
     
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  id = "tabs",
                  tabPanel("Yards", htmltools::div(style = "display:inline-block", plotlyOutput("TotalReceivingYards", width = 700, height = 250)),
                                    htmltools::div(style = "display:inline-block", plotlyOutput("TotalRushingYards", width = 700, height = 250)),
                                    htmltools::div(style = "display:inline-block",plotlyOutput("TotalPassingYards", width = 700, height = 250)),
                                    htmltools::div(style = "display:inline-block", plotlyOutput("TotalReturnYards", width = 700, height = 250)),
                                    htmltools::div(style = "display:inline-block", plotlyOutput("TotalYards", width = 700, height = 250))),
                  tabPanel("Touchdowns",htmltools::div(style = "display:inline-block", plotlyOutput("TotalReceivingTDs", width = 700, height = 250)),
                                        htmltools::div(style = "display:inline-block",plotlyOutput("TotalRushingTDs", width = 700, height = 250)),
                                        htmltools::div(style = "display:inline-block",plotlyOutput("TotalPassingTDs", width = 700, height = 250)),
                                        htmltools::div(style = "display:inline-block",plotlyOutput("TotalReturnTDs", width = 700, height = 250)),
                                        htmltools::div(style = "display:inline-block",plotlyOutput("TotalTDs", width = 700, height = 250))),
                  tabPanel("3rd Down Efficiency",htmltools::div(style = "display:inline-block",plotlyOutput("ThirdDown", width = 800, height = 400)), 
                                                DT::dataTableOutput("ThirdDowntable"))
      )
      
    )
  )
)


server <- function(input, output, session) {
  
  
##### filtered Data 
  
  thirdtabtable <- c('player', 'week', 'homeTeam', 'awayTeam', 'TotalPlays3rdDown', 'Numberof3rdDownConversions', 'Player3rdDownEfficiency')
  
  filteredData <- reactive({
    AllPrviousHeisman[which(AllPrviousHeisman$player %in% input$Heismancompare1),which(names(AllPrviousHeisman) %in% thirdtabtable )]
  })

  
  
####################################### YARDS  
  # Total Receiving Yards
  output$TotalReceivingYards <- renderPlotly({ 
          ggplotly(
             AllPrviousHeisman %>%
               filter(player %in% input$Heismancompare1 ) %>%
               ggplot(aes(x = week, y = CumulativeReceivingYards, group = player, color = player, text = paste(awayTeam, " at ", homeTeam))) + geom_line() + geom_point() +
               labs(x = "Week", y = "Total Receiving Yards"))
 
  })
    
  
  # Total Rushing Yards
output$TotalRushingYards <- renderPlotly({   
     ggplotly(
       AllPrviousHeisman %>%
         filter(player %in% input$Heismancompare1 ) %>%
         ggplot(aes(x = week, y = CumulativeRushingYards, group = player, color = player, text = paste(awayTeam, " at ", homeTeam))) + geom_line() + geom_point() +
         labs(x = "Week", y = "Total Rushing Yards"))   
  
  })
             
  


# Total Passing Yards
output$TotalPassingYards <- renderPlotly({   
  ggplotly(
    AllPrviousHeisman %>%
      filter(player %in% input$Heismancompare1 ) %>%
      ggplot(aes(x = week, y = CumulativePassYards, group = player, color = player, text = paste(awayTeam, " at ", homeTeam))) + geom_line() + geom_point() +
      labs(x = "Week", y = "Total Passing Yards"))   
  
})


# Total Return Yards
output$TotalReturnYards <- renderPlotly({   
  ggplotly(
    AllPrviousHeisman %>%
      filter(player %in% input$Heismancompare1 ) %>%
      ggplot(aes(x = week, y = CumulativeReturnYards, group = player, color = player, text = paste(awayTeam, " at ", homeTeam))) + geom_line() + geom_point() +
      labs(x = "Week", y = "Total Return Yards"))   
  
})



# Total  Yards
output$TotalYards <- renderPlotly({   
  ggplotly(
    AllPrviousHeisman %>%
      filter(player %in% input$Heismancompare1 ) %>%
      ggplot(aes(x = week, y = CumulativeAllPurposeYards, group = player, color = player, text = paste(awayTeam, " at ", homeTeam))) + geom_line() + geom_point() +
      labs(x = "Week", y = "Total All Purpose Yards"))   
  
})
  
############################################### TOUCHDOWNS 


# Total Receiving TDS
output$TotalReceivingTDs <- renderPlotly({ 
  ggplotly(
    AllPrviousHeisman %>%
      filter(player %in% input$Heismancompare1 ) %>%
      ggplot(aes(x = week, y = CumulativeReceivingTDs, group = player, color = player, text = paste(awayTeam, " at ", homeTeam))) + geom_line() + geom_point() +
      labs(x = "Week", y = "Total Receiving TDs"))
  
})


# Total Rushing TDs
output$TotalRushingTDs <- renderPlotly({   
  ggplotly(
    AllPrviousHeisman %>%
      filter(player %in% input$Heismancompare1 ) %>%
      ggplot(aes(x = week, y = CumulativeRushingTDs, group = player, color = player, text = paste(awayTeam, " at ", homeTeam))) + geom_line() + geom_point() +
      labs(x = "Week", y = "Total Rushing TDs"))   
  
})




# Total Passing TDs
output$TotalPassingTDs <- renderPlotly({   
  ggplotly(
    AllPrviousHeisman %>%
      filter(player %in% input$Heismancompare1 ) %>%
      ggplot(aes(x = week, y = CumulativePassingTDs, group = player, color = player, text = paste(awayTeam, " at ", homeTeam))) + geom_line() + geom_point() +
      labs(x = "Week", y = "Total Passing TDs"))   
  
})


# Total Return TDs
output$TotalReturnTDs <- renderPlotly({   
  ggplotly(
    AllPrviousHeisman %>%
      filter(player %in% input$Heismancompare1 ) %>%
      ggplot(aes(x = week, y = CumulativeReturnTDs, group = player, color = player, text = paste(awayTeam, " at ", homeTeam))) + geom_line() + geom_point() +
      labs(x = "Week", y = "Total Return TDs"))   
  
})



# Total TDs
output$TotalTDs <- renderPlotly({   
  ggplotly(
    AllPrviousHeisman %>%
      filter(player %in% input$Heismancompare1 ) %>%
      ggplot(aes(x = week, y = CumulativeAllTouchdowns, group = player, color = player, text = paste(awayTeam, " at ", homeTeam))) + geom_line() + geom_point() +
      labs(x = "Week", y = "Total Touchdowns"))   
  
})
  

##################################### Third Down


output$ThirdDown <- renderPlotly({   
  ggplotly(
    AllPrviousHeisman %>%
      filter(player %in% input$Heismancompare1 ) %>%
      ggplot(aes(x = week, y = Player3rdDownEfficiency, group = player, color = player, text = paste(awayTeam, " at ", homeTeam))) + geom_line() + geom_point() +
      labs(x = "Week", y = "Game by Game Third Down Conversion Efficiency"))  
  
})


output$ThirdDowntable <- DT::renderDataTable({
  DT::datatable(filteredData(), options = list(pageLength = 12))
})


  
}

shinyApp(ui, server)