library(plotly)
library(shiny)
library(ggplot2)
library(dplyr)

# user interface
ui <- fluidPage(
  titlePanel("Heisman-to-Heisman hopeful Comparison Tool"),
  mainPanel(
    htmltools::div(style = "display:inline-block", plotlyOutput("x", width = 800, height = 250)),
    wellPanel(
      style = "display:inline-block; vertical-align:top;", 
      
      # create a dropdown that let's you selected all the different heisman ppl 
      selectizeInput('Heismancompare1',
                     label = "Select (multiple) Heisman or Heisman Hopeful",
                     choices = unique(AllPrviousHeisman$player),
                     multiple = T,
                     selected = c('Reggie Bush', 'Cameron Newton')),
      # # selectizeInput('Heismancompare2', 
      # #                label = "Select 2nd  Heisman or Heisman Hopeful", 
      # #                choices = unique(AllPrviousHeisman$player), 
      # #                multiple = F,
      # #                selected = 'Cameron Newton'), 
      # selectizeInput('whatvaluetohaveontheY',
      #                label = "Pick a Comparison Measure", 
      #                choices = unique(AllPrviousHeisman2$Measurementtext),
      #                multiple= F,
      #                selected = "RunningTotal_HeismanRushingYards")
      
      
      # uiOutput("gameorcumulative"),#add selectinput boxs
      uiOutput("yardsorTDs"), 
      uiOutput("playType"), 
      uiOutput("ComparisonMeasure")
    )
    
  )
)


server <- function(input, output, session) {
  
  
  
  # output$gameorcumulative = renderUI({
  #           
  #       radioButtons(
  #         inputId = "gamebygameorcumulative",
  #         label = "Select Visualization Preference",
  #         choices = c("Game by Game", "Running Total"),
  #         selected = "Game by Game"
  #       )
  #   
  # })
  
  
  output$yardsorTDs = renderUI({
    
    radioButtons(
      inputId = "yardsorTDs",
      label = "Select Yards or TDs",
      choices = c("Yards", "TDs"),
      selected = "Yards"
    )
    
  })
  
  
  output$playType = renderUI({
    
    radioButtons(
      inputId = "playType",
      label = "Select play type",
      choices = c("Passing", "Rushing", "Special Teams", "Receiving", "Total"),
      selected = "Rushing"
    )
    
  })
  
  output$ComparisonMeasure = renderUI({
    
    dataavailable = lookupTableHeisman[
      lookupTableHeisman$Type==input$playType 
      & lookupTableHeisman$Yards.or.TDs.or.attempts==input$yardsorTDs
      , "Comparison.Measure"]
    
    
    
    selectInput(inputId ='ComparisonMeasure',
                label = "Cumulative or Game by Game",
                choices = (dataavailable),
                selected = "GameRushingYards")
    
  })
  
  
  
  
  
  
  # the 'x' graph that shows up first 
  output$x <- renderPlotly({
    if(is.null(input$ComparisonMeasure) || is.null(input$playType) || is.null(input$yardsorTDs)){return()}
    print(
      
      
      ggplotly(
        AllPrviousHeisman %>%
          filter(player %in% input$Heismancompare1 ) %>%
          ggplot(aes(x = week, y = get(input$ComparisonMeasure), group = player, color = player, text = paste(awayTeam, " at ", homeTeam))) + geom_line() + geom_point() +
          labs(x = "Week", y = "Comparison Measure"))
    )
    
    
    
    
    
    # AllPrviousHeisman %>%
    #  group_by(player, week, homeTeam, awayTeam) %>%
    #   filter(player %in% input$Heismancompare1 ) %>%
    #   plot_ly(  x = ~week, y = ~get(input$whatvaluetohaveontheY), group = ~player,  color = ~player, type='scatter',
    #             mode = "lines+markers", text = ~paste(awayTeam," at", homeTeam )) %>%
    #   add_trace(data = AllPrviousHeisman2, x = ~week, y = ~get(input$whatvaluetohaveontheY), group = ~player, color = ~player, mode = 'line')
    
    
    # AllPrviousHeisman %>%
    #   group_by(player, week, homeTeam, awayTeam) %>%
    #   filter(player %in% input$Heismancompare1 ) %>%
    #   plot_ly( ) %>%
    #   add_trace(x = ~week, y = ~get(input$whatvaluetohaveontheY),group = ~player,  color = ~player, type='scatter', mode='marker' ) %>%
    #   add_trace(x = week, y = get(input$whatvaluetohaveontheY),group = player,  color = player, type='scatter', mode='line' )
    
  })
}

shinyApp(ui, server)