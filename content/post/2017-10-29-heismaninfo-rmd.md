---
title: Heisman 
author: ~
date: '2017-10-29'
slug: heismaninfo-rmd
categories: []
tags: []
---

# Creating a Comparison Tool Between years for Heisman race 

I wanted to see how different Heisman contenders compared to one another at different points in the season. In order to do that you need data. I found data from this reddit post: https://www.reddit.com/r/CFBAnalysis/comments/6htfc6/play_by_play_data_dump_20012016/ and then started to manipulate into some interesting use cases. 

I load the entire dataset and then run the following function. The function breaks down Touchdowns, Attemps, Yards, Third Down conversion rates, you name it, then filters everything into a Shiny web App below. 



# The Function 

```{r setup, eval=FALSE}
library(readr)
library(magrittr)
library(dplyr)

files = list.files(pattern = "*.csv")

# Loads every year from 2001 to 2017
tbl = lapply(files, read_csv) %>% bind_rows()

# add a tag if it was a touchdown or not 

tbl <- tbl %>%
        mutate(
          TouchdownY_N = ifelse(grepl(paste('.*',"touchdown", '.*', sep=""), description) | grepl(paste('.*',"TD", '.*', sep=""),description) |
                                  grepl(paste('.*',"TOUCHDOWN", '.*', sep=""),description) , 1, 0)
          )

# add a tag if it was a 3rd down 

tbl <- tbl %>%
        mutate(
          ThirdDownY_N = ifelse( tbl$down == 3, 1,0) 
          )


# add a tag if it was a 1st down 

tbl <- tbl %>%
        mutate(
          Resultedin1stDown = ifelse(grepl(paste('.*',"1ST DOWN", '.*', sep=""), description) | grepl(paste('.*',"first down", '.*', sep=""),description) |
                                  grepl(paste('.*',"1ST down", '.*', sep=""),description) , 1, 0)
          )


```


```{r, Heisman Data Function, eval = FALSE }

Heisman.data.prep <- function(fun.team, fun.heisman,  fun.year, fun.rush.or.run){
  
  
  
  
                 tbl %>%
                      filter(
                        (offenseTeam == fun.team | defenseTeam == fun.team),
                        year == fun.year
                      ) %>%
                      mutate(
                       
                        
                        player = fun.heisman,
                         Year = fun.year,
                        
                         # index for game number
                        game = match(gameId, unique(gameId)),
              
                        # Pass Attempt by Heisman, (Y/N)
                        HeismanThrew = ifelse( grepl(
                        paste('.*',fun.heisman, " pass", '.*', sep=""),description), 1, 0),
                        
                        # Rush Attempt by Heisman, (Y/N)
                        HeismanRush = ifelse( grepl(
                        paste('.*',fun.heisman, " ", fun.rush.or.run, '.*', sep=""),description), 1, 0),
                       
                        # Return Attempt by Heisman
                        HeismanReturn = ifelse( grepl(
                        paste('.*',fun.heisman, " return", '.*', sep=""),description), 1, 0), 
                        
                        # Catch made by Heisman
                        HeismanReception = ifelse( grepl(
                        paste('.*', "complete to ",fun.heisman, '.*', sep=""),description), 1, 0),
                        
                        # Tag if Heisman got sacked
                        HeismanSacked = ifelse( grepl(
                        paste('.*',fun.heisman, " sacked", '.*', sep=""),description), 1, 0),
                        
                        # Tag if play got first down
                        resultinplay1stdown = ifelse( grepl(
                        paste('.*',"1ST down", '.*', sep=""),description), 1, 0), 
                        
                        # Tag if play involved Heisman
                        involvedHeisman = ifelse( grepl(
                          paste('.*',fun.heisman, '.*', sep=""),description), 1, 0)) %>% 
                      
                    mutate(  
                        # Passing Touchdown nvolving Heisman
                        PassingTouchdownbyHeisman = ifelse(
                          HeismanThrew == 1 & TouchdownY_N ==1, 1,0
                        ), 
                        
                        # Rushing Touchdown by Heisman
                        RushingTouchdownbyHeisman = ifelse(
                          HeismanRush ==1 & TouchdownY_N ==1 , 1, 0
                        ), 
                        
                        # REceiving Touchdown by Heisman 
                        ReceivingTouchdownbyHeisman = ifelse(
                        HeismanReception ==1 & TouchdownY_N ==1,1,0
                        ),
                        
                        # Return Touchdown by Heisman
                        ReturnTouchdownbyHeisman = ifelse(
                          HeismanReturn == 1 & TouchdownY_N ==1, 1, 0
                        ), 
                        
                        # third down and Heisman ha ball in hands 
                        
                        ThirdDownHeismanInvolved = ifelse(
                          ThirdDownY_N == 1 & involvedHeisman == 1, 1, 0
                        ), 
                        
                        # third down involved heisman and converted 
                        
                        ThirdDownSuccesswithHeisman = ifelse(
                          ThirdDownY_N == 1 & involvedHeisman == 1 & Resultedin1stDown == 1, 1, 0
                        )
                        
                        
                      ) %>%
                      group_by(player, year, week, homeTeam, awayTeam, game) %>%
                      summarise(
                        # Game passes
                        GamePasses = sum(HeismanThrew), 
                        
                        # Game rushes
                        GameRushes = sum(HeismanRush), 
                        
                        # Catches by Heisman
                        GameCatches = sum(HeismanReception), 
                        
                        # Times got Sacked
                        GameSacked = sum(HeismanSacked), 
                        
                        # Game Yards Passing 
                        GamePassingYards = sum(yardsGained[HeismanThrew==1]), 
                        
                        # Game Passing TD
                        GamePassingTDS = sum(PassingTouchdownbyHeisman),
                        
                        # Game Yards Rushing 
                        GameRushingYards = sum(yardsGained[HeismanRush==1]), 
                        
                        # Game Rushing TD
                        RushingTD = sum(RushingTouchdownbyHeisman), 
                        
                        # Game Yards Returning 
                        GameReturnYards = sum(yardsGained[HeismanReturn==1]), 
                        
                        # Game Return TDs
                        GameReturnTDS = sum(ReturnTouchdownbyHeisman), 
                        
                        # Game Receiving Yards
                        GameReceivingYards = sum(yardsGained[HeismanReception==1]), 
                        
                        # Game receiving TDS
                        RecivingTDS = sum(ReceivingTouchdownbyHeisman),
                        
                        # Game All Purpose Yards 
                        AllPurposeYards = sum(GameRushingYards) + sum(GamePassingYards) + sum(GameReturnYards) + sum(GameReceivingYards), 
                        
                        # Game Touchdowns 
                        GameTouchdowns = sum(RecivingTDS+ GameReturnTDS +RushingTD + GamePassingTDS),
                        
                        # Ball in Hand on third down
                        TotalPlays3rdDown = sum(ThirdDownHeismanInvolved), 
                        
                        # Had ball and was successful on third down 
                        Numberof3rdDownConversions = sum(ThirdDownSuccesswithHeisman),
                        
                        # Third Down success 
                        Player3rdDownEfficiency = format(sum(ThirdDownSuccesswithHeisman)/sum(ThirdDownHeismanInvolved),2)
                      )
  
  
  
}

```


# Prep for the Shiny Web App


```{r, heisman function test, eval = FALSE}

EricCrouchdata <- Heisman.data.prep("Nebraska", "Eric Crouch \\(NEB\\)", 2001, "rush" )

JasonWhiteData <- Heisman.data.prep("Oklahoma", "Jason White \\(OKLA\\)", 2003, "rush" )

MattLeinartdata <- Heisman.data.prep("USC", "Matt Leinart \\(USC\\)", 2004, "rush" )

ReggieBushData <- Heisman.data.prep("USC", "Reggie Bush", 2005, "rush")

TroySmithData <- Heisman.data.prep("Ohio State", "Troy Smith", 2005, "rush")

TimTebowData <- Heisman.data.prep("Florida", "Tim Tebow", 2007, "rush")

SamBradfordData <- Heisman.data.prep("Oklahoma", "Sam Bradford", 2008, "rush")

MarkIngramData <- Heisman.data.prep("Alabama", "Mark Ingram", 2009, "rush")

CamNewtonData <- Heisman.data.prep("Auburn", "Cameron Newton", 2010, "rush")

RG3Data <- Heisman.data.prep("Baylor", "Robert Griffin III" , 2011, "rush")

Johnnyfootballdata <- Heisman.data.prep("Texas A&M", "Johnny Manziel", 2012, "rush")

JameisWinstonData <- Heisman.data.prep("FSU", "Jameis Winston", 2013, "rush")

MarcusMariotaData <- Heisman.data.prep("Oregon", "Marcus Mariota", 2014, "run")

DerrickHenrydata <- Heisman.data.prep("Alabama", "Derrick Henry", 2015, "run")

LamarJacksonData <- Heisman.data.prep("Louisville", "Lamar Jackson", 2016, "run")

HHSaquonBarkley <- Heisman.data.prep("Penn State", "Saquon Barkley", 2017, "run")

HHBakerMayfield <- Heisman.data.prep("Oklahoma", "Baker Mayfield", 2017, "run")

HHJTBarret <- Heisman.data.prep("OSU", "J.T. Barrett", 2017, "run")

HHLamarJackson <- Heisman.data.prep("Louisville", "Lamar Jackson", 2017, "run")

HHBryceLove <- Heisman.data.prep("Stanford", "Bryce Love", 2017, "run")

HHJoshAdams <- Heisman.data.prep("Notre Dame", "Josh Adams", 2017, "run")

```



```{r, place into one dataset, eval = FALSE }

AllPrviousHeisman <- rbind(EricCrouchdata,RG3Data,HHSaquonBarkley, HHJTBarret, 
                           HHLamarJackson, HHBryceLove,  HHJoshAdams, HHBakerMayfield,LamarJacksonData,
                           DerrickHenrydata, MarcusMariotaData,  
                           Johnnyfootballdata, JameisWinstonData, CamNewtonData, 
                           MarkIngramData, JasonWhiteData, SamBradfordData, MattLeinartdata, ReggieBushData, TroySmithData, TimTebowData  )

```


```{r, add custom counts and remove funny names, eval=FALSE  }

AllPrviousHeisman <-  AllPrviousHeisman %>% ungroup() %>%
                          mutate( 
                            player = 
                            ifelse(
                              player == 'Eric Crouch \\(NEB\\)', 'Eric Crouch', 
                            ifelse(
                              player == 'Jason White \\(OKLA\\)', 'Jason White',
                            ifelse(
                              player == 'Matt Leinart \\(USC\\)', 'Matt Leinart',
                            ifelse( 
                              player == "Lamar Jackson" & year == 2016, "Lamar Jackson 2016", 
                            ifelse(
                              player == "Lamar Jackson" & year == 2017, "Lamar Jackson 2017",
                            
                              player
                            )
                            )
                            ))))



AllPrviousHeisman <- AllPrviousHeisman %>% ungroup() %>%
  group_by(player) %>%
                      mutate(
                        # create columns for runnings Games
                        CumulativePassAttemps = cumsum(GamePasses),
                        CumulativeRushAttepmts = cumsum(GameRushes),
                        CumulativeCatches = cumsum(GameCatches), 
                        CumulativeBeenSacked = cumsum(GameSacked), 
                        CumulativePassYards = cumsum(GamePassingYards), 
                        CumulativePassingTDs = cumsum(GamePassingTDS), 
                        CumulativeRushingYards = cumsum(GameRushingYards),
                        CumulativeRushingTDs = cumsum(RushingTD),
                        CumulativeReturnYards = cumsum(GameReturnYards), 
                        CumulativeReturnTDs = cumsum(GameReturnTDS),
                        CumulativeReceivingYards = cumsum(GameReceivingYards), 
                        CumulativeReceivingTDs = cumsum(RecivingTDS), 
                        CumulativeAllPurposeYards = cumsum(AllPurposeYards),
                        CumulativeAllTouchdowns = cumsum(GameTouchdowns)
                                              ) %>% ungroup()




```


```{r, send it, eval = FALSE}

 write.csv(AllPrviousHeisman,'C:/Users/John/Desktop/John Friel/Examples of Work/AllBlog/HeismanwithTabsandSummary/AllPrviousHeisman.csv')



```



And then I have all the information I need to run the following app. I took the data from a play by play state to a summerized state to place into a intuitive web application.











 
 
 <div class="iframe_container">
 
 
  <iframe width="1200" height="1000" src="https://jpf5046.shinyapps.io/HeismanCompare/" frameborder="1" allowfullscreen></iframe>
</div>
 
























































































