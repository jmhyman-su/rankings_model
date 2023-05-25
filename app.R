#load data from github and from initial results
library(shiny)
library(DT)
library(dplyr)
library(janitor)
library(stringi)
library(shinyWidgets)
library(fontawesome)

#setwd("/Users/jameshyman/Downloads/Tourney Results/app1")

#load in open player rankings and rename
player <- read.csv("player_ratings_1v2_df.csv")
names(player)[2] <- 'Score'
names(player)[8] <- 'Rank'
names(player)[3] <- 'Wins'
names(player)[4] <- 'Losses'
names(player)[5] <- 'Tournaments'
names(player)[6] <- 'Avg Partner Rating'
names(player)[7] <- 'Avg Opp. Rating'

#remove ties
player$Rank <- as.numeric(sub('T',"", player$Rank))
player$prev_rank <- as.numeric(sub('T',"", player$prev_rank))

#create change in ranking column and add colored arrows
player <- player %>%
  mutate(change = ifelse(!is.na(prev_rank), prev_rank - Rank, NA),
         change2 = ifelse(!is.na(prev_rank), prev_rank - Rank, NA))

player <- player %>%
  mutate(Change = ifelse(change > 0, paste0(fa(name = "arrow-up", fill = "green"), change),
                         ifelse(change < 0, paste0(fa(name = "arrow-down", fill = "red"), abs(change)),
                                ifelse(is.na(change), "NEW", "--"))))

#tidy up table
player <- player %>%
  mutate(Change = ifelse(is.na(Change), "NEW", Change)) %>%
  arrange(Rank) %>%
  select(Name, Rank, Change, Score, Wins, Losses, Tournaments, `Avg Partner Rating`, `Avg Opp. Rating`, change2)


#repeat process for womens rankings (could make a function to elimiate duplicate code)
playerW <- read.csv("w_player_ratings_1v2_df.csv")
names(playerW)[2] <- 'Score'
names(playerW)[8] <- 'Rank'
names(playerW)[3] <- 'Wins'
names(playerW)[4] <- 'Losses'
names(playerW)[5] <- 'Tournaments'
names(playerW)[6] <- 'Avg Partner Rating'
names(playerW)[7] <- 'Avg Opp. Rating'

playerW$Rank <- as.numeric(sub('T',"", playerW$Rank))
playerW$prev_rank <- as.numeric(sub('T',"", playerW$prev_rank))

playerW <- playerW %>%
  mutate(change = ifelse(!is.na(prev_rank), prev_rank - Rank, NA),
         change2 = ifelse(!is.na(prev_rank), prev_rank - Rank, NA))

playerW <- playerW %>%
  mutate(Change = ifelse(change > 0, paste0(fa(name = "arrow-up", fill = "green"), change),
                         ifelse(change < 0, paste0(fa(name = "arrow-down", fill = "red"), abs(change)),
                                ifelse(is.na(change), "NEW", "--"))))
playerW <- playerW %>%
  mutate(Change = ifelse(is.na(Change), "NEW", Change)) %>%
  arrange(Rank) %>%
  select(Name, Rank, Change, Score, Wins, Losses, Tournaments, `Avg Partner Rating`, `Avg Opp. Rating`, change2)

#round values
player$Score <- round(player$Score, 2)
playerW$Score <- round(playerW$Score, 2)

#create UI
## currenly only looking at the player rankings tab, the team rankings and results pages have been removed
ui <- fluidPage(
  #navigation bar, make sure logo is in the www filder within directory
  navbarPage(title = div(img(src = "USA-Roundnet-Logo.png", height = 25, width = 25), "USA Roundnet Rankings"),
             #Player rankings page
             tabPanel("Player Rankings", fluid = TRUE,
                      fluidRow("Last Updated: March 9th, 2023"),
                      fluidRow(
                        column(6,
                               titlePanel("Open Rankings"),
                               dataTableOutput('player')
                        ),
                        column(6,
                               titlePanel("Women's Rankings"),
                               dataTableOutput('playerW'))
                      )
             )
             
  )
  
)

#create server
server <- function(input, output, session) {
  
  output$player <- renderDataTable({
    DT::datatable(player, escape = F, options = list(iDisplayLength = 25, scrollX = TRUE, 
                                                     columnDefs = list(list(className = 'dt-right', targets = "_all"), 
                                                                       list(orderData = 9, targets = 2),
                                                                       list(visible = FALSE, targets = 9))
    ), rownames= FALSE)
  })
  
  
  output$playerW <- renderDataTable({
    DT::datatable(playerW, escape = F, options = list(iDisplayLength = 25, scrollX = TRUE,
                                                      columnDefs = list(list(className = 'dt-right', targets = "_all"),
                                                                        list(orderData = 9, targets = 2),
                                                                        list(visible = FALSE, targets = 9)
                                                      )), rownames= FALSE)
  })

}

#run app, use run app button instead of running this line so picture loads
shinyApp(ui = ui, server = server)

