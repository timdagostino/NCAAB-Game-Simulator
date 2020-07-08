# R Shiny Dashboard for NCAA Simulator
library(shiny)
library (shinyBS)
library (shinythemes)
library (rvest)
library (dplyr)
library (rvest)
library (dplyr)
library (DT)
library (shinycssloaders)
library (shinycustomloader)
library (shinydashboard)
library (profvis)


#Insert URL ------
URL <- 'https://www.sports-reference.com/cbb/seasons/2020-school-stats.html'

#Read the HTML code from the website ------

webpage <- read_html(URL)

#Team Names
team_name_html <- html_nodes(webpage, '.left')
team_name <- html_text(team_name_html)
noquote(team_name)
team_name <- head(team_name, -1)

#Rank
rank_html <- html_nodes(webpage, 'th.right')
rank <- html_text(rank_html)
rank <- as.numeric(rank)

#Overall Number of Games
overall_games_html <- html_nodes(webpage, '.left+ .right')
overall_games <- html_text(overall_games_html)
overall_games <- head(overall_games, -1)
overall_games <- as.numeric(overall_games)

#Overall Wins
overall_wins_html <- html_nodes(webpage, '.right:nth-child(4)')
overall_wins <- html_text(overall_wins_html)
overall_wins <- head(overall_wins, -1)
overall_wins <- as.numeric(overall_wins)

#Overall Losses
overall_losses_html <- html_nodes(webpage, '.right:nth-child(5)')
overall_losses <- html_text(overall_losses_html)
overall_losses <- as.numeric(overall_losses)

#Win Percentage
wp_html <- html_nodes(webpage, '.right:nth-child(6)')
wp <- html_text(wp_html)
wp <- as.numeric(wp)

#Simple Rating System (SRS)
srs_html <- html_nodes(webpage, '.right:nth-child(7)')
srs <- html_text(srs_html)
srs <- as.numeric(srs)

#Strength of Schedule
sos_html <- html_nodes(webpage, '.right:nth-child(8)')
sos <- html_text(sos_html)
sos <- as.numeric(sos)

#Conference Wins
conf_wins_html <- html_nodes(webpage, '.right:nth-child(9)')
conf_wins <- html_text(conf_wins_html)
conf_wins <- as.numeric(conf_wins)

#Conference Losses
conf_losses_html <- html_nodes(webpage, '.right:nth-child(10)')
conf_losses <- html_text(conf_losses_html)
conf_losses <- as.numeric(conf_losses)

#Home Wins
home_wins_html <- html_nodes(webpage, '.right:nth-child(11)')
home_wins <- html_text(home_wins_html)
home_wins <- as.numeric(home_wins)

#Home Losses
home_losses_html <- html_nodes(webpage, '.right:nth-child(12)')
home_losses <- html_text(home_losses_html)
home_losses <- as.numeric(home_losses)

#Away Wins
away_wins_html <- html_nodes(webpage, '.right:nth-child(13)')
away_wins <- html_text(away_wins_html)
away_wins <- as.numeric(away_wins)

#Away Losses
away_losses_html <- html_nodes(webpage, '.right:nth-child(14)')
away_losses <- html_text(away_losses_html)
away_losses <- as.numeric(away_losses)

#Total Points Scored (season)
tp_html <- html_nodes(webpage, '.right:nth-child(15)')
tp <- html_text(tp_html)
tp <- as.numeric(tp)

#Total Points Allowed (season)
tp_allowed_html <- html_nodes(webpage, '.right:nth-child(16)')
tp_allowed <- html_text(tp_allowed_html)
tp_allowed <- as.numeric(tp_allowed)

#Minutes Played
mp_html <- html_nodes(webpage, '.right:nth-child(18)')
mp <- html_text(mp_html)
mp <- as.numeric(mp)

#Field Goals
fg_count_html <- html_nodes(webpage, '.right:nth-child(19)')
fg_count <- html_text(fg_count_html)
fg_count <- as.numeric(fg_count)

#Field Goals Attempted
fg_att_html <- html_nodes(webpage, '.right:nth-child(20)')
fg_att <- html_text(fg_att_html)
fg_att <- as.numeric(fg_att)

#Field Goal Percentage
fg_pct_html <- html_nodes(webpage, '.right:nth-child(21)')
fg_pct <- html_text(fg_pct_html)
fg_pct <- as.numeric(fg_pct)

#Three Pointers
three_count_html <- html_nodes(webpage, '.right:nth-child(22)')
three_count <- html_text(three_count_html)
three_count <- as.numeric(three_count)

#Three Pointers Attempted
three_att_html <- html_nodes(webpage, '.right:nth-child(23)')
three_att <- html_text(three_att_html)
three_att <- as.numeric(three_att)

#Three Pointers Percentage
three_pct_html <- html_nodes(webpage, '.right:nth-child(24)')
three_pct <- html_text(three_pct_html)
three_pct <- as.numeric(three_pct)

#Free Throws
ft_count_html <- html_nodes(webpage, '.right:nth-child(25)')
ft_count <- html_text(ft_count_html)
ft_count <- as.numeric(ft_count)

#Free Throws Attempted
ft_att_html <- html_nodes(webpage, '.right:nth-child(26)')
ft_att <- html_text(ft_att_html)
ft_att <- as.numeric(ft_att)

#Free Throw Percentage
ft_pct_html <- html_nodes(webpage, '.right:nth-child(27)')
ft_pct <- html_text(ft_pct_html)
ft_pct <- as.numeric(ft_pct)

#Offensive Rebounds
orb_html <- html_nodes(webpage, '.right:nth-child(28)')
orb <- html_text(orb_html)
orb <- as.numeric(orb)

#Total Rebounds
trb_html <- html_nodes(webpage, '.right:nth-child(29)')
trb <- html_text(trb_html)
trb <- as.numeric(trb)

#Steals
steals_html <- html_nodes(webpage, '.right:nth-child(31)')
steals <- html_text(steals_html)
steals <- as.numeric(steals)

#Blocks
blk_html <- html_nodes(webpage, '.right:nth-child(32)')
blk <- html_text(blk_html)
blk <- as.numeric(blk)

#Turnovers
tov_html <- html_nodes(webpage, '.right:nth-child(33)')
tov <- html_text(tov_html)
tov <- as.numeric(tov)

#Personal Fouls
pf_html <- html_nodes(webpage, '.right:nth-child(34)')
pf <- html_text(pf_html)
pf <- as.numeric(pf)

#Advanced Team Stats ------------------------------

#Insert URL
URL <- 'https://www.sports-reference.com/cbb/seasons/2020-advanced-school-stats.html'

#Read the HTML code from the website

webpage_adv <- read_html(URL)

#Pace of Game (An estimate of school posessions per 40 minutes)
pace_html <- html_nodes(webpage_adv, '.right:nth-child(18)')
pace <- html_text(pace_html)
pace <- as.numeric(pace)

#Offensive Rating (An estimate of points scored per 100 posessions)
ortg_html <- html_nodes(webpage_adv, '.right:nth-child(19)')
ortg <- html_text(ortg_html)
ortg <- as.numeric(ortg)

#Free Throw Attempt Rate (Number of Free  Throw Attempts per FG attempt)
ftr_html <- html_nodes(webpage_adv, '.right:nth-child(19)')
ftr <- html_text(ftr_html)
ftr <- as.numeric(ftr)

#True Shooting Percentage (A measure of shooting efficinecy that takes into account all shot types)
ts_pct_html <- html_nodes(webpage_adv, '.right:nth-child(22)')
ts_pct <- html_text(ts_pct_html)
ts_pct <- as.numeric(ts_pct)

#Total Rebound Percentage (An estimate of the percetnage of available rebounds a player grabbed while on the floor)
trb_pct_html <- html_nodes(webpage_adv, '.right:nth-child(23)')
trb_pct <- html_text(trb_pct_html)
trb_pct <- as.numeric(trb_pct)
trb_pct <- (trb_pct/100)

#Steal Percentage (An estimate of the number of opponent posessions that end with a steal)
steal_pct_html <- html_nodes(webpage_adv, '.right:nth-child(25)')
steal_pct <- html_text(steal_pct_html)
steal_pct <- as.numeric(steal_pct)
steal_pct <- (steal_pct/100)

#Block Percentage (An estimate of the percentage of opponent two-point FG attempts that are blocked)
block_pct_html <- html_nodes(webpage_adv, '.right:nth-child(26)')
block_pct <- html_text(block_pct_html)
block_pct <- as.numeric(block_pct)
block_pct <- (block_pct/100)

#Turnover Percentage (An estimate of turnovers per 100 plays)
tov_pct_html <- html_nodes(webpage_adv, '.right:nth-child(28)')
tov_pct <- html_text(tov_pct_html)
tov_pct <- as.numeric(tov_pct)
tov_pct <- (tov_pct/100)

#Offensive Rebound Percentage (An estimate of the percentage of available offensive rebounds grabbed by a player on the floor)
orb_pct_html <- html_nodes(webpage_adv, '.right:nth-child(29)')
orb_pct <- html_text(orb_pct_html)
orb_pct <- as.numeric(orb_pct)
orb_pct <- (orb_pct/100)


#Create Dataframe
ncaa_team_stats_2020 <- data.frame(team_name, rank, overall_games, 
                              overall_wins, overall_losses, wp,
                              srs, sos, conf_wins, 
                              conf_losses, home_wins, home_losses,
                              away_wins, away_losses, tp,
                              tp_allowed, mp, fg_count,
                              fg_att, fg_pct, three_count,
                              three_att, three_pct, ft_count,
                              ft_att, ft_pct, orb, ftr,
                              trb, steals, blk,
                              tov, pf, pace,
                              ortg, ts_pct, trb_pct,
                              steal_pct, block_pct, tov_pct,
                              orb_pct)



team_name_unique <- sort(unique(ncaa_team_stats_2020$team_name))

#Establish Bins
sos_bins <- cut(ncaa_team_stats_2020$sos, 7, include.lowest=TRUE, 
                labels=c("Very Weak", "Weak", "Moderately Weak", "Average", "Moderately Strong", "Strong", "Very Strong"))

ncaa_team_stats_2020 <- cbind(ncaa_team_stats_2020,sos_bins)


# Define UI ----
ui <- fluidPage(
  theme = shinytheme("slate"),
  pageWithSidebar(
    headerPanel("NCAA Men's Basketball Simulator"),
    sidebarPanel(
      sliderInput("num_games", "Number of Games to Simulate:",
                  min = 500, max = 10000, value = 5000, step = 500),
      selectInput('home', 'Select Home Team', choices = team_name_unique, selected = ''),
      selectInput('away', 'Select Away Team', choices = team_name_unique, selected = ''),
      checkboxInput("checkbox", label = "Adjust for Home Field Advantage", value = FALSE),
      checkboxInput("checkbox2", label = "Adjust for Strength of Schedule", value = FALSE),
      actionButton("run",label = "Run Simulation"),
      helpText(h3("Instructions:")),
      helpText("Select the approproate HOME and AWAY teams, then hit the Run Simulation button. The simulator uses real game data to simulate every posession for the specified number of games."),
      helpText("After clicking the Run Simulation button, please allow 60 seconds for the summary and game results to appear (Processing power is limited on the Shiny servers)"),
      br(),
      helpText(h5("Created by Tim D'Agostino (2019)"))
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Summary", 
                           
                           fluidRow(
                             
                             box(
                               title = "Simulation Summary",
                               status = "primary",
                               solidHeader = TRUE,
                               width = 9,
                               withLoader(tableOutput("sum"), type = "image", loader = "bounce.gif")
                             )
                           )
                  ),
                  
                  tabPanel("Simulated Game Results",
                           
                           box(
                             title = "Individual Game Results",
                             status = "primary",
                             solidHeader = TRUE,
                             width = 9,
                             withLoader(tableOutput('data'), type = "image", loader = "bounce.gif")
                           )
                  ),
                  
                  tabPanel("Vegas' Odds",
                           
                           box(
                             title = "Vegas' Odds",
                             status = "primary",
                             solidHeader = TRUE,
                             width = 9
                           )
                  )
      )
      
      
      
    )
  )
  
)


# Define server logic ----
server <- function(input, output) {
  
  
  button_push <- observeEvent(input$run, {
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    progress$set(message = "Simulation in Progress", value = 0)
    #Start Simulation Here --------------------------------------
    
    #Set Home Team
    home_team <- team_stats_2020 %>%
      filter(team_name == input$home)
    
    #Set Away Team
    away_team <- team_stats_2020 %>%
      filter(team_name == input$away)
    
    #Set Adjustment
    home_adj <- switch(as.character(home_team$sos_bins), 
                       "Very Weak" = -.075, 
                       "Weak" = -.05, 
                       "Moderately Weak" = -.025, 
                       "Average" = 0, 
                       "Moderately Strong" = .025,
                       "Strong" = .05,
                       "Very Strong" = .075)
    
    away_adj <- switch(as.character(away_team$sos_bins), 
                       "Very Weak" = -.075, 
                       "Weak" = -.05, 
                       "Moderately Weak" = -.025, 
                       "Average" = 0, 
                       "Moderately Strong" = .025,
                       "Strong" = .05,
                       "Very Strong" = .075)
    
  
    
    #Establish Variables
    home_team_total_score <- 0
    away_team_total_score <- 0
    home_team_ft_att <- 0
    home_team_ft_points <- 0
    away_team_ft_att <- 0
    away_team_ft_points <- 0
    spread <- 0
    
    #Establish Home Field Advantage
    home_win_pct <- home_team$home_wins/(home_team$home_wins + home_team$home_losses)
    
    if (input$checkbox == TRUE) {
      
      home_adv <- home_win_pct/20
    } else {
      home_adv <- 0
    }
    
    if (input$checkbox2 == TRUE) {
      
      
      #Create Adjustment Functions
      home_stat_adjust <- function(stat) {
        new_home_stat <- stat + (stat*home_adj)
        return(new_home_stat)
      }
      
      away_stat_adjust <- function(stat) {
        new_away_stat <- stat + (stat*away_adj)
        return(new_away_stat)
      }
      
      #Adjust Home
      home_team$fg_pct <- home_stat_adjust(home_team$fg_pct)
      home_team$three_pct <- home_stat_adjust(home_team$three_pct)
      home_team$ts_pct <- home_stat_adjust(home_team$ts_pct)
      home_team$trb_pct <- home_stat_adjust(home_team$trb_pct)
      home_team$steal_pct <- home_stat_adjust(home_team$steal_pct)
      home_team$block_pct <- home_stat_adjust(home_team$block_pct)
      home_team$orb_pct <- home_stat_adjust(home_team$orb_pct)
      
      home_team$tov_pct <- home_team$tov_pct - (home_team$tov_pct*home_adj)
      
      #Adjust Away
      away_team$fg_pct <- away_stat_adjust(away_team$fg_pct)
      away_team$three_pct <- away_stat_adjust(away_team$three_pct)
      away_team$ts_pct <- away_stat_adjust(away_team$ts_pct)
      away_team$trb_pct <- away_stat_adjust(away_team$trb_pct)
      away_team$steal_pct <- away_stat_adjust(away_team$steal_pct)
      away_team$block_pct <- away_stat_adjust(away_team$block_pct)
      away_team$orb_pct <- away_stat_adjust(away_team$orb_pct)
      
      away_team$tov_pct <- away_team$tov_pct - (away_team$tov_pct*away_adj)
      
    }
    
    #Reset Simulation Summary
    sim_sum <- 0
    
    x <- as.integer(1) #Game Count for Data Frame Entry
    
    #Establish Max Shot Clock
    sc_max <- 30
    sc_min <- 10
    
    #Percentage of Shots, Field Goals and Three Pointers
    home_team_pct_fg <- home_team$fg_count / (home_team$fg_count + home_team$three_count)
    home_team_pct_th <- home_team$three_count / (home_team$fg_count + home_team$three_count)
    away_team_pct_fg <- away_team$fg_count / (away_team$fg_count + away_team$three_count)
    away_team_pct_th <- away_team$three_count / (away_team$fg_count + away_team$three_count)
    
    #Number of Simulations
    
    loop <- input$num_games
    i <- loop
    
    while (i > 0)
    {
      
      #Set Scores for game simulation
      home_team_score <- 0
      away_team_score <- 0
      
      #Set clock for game simulation
      clock <- 2400
      
      #Set counters for fg attempts during game simulation
      home_team_fg_att <- 0
      away_team_fg_att <- 0
      
      #Tipoff
      if (runif(1) >= .51) {
        home_team_pos <- 1
        away_team_pos <- 0
      } else {
        home_team_pos <- 0
        away_team_pos <- 1
      }
      
      while (clock > 0) {
        
        #Home Team Posession
        if (home_team_pos == 1) {
          
          if(runif(1) > (home_team$tov_pct - (home_team$tov_pct*home_adv))) { #Testing for Turnover
            
            if (runif(1) <= home_team_pct_fg) { #Testing for Shot Type - FG
              shot_type <- 2 #FG Shot Type
              home_team_fg_att <- home_team_fg_att + 1
              
              if (runif(1) > away_team$block_pct) { #Testing for Blocked Shot
                
                if (runif(1) <= (home_team$fg_pct + (home_team$fg_pct*home_adv))) { #Testing if Shot is Made
                  home_team_score <- home_team_score + 2 #Shot is Made
                  
                  #Calculating Time of Posession
                  top <- runif(1, min=sc_min, max=sc_max)
                  
                  #Adjusting Clock Time
                  clock <- clock - top
                  
                  #Posession Change
                  home_team_pos <- 0
                  away_team_pos <- 1
                  
                } else { #Shot is Missed
                  if ( runif(1) <= (home_team$orb_pct + (home_team$orb_pct*home_adv))) { #Testing for Offensive Rebound
                    #Calculating Time of Posession
                    top <- runif(1, min=sc_min, max=sc_max)
                    
                    #Adjusting Clock Time
                    clock <- clock - top
                    
                  } else { #Defense grabs the rebound
                    #Calculating Time of Posession
                    top <- runif(1, min=sc_min, max=sc_max)
                    
                    #Adjusting Clock Time
                    clock <- clock - top
                    
                    #Posession Change
                    home_team_pos <- 0
                    away_team_pos <- 1
                  }
                }
              } else { #Shot it Blocked
                
                #Calculating Time of Posession
                top <- runif(1, min=sc_min, max=sc_max)
                
                #Adjusting Clock Time
                clock <- clock - top
                
                #Posession Change
                home_team_pos <- 0
                away_team_pos <- 1
              }
              
            } else { #Shot Type - Three Point
              shot_type <- 3 #Three Point Shot
              
              if (runif(1) <= (home_team$three_pct + (home_team$three_pct*home_adv))) { #Testing if Shot is Made
                home_team_score <- home_team_score + 3 #Shot is Made
                
                #Calculating Time of Posession
                top <- runif(1, min=sc_min, max=sc_max)
                
                #Adjusting Clock Time
                clock <- clock - top
                
                #Posession Change
                home_team_pos <- 0
                away_team_pos <- 1
                
              } else { #Shot is Missed
                if ( runif(1) <= (home_team$orb_pct + (home_team$orb_pct*home_adv))) { #Testing for Offensive Rebound
                  #Calculating Time of Posession
                  top <- runif(1, min=sc_min, max=sc_max)
                  
                  #Adjusting Clock Time
                  clock <- clock - top
                  
                } else { #Defense grabs the rebound
                  #Calculating Time of Posession
                  top <- runif(1, min=sc_min, max=sc_max)
                  
                  #Adjusting Clock Time
                  clock <- clock - top
                  
                  #Posession Change
                  home_team_pos <- 0
                  away_team_pos <- 1
                }
              }
              
            }
            
          } else { #Forced Turnover
            
            #Calculating Time of Posession
            top <- runif(1, min=sc_min, max=sc_max)
            
            #Adjusting Clock Time
            clock <- clock - top
            
            #Posession Change
            home_team_pos <- 0
            away_team_pos <- 1
          }
        } else { #Away Team Posession
          
          away_team_pos == 1
          
          if(runif(1) > away_team$tov_pct) { #Testing for Turnover
            
            if (runif(1) <= away_team_pct_fg) { #Testing for Shot Type - FG
              shot_type <- 2 #FG Shot Type
              away_team_fg_att <- away_team_fg_att + 1
              
              if (runif(1) > home_team$block_pct) { #Testing for Blocked Shot
                
                if (runif(1) <= away_team$fg_pct) { #Testing if Shot is Made
                  away_team_score <- away_team_score + 2 #Shot is Made
                  
                  #Calculating Time of Posession
                  top <- runif(1, min=sc_min, max=sc_max)
                  
                  #Adjusting Clock Time
                  clock <- clock - top
                  
                  #Posession Change
                  home_team_pos <- 1
                  away_team_pos <- 0
                  
                } else { #Shot is Missed
                  if ( runif(1) <= away_team$orb_pct) { #Testing for Offensive Rebound
                    #Calculating Time of Posession
                    top <- runif(1, min=sc_min, max=sc_max)
                    
                    #Adjusting Clock Time
                    clock <- clock - top
                    
                  } else { #Defense grabs the rebound
                    #Calculating Time of Posession
                    top <- runif(1, min=sc_min, max=sc_max)
                    
                    #Adjusting Clock Time
                    clock <- clock - top
                    
                    #Posession Change
                    home_team_pos <- 1
                    away_team_pos <- 0
                  }
                }
              } else { #Shot it Blocked
                
                #Calculating Time of Posession
                top <- runif(1, min=sc_min, max=sc_max)
                
                #Adjusting Clock Time
                clock <- clock - top
                
                #Posession Change
                home_team_pos <- 1
                away_team_pos <- 0
              }
              
            } else { #Shot Type - Three Point
              shot_type <- 3 #Three Point Shot
              
              if (runif(1) <= away_team$fg_pct) { #Testing if Shot is Made
                away_team_score <- away_team_score + 3 #Shot is Made
                
                #Calculating Time of Posession
                top <- runif(1, min=sc_min, max=sc_max)
                
                #Adjusting Clock Time
                clock <- clock - top
                
                #Posession Change
                home_team_pos <- 1
                away_team_pos <- 0
                
              } else { #Shot is Missed
                if ( runif(1) <= away_team$orb_pct) { #Testing for Offensive Rebound
                  #Calculating Time of Posession
                  top <- runif(1, min=sc_min, max=sc_max)
                  
                  #Adjusting Clock Time
                  clock <- clock - top
                  
                } else { #Defense grabs the rebound
                  #Calculating Time of Posession
                  top <- runif(1, min=sc_min, max=sc_max)
                  
                  #Adjusting Clock Time
                  clock <- clock - top
                  
                  #Posession Change
                  home_team_pos <- 1
                  away_team_pos <- 0
                }
              }
              
            }
            
          } else { #Forced Turnover
            
            #Calculating Time of Posession
            top <- runif(1, min=sc_min, max=sc_max)
            
            #Adjusting Clock Time
            clock <- clock - top
            
            #Posession Change
            home_team_pos <- 1
            away_team_pos <- 0
          }
        }
        
      }   
      
      #Add in Free Throws
      
      #Home Team
      home_team_ft_att <- home_team_fg_att*home_team$ft_pct #FT Attempted
      home_team_ft_points <- home_team_ft_att*home_team$ft_pct #FT Made
      
      #Away Team
      away_team_ft_att <- away_team_fg_att*away_team$ft_pct #FT Attempted
      away_team_ft_points <- away_team_ft_att*away_team$ft_pct #FT Made
      
      #Add to total score & Convert to type Int
      home_team_score <- as.integer(home_team_score + home_team_ft_points)
      away_team_score <- as.integer(away_team_score + away_team_ft_points)
      
      
      if (home_team_score > away_team_score) { #Determine Game Winner
        
        home_team_wins <- 1
        away_team_wins <- 0

        
      } else {
        
        home_team_wins <- 0
        away_team_wins <- 1
        
      }
      
      spread <- (home_team_score - away_team_score)
      
      #Compile Game Summary Data 
      game_sum <- c(Game = x, 
                    Home = home_team_score, 
                    Away = away_team_score, 
                    Spread = spread, 
                    Home.Win = home_team_wins, 
                    Away.Win = away_team_wins)
      
      sim_sum <- data.frame(rbind(sim_sum, game_sum))
      
      i <- i - 1 #Reduce Loop Count (Total Simulations Left to Run)
      
      progress$inc(1/i, detail = paste("Game Count:", x))
      x <- x + 1
    }
    
    
    sim_sum <- sim_sum[-1,]
    
    output$data <- renderTable(sim_sum)
    
    overall_sum <- colSums(sim_sum)/loop
    
    overall_sum <- overall_sum[-1]
    

    overall_sum <- type.convert(data.frame(overall_sum))
    
    overall_sum$Home.Score <- round(overall_sum$Home.Score)
    overall_sum <- t(overall_sum)
      
    output$sum <- renderTable(overall_sum)
    
    
  })
  
  
}


  
# Run the app ----
shinyApp(ui = ui, server = server)