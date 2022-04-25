# R Shiny Dashboard for NCAA Simulator
library (shiny)
install.packages("plyr")
install.packages("dplyr")
install.packages("shinycustomloader")
install.packages("DT")
install.packages("shinycssloaders")
install.packages("profvis")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("colorfindr")
install.packages("shinyalert")
library (shinyBS)
library (shinythemes)
library (rvest)
#library (plotly)
library (plyr)
library (dplyr)
library (rvest)
library (dplyr)
library (DT)
library (shinycssloaders)
library (shinycustomloader)
library (shinydashboard)
library (profvis)
library (ggplot2)
library (ggthemes)
library (colorfindr)
library (shinyalert)


#Data
source("NCAA_Data_Scraper.R")


# Define UI ----
ui <- fluidPage(
  theme = shinytheme("flatly"),
  pageWithSidebar(
    headerPanel(title=div(img(src="ncaa.png", height = "10%", width = "10%"),"NCAA Men's Basketball Simulator")),
    sidebarPanel(
      sliderInput("num_games", "Number of Games to Simulate:",
                  min = 500, max = 10000, value = 3500, step = 500),
      selectInput('home', 'Select Home Team', choices = team_name_unique, selected = 'Villanova'),
      selectInput('away', 'Select Away Team', choices = team_name_unique, selected = 'Georgetown'),
      #checkboxInput("checkbox_hfa", label = "Adjust for Home Field Advantage", value = FALSE),
      checkboxInput("checkbox_sos", label = "Adjust for Strength of Schedule", value = FALSE),
      checkboxInput("checkbox_norm", label = "Normalize Stats (Caution: WIP - Expect Bugs)", value = FALSE),
      helpText(em("Selecting this normalizes team performance based on a selection of opponent performance metrics.")),
      br(),
      actionButton("run",label = "Run Simulation"),
      helpText(h3("Instructions:")),
      helpText("Select the approproate HOME and AWAY teams, then hit the Run Simulation button. The simulator uses real game data to simulate every posession for the specified number of games."),
      helpText("After clicking the Run Simulation button, please allow 30 seconds for the summary and game results to appear."),
      br(),
      helpText(h5("Created by Tim D'Agostino (2020)")),
      helpText(h5("Contact: timothydagostino16@gmail.com"))
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Simulate a Game", 
                           
                           fluidRow(
                             
                             column(12, align = 'center',
                                    
                                    h2(strong("Predicted Outcome")),
                                    HTML('<hr style="color: gray;">'),
                                    br(),
                                    
                                    fluidRow(
                                      
                                      column(1),
                                      
                                      column(4, 
                                             imageOutput("away_image"),
                                             h3(strong(textOutput("away_name"))),
                                             HTML('<hr style="color: gray;">'),
                                             h1(strong(textOutput("away_score"))),
                                             h3(textOutput("away_spread"))
                                             
                                      ), 
                                      column(2,
                                             br(),
                                             br(),
                                             img(src="vs.png", height = "40%", width = "40%"),
                                             br(),
                                             br(),
                                             br(),
                                             h4(strong("Over/Under")),
                                             HTML('<hr style="color: gray;">'),
                                             h2(strong(textOutput("over_under")))
                                      ),
                                      
                                      column(4, 
                                             imageOutput("home_image"),
                                             h3(strong(textOutput("home_name"))),
                                             HTML('<hr style="color: gray;">'),
                                             h1(strong(textOutput("home_score"))),
                                             h3(textOutput("home_spread"))
                                      ),
                                      
                                      column(1),
                                    ),
                                    
                                    fluidRow(
                                      HTML('<hr style="color: black; border: 2px solid GhostWhite; border-radius: 25px;">'),
                                      h4(strong("Win Probability (%)")),
                                      
                                      column(1),
                                      
                                      column(5, align = "right",
                                            br(),
                                            plotOutput("wp_away", height = 40, width = 350)
                                              ),
                                      
                                      column(5, align = "left",
                                             br(),
                                             plotOutput("wp_home", height = 40, width = 350)
                                      ),
                                      
                                      column(1)
                                      ),
                                    
                                    fluidRow( 
                                      column(2),
                                      
                                      column(8,
                                             fluidRow(
                                               br(),
                                               h5(strong("Simulation Spread Results")),
                                               plotOutput("games_spread"))
                                             ),
                                             
                                      column(2),
                                             
                                      ),
                        
                                    
                             ),
                             
                           ), tags$head(tags$link(rel = "stylesheet",
                                                  type = "text/css", href = "style.css")) #css Stylesheet
                             
                             
                           ),
                  
                  
                  tabPanel("Simulated Game Results",
                           
                           box(
                             title = "Individual Game Results",
                             status = "primary",
                             solidHeader = TRUE,
                             DTOutput("data"),
                             width = 9
                           )
                  ),
                  
                  tabPanel("Team Stats",
                           
                           fluidRow(
                             
                             column(12, 
                             DTOutput("team_stats")
                             )
                           )
                  )
                  
                  # tabPanel("Upcoming Game Predictions",
                  #          
                  #          fluidRow(
                  #            
                  #            column(12)
                  #            
                  #            )
                  #          )
                  
                  
                  
      )
      
      
      
    )
    
  ),
  
  useShinyalert() #Shiny Alert
  
)

# Define server logic ----
server <- function(input, output) {
  
  shinyalert(
    title = "Insufficient Stats Warning",
    text = " Please be aware: If you select a team and the simulation fails to run, it is because one of the selected teams has insuffiencit stats to support the simulation model. This issue should resolve itself in the coming weeks.",
    size = "m", 
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = FALSE,
    type = "warning",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
  
  #Create Table with Team Stats
  output$team_stats <- DT::renderDT({datatable(ncaa_team_stats_2021,
                                               style = 'bootstrap',
                                               options = list(
                                                 columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                 pageLength = 25,
                                                 paging = TRUE,
                                                 searching = TRUE,
                                                 scrollX = TRUE,
                                                 fixedHeader = TRUE,
                                                 searchHighlight = TRUE,
                                                 autoWidth = TRUE,
                                                 ordering = TRUE,
                                                 dom = 'tipBf'),
                                               filter = 'top',
                                               rownames = FALSE,
                                               class = "display") %>%
      formatPercentage(c("ft_pct", "ftr", "ts_pct", "trb_pct", "steal_pct", "block_pct", "tov_pct", "orb_pct"))
  })
  
  #Render Home Team Image
  output$home_image <- renderImage({
    filename <- normalizePath(file.path("www",
                              paste0(input$home, ".png")))
    
    list(src = filename, height = "100px", width = "100px")
  }, deleteFile = FALSE)
  
  #Render Home Team Name
  output$home_name <- renderText({input$home})

  #Render Away Team Image
  output$away_image <- renderImage({
    filename <- normalizePath(file.path("www",
                              paste0(input$away, ".png")))
    
  #Render Away Team Name
  output$away_name <- renderText({input$away})
    
    list(src = filename, height = "100px", width = "100px")
  }, deleteFile = FALSE)
  
  #Begin Simulation Here
  button_push <- observeEvent(input$run, {

    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    progress$set(message = "Simulation in Progress", value = 0)
    #Start Simulation Here --------------------------------------
    
    #Set Home Team
    home_team <- ncaa_team_stats_2021 %>%
      filter(team_name == input$home)
    
    #Set Away Team
    away_team <- ncaa_team_stats_2021 %>%
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
    
    home_adv <- 0
    
    # if (input$checkbox_hfa == TRUE) {
    #      
    #  home_adv <- as.double(home_win_pct*.10)
    # } 
    
    if (input$checkbox_sos == TRUE) {
      
      #Create Adjustment Functions
      home_stat_adjust <- function(stat) {
        new_home_stat <- stat + (stat*home_adj)
        return(new_home_stat)
      }
      
      away_stat_adjust <- function(stat) {
        new_away_stat <- stat + (stat*away_adj)
        return(new_away_stat)
      }
      
      #Adjust Home Stats
      home_team$fg_pct <- home_stat_adjust(home_team$fg_pct)
      home_team$three_pct <- home_stat_adjust(home_team$three_pct)
      home_team$ts_pct <- home_stat_adjust(home_team$ts_pct)
      home_team$trb_pct <- home_stat_adjust(home_team$trb_pct)
      home_team$steal_pct <- home_stat_adjust(home_team$steal_pct)
      home_team$block_pct <- home_stat_adjust(home_team$block_pct)
      home_team$orb_pct <- home_stat_adjust(home_team$orb_pct)
      
      home_team$tov_pct <- home_team$tov_pct - (home_team$tov_pct*home_adj)
      
      #Adjust Away Stats
      away_team$fg_pct <- away_stat_adjust(away_team$fg_pct)
      away_team$three_pct <- away_stat_adjust(away_team$three_pct)
      away_team$ts_pct <- away_stat_adjust(away_team$ts_pct)
      away_team$trb_pct <- away_stat_adjust(away_team$trb_pct)
      away_team$steal_pct <- away_stat_adjust(away_team$steal_pct)
      away_team$block_pct <- away_stat_adjust(away_team$block_pct)
      away_team$orb_pct <- away_stat_adjust(away_team$orb_pct)
      
      away_team$tov_pct <- away_team$tov_pct - (away_team$tov_pct*away_adj)
      
    }
    
    #Normalize Stats/Non Normalized Stats
    if (input$checkbox_norm == TRUE) {
      
      #Home Team
      home_team$fg_pct <- mean(c(home_team$fg_pct, away_team$opp_fg_pct))
      home_team$three_pct <- mean(c(home_team$three_pct, away_team$opp_three_pct))
      home_team$block_pct <- mean(c(home_team$block_pct, away_team$opp_block_pct))
      home_team$orb_pct <- mean(c(home_team$orb_pct, away_team$opp_orb_pct))
      home_team$tov_pct <- mean(c(home_team$tov_pct, away_team$opp_tov_pct))
      
      #Away Team
      away_team$fg_pct <- mean(c(away_team$fg_pct, home_team$opp_fg_pct))
      away_team$three_pct <- mean(c(away_team$three_pct, home_team$opp_three_pct))
      away_team$block_pct <- mean(c(away_team$block_pct, home_team$opp_block_pct))
      away_team$orb_pct <- mean(c(away_team$orb_pct, home_team$opp_orb_pct))
      away_team$tov_pct <- mean(c(away_team$tov_pct, home_team$opp_tov_pct))
    }
    
    
    #Reset Simulation Summary
    sim_sum <- data.frame()
    
    x <- as.integer(1) #Game Count for Data Frame Entry
    
    #Establish Max Shot Clock
    sc_max <- 30
    sc_min <- 4
    
    #Percentage of Field Goals and Three Pointers
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
              
              if (runif(1) <= away_team$three_pct) { #Testing if Shot is Made
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
      home_team_ft_att <- round(home_team_fg_att*home_team$ftr, digits = 0) #FT Attempted
      home_team_ft_points <- round(home_team_ft_att*home_team$ft_pct, digits = 0) #FT Made
      
      #Away Team
      away_team_ft_att <- round(away_team_fg_att*away_team$ftr, digits = 0) #FT Attempted
      away_team_ft_points <- round(away_team_ft_att*away_team$ft_pct, digits = 0) #FT Made
      
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
      
      sim_sum <- data.frame(rbind(game_sum, sim_sum))
      names(sim_sum) <- c("Game", "Home", "Away", "Spread", "Home.Win", "Away.Win")
      
      i <- i - 1 #Reduce Loop Count (Total Simulations Left to Run)
      
      progress$inc(1/i, detail = paste("Game Count:", x))
      x <- x + 1
    }
    
    games_summary <- sim_sum %>% select(Home:Spread) 
    
    #Create Table with Individual Game Stats
    output$data <- output$sum <- DT::renderDT({datatable(games_summary,
                                                         style = 'bootstrap',
                                                         options = list(
                                                           columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                           paging = TRUE,
                                                           searching = FALSE,
                                                           fixedColumns = TRUE,
                                                           autoWidth = TRUE,
                                                           ordering = TRUE,
                                                           dom = 'tfBp'),
                                                         rownames = FALSE,
                                                         class = "display") %>%
        formatRound(c("Home", "Away", "Spread"), digits = 0)
    })
    
    #Generate Plot to show Game Spread
    output$games_spread <- renderPlot({
      
      spread <- ggplot(sim_sum, aes(Spread)) + 
        geom_histogram(binwidth = 2, color = "dodgerblue", fill="white", position="dodge") +
        geom_vline(aes(xintercept=mean(Spread)),
                   color="dodgerblue3", linetype="dashed", size=1) +
        xlab("Point Spread") +
        ylab("Game Count")
      
      print(spread + theme_minimal(base_size = 12))
    })
    
    #Establish and Clean Final DF
    overall_sum <- colSums(sim_sum)/loop
    overall_sum <- overall_sum[-1]
    overall_sum <- t(type.convert(data.frame(overall_sum)))
    
    #Render Win Probability Chart Data Frames
    home_wp_df <- data.frame(team = "Home", wp = overall_sum[4], image = normalizePath(file.path("www",
                                                                                                    paste0(input$home, ".png"))))
    away_wp_df <- data.frame(team = "Away", wp = overall_sum[5], image = normalizePath(file.path("www",
                                                                                                 paste0(input$away, ".png"))))
    
    #Render Win Probabilty Chart - Home Team
    output$wp_home <- renderPlot({
      wp_home <- ggplot(home_wp_df, aes(team, wp)) +
        coord_flip() +
        geom_bar(position = "stack", stat = "identity", fill = get_colors(normalizePath(file.path("www",
                                                                                                  paste0(input$home, ".png"))), top_n = 1, get_stats = FALSE)) +
        ylim(0,1.2) +
        geom_hline(yintercept=1, size = 2, color = "lightgray") +
        geom_hline(yintercept=0, size = 2, color = "black") +
        geom_text(size = 7, label = paste0(round(home_wp_df[2]*100, digits = 1), "%"), nudge_y = .2)
      wp_home + theme_void()
      
    })
    
    #Render Win Probabilty Chart - Away Team
    output$wp_away <- renderPlot({
      wp_away <- ggplot(away_wp_df, aes(team, -wp)) +
        coord_flip() +
        geom_bar(position = "stack", stat = "identity", fill = get_colors(normalizePath(file.path("www",
                                                                                                  paste0(input$away, ".png"))), top_n = 1, get_stats = FALSE)) +
        ylim(-1.2,0) +
        geom_hline(yintercept=-1, size = 2, color = "lightgray") +
        geom_hline(yintercept=0, size = 2, color = "black") +
        geom_text(size = 7, label = paste0(round(away_wp_df[2]*100, digits = 1), "%"), nudge_y = -.2)
      wp_away + theme_void()
    })
    
    
    
    #Render Simulation Scores for Output
    h_score <- floor(overall_sum[1,1])
    a_score <- floor(overall_sum[1,2])
    
    output$home_score <- renderText({floor(overall_sum[1,1])})
    output$away_score <- renderText({floor(overall_sum[1,2])})
    
    #Calculate Over/Under
    output$over_under <- renderText({floor(overall_sum[1,1]) + floor(overall_sum[1,2])})
    
    #Render Spread String for Main Display
    abs_spread <- abs(h_score-a_score)
    
    if (h_score >= a_score) {
      output$home_spread <-renderText({paste0("(-", abs_spread, ")")})
      output$away_spread <-renderText({paste0("(+", abs_spread, ")")})
    } else {
      output$home_spread <-renderText({paste0("(+", abs_spread, ")")})
      output$away_spread <-renderText({paste0("(-", abs_spread, ")")})
    }
    
    
    #Create Table with Overall Simulation Stats
    output$sum <- DT::renderDT({datatable(overall_sum,
                                          style = 'bootstrap',
                                          options = list(
                                            columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                            paging = FALSE,
                                            searching = FALSE,
                                            fixedColumns = TRUE,
                                            autoWidth = TRUE,
                                            ordering = TRUE,
                                            dom = 'tB'),
                                          rownames = FALSE,
                                          class = "display") %>%
        formatPercentage(c("Home.Win", "Away.Win")) %>%
        formatRound(c("Home", "Away", "Spread"), digits = 0)
    })

    
    
  })
  
  
}
  
# Run the app ----
shinyApp(ui = ui, server = server)

