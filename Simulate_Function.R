library (rvest)
library (dplyr)

#Scrape Team Data
source("NCAA_Data_Scraper.R")

#Scrape Upcoming Game Data
#Insert URL ------
URL <- 'https://www.sports-reference.com/cbb/'

#Read the HTML code from the website 
webpage <- read_html(URL)

#Home Team Names
home_teams_html <- html_nodes(webpage, '#scores thead+ tbody tr:nth-child(1) a')
home_teams <- html_text(home_teams_html)
noquote(home_teams)
home_teams <- list(head(home_teams, -1))

#Away Team Names
away_teams_html <- html_nodes(webpage, '.expanded thead+ tbody tr+ tr a')
away_teams <- html_text(away_teams_html)
noquote(away_teams)
away_teams <- list(head(away_teams, -1))

#Compile Stats for Home Teams
game_count <- lengths(home_teams)

#Append home and away team stats into a DF Function
append_team_stats <- function(team_name, num_games) {
  
  iterations <- num_games
  x <- iterations
  
  for (i in seq_along(home_teams)) {
    output[[i]] <- ncaa
  }
  
}

#Basketball Simulation function
simulate_bg <- function(home_team, away_team, sim_count = 5000, adjust_sos = TRUE, adjust_hfa = TRUE) {
  
#Set Home Team
home_team <- home_stats

#Set Away Team
away_team <- away_stats

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

if (adjust_hfa == TRUE) {
  
  home_adv <- home_win_pct/20
} else {
  home_adv <- 0
}

if (adjust_sos == TRUE) {
  
  
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
i <- sim_count
loop <- i

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
  
  x <- x + 1
}
overall_sum <- colSums(sim_sum)/loop

overall_sum <- overall_sum[-1]


overall_sum <- type.convert(data.frame(overall_sum))

overall_sum <- t(overall_sum)

return(overall_sum)
}

vec_simulate_bg <-  Vectorize(simulate_bg)
