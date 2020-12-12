#Set Home Team
home_team <- ncaa_team_stats_2020 %>%
  filter(team_name == "Davidson")

#Set Away Team
away_team <- ncaa_team_stats_2020 %>%
  filter(team_name == "Davidson")

#Home Advantage?
check <-  TRUE
home_win_pct <- home_team$home_wins/(home_team$home_wins + home_team$home_losses)

if (check == TRUE) {
   home_adv <- home_win_pct*.1
 } else {
   home_adv <- 0
 }
#home_adv <- 0 #Temporary Fix, Button Not Working


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
  
#SOS Adjustment
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
  

#Reset Simulation Summary
sim_sum <- data.frame()

x <- as.integer(1) #Game Count for Data Frame Entry

#Establish Max Shot Clock
sc_max <- 30
sc_min <- 8

#Percentage of Shots, Field Goals and Three Pointers
home_team_pct_fg <- home_team$fg_count / (home_team$fg_count + home_team$three_count)
home_team_pct_th <- home_team$three_count / (home_team$fg_count + home_team$three_count)
away_team_pct_fg <- away_team$fg_count / (away_team$fg_count + away_team$three_count)
away_team_pct_th <- away_team$three_count / (away_team$fg_count + away_team$three_count)
  
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
    paste0("Home Team Wins Tipoff")
  } else {
    home_team_pos <- 0
    away_team_pos <- 1
    paste0("Away Team Wins Tipoff")
  }
  
  while (clock > 0) {
    
    #Home Team Posession
    if (home_team_pos == 1) {
      
      if(runif(1) > (home_team$tov_pct - (home_team$tov_pct*home_adv))) { #Testing for Turnover
        print(paste0("Home Team - No Turnover", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
        
        if (runif(1) <= home_team_pct_fg) { #Testing for Shot Type - FG
          print(paste0("Home Team - FG Attempt", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
          shot_type <- 2 #FG Shot Type
          home_team_fg_att <- home_team_fg_att + 1
          
          if (runif(1) > away_team$block_pct) { #Testing for Blocked Shot
            print(paste0("Home Team - No Blocked Shot", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
            
            if (runif(1) <= (home_team$fg_pct + (home_team$fg_pct*home_adv))) { #Testing if Shot is Made
    
              home_team_score <- home_team_score + 2 #Shot is Made
              
              #Calculating Time of Posession
              top <- runif(1, min=sc_min, max=sc_max)
              
              #Adjusting Clock Time
              clock <- clock - top
              
              print(paste0("Home Team - FG Successful", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
              
              #Posession Change
              home_team_pos <- 0
              away_team_pos <- 1
              
            } else { #Shot is Missed
              print(paste0("Home Team - Missed FG", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
              if ( runif(1) <= (home_team$orb_pct + (home_team$orb_pct*home_adv))) { #Testing for Offensive Rebound
                #Calculating Time of Posession
                top <- runif(1, min=sc_min, max=sc_max)
                
                #Adjusting Clock Time
                clock <- clock - top
                
                print(paste0("Home Team - Rebound Successful", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
                
              } else { #Defense grabs the rebound
                #Calculating Time of Posession
                top <- runif(1, min=sc_min, max=sc_max)
                
                #Adjusting Clock Time
                clock <- clock - top
                
                print(paste0("Home Team - Rebound Unsuccessful", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
                
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
            
            print(paste0("Home Team - Blocked Shot", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
            
            #Posession Change
            home_team_pos <- 0
            away_team_pos <- 1
          }
          
        } else { #Shot Type - Three Point
          shot_type <- 3 #Three Point Shot
          
          print(paste0("Home Team - 3PT Attempt", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
          
          if (runif(1) <= (home_team$three_pct + (home_team$three_pct*home_adv))) { #Testing if Shot is Made
            home_team_score <- home_team_score + 3 #Shot is Made
            
            #Calculating Time of Posession
            top <- runif(1, min=sc_min, max=sc_max)
            
            #Adjusting Clock Time
            clock <- clock - top
            
            print(paste0("Home Team - 3PT Successful", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
            
            #Posession Change
            home_team_pos <- 0
            away_team_pos <- 1
            
          } else { #Shot is Missed
            print(paste0("Home Team - Missed 3PT", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
            if ( runif(1) <= (home_team$orb_pct + (home_team$orb_pct*home_adv))) { #Testing for Offensive Rebound
              #Calculating Time of Posession
              top <- runif(1, min=sc_min, max=sc_max)
              
              #Adjusting Clock Time
              clock <- clock - top
              
              print(paste0("Home Team - Rebound Successful", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
              
            } else { #Defense grabs the rebound
              #Calculating Time of Posession
              top <- runif(1, min=sc_min, max=sc_max)
              
              #Adjusting Clock Time
              clock <- clock - top
              
              print(paste0("Home Team - Rebound Unsuccessful", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
              
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
        
        print(paste0("Home Team - Turnover", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
        
        #Posession Change
        home_team_pos <- 0
        away_team_pos <- 1
      }
    } else { #Away Team Posession
      
      away_team_pos == 1
      
      if(runif(1) > away_team$tov_pct) { #Testing for Turnover
        
        print(paste0("Away Team - No Turnover", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
        
        if (runif(1) <= away_team_pct_fg) { #Testing for Shot Type - FG
          shot_type <- 2 #FG Shot Type
          away_team_fg_att <- away_team_fg_att + 1
          
          print(paste0("Away Team - FG Attempt", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
          
          if (runif(1) > home_team$block_pct) { #Testing for Blocked Shot
            
            print(paste0("Away Team - No Blocked Shot", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
            
            if (runif(1) <= away_team$fg_pct) { #Testing if Shot is Made
              away_team_score <- away_team_score + 2 #Shot is Made
              
              #Calculating Time of Posession
              top <- runif(1, min=sc_min, max=sc_max)
              
              #Adjusting Clock Time
              clock <- clock - top
              
              print(paste0("Away Team - FG Successful", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
              
              #Posession Change
              home_team_pos <- 1
              away_team_pos <- 0
              
            } else { #Shot is Missed
              print(paste0("Away Team - Missed FG", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
              
              if ( runif(1) <= away_team$orb_pct) { #Testing for Offensive Rebound
                #Calculating Time of Posession
                top <- runif(1, min=sc_min, max=sc_max)
                
                #Adjusting Clock Time
                clock <- clock - top
                
                print(paste0("Away Team - Rebound Successful", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
                
              } else { #Defense grabs the rebound
                #Calculating Time of Posession
                top <- runif(1, min=sc_min, max=sc_max)
                
                #Adjusting Clock Time
                clock <- clock - top
                
                print(paste0("Away Team - Rebound Unsuccessful", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
                
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
            
            print(paste0("Away Team - Rebound Unsuccessful", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
            
            #Posession Change
            home_team_pos <- 1
            away_team_pos <- 0
          }
          
        } else { #Shot Type - Three Point
          shot_type <- 3 #Three Point Shot
          
          print(paste0("Away Team - 3PT Attempt", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
          
          if (runif(1) <= away_team$three_pct) { #Testing if Shot is Made
            away_team_score <- away_team_score + 3 #Shot is Made
            
            #Calculating Time of Posession
            top <- runif(1, min=sc_min, max=sc_max)
            
            #Adjusting Clock Time
            clock <- clock - top
            
            print(paste0("Away Team - 3PT Successful", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
            
            #Posession Change
            home_team_pos <- 1
            away_team_pos <- 0
            
          } else { #Shot is Missed
            print(paste0("Away Team - Missed 3PT", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
            
            if ( runif(1) <= away_team$orb_pct) { #Testing for Offensive Rebound
              #Calculating Time of Posession
              top <- runif(1, min=sc_min, max=sc_max)
              
              #Adjusting Clock Time
              clock <- clock - top
              
              print(paste0("Away Team - Rebound Successful", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
              
            } else { #Defense grabs the rebound
              #Calculating Time of Posession
              top <- runif(1, min=sc_min, max=sc_max)
              
              #Adjusting Clock Time
              clock <- clock - top
              
              print(paste0("Away Team - Rebound Unsuccessful", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
              
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
        
        print(paste0("Away Team - Turnover", " | Home Score: ", home_team_score, " | Away Score: ", away_team_score, " | Time Remaining: ", clock))
        
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
  paste0("Home Team FT pts: ", home_team_ft_points)
  
  #Away Team
  away_team_ft_att <- round(away_team_fg_att*away_team$ftr, digits = 0) #FT Attempted
  away_team_ft_points <- round(away_team_ft_att*away_team$ft_pct, digits = 0) #FT Made
  paste0("Away Team FT pts: ", away_team_ft_points)
  
  #Add to total score & Convert to type Int
  home_team_score <- as.integer(home_team_score + home_team_ft_points)
  away_team_score <- as.integer(away_team_score + away_team_ft_points)
  
  paste0("Home Team Score: ", home_team_score, " | Away Team Score: ", away_team_score)
  
  