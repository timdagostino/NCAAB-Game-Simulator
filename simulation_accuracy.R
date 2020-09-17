library (dplyr)
library (pastecs)

complete_analysis <- data.frame()


#Begin HERE---

#Generate Random Date
date <- sample(seq(as.Date('2019/01/01'), as.Date('2020/01/01'), by="day"), 1)

#Generate/Predict Sample Scores
pred_home_score <- sample(70:100, 10, replace = TRUE)
pred_away_score <- sample(70:100, 10, replace = TRUE)
pred_home_line <- (pred_home_score - pred_away_score)*-1
pred_total <- (pred_home_score + pred_away_score)

#Generate/Scrape Betting Lines
over_under <- sample(150:190, 10, replace = TRUE)
home_line <- sample(-10:10, 10, replace = TRUE)

#Generate DF with Game Predictions and Gambling Lines
prelim_predictions_gl <- data.frame(
  "game_id" = 1:10,
  "date" = date,
  "home_team" = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
  "away_team" = c("AA", "BB", "CC", "DD", "EE", "FF", "GG", "HH", "II", "JJ"),
  "pred_home_score" = pred_home_score,
  "pred_away_score" = pred_away_score,
  "pred_home_line" = pred_home_line,
  "pred_total" = pred_total,
  "over_under" = over_under,
  "home_line" = home_line)

final_perdictions_gl <- prelim_predictions_gl %>%
  mutate(over_under_pred = ifelse(over_under <= pred_total, "OVER", "UNDER")) %>%
  mutate(line_pred = ifelse(home_line >= pred_home_line, "HOME COVER", "AWAY COVER"))

#Generate/Scrape Real Scores
real_home_score <- sample(70:100, 10, replace = TRUE)
real_away_score <- sample(70:100, 10, replace = TRUE)
real_spread <- (real_home_score - real_away_score)
real_total <- (real_home_score + real_away_score)

#Generate DF with Real Game Outcome Data
real_outcomes <- data.frame(
  "game_id" = 1:10,
  "home_team" = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
  "away_team" = c("AA", "BB", "CC", "DD", "EE", "FF", "GG", "HH", "II", "JJ"),
  "real_home_score" = real_home_score,
  "real_away_score" = real_away_score,
  "real_spread" = real_spread,
  "real_total" = real_total)

#Merge Predictions DF and Real Game Outcome DF
pred_and_real <- merge(final_perdictions_gl, real_outcomes[, c("game_id", "real_home_score", "real_away_score", "real_spread", "real_total")], by = "game_id")

#Compute Game Analysis and Append to Overall Data Table
analysis <- pred_and_real %>%
  mutate(spread_var = real_spread - pred_home_line) %>%
  mutate(vegas_spread_var = real_spread - home_line) %>%
  mutate(total_var = real_total - pred_total) %>%
  mutate(vegas_total_var = real_total - over_under) %>%
  mutate(line_cover = if_else(real_spread >= pred_home_line, "HOME COVER", "AWAY COVER")) %>%
  mutate(over_under_result = if_else(real_total >= over_under & over_under_pred == "OVER", "CORRECT", 
                                     if_else(real_total <= over_under & over_under_pred == "UNDER", "CORRECT", "INCORRECT"))) %>%
  mutate(line_cover_result = if_else(line_cover == line_pred, "CORRECT", "INCORRECT"))

#Add to Final Dataset
complete_analysis <- rbind(complete_analysis, analysis)

#End HERE-----

#Over/Under Accuracy Summary
over_under_acc <- round(prop.table(table(complete_analysis$over_under_pred, complete_analysis$over_under_result), 1),2)

#Game Line Accuracy Summary
line_acc <- round(prop.table(table(complete_analysis$line_pred, complete_analysis$line_cover_result), 1),2)

