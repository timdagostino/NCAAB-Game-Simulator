library(ggplot2)
library(ggthemes)
library(scales)
library(ggimage)
library(colorfindr)
library(ggchicklet)

loop <- 5
sim_sum <- data.frame()

test_game_sum <- data.frame(c(1, 2, 3, 4, 5),
                            c(71, 92, 83, 87, 84),
                            c(94, 83, 85, 69, 57),
                            c(-23, 9, -2, 18, 27),
                            c(0, 1, 0, 1, 1),
                            c(1, 0, 1, 0, 0))

sim_sum <- data.frame(rbind(sim_sum, test_game_sum))

names(sim_sum) <- c("Game", "Home", "Away", "Spread", "Home.Win", "Away.Win")

games_summary <- sim_sum %>% select(Home:Spread) 

overall_sum <- colSums(sim_sum)/loop
overall_sum <- overall_sum[-1]
overall_sum <- t(type.convert(data.frame(overall_sum)))
  

home_wp_df <- data.frame(team = "Home", wp = overall_sum[4], image = normalizePath(file.path("www",
                                                                                             paste0("Villanova", ".png"))))
away_wp_df <- data.frame(team = "Away", wp = overall_sum[5])

#Create WP Plot - Home
wp <- ggplot(home_wp_df, aes(team, wp, image = image)) +
  geom_text(size = 15, label = paste(home_wp_df[2]*100, "%"), nudge_y = .2) +
  coord_flip() +
  geom_chicklet(radius = grid::unit(15, 'mm'), fill = 'skyblue') +
  ylim(0,1.1) +
  geom_hline(yintercept=1)
wp + theme_void()



#Create WP Plot - Away
wp <- ggplot(away_wp_df, aes(team, -wp)) +
  coord_flip() +
  geom_bar(position = "stack", stat = "identity", fill = "gray48") +
  ylim(-1.3,0) +
  geom_text(size = 15, label = paste(away_wp_df[2]*100, "%"), nudge_x = .3) +
  geom_hline(yintercept=-1, size = 3, color = "lightgray")
wp + theme_void()

#-----------------------------------------

get_colors(normalizePath(file.path("www",
                                   paste0("Villanova", ".png"))), top_n = 1, get_stats = FALSE)


