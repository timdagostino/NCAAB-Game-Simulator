library (rvest)

#Insert URL ------
URL <- 'https://www.sports-reference.com/cbb/seasons/2021-school-stats.html'

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
conf_wins_html <- html_nodes(webpage, '.right:nth-child(10)')
conf_wins <- html_text(conf_wins_html)
conf_wins <- as.numeric(conf_wins)

#Conference Losses
conf_losses_html <- html_nodes(webpage, '.right:nth-child(11)')
conf_losses <- html_text(conf_losses_html)
conf_losses <- as.numeric(conf_losses)

#Home Wins
home_wins_html <- html_nodes(webpage, '.right:nth-child(13)')
home_wins <- html_text(home_wins_html)
home_wins <- as.numeric(home_wins)

#Home Losses
home_losses_html <- html_nodes(webpage, '.right:nth-child(14)')
home_losses <- html_text(home_losses_html)
home_losses <- as.numeric(home_losses)

#Away Wins
away_wins_html <- html_nodes(webpage, '.right:nth-child(16)')
away_wins <- html_text(away_wins_html)
away_wins <- as.numeric(away_wins)

#Away Losses
away_losses_html <- html_nodes(webpage, '.right:nth-child(17)')
away_losses <- html_text(away_losses_html)
away_losses <- as.numeric(away_losses)

#Total Points Scored (season)
tp_html <- html_nodes(webpage, '.right:nth-child(19)')
tp <- html_text(tp_html)
tp <- as.numeric(tp)

#Total Points Allowed (season)
tp_allowed_html <- html_nodes(webpage, '.right:nth-child(20)')
tp_allowed <- html_text(tp_allowed_html)
tp_allowed <- as.numeric(tp_allowed)

#Minutes Played
mp_html <- html_nodes(webpage, '.right:nth-child(22)')
mp <- html_text(mp_html)
mp <- as.numeric(mp)

#Field Goals
fg_count_html <- html_nodes(webpage, '.right:nth-child(23)')
fg_count <- html_text(fg_count_html)
fg_count <- as.numeric(fg_count)

#Field Goals Attempted
fg_att_html <- html_nodes(webpage, '.right:nth-child(24)')
fg_att <- html_text(fg_att_html)
fg_att <- as.numeric(fg_att)

#Field Goal Percentage
fg_pct_html <- html_nodes(webpage, '.right:nth-child(25)')
fg_pct <- html_text(fg_pct_html)
fg_pct <- as.numeric(fg_pct)

#Three Pointers
three_count_html <- html_nodes(webpage, '.right:nth-child(26)')
three_count <- html_text(three_count_html)
three_count <- as.numeric(three_count)

#Three Pointers Attempted
three_att_html <- html_nodes(webpage, '.right:nth-child(27)')
three_att <- html_text(three_att_html)
three_att <- as.numeric(three_att)

#Three Pointers Percentage
three_pct_html <- html_nodes(webpage, '.right:nth-child(28)')
three_pct <- html_text(three_pct_html)
three_pct <- as.numeric(three_pct)

#Free Throws
ft_count_html <- html_nodes(webpage, '.right:nth-child(29)')
ft_count <- html_text(ft_count_html)
ft_count <- as.numeric(ft_count)

#Free Throws Attempted
ft_att_html <- html_nodes(webpage, '.right:nth-child(30)')
ft_att <- html_text(ft_att_html)
ft_att <- as.numeric(ft_att)

#Free Throw Percentage
ft_pct_html <- html_nodes(webpage, '.right:nth-child(31)')
ft_pct <- html_text(ft_pct_html)
ft_pct <- as.numeric(ft_pct)

#Offensive Rebounds
orb_html <- html_nodes(webpage, '.right:nth-child(32)')
orb <- html_text(orb_html)
orb <- as.numeric(orb)

#Total Rebounds
trb_html <- html_nodes(webpage, '.right:nth-child(33)')
trb <- html_text(trb_html)
trb <- as.numeric(trb)

#Steals
steals_html <- html_nodes(webpage, '.right:nth-child(35)')
steals <- html_text(steals_html)
steals <- as.numeric(steals)

#Blocks
blk_html <- html_nodes(webpage, '.right:nth-child(36)')
blk <- html_text(blk_html)
blk <- as.numeric(blk)

#Turnovers
tov_html <- html_nodes(webpage, '.right:nth-child(37)')
tov <- html_text(tov_html)
tov <- as.numeric(tov)

#Personal Fouls
pf_html <- html_nodes(webpage, '.right:nth-child(38)')
pf <- html_text(pf_html)
pf <- as.numeric(pf)

#Advanced Team Stats ------------------------------

#Insert URL
URL <- 'https://www.sports-reference.com/cbb/seasons/2021-advanced-school-stats.html'

#Read the HTML code from the website

webpage_adv <- read_html(URL)

#Pace of Game (An estimate of school posessions per 40 minutes)
pace_html <- html_nodes(webpage_adv, '.right:nth-child(22)')
pace <- html_text(pace_html)
pace <- as.numeric(pace)

#Offensive Rating (An estimate of points scored per 100 posessions)
ortg_html <- html_nodes(webpage_adv, '.right:nth-child(23)')
ortg <- html_text(ortg_html)
ortg <- as.numeric(ortg)

#Free Throw Attempt Rate (Number of Free  Throw Attempts per FG attempt)
ftr_html <- html_nodes(webpage_adv, '.right:nth-child(24)')
ftr <- html_text(ftr_html)
ftr <- as.numeric(ftr)

#True Shooting Percentage (A measure of shooting efficinecy that takes into account all shot types)
ts_pct_html <- html_nodes(webpage_adv, '.right:nth-child(26)')
ts_pct <- html_text(ts_pct_html)
ts_pct <- as.numeric(ts_pct)

#Total Rebound Percentage (An estimate of the percetnage of available rebounds a player grabbed while on the floor)
trb_pct_html <- html_nodes(webpage_adv, '.right:nth-child(27)')
trb_pct <- html_text(trb_pct_html)
trb_pct <- as.numeric(trb_pct)
trb_pct <- (trb_pct/100)

#Steal Percentage (An estimate of the number of opponent posessions that end with a steal)
steal_pct_html <- html_nodes(webpage_adv, '.right:nth-child(29)')
steal_pct <- html_text(steal_pct_html)
steal_pct <- as.numeric(steal_pct)
steal_pct <- (steal_pct/100)

#Block Percentage (An estimate of the percentage of opponent two-point FG attempts that are blocked)
block_pct_html <- html_nodes(webpage_adv, '.right:nth-child(30)')
block_pct <- html_text(block_pct_html)
block_pct <- as.numeric(block_pct)
block_pct <- (block_pct/100)

#Turnover Percentage (An estimate of turnovers per 100 plays)
tov_pct_html <- html_nodes(webpage_adv, '.right:nth-child(32)')
tov_pct <- html_text(tov_pct_html)
tov_pct <- as.numeric(tov_pct)
tov_pct <- (tov_pct/100)

#Offensive Rebound Percentage (An estimate of the percentage of available offensive rebounds grabbed by a player on the floor)
orb_pct_html <- html_nodes(webpage_adv, '.right:nth-child(33)')
orb_pct <- html_text(orb_pct_html)
orb_pct <- as.numeric(orb_pct)
orb_pct <- (orb_pct/100)

#Basic Opponent Stats ------------------------------

#Insert URL
URL <- 'https://www.sports-reference.com/cbb/seasons/2021-opponent-stats.html'

#Read the HTML code from the website

webpage_adv <- read_html(URL)

#Opp. Field Goals
opp_fg_pct_html <- html_nodes(webpage_adv, '.right:nth-child(25)')
opp_fg_pct <- html_text(opp_fg_pct_html)
opp_fg_pct <- as.numeric(opp_fg_pct)

#Opp. Three Point Percentage
opp_three_pct_html <- html_nodes(webpage_adv, '.right:nth-child(28)')
opp_three_pct <- html_text(opp_three_pct_html)
opp_three_pct <- as.numeric(opp_three_pct)

#Advanced Opponent Stats ------------------------------

#Insert URL
URL <- 'https://www.sports-reference.com/cbb/seasons/2021-advanced-opponent-stats.html'

#Read the HTML code from the website

webpage_adv <- read_html(URL)

#Opp. Block Percentage
opp_block_pct_html <- html_nodes(webpage_adv, '.right:nth-child(30)')
opp_block_pct <- html_text(opp_block_pct_html)
opp_block_pct <- as.numeric(opp_block_pct)
opp_block_pct <- (opp_block_pct/100)

#Opp. Turnover Percentage
opp_tov_pct_html <- html_nodes(webpage_adv, '.right:nth-child(32)')
opp_tov_pct <- html_text(opp_tov_pct_html)
opp_tov_pct <- as.numeric(opp_tov_pct)
opp_tov_pct <- (opp_tov_pct/100)

#Opp. Offensive Rebound Percentage
opp_orb_pct_html <- html_nodes(webpage_adv, '.right:nth-child(33)')
opp_orb_pct <- html_text(opp_orb_pct_html)
opp_orb_pct <- as.numeric(opp_orb_pct)
opp_orb_pct <- (opp_orb_pct/100)


#Create Dataframe
ncaa_team_stats_2021 <- data.frame(team_name, rank, overall_games, 
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
                                   orb_pct, opp_fg_pct, opp_three_pct, 
                                   opp_block_pct, opp_tov_pct, opp_orb_pct)



team_name_unique <- sort(unique(ncaa_team_stats_2021$team_name))

#Establish Bins
sos_bins <- cut(ncaa_team_stats_2021$sos, 7, include.lowest=TRUE, 
                labels=c("Very Weak", "Weak", "Moderately Weak", "Average", "Moderately Strong", "Strong", "Very Strong"))

ncaa_team_stats_2021 <- cbind(ncaa_team_stats_2021,sos_bins)