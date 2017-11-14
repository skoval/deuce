## ----setup, include = FALSE, warning = FALSE-----------------------------
knitr::opts_chunk$set(echo = TRUE)

library(knitr)
library(htmlTable)
library(deuce)

datasets <- data(package = "deuce")

## ----eval = FALSE--------------------------------------------------------
#  library(devtools)
#  
#  devtools::install_github("skoval/deuce")

## ----eval = FALSE--------------------------------------------------------
#  library(deuce)

## ----echo = FALSE--------------------------------------------------------
table <- datasets$results[,c("Item","Title")]

colnames(table) <- c("Name", "Description")

htmlTable(table, 
          rnames = F, 
          col.rgroup = c("none", "#F7F7F7"),
          align = c("ll"),
          css.cell = "padding-left: 5%; padding-right:0%; padding-top: 2%;padding-bottom: 2%;width:40%;"
          )

## ----warning = FALSE, fig.height = 6, fig.width = 6----------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(scales)

data("atp_matches")
data("wta_matches")

# Subset matches to first round at grand slams
atp_matches <- atp_matches %>%
    filter(tourney_level == "Grand Slams", round == "R128", year >= 1990)

atp_age <- atp_matches %>%
  select(year, winner_age, loser_age) %>%
  gather("player", "age", winner_age, loser_age)

wta_matches <- wta_matches %>%
    filter(tourney_level == "Grand Slams", round == "R128", year >= 1990)

wta_age <- wta_matches %>%
  select(year, winner_age, loser_age) %>%
  gather("player", "age", winner_age, loser_age)

atp_age$tour <- "ATP"
wta_age$tour <- "WTA"

ages <- rbind(atp_age, wta_age)

ages %>%
  filter(!is.na(age)) %>%
  ggplot(aes(y = age, x = year, colour = tour)) + 
  stat_summary(fun.data = "mean_se", geom = "pointrange") + 
  scale_colour_tableau(name = "") + 
  theme_gdocs() + theme(legend.position = "bottom", legend.direction = "horizontal") + 
  scale_y_continuous("Mean Age (SE)", breaks = scales::pretty_breaks(n = 10)) + 
  scale_x_continuous("Year", breaks = scales::pretty_breaks(n = 10)) + 
  ggtitle("Aging Trends Among Grand Slam Players")

## ----warning = FALSE, fig.height = 5, fig.width = 6----------------------
data("atp_elo")
data("wta_elo")

peak_atp_elo <- atp_elo %>%
    group_by(player_name) %>%
    dplyr::summarise(
      peak.elo = max(overall_elo, na.rm = T)
    )

peak_atp_elo <- peak_atp_elo[order(peak_atp_elo$peak.elo, decreasing = T),][1:10,]

peak_wta_elo <- wta_elo %>%
    group_by(player_name) %>%
    dplyr::summarise(
      peak.elo = max(overall_elo, na.rm = T)
    )

peak_wta_elo <- peak_wta_elo[order(peak_wta_elo$peak.elo, decreasing = T),][1:10,]

peak_atp_elo$tour <- "ATP"
peak_wta_elo$tour <- "WTA"

peak_elo <- rbind(peak_atp_elo, peak_wta_elo)

peak_elo$player_name <- factor(peak_elo$player_name, levels = peak_elo$player_name[order(peak_elo$peak.elo)], order = T) 

peak_elo %>%
  ggplot(aes(y = peak.elo, x = player_name, colour = tour)) + 
  facet_wrap(~ tour, scales = "free") + 
  geom_point(size = 2) +
  scale_color_tableau() + 
  theme_gdocs() + theme(legend.position = "none") + 
  scale_y_continuous("Career Peak Elo") + 
  scale_x_discrete("") + 
  coord_flip()

## ----warning = FALSE, fig.height = 5, fig.width = 6----------------------
data("atp_matches")

atp_2016 <- atp_matches %>%
  filter(year == 2016) %>%
  dplyr::mutate(
    winner.serve.won = w_1stWon + w_2ndWon,
    loser.serve.won = l_1stWon + l_2ndWon,
    winner.points.won = (l_svpt - (loser.serve.won)) + winner.serve.won,
    loser.points.won = (w_svpt - (winner.serve.won)) + loser.serve.won,
    winner.breaks.won = l_bpFaced - l_bpSaved,
    loser.breaks.won = w_bpFaced - w_bpSaved
  )

atp_2016_winner <- atp_2016 %>%
  select(match_id, tourney_level, winner_name, winner.points.won, winner.breaks.won)

atp_2016_loser <- atp_2016 %>%
  select(match_id, tourney_level, loser_name, loser.points.won, loser.breaks.won)

names(atp_2016_winner) <- sub("winner", "player", names(atp_2016_winner))
names(atp_2016_loser) <- sub("loser", "player", names(atp_2016_loser))

atp_2016 <- rbind(atp_2016_winner, atp_2016_loser)

atp_2016 <- atp_2016 %>%
  group_by(match_id) %>%
  dplyr::mutate(
    opponent.points.won = player.points.won[2:1],
    opponent.breaks.won = player.breaks.won[2:1]
  )

atp_2016 <- atp_2016 %>%
  filter(!is.na(player.points.won)) %>%
  group_by(player_name) %>%
  dplyr::summarise(
    n = n(),
    slam = any(tourney_level == "Grand Slams"),
    pythag.points = sum(player.points.won)^2 / (sum(player.points.won)^2 + sum(opponent.points.won)^2),
    pythag.breaks = sum(player.breaks.won)^2 / (sum(player.breaks.won)^2 + sum(opponent.breaks.won)^2)
  )

atp_2017 <- atp_matches %>%
  filter(year == 2017) %>%
  select(match_id, winner_name, loser_name) %>%
  gather("result", "player_name", winner_name, loser_name)

atp_2017 <- atp_2017 %>%
  group_by(player_name) %>%
  dplyr::summarise(
    wins = mean(result == "winner_name")
  )

# Combine for player with more than 50 matches and grand slam appearance
atp_stats <- merge(
  atp_2016 %>% filter(n >= 50, slam),
  atp_2017,
  by = "player_name"
)

atp_stats %>%
  ggplot(aes(y = wins * 100, x = pythag.points * 100)) + 
  geom_point(size = 2, col = tableau_color_pal()(2)[1]) + 
 geom_smooth(method = "lm", level = 0, col = tableau_color_pal()(2)[1], alpha = 0.5) +  
  scale_y_continuous("2017 Win Percentage", lim = c(0, 100)) + 
  scale_x_continuous("2016 Pythagorean Expectation", lim = c(0, 100)) + 
  theme_bw() + 
  ggtitle("Pythagorean for Points Won")

atp_stats %>%
  ggplot(aes(y = wins * 100, x = pythag.breaks * 100)) + 
  geom_point(size = 2, col = tableau_color_pal()(2)[2]) + 
  geom_smooth(method = "lm", level = 0, col = tableau_color_pal()(2)[2], alpha = 0.5) +
  scale_y_continuous("2017 Win Percentage", lim = c(0, 100)) + 
  scale_x_continuous("2016 Pythagorean Expectation", lim = c(0, 100)) + 
  theme_bw() + 
  ggtitle("Pythagorean for Break Points Won")

## ----warning = FALSE-----------------------------------------------------
library(BradleyTerry2)

data("wta_matches")

wta_matches <- wta_matches %>%
  filter(year == 2017, tourney_level %in% c("Grand Slams", "Premier","Premier Mandatory")) %>%
  dplyr::mutate(
    outcome = 1
  )

player.levels <- unique(c(as.character(wta_matches$winner_name), as.character(wta_matches$loser_name)))
  
wta_matches$winner_name <- factor(wta_matches$winner_name, levels = player.levels)
wta_matches$loser_name <- factor(wta_matches$loser_name, levels = player.levels)

fit <- BTm(wta_matches$outcome, wta_matches$winner_name, wta_matches$loser_name)

abilities <- BTabilities(fit)

# Top 20
abilities[order(abilities[,1], decreasing = T),][1:20,]

## ----warning = FALSE, fig.height = 5, fig.width = 6----------------------
data("gs_point_by_point")

gs_point_by_point <- gs_point_by_point %>%
  dplyr::mutate(
    ServeNumber = ifelse((is.na(ServeNumber) | ServeNumber == 0) &
                         (P1FirstSrvIn == 1 | P2FirstSrvIn == 1), 1,
                         ifelse((is.na(ServeNumber) | ServeNumber == 0), 2, ServeNumber))
  )


gs_point_by_point %>%
  filter(!is.na(ServeNumber), Speed_MPH != 0) %>%
  ggplot(aes(y = Speed_MPH, x = Tour, fill = Tour)) + 
  geom_boxplot(alpha = 0.5) + 
  scale_fill_tableau() + 
  theme_gdocs() + theme(legend.position = "none") + 
  facet_grid(. ~ factor(ServeNumber, labels = c("First Serve", "Second Serve"))) + 
  scale_y_continuous("Service Speed (MPH)", breaks = scales::pretty_breaks(n = 10)) 

## ----warning = FALSE, fig.height = 5, fig.width = 6----------------------
data("mcp_points")

# Cound double faults as 1 shot
mcp_points <- mcp_points %>%
  dplyr::mutate(
    year = as.numeric(substr(match_id, 1, 4)),
    ATP = ifelse(grepl("[0-9]-M-", match_id), "ATP", "WTA"),
    rallyCount = as.numeric(ifelse(rallyCount == 0, 1, rallyCount))
  ) %>%
  filter(year >= 2000, !is.na(rallyCount))

mcp_points %>%
  ggplot(aes(y = rallyCount, x = year, fill = ATP, colour = ATP)) + 
  geom_smooth(alpha = 0.3) + 
  scale_fill_tableau(name = "") + 
  scale_colour_tableau(name = "") + 
  scale_y_continuous("Rally Length", breaks = scales::pretty_breaks(n = 10)) + 
  scale_x_continuous("", breaks = scales::pretty_breaks(n = 10)) + 
  expand_limits(y = 1) + 
  theme_gdocs() + theme(legend.position = "bottom", legend.direction = "horizontal") + 
  ggtitle("Rally Length Trends")

## ----echo = FALSE--------------------------------------------------------
table <- data.frame(
  Name = c("elo_prediction", "fetch_activity", "fetch_atp_rankings", "fetch_atp_tournaments", 
"fetch_draw", "fetch_head_to_head", "fetch_wta_rankings", "in_match_win", 
"match_win"),
  Description = c("Predict match outcomes based on Elo ratings",
                   "Download ATP match activity", "Download ATP rankings", "Download ATP tournament calendar", "Download tournament draws", "Download player head to head results", "Download current WTA rankings", "Calculating point-by-point match win probabilities", "Calculate pre-match win probability")
)

htmlTable(table, 
          rnames = F, 
          col.rgroup = c("none", "#F7F7F7"),
          align = c("ll"),
          css.cell = "padding-left: 5%; padding-right:0%; padding-top: 2%;padding-bottom: 2%;width:40%;"
          )


## ----warning = FALSE-----------------------------------------------------
fedal <- fetch_head_to_head("Roger Federer", "Rafael Nadal")

head(fedal)

nrow(fedal)

## ----echo = FALSE--------------------------------------------------------
results <- fedal %>%
  gather("result", "player", winner:loser) %>%
  group_by(player, surface) %>%
  dplyr::summarise(
    wins = sum(result == "winner"),
    losses = sum(result == "loser")
  )

results[order(results$wins, decreasing = T),]

results %>% 
  group_by(player) %>%
  dplyr::summarise(
    wins = sum(wins)
  )

## ----echo = FALSE--------------------------------------------------------
rankings <- fetch_wta_rankings(1, 100)

rankings[1:10,]

## ----echo = FALSE, fig.height = 6, fig.width = 6-------------------------
params <- data.frame(
  serve = rep(0.65, 12),
  opponent = rep(seq(0.6, 0.7, by = 0.02), 2),
  bestof3 = rep(c(T, F), each = 6)
)

params$win <- mapply(
  match_win,
  serve = params$serve,
  return = 1 - params$opponent,
  bestof3 = params$bestof3
)

params %>%
  ggplot(aes(y = win * 100, x = opponent * 100, 
             col = factor(bestof3, labels = c("best of 5", "best of 3")))) + 
           geom_line() + 
           scale_colour_tableau(name = "Format") +
           theme_gdocs() + 
          scale_y_continuous("Match Win %") + 
          scale_x_continuous("Opponent Serve Win %") + 
           theme(legend.position = "bottom")

## ----echo = FALSE--------------------------------------------------------
in_match_win(0, 0, 4, 3, 0, 0, 0.65, 0.68, advantage = F, bestof3 = T)

## ----echo = FALSE--------------------------------------------------------
in_match_win(0, 0, 0, 0, 1, 0, 0.65, 0.68, advantage = F, bestof3 = T)

## ----echo = FALSE--------------------------------------------------------
in_match_win(0, 0, 0, 0, 1, 1, 0.65, 0.68, advantage = F, bestof3 = T)

