library(dplyr)
library(skellam)
library(ggplot2)
library(tidyr)
library(readr)
library(stringr)
library(RCurl)

team_data_1 <- read_csv("team_data_1.csv")

extractYear <- function(date) {
  if(length(grep("2015", date)) > 0) {
    return("2015")
  } else if(length(grep("2016", date)) > 0) {
    return("2016")
  } else if(length(grep("2017", date)) > 0) {
    return("2017")
  } else if(length(grep("2018", date)) > 0) {
    return("2018")
  } else if(length(grep("2019", date)) > 0) {
    return("2019")
  } else if(length(grep("2020", date)) > 0) {
    return("2020")
  } else {
    return("")
  }
}

year <- NULL
for (i in 1:nrow(team_data_1)) {
  year <- c(year, extractYear(team_data_1[i,"date"]))
}
year <- as.factor(year)

### Extracting pitcher's names

extractPitcherName <- function(opp_pitcher) {
  if(length(grep("Max Fried", opp_pitcher)) > 0) {
    return("Max Fried")
  } else if(length(grep("Tucker Davidson", opp_pitcher)) > 0) {
    return("Tucker Davidson")
  } else if(length(grep("Charlie Morton", opp_pitcher)) > 0) {
    return("Charlie Morton")
  } else if(length(grep("Ian Anderson", opp_pitcher)) > 0) {
    return("Ian Anderson")
  } else if(length(grep("Drew Smyly", opp_pitcher)) > 0) {
    return("Drew Smyly")
  } else if(length(grep("Huascar Ynoa", opp_pitcher)) > 0) {
    return("Huascar Ynoa")
  } else if(length(grep("Aaron Nola", opp_pitcher)) > 0) {
    return("Aaron Nola")
  } else if(length(grep("Zach Wheeler", opp_pitcher)) > 0) {
    return("Zach Wheeler")
  } else if(length(grep("Chase Anderson", opp_pitcher)) > 0) {
    return("Chase Anderson")
  } else if(length(grep("Zach Eflin", opp_pitcher)) > 0) {
    return("Zach Eflin")
  } else if(length(grep("Matt Moore", opp_pitcher)) > 0) {
    return("Matt Moore")
  } else {
    return("")
  }
}

opposing_pitcher <- NULL
for (i in 1:nrow(team_data_1)) {
  opposing_pitcher <- c(opposing_pitcher, extractPitcherName(team_data_1[i,"opp_pitcher"]))
}
opposing_pitcher <- as.factor(opposing_pitcher)

overalldata <- cbind(team_data_1, year, opposing_pitcher)

### Creating data sets

### 2015

season_2015_home <- overalldata[ which(overalldata$year == "2015" & overalldata$`home?` == 1), ]
season_2015_away <- overalldata[ which(overalldata$year == "2015" & overalldata$`home?` == 0), ]

### 2016

season_2016_home <- overalldata[ which(overalldata$year == "2016" & overalldata$`home?` == 1), ]
season_2016_away <- overalldata[ which(overalldata$year == "2016" & overalldata$`home?` == 0), ]

### 2017

season_2017_home <- overalldata[ which(overalldata$year == "2017" & overalldata$`home?` == 1), ]
season_2017_away <- overalldata[ which(overalldata$year == "2017" & overalldata$`home?` == 0), ]

### 2018

season_2018_home <- overalldata[ which(overalldata$year == "2018" & overalldata$`home?` == 1), ]
season_2018_away <- overalldata[ which(overalldata$year == "2018" & overalldata$`home?` == 0), ]

### 2019

season_2019_home <- overalldata[ which(overalldata$year == "2019" & overalldata$`home?` == 1), ]
season_2019_away <- overalldata[ which(overalldata$year == "2019" & overalldata$`home?` == 0), ]

### 2020

season_2020_home <- overalldata[ which(overalldata$year == "2020" & overalldata$`home?` == 1), ]
season_2020_away <- overalldata[ which(overalldata$year == "2020" & overalldata$`home?` == 0), ]

### Establishing Poisson models for each season

### Poisson model for 2015 season

poisson_model_2015 <- rbind(
  data.frame(runs = season_2015_home$runs_scored,
             team = season_2015_home$team,
             opponent = season_2015_home$opp,
             home = 1),
  data.frame(runs = season_2015_away$runs_scored,
             team = season_2015_away$team,
             opponent = season_2015_away$opp,
             home = 0)) %>%
  glm(runs ~ team + opponent + home, family = poisson(link = "log"), data=.)

### Poisson model for 2016 season

poisson_model_2016 <- rbind(
  data.frame(runs = season_2016_home$runs_scored,
             team = season_2016_home$team,
             opponent = season_2016_home$opp,
             home = 1),
  data.frame(runs = season_2016_away$runs_scored,
             team = season_2016_away$team,
             opponent = season_2016_away$opp,
             home = 0)) %>%
  glm(runs ~ team + opponent + home, family = poisson(link = "log"), data=.)

### Poisson model for 2017 season

poisson_model_2017 <- rbind(
  data.frame(runs = season_2017_home$runs_scored,
             team = season_2017_home$team,
             opponent = season_2017_home$opp,
             home = 1),
  data.frame(runs = season_2017_away$runs_scored,
             team = season_2017_away$team,
             opponent = season_2017_away$opp,
             home = 0)) %>%
  glm(runs ~ team + opponent + home, family = poisson(link = "log"), data=.)

### Poisson model for 2018 season

poisson_model_2018 <- rbind(
  data.frame(runs = season_2018_home$runs_scored,
             team = season_2018_home$team,
             opponent = season_2018_home$opp,
             home = 1),
  data.frame(runs = season_2018_away$runs_scored,
             team = season_2018_away$team,
             opponent = season_2018_away$opp,
             home = 0)) %>%
  glm(runs ~ team + opponent + home, family = poisson(link = "log"), data=.)

### Poisson model for 2019 season

poisson_model_2019 <- rbind(
  data.frame(runs = season_2019_home$runs_scored,
             team = season_2019_home$team,
             opponent = season_2019_home$opp,
             home = 1),
  data.frame(runs = season_2019_away$runs_scored,
             team = season_2019_away$team,
             opponent = season_2019_away$opp,
             home = 0)) %>%
  glm(runs ~ team + opponent + home, family = poisson(link = "log"), data=.)

### Poisson model for 2020 season

poisson_model_2020 <- rbind(
  data.frame(runs = season_2020_home$runs_scored,
             team = season_2020_home$team,
             opponent = season_2020_home$opp,
             home = 1),
  data.frame(runs = season_2020_away$runs_scored,
             team = season_2020_away$team,
             opponent = season_2020_away$opp,
             home = 0)) %>%
  glm(runs ~ team + opponent + home, family = poisson(link = "log"), data=.)

### Poisson model overall

poisson_model_overall <- rbind(
  data.frame(runs = overalldata$runs_scored,
             team = overalldata$team,
             opponent = overalldata$opp,
             
             home = 1),
  data.frame(runs = overalldata$runs_scored,
             team = overalldata$team,
             opponent = overalldata$opp,
             home = 0)) %>%
  glm(runs ~ team + opponent + home, family = poisson(link = "log"), data=.)

### Poisson model for pitchers

poisson_model_pitcher <- rbind(
  data.frame(runs = overalldata$runs_scored,
             team = overalldata$team,
             opponent = overalldata$opp,
             opppitcher = overalldata$opposing_pitcher,
             home = 1),
  data.frame(runs = overalldata$runs_scored,
             team = overalldata$team,
             opponent = overalldata$opp,
             opppitcher = overalldata$opposing_pitcher,
             home = 0)) %>%
  glm(runs ~ team + opponent + home + opppitcher, family = poisson(link = "log"), data=.)

### Creating game simulation function

simulate_game <- function(hometeam, homepitcher, awayteam, awaypitcher) {
  avg_runs_scored_home_2015 <- predict(poisson_model_2015,
                                       data.frame(home = 1, team = hometeam, opponent = awayteam),
                                       type = "response")
  avg_runs_scored_away_2015 <- predict(poisson_model_2015,
                                       data.frame(home = 0, team = awayteam, opponent = hometeam),
                                       type = "response")
  avg_runs_scored_home_2016 <- predict(poisson_model_2016,
                                       data.frame(home = 1, team = hometeam, opponent = awayteam),
                                       type = "response")
  avg_runs_scored_away_2016 <- predict(poisson_model_2016,
                                       data.frame(home = 0, team = awayteam, opponent = hometeam),
                                       type = "response")
  avg_runs_scored_home_2017 <- predict(poisson_model_2017,
                                       data.frame(home = 1, team = hometeam, opponent = awayteam),
                                       type = "response")
  avg_runs_scored_away_2017 <- predict(poisson_model_2017,
                                       data.frame(home = 0, team = awayteam, opponent = hometeam),
                                       type = "response")
  avg_runs_scored_home_2018 <- predict(poisson_model_2018,
                                       data.frame(home = 1, team = hometeam, opponent = awayteam),
                                       type = "response")
  avg_runs_scored_away_2018 <- predict(poisson_model_2018,
                                       data.frame(home = 0, team = awayteam, opponent = hometeam),
                                       type = "response")
  avg_runs_scored_home_2019 <- predict(poisson_model_2019,
                                       data.frame(home = 1, team = hometeam, opponent = awayteam),
                                       type = "response")
  avg_runs_scored_away_2019 <- predict(poisson_model_2019,
                                       data.frame(home = 0, team = awayteam, opponent = hometeam),
                                       type = "response")
  avg_runs_scored_home_2020 <- predict(poisson_model_2020,
                                       data.frame(home = 1, team = hometeam, opponent = awayteam),
                                       type = "response")
  avg_runs_scored_away_2020 <- predict(poisson_model_2020,
                                       data.frame(home = 0, team = awayteam, opponent = hometeam),
                                       type = "response")
  avg_runs_scored_home_total <- predict(poisson_model_overall,
                                        data.frame(home = 1, team = hometeam, opponent = awayteam),
                                        type = "response")
  avg_runs_scored_away_total <- predict(poisson_model_overall,
                                        data.frame(home = 0, team = awayteam, opponent = hometeam),
                                        type = "response")
  avg_runs_scored_home_pitcher <- predict(poisson_model_pitcher,
                                        data.frame(home = 1, team = hometeam, opponent = awayteam,
                                                   opppitcher = awaypitcher), type = "response")
  avg_runs_scored_away_pitcher <- predict(poisson_model_pitcher,
                                          data.frame(home = 0, team = awayteam, opponent = hometeam,
                                                     opppitcher = homepitcher), type = "response")
  avg_runs_scored_home_overall <- ((avg_runs_scored_home_2015 * 1) + (avg_runs_scored_home_2016 * 1)
                                  + (avg_runs_scored_home_2017 * 2) + (avg_runs_scored_home_2018 * 2)
                                  + (avg_runs_scored_home_2019 * 3) 
                                  + (avg_runs_scored_home_2020 * 3) 
                                  + (avg_runs_scored_home_pitcher * 2))/14
  avg_runs_scored_away_overall <- ((avg_runs_scored_away_2015 * 1) + (avg_runs_scored_away_2016 * 1)
                                  + (avg_runs_scored_away_2017 * 2) + (avg_runs_scored_away_2018 * 2)
                                  + (avg_runs_scored_away_2019 * 3) 
                                  + (avg_runs_scored_away_2020 * 3)
                                  + (avg_runs_scored_away_pitcher *2))/14
  dpois(0:10, avg_runs_scored_home_overall) %o% dpois(0:10, avg_runs_scored_away_overall)
}

### lower.tri tells probability home team wins
### upper.tri tells probability away team wins
### diag tells probability game goes to extra innings

home_team_win <- function(home_team, home_pitcher, away_team, away_pitcher) {
  game <- simulate_game(hometeam = home_team, homepitcher = home_pitcher, awayteam = away_team, 
                        awaypitcher = away_pitcher)
  sum(game[lower.tri(game)])
}

away_team_win <- function(home_team, home_pitcher, away_team, away_pitcher) {
  game <- simulate_game(hometeam = home_team, homepitcher = home_pitcher, awayteam = away_team, 
                        awaypitcher = away_pitcher)
  sum(game[upper.tri(game)])
}

extra_innings <- function(home_team, home_pitcher, away_team, away_pitcher) {
  game <- simulate_game(hometeam = home_team, homepitcher = home_pitcher, awayteam = away_team, 
                        awaypitcher = away_pitcher)
  sum(diag(game))
}

home_team_win("PHI", "Aaron Nola", "ATL", "Max Fried")
away_team_win("PHI", "Aaron Nola", "ATL", "Max Fried")
extra_innings("PHI", "Aaron Nola", "ATL", "Max Fried")

