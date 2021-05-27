# Load libraries
library(devtools)
library(tidyverse)
library(caTools)

#Function for Loading Play by Play Data
load_data <- function(start_year, end_year) {
  # For each year, load data
  data <- data.frame()
  for (year in seq(start_year, end_year, by = 1)) {
    # Download file to cache if not present
    url2 = str_interp("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_${year}.csv")
    pbp_single_year <- read_csv(url(url2))
    data <- bind_rows(data, pbp_single_year)
    url3 = str_interp("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/post_season/post_pbp_${year}.csv")
    pbp_single_year <- read_csv(url(url3))
    data <- bind_rows(data, pbp_single_year)
  }
  return(data)
}

#Function for Loading Game Data
load_data2 <- function(start_year, end_year) {
  # For each year, load data
  data <- data.frame()
  for (year in seq(start_year, end_year, by = 1)) {
    # Download file to cache if not present
    url2 = str_interp("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_${year}.csv")
    pbp_single_year <- read_csv(url(url2))
    data <- bind_rows(data, pbp_single_year)
    url3 = str_interp("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/post_season/post_games_${year}.csv")
    pbp_single_year <- read_csv(url(url3))
    data <- bind_rows(data, pbp_single_year)
  }
  return(data)
}

games<-load_data2(2016,2019)

pbp<-load_data(2016,2019)

gid<-table(games$game_id)
pid<-table(pbp$game_id)

#Joining the Two data sets
pbp_final=full_join(games,pbp,by="game_id")

#Creating the binomial Poswin variable
pbp_final = pbp_final %>% mutate(winner = ifelse(home_score > away_score, home_team.x, away_team.x))
pbp_final = pbp_final %>% mutate(poswins = ifelse(winner == posteam, "Yes","No"))
pbp_final$qtr = as.factor(pbp_final$qtr) 
pbp_final$down = as.factor(pbp_final$down)
pbp_final$poswins = as.factor(pbp_final$poswins)

#Filtering the data to only show relevant plays
pbp_reduced = pbp_final %>% filter(play_type != "No Play" & qtr != 5 & down != "NA" & poswins != "NA") %>% select(game_id, game_date, posteam, home_team.x, away_team.x, winner, qtr, down, ydstogo, game_seconds_remaining, yardline_100, score_differential, poswins)


#Setting the seed and a test train split of 80 to 20. 
set.seed(123)
split = sample.split(pbp_reduced$poswins, SplitRatio = 0.8)
train = pbp_reduced %>% filter(split == TRUE)
test = pbp_reduced %>% filter(split == FALSE)

#Binomial Model
model1 = glm(poswins ~ qtr + down + ydstogo + game_seconds_remaining + yardline_100 + score_differential, train, family = "binomial")
summary(model1)

pred1 = predict(model1, train, type = "response")
train = cbind(train,pred1)

#Win Probability Percentages for Home Teams
train = mutate(train, pred1h = ifelse(posteam == home_team.x, pred1, 1-pred1))

#Win Percentage Plot for WAS At MIA 
ggplot(filter(train, game_id == "2019101305"),aes(x=game_seconds_remaining,y=pred1h)) + geom_line(size=2, colour="orange") + scale_x_reverse() + ylim(c(0,1)) + theme_minimal() + xlab("Time Remaining (seconds)") + ylab("Home Win Probability")

#RAW data for WAS At MIA
WasAtMIA = train %>% filter(game_id== "2019101305")

write.csv(pbp_final,"C:\\Users\\stanma02\\Desktop\\pbp_final.csv",row.names=FALSE)
