library(dplyr)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(grid)
library(ggplot2)
library(ggthemes)
library(png)
library(ggvis)
library(GeomMLBStadiums)
library(plotly)
library(data.table)

batter_df <- read.csv('pitch_df_app.csv')

#batter_df <- read.csv('~/Documents/statcast/batter_df_20200605.csv')
#pitcher_df <- read.csv('~/Documents/statcast/pitcher_df_20200605.csv')
rh <- readPNG("right-handed-batter.png")
lh <- readPNG("left-handed-batter.png")

batter_df$pitch_count <- paste(batter_df$count.balls.start, batter_df$count.strikes.start, sep='-')
batter_df$player_image <- paste0("https://securea.mlb.com/mlb/images/players/head_shot/",batter_df$matchup.batter.id,".jpg")

batter_df <- batter_df %>%
  rename(event = 'result.event',
         eventType = 'result.eventType',
         pitch = 'details.type.description')

batter_df <- batter_df %>%
  filter(!is.na(details.type.code))

#### Added Transformation ####
batter_df$hc_x <- as.numeric(batter_df$hitData.coordinates.coordX)
batter_df$hc_y <- as.numeric(batter_df$hitData.coordinates.coordY)

batter_df <- mlbam_xy_transformation(batter_df, x="hc_x", y="hc_y")


# get player name for UI
batters <- sort(unique(batter_df$matchup.batter.fullName))

### Create the strike zone ###
# Overlay a strike zone
topKzone <- 3.5
botKzone <- 1.6
inKzone <- -0.95
outKzone <- 0.95
kZone <- data.frame(
  x=c(inKzone, outKzone, outKzone, inKzone, inKzone),
  y=c(botKzone, botKzone, topKzone, topKzone, botKzone)
)

homeplate <- data.frame(
  x=c(-0.9, -0.98, 0, .98, .9, -0.9),
  y=c(.2, -.15, -.5, -.15, .2, .2)
)


batter_df %>%
  select(game_pk, game_date, event, matchup.batter.fullName) %>%
  group_by(event) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


stadiums <- unique(MLBStadiumsPathData$team)

