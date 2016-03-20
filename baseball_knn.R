# Simplest possible scenario is
# 1. Single-season performances only
# 2. Restrict to players who play more than 120 games in season
# 3. Only offensive stats. H, R, RBI, 2B, 3B, HR

batting <- read.csv("batting.csv", stringsAsFactors = FALSE)
pitching <- read.csv("pitching.csv", stringsAsFactors = FALSE)
player_names <- read.csv("master.csv", stringsAsFactors = FALSE)
fielding <- read.csv("fielding.csv", stringsAsFactors = FALSE)

library(dplyr)

minimum_game_threshold <- 100

# Compute total statistics for all teams that a player hit for that year
rollup_batting <- batting %>%
    group_by(yearID, playerID) %>%
    summarize(
        G = sum(G),
        AB = sum(AB),
        R = sum(R),
        H = sum(H),
        X2B = sum(X2B),
        X3B = sum(X3B),
        HR = sum(HR),
        RBI = sum(RBI),
        SB = sum(SB),
        CS = sum(CS),
        BB = sum(BB)) %>%
    mutate(
        AVG = H / AB,
        S = H - (X2B + X3B + HR),
        TB = S + 2 * X2B + 3 * X3B + 4 * HR,
        SLG = TB / AB)

batting_training <- rollup_batting %>%
    filter(yearID > 1990 & yearID < 2014 & G > minimum_game_threshold)


batting_testing <- rollup_batting %>%
    filter(yearID == 2014 & G > minimum_game_threshold)

c1 <- batting_testing$playerID
c2 <- rollup_batting %>%
    filter(yearID == 2015) %>%
    select(playerID)
c3 <- c2$playerID
c4 <- as.data.frame(intersect(c1, c3))
names(c4) <- "playerID"

batting_validation <- rollup_batting %>%
    filter(yearID == 2015) %>%
    inner_join(c4)
    
# Ensure that we have the intersection 
batting_testing <- batting_testing %>%
    inner_join(c4, by = "playerID")

player_ids <- batting_training$playerID
library(class)
results <- knn(batting_training, batting_testing, player_ids, k = 3)
