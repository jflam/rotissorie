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
        SO = sum(SO),
        SB = sum(SB),
        CS = sum(CS),
        BB = sum(BB)) %>%
    mutate(
        AVG = H / AB,
        S = H - (X2B + X3B + HR),
        TB = S + 2 * X2B + 3 * X3B + 4 * HR,
        SLG = TB / AB,
        ID = sprintf("%s:%s", playerID, yearID))

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

# Remove all non-numeric data
training <- batting_training[3:18]
testing <- batting_testing[3:18]
validation <- batting_validation[3:18]

player_ids <- batting_training$ID
library(class)
results <- knn(training, testing, player_ids, k = 3)

# Print out a report so we can eyeball the results
library(DT)

b0 <- as.data.frame(batting_testing$playerID)
names(b0) <- "playerID"
b0$ID <- results

b1 <- batting_training[c("ID", "playerID", "yearID", "G", "R", "X2B", "X3B", "HR", "SO", "RBI", "SB")]
names(b1)[names(b1)=="playerID"] <- "referenceID"
b2 <- batting_testing[c('playerID', 'G', 'R', 'X2B', 'X3B', 'HR', "SO", 'RBI', 'SB')]

comparison <- b0 %>% inner_join(b1) %>% inner_join(b2, by = "playerID") %>%
    select(playerID, G.x, R.x, X2B.x, X3B.x, SO.x, HR.x, RBI.x, SB.x, referenceID, yearID, G.y, R.y, X2B.y, X3B.y, SO.y, HR.y, RBI.y, SB.y)

datatable(comparison)

# Dataframe containing referenceIDs and year + 1
z1 <- comparison[c("playerID", "referenceID", "yearID")]
z1$yearID = z1$yearID + 1
prediction <- z1 %>%
    inner_join(batting_training, by = c("referenceID" = "playerID", "yearID")) %>%
    select(playerID, referenceID, yearID, G, AB, R, H, X2B, X3B, HR, RBI, SO, SB, BB, AVG)

# Prediction comparison
predict_comparison <- prediction %>%
    inner_join(batting_validation, by = "playerID") %>%
    select(playerID, G.y, AB.y, R.y, H.y, X2B.y, X3B.y, HR.y, RBI.y, SO.y, SB.y, AVG.y, referenceID, yearID.x, G.x, AB.x, R.x, H.x, X2B.x, X3B.x, HR.x, RBI.x, SO.x, SB.x, AVG.x)

datatable(predict_comparison) %>%
    formatRound(c("AVG.y", "AVG.x"), digits = 3)

# Generate the prediction table with positions
