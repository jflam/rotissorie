# Read all of the data into global variables

library(dplyr)

batting <- read.csv("batting.csv", stringsAsFactors = FALSE)
pitching <- read.csv("pitching.csv", stringsAsFactors = FALSE)
player_names <- read.csv("master.csv", stringsAsFactors = FALSE)
fielding <- read.csv("fielding.csv", stringsAsFactors = FALSE)

# Parameters:
# 1. Year to predict
# 2. Year to compare against
# 3. Minimum number of games played to consider
# Returns: list containing 
#   data frame with predictions
#   data frame with standard deviation of predictions from actuals
# Algorithm:
# Simplest possible scenario is
# 1. Single-season performances only
# 2. Restrict to players who play more than 120 games in season
# 3. Only offensive stats. H, R, RBI, 2B, 3B, HR

prediction_year <- 2014
years_to_train <- 30
minimum_game_threshold <- 100

predict_batting_statistics <- function(prediction_year, years_to_train, minimum_game_threshold) {
    start_training_year = prediction_year - years_to_train

    # Batting statistics have line items for each team a player played on during the year
    # We don't care about this, so we need to compute the rollup for each year

    rollup_batting <- batting %>%
        filter(yearID >= start_training_year & G > minimum_game_threshold) %>%
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
           # BUG in R? If we try to call as.factor() on sprintf() below it 
           # winds up with all of the yearIDs being the same year in the factors (1984)
           ID = sprintf("%s-%s", playerID, yearID)
        ) %>%
        as.data.frame()

    training_set <- rollup_batting %>%
        filter(yearID < prediction_year)

    testing_set <- rollup_batting %>%
        filter(yearID == prediction_year)

    player_ids <- intersect(
        testing_set %>%
            select(playerID),
        rollup_batting %>%
            filter(yearID == prediction_year + 1) %>%
            select(playerID)
    )

    answer_set <- rollup_batting %>%
        filter(yearID == 2015) %>%
        inner_join(player_ids, by = "playerID")

    testing_set <- testing_set %>%
        inner_join(player_ids, by = "playerID")

    # Extract just the features that we want by excluding
    # first two and last columns
    cols_to_select <- ncol(training_set) - 1
    training <- training_set[3:cols_to_select]
    testing <- testing_set[3:cols_to_select]
    answer <- answer_set[3:cols_to_select]

    # Now train the model using knn
    library(class)
    results <- knn(training, testing, training_set$ID, k = 3)

    # Construct a table that contains tuples of (playerID, predictedPlayerID)
    similarity_results <- data.frame(
        playerID = testing_set$playerID,
        stringsAsFactors = FALSE
    )
    similarity_results$referenceID = as.character(results)

    # Construct a comparison table that contains stats for playerID and stats for predictedPlayerID
    similarity_stats <- similarity_results %>%
        inner_join(player_names, by = "playerID") %>%
        inner_join(testing_set, by = "playerID") %>%
        inner_join(training_set, by = c("referenceID" = "ID")) %>%
        select(
            FirstName = nameFirst,
            LastName = nameLast,
            PlayerID = playerID.x,
            G = G.x,
            AB = AB.x,
            R = R.x,
            H = H.x,
            X2B = X2B.x,
            X3B = X3B.x,
            HR = HR.x,
            RBI = RBI.x,
            SO = SO.x,
            SB = SB.x,
            BB = BB.x,
            ReferenceID = playerID.y,
            YearID = yearID.y,
            "G(p)" = G.y,
            "AB(p)" = AB.y,
            "R(p)" = R.y,
            "H(p)" = H.y,
            "X2B(p)" = X2B.y,
            "X3B(p)" = X3B.y,
            "HR(p)" = HR.y,
            "RBI(p)" = RBI.y,
            "SO(p)" = SO.y,
            "SB(p)" = SB.y,
            "BB(p)" = BB.y
        )

    # Compute standard deviation in difference between player and similar
    similarity_stddev <- data.frame(
        "G(rms)" = sd(similarity_stats$G - similarity_stats$"G(p)"),
        "AB(rms)" = sd(similarity_stats$AB - similarity_stats$"AB(p)"),
        "R(rms)" = sd(similarity_stats$R - similarity_stats$"R(p)"),
        "H(rms)" = sd(similarity_stats$H - similarity_stats$"H(p)"),
        "X2B(rms)" = sd(similarity_stats$X2B - similarity_stats$"X2B(p)"),
        "X3B(rms)" = sd(similarity_stats$X3B - similarity_stats$"X3B(p)"),
        "HR(rms)" = sd(similarity_stats$HR - similarity_stats$"HR(p)"),
        "RBI(rms)" = sd(similarity_stats$RBI - similarity_stats$"RBI(p)"),
        "SO(rms)" = sd(similarity_stats$SO - similarity_stats$"SO(p)"),
        "SB(rms)" = sd(similarity_stats$SB - similarity_stats$"SB(p)"),
        "BB(rms)" = sd(similarity_stats$BB - similarity_stats$"BB(p)")
    )

    # Generate prediction stats by taking stats from the reference player's following year
    prediction_stats <- similarity_stats %>%
        select(FirstName, LastName, PlayerID, ReferenceID, YearID) %>%
        mutate(YearID = YearID + 1) %>%
        inner_join(answer_set, by = c("PlayerID" = "playerID")) %>%
        inner_join(training_set, by = c("ReferenceID" = "playerID", "YearID" = "yearID")) %>%
        select(
            FirstName,
            LastName,
            PlayerID,
            G = G.x,
            AB = AB.x,
            R = R.x,
            H = H.x,
            X2B = X2B.x,
            X3B = X3B.x,
            HR = HR.x,
            RBI = RBI.x,
            SO = SO.x,
            SB = SB.x,
            BB = BB.x,
            ReferenceID,
            YearID,
            "G(p)" = G.y,
            "AB(p)" = AB.y,
            "R(p)" = R.y,
            "H(p)" = H.y,
            "X2B(p)" = X2B.y,
            "X3B(p)" = X3B.y,
            "HR(p)" = HR.y,
            "RBI(p)" = RBI.y,
            "SO(p)" = SO.y,
            "SB(p)" = SB.y,
            "BB(p)" = BB.y
        )

    # Compute standard deviation in difference between actual and prediction
    prediction_stddev <- data.frame(
        "G(rms)" = sd(prediction_stats$G - prediction_stats$"G(p)"),
        "AB(rms)" = sd(prediction_stats$AB - prediction_stats$"AB(p)"),
        "R(rms)" = sd(prediction_stats$R - prediction_stats$"R(p)"),
        "H(rms)" = sd(prediction_stats$H - prediction_stats$"H(p)"),
        "X2B(rms)" = sd(prediction_stats$X2B - prediction_stats$"X2B(p)"),
        "X3B(rms)" = sd(prediction_stats$X3B - prediction_stats$"X3B(p)"),
        "HR(rms)" = sd(prediction_stats$HR - prediction_stats$"HR(p)"),
        "RBI(rms)" = sd(prediction_stats$RBI - prediction_stats$"RBI(p)"),
        "SO(rms)" = sd(prediction_stats$SO - prediction_stats$"SO(p)"),
        "SB(rms)" = sd(prediction_stats$SB - prediction_stats$"SB(p)"),
        "BB(rms)" = sd(prediction_stats$BB - prediction_stats$"BB(p)")
    )

    # Compute table of eligible players by position and their predicted stats
    eligible_hitters_stats_by_position <- fielding %>%
        filter(yearID == prediction_year) %>%
        select(playerID, POS) %>%
        inner_join(prediction_stats, by = c("playerID" = "PlayerID")) %>%
        select(
            FirstName,
            LastName,
            POS,
            G,
            AB,
            R,
            H,
            X2B,
            X3B,
            HR,
            RBI,
            SO,
            SB,
            BB
        )

    # Construct list with results - predictions and standard deviation of the predictions
    list(predictions = eligible_hitters_stats_by_position, stddev = prediction_stddev)
}

results <- predict_batting_statistics(2013, 30, 100)
library(DT)
datatable(results$predictions)