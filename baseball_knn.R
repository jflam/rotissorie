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
#   data frame with goodness 
# Algorithm:
# Simplest possible scenario is
# 1. Single-season performances only
# 2. Restrict to players who play more than 120 games in season
# 3. Only offensive stats. H, R, RBI, 2B, 3B, HR

prediction_year <- 2014
years_to_train <- 30
minimum_game_threshold <- 100

# predict_batting_statistics <- function(prediction_year, years_to_train, minimum_game_threshold) {
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
           ID = sprintf("%s:%s", playerID, yearID))

    training_set <- rollup_batting %>%
        filter(yearID < prediction_year)

    testing_set <- rollup_batting %>%
        filter(yearID == prediction_year)

    c1 <- testing_set$playerID
    c2 <- rollup_batting %>%
        filter(yearID == 2015) %>%
        select(playerID)
    c3 <- c2$playerID
    c4 <- as.data.frame(intersect(c1, c3))
    c5 <- data.frame(lapply(c4, as.character), stringsAsFactors = FALSE)
    names(c5) <- "playerID"

    answer_set <- rollup_batting %>%
        filter(yearID == 2015) %>%
        inner_join(c5)

    testing_set <- testing_set %>%
        inner_join(c5, by = "playerID")

    # Extract just the features that we want by excluding
    # first two and last columns
    cols_to_select <- ncol(training_set) - 1
    training <- training_set[3:cols_to_select]
    testing <- testing_set[3:cols_to_select]
    answer <- answer_set[3:cols_to_select]

    # Extract the labels for the training data 
    player_ids <- training_set$ID

    # Now train the model using knn
    library(class)
    results <- knn(training, testing, player_ids, k = 3)

    b0 <- as.data.frame(testing_set$playerID)
    names(b0) <- "playerID"
    b0$ID <- results
    b1 <- training_set[c("ID", "playerID", "yearID", "G", "R", "X2B", "X3B", "HR", "SO", "RBI", "SB")]
    names(b1)[names(b1) == "playerID"] <- "referenceID"
    b2 <- testing_set[c('playerID', 'G', 'R', 'X2B', 'X3B', 'HR', "SO", 'RBI', 'SB')]

    comparison <- b0 %>% inner_join(b1) %>% inner_join(b2, by = "playerID") %>%
        select(playerID, G.x, R.x, X2B.x, X3B.x, SO.x, HR.x, RBI.x, SB.x, referenceID, yearID, G.y, R.y, X2B.y, X3B.y, SO.y, HR.y, RBI.y, SB.y)

    # Use this to generate names
    eligible_hitters_by_position <- fielding %>%
        filter(yearID == prediction_year) %>%
        select(playerID, POS) %>%
        inner_join(player_names, by = "playerID") %>%
        select(playerID, FirstName = nameFirst, LastName = nameLast, POS)

    # Raw prediction and positions
    z1 <- comparison[c("playerID", "referenceID", "yearID")]
    z1$yearID = z1$yearID + 1
    prediction <- z1 %>%
        inner_join(training_set, by = c("referenceID" = "playerID", "yearID")) %>%
        inner_join(eligible_hitters_by_position, by = "playerID") %>%
        select(FirstName, LastName, POS, referenceID, yearID, G, AB, R, H, X2B, X3B, HR, RBI, SO, SB, BB, AVG)

    library(DT)
    datatable(prediction) %>%
        formatRound("AVG", digits = 3)

    # Prediction comparison
    predict_comparison <- prediction %>%
        inner_join(answer_set, by = "playerID") %>%
        select(playerID, G.y, AB.y, R.y, H.y, X2B.y, X3B.y, HR.y, RBI.y, SO.y, SB.y, AVG.y, referenceID, yearID.x, G.x, AB.x, R.x, H.x, X2B.x, X3B.x, HR.x, RBI.x, SO.x, SB.x, AVG.x) %>%
        inner_join(eligible_hitters_by_position, by = "playerID") %>%
        select(FirstName, LastName, POS, G.y, AB.y, R.y, H.y, X2B.y, X3B.y, HR.y, RBI.y, SO.y, SB.y, AVG.y, referenceID, yearID.x, G.x, AB.x, R.x, H.x, X2B.x, X3B.x, HR.x, RBI.x, SO.x, SB.x, AVG.x)

    datatable(predict_comparison) %>%
        formatRound(c("AVG.y", "AVG.x"), digits = 3)

    # Compute RMS of deviation from actual
    deviation <- data.frame(
            "HR" = sqrt(mean(predict_comparison$HR.y - predict_comparison$HR.x) ^ 2),
            "RBI" = sqrt(mean(predict_comparison$RBI.y - predict_comparison$RBI.x) ^ 2),
            "R" = sqrt(mean(predict_comparison$R.y - predict_comparison$R.x) ^ 2),
            "SB" = sqrt(mean(predict_comparison$SB.y - predict_comparison$SB.x) ^ 2),
            "AVG" = sqrt(mean(predict_comparison$AVG.y - predict_comparison$AVG.x) ^ 2))