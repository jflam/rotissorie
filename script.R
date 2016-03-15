# This script will subset the Lahman database such that we retain only the data for the past 3 years

batting <- read.csv("batting.csv", stringsAsFactors = FALSE)
pitching <- read.csv("pitching.csv", stringsAsFactors = FALSE)
player_names <- read.csv("master.csv", stringsAsFactors = FALSE)
fielding <- read.csv("fielding.csv", stringsAsFactors = FALSE)

library(dplyr)

batting_2015 <- batting %>%
    filter(yearID == 2015)

# Compute total statistics for all teams that a player played for 
rollup_batting_2015 <- player_names %>%
    select(playerID, nameFirst, nameLast) %>%
    inner_join(
        batting_2015 %>%
        filter(AB > 0) %>%
        group_by(playerID) %>%
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
            BB = sum(BB)
        ) %>%
        mutate(
            AVG = round(H / AB, digits = 3),
            S = H - (X2B + X3B + HR),
            TB = S + 2 * X2B + 3 * X3B + 4 * HR,
            SLG = round(TB / AB, digits = 3))
    )

# To be eligible, player must have >= 20 games at any position
fielding_2015 <- fielding %>%
    filter(yearID == 2015 & G >= 20)

# Now lists
catchers <- fielding_2015 %>%
    filter(POS == 'C') %>%
    inner_join(rollup_batting_2015, by = "playerID")

c1 <- fielding_2015 %>%
    filter(POS == 'C')

c2 <- c1 %>% inner_join(batting_2015, by = "playerID")


pitching_2015 <- pitching %>%
    filter(yearID == 2015)

