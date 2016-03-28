# In this part of the demo I'm going to show you:
# 1. Loading SQL query results into an R data frame
# 2. Querying local data frames
# 3. Doing joins against data locally 
# 4. Examining summary statistics for data frames
# 5. Visualizing tables using HTML
# 6. 

library(RODBC)
conn <- odbcConnect("baseball")

batters <- sqlQuery(conn, "select * from Batting", stringsAsFactors = FALSE)

# Examine the summary 
# Note how the maximum values are accurate if you know your BB stats
summary(batters)

# Let's take a closer look at the data
SQL <- "select * from Batting where HR = 73"
hr_leader <- sqlQuery(conn, SQL)
hr_leader

# We can, of course join using SQL
SQL <- "select * from Batting as b inner join Master as p on b.playerID = p.playerID where HR = 73"
hr_leader_details <- sqlQuery(conn, SQL)
hr_leader_details

# We can do joins locally too
master <- sqlQuery(conn, "select * from Master", stringsAsFactors = FALSE)

library(dplyr)
hr_local_join <- batters %>%
    inner_join(master, by = "playerID") %>%
    filter(HR == 73)
hr_local_join

# Let's take a closer look at the batting data
kenny_lofton <- batters %>%
    filter(playerID == "loftoke01")
kenny_lofton

# Notice that there are column names that start with a number
# e.g., 2B and 3B. These are not valid R identifiers, so we 
# need to rename them

# TODO: why this not work? Rename column helper function
# TODO: how to magnittr
rename_col <- function(df, old_name, new_name) {
    names(df)[names(df) == old_name] <- new_name
    df
}

fixup_df_names <- function(df) {
    names(df)[names(df) == "2B"] <- "X2B"
    names(df)[names(df) == "3B"] <- "X3B"
    df
}

kenny_lofton <- fixup_df_names(kenny_lofton)
# We can see that if we care about yearly statistics that we need
# to do some summarization of the data frame
rollup <- function(df) {
    result <- df %>%
        group_by(playerID, yearID) %>%
        summarize(
            G = sum(G), AB = sum(AB), R = sum(R), H = sum(H), H2B = sum("2B"), H3B = sum("3B")
        )
    result
}

kenny_lofton_annual <- rollup(kenny_lofton)
kenny_lofton_annual

# Looking at tables on a console is OK, but there are better ways
# Let's visualize it using an interactive chart
library(DT)
datatable(kenny_lofton)


# So far, we have been looking at R as an equivalence class of SQL
# Though a) slower, b) more limited size of data sets. 
# Let's look at it through a new lens: visualization

# Look at a plot of HR vs. RBI for all hitters
plot(batters$HR, batters$RBI)


# Plot shows lots of data skewed towards zero; let's filter by 
# people that are paid to hit (i.e., not pitchers)




# Better ways of visualizing data
library(DT)
datatable(batters)




# Do some local joins
player_names <- sqlQuery(conn, "select * from Master")