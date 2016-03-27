# Experiments with SQL integration

library(RODBC)

conn <- odbcConnect("baseball")
batters <- sqlQuery(conn, "select * from Batting")