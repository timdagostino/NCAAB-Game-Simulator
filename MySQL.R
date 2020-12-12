library(DBI)

as.data.frame(ncaa_team_stats_2020)
  
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "66VWMoTqkC",
                 host = "remotemysql.com",
                 port = 3306,
                 user = "66VWMoTqkC",
                 password = "xN0xMa34IV")

dbListTables(conn = con)

dbCreateTable(conn = con, name = "test3", fields = ncaa_team_stats_2020, temporary = FALSE, row.names = NULL)

dbReadTable(conn = con, name = "test3")

dbWriteTable(conn = con, name = "test3", value = ncaa_team_stats_2020, append = TRUE, row.name = FALSE)

dbAppendTable(conn = con, name = "test3", value = sqlData(con = con, ncaa_team_stats_2020))

sqlAppendTable(con = con, table = "test3", values = ncaa_team_stats_2020, row.names = NA)


dbB

con <- dbConnect(
  bigrquery::bigquery(),
  project = "ncaa-simulator",
  dataset = "2020_ncaab_predictions",
  billing = billing
)
