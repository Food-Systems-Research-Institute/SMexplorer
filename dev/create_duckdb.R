# This script creates appdata.duckdb.
# Have to run it in Silk to recreate db, because it can't be pushed for some
# reason.


# Plan --------------------------------------------------------------------

# metrics and metadata should be saved as table to duckdb in data
# also fips key probably


pacman::p_load(
  duckdb,
  DBI
)
load('data/neast_county_metrics.rda')
load('data/neast_state_metrics.rda')
load('data/metadata.rda')



## Save to DB --------------------------------------------------------------


con <- dbConnect(duckdb::duckdb(), dbdir = 'data/appdata.duckdb')
dbWriteTable(con, 'neast_county_metrics', as.data.frame(neast_county_metrics), overwrite = TRUE)
dbWriteTable(con, 'neast_state_metrics', as.data.frame(neast_state_metrics), overwrite = TRUE)
dbWriteTable(con, 'metadata', as.data.frame(metadata), overwrite = TRUE)

# Check
dbListTables(con)
dbGetQuery(con, 'select * from metadata')


dbDisconnect(con)
