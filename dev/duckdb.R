pacman::p_load(
  dplyr,
  duckdb,
  profvis,
  readr,
  microbenchmark,
  tictoc,
  dbplyr,
  arrow,
  RSQLite,
  fst
)

str(neast_county_metrics)
dat <- neast_county_metrics %>% 
  dplyr::filter(!is.na(value))
str(dat)

write_csv(dat, 'dev/neast_county_metrics.csv')
saveRDS(dat, 'dev/neast_county_metrics.rds')
write_parquet(dat, 'dev/neast_county_metrics.parquet')
write.fst(dat, 'dev/neast_county_metrics.fst')

# duckdb
tic()
con <- dbConnect(duckdb())
toc()

# Read into duckdb
tic()
duckdb_read_csv(
  con, 
  name = 'test',
  files = 'dev/neast_county_metrics.csv',
  na.strings = 'NA'
)
toc()

# Filter
out <- dbGetQuery(con, "select * from test where variable_name = 'landAcres'")
str(out)

# Also a normal db
con2 <- dbConnect(RSQLite::SQLite())
dbWriteTable(con2, 'test', dat)
dbGetQuery(con2, 'select * from test')


# Reading -----------------------------------------------------------------

results <- microbenchmark(
  read.csv('dev/neast_county_metrics.csv'),
  read_csv('dev/neast_county_metrics.csv'),
  readRDS('dev/neast_county_metrics.rds'),
  read_parquet('dev/neast_county_metrics.parquet'),
  dbGetQuery(con, 'select * from test'),
  dbGetQuery(con2, 'select * from test'),
  load('dev/neast_county_metrics.rda'),
  read.fst('dev/neast_county_metrics.fst'),
  times = 5
)
results
# parquet fastest, then duckdb. fst is nearly as fast!
# duckdb is much faster than RSQLite for some reason
# load is one of slowest, along with read.csv
# read_csv an readRDS are about the same

df <- read.fst('dev/neast_county_metrics.fst')
str(df)



# Filter ------------------------------------------------------------------

# Compare in memory to pulling from db or parquet
results <- microbenchmark(
  'df in mem' = dplyr::filter(dat, variable_name == 'landAcres'),
  'dbplyr' = tbl(con2, 'test') %>% dplyr::filter(variable_name == 'landAcres'),
  'duckdb query' = dbGetQuery(con, "select * from test where variable_name = 'landAcres'"),
  'RSQLite' = dbGetQuery(con, "select * from test where variable_name = 'landAcres'"),
  times = 5
)
results



# Save to DB --------------------------------------------------------------


# Try saving multiple to DB

dbDisconnect(con)
dbDisconnect(con2)

con <- dbConnect(duckdb::duckdb(), dbdir = 'dev/appdata.duckdb')
dbWriteTable(con, 'neast_county_metrics', neast_county_metrics)
dbWriteTable(con, 'neast_state_metrics', neast_state_metrics)
dbWriteTable(con, 'metadata', metadata)

# Check
dbListTables(con)
dbGetQuery(con, 'select * from metadata')


# Metadata ----------------------------------------------------------------


write_parquet(metadata, 'dev/metadata.parquet')
write_csv(metadata, 'dev/metadata.csv')

# Is it still faster to use db with metadata
results <- microbenchmark(
  read_csv('dev/metadata.csv'),
  read_parquet('dev/metadata.parquet'),
  dbGetQuery(con, 'select * from metadata'),
  load('dev/metadata.rda'),
  times = 5
)
results

# duck is fastest, parquet close, both 10x better than others