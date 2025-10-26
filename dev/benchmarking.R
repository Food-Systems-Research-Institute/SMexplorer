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
  fst,
  qs,
  sfarrow
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


# Benchmarking ------------------------------------------------------------
## Reading -----------------------------------------------------------------

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



## Filter ------------------------------------------------------------------

# Compare in memory to pulling from db or parquet
results <- microbenchmark(
  'df in mem' = dplyr::filter(dat, variable_name == 'landAcres'),
  'dbplyr' = tbl(con2, 'test') %>% dplyr::filter(variable_name == 'landAcres'),
  'duckdb query' = dbGetQuery(con, "select * from test where variable_name = 'landAcres'"),
  'RSQLite' = dbGetQuery(con, "select * from test where variable_name = 'landAcres'"),
  times = 5
)
results



## Metadata ----------------------------------------------------------------


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



# Spatial -----------------------------------------------------------------


saveRDS(neast_county_spatial_2024, 'dev/neast_county_spatial.rds')
qsave(neast_county_spatial_2024, 'dev/neast_county_spatial.qs')

microbenchmark(
  'rds' = readRDS('dev/neast_county_spatial.rds'),
  'qs' = qread('dev/neast_county_spatial.qs'),
  'rda' = load('data/neast_county_spatial_2024.rda'),
  times = 5
)
# qs is 5 times faster than rds, 60 times faster than rda

