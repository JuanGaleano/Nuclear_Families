#NOTE: to run this script you need first to download the full ipums samples defined in **samples** and save the as Rda files. Also, before saving them you need to filter cases of individual living in private households.

library(tidyverse)
library(haven)
library(data.table)

options(scipen=999)

base_path <- "./folder with ipums samples as Rda files/"

samples <- c(
  "BRA_2010_IPUMS",
  "CHL_2002_IPUMS",
  "CUB_2012_IPUMS",
  "DOM_2010_IPUMS",
  "MEX_2020_IPUMS",
  "PRI_2020_IPUMS", 
  "TTO_2011_IPUMS",
  "URY_2011_IPUMS",
  "GTM_2002_IPUMS",
  "JAM_2001_IPUMS",
  "PRY_2002_IPUMS",
  "USA_2020_IPUMS",
  "BWA_2011_IPUMS",
  "CMR_2005_IPUMS",
  "GHA_2010_IPUMS",
  "GIN_2014_IPUMS",
  "MOZ_2007_IPUMS",
  "SLE_2015_IPUMS", 
  "ZAF_2011_IPUMS",
  "UGA_2014_IPUMS",
  "ZMB_2010_IPUMS",
  "IDN_2010_IPUMS",
  "PHL_2010_IPUMS"
)

file_paths <- file.path(base_path, paste0(samples, ".Rda"))

data_list <- map(file_paths, function(path) {
  env <- new.env()
  load(path, envir = env)
  as.list(env)[[1]]
})

names(data_list) <- samples

data_list<-lapply(data_list, function(df) {
  # Convert dataframe to data.table in place for maximum performance
  df<-setDT(df)
  
  # Efficiently evaluate group-by condition and assign new column
  df<-df[, step := as.integer(any(RELATED == 3300)), by = SERIAL]
  
  
})

table <-lapply(data_list, function(df) {
  
  df<-df|>
    mutate(
      AGE2= if_else(AGE2 == "30-34"| AGE2=="35-39","30-39", AGE2))
  
  
  df<-setDT(df)
  
  # Efficiently evaluate group-by condition and assign new column
  df<-df[, cond := any(AGE2 == "30-39"), by = SERIAL]
  
  
  df<-df|>
    filter(cond==1, RELATE==3)|>
    group_by(SAMPLE, RELATED)|>
    summarise(POPW=n())|>
    ungroup()
})

table_child <- data.table::rbindlist(table)
