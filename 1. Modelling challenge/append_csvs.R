# list out all .csv's
csvs <- list.files(path = "../research", pattern = "^CBMS")

# get correct names from file names
names(csvs) <- 
  csvs %>% 
  strsplit(split = ".csv") %>% 
  unlist() %>% 
  strsplit(split="_|-") %>% 
  map_chr(2)

# read in re-set names
all <- 
  csvs %>% 
  paste0("../research/", .) %>% 
  set_names(nm = names(csvs)) %>% 
  map_df( ~ readr::read_csv(.) , .id = "csv_source")

# write out appended version
readr::write_csv(all, "../research/CBMS_csvs_appended.csv")
