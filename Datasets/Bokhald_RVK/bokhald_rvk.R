library(tidyverse)
library(janitor)
library(readxl)
library(glue)
library(arrow)


source("Datasets/Bokhald_RVK/read_data.R")


write_parquet(
  d,
  "Datasets/Bokhald_RVK/bokhald_rvk.parquet"
)



