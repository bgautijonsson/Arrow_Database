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



url <- "https://github.com/bgautijonsson/Arrow_Database/blob/main/Datasets/Bokhald_RVK/bokhald_rvk.parquet?raw=true"

arrow::install_arrow()


open_dataset(url)
read_arrow(url)

d |> 
  group_by(dags) |> 
  write_dataset("Datasets/Bokhald_RVK/bokhald_rvk/", format = "parquet")


