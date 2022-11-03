library(tidyverse)
library(janitor)
library(readxl)
library(glue)
library(lubridate)

d_22 <- read_csv2("Datasets/Bokhald_RVK/uppg202112island.csv", locale = locale(encoding = "ISO-8859-1")) |> 
    filter(tegund2 == "Launakostnaður") |> 
    group_by(samtala0, samtala1, samtala2, samtala3, ar, arsfjordungur) |> 
    summarise(raun = sum(raun),
              .groups = "drop") |> 
    arrange(desc(raun))

d_21 <- read_csv2("Datasets/Bokhald_RVK/uppg202112island.csv", locale = locale(encoding = "ISO-8859-1")) |> 
    filter(tegund2 == "Launakostnaður") |> 
    group_by(samtala0, samtala1, samtala2, samtala3, ar, arsfjordungur) |> 
    summarise(raun = sum(raun),
              .groups = "drop") |> 
    arrange(desc(raun))


d_20 <- read_csv2("Datasets/Bokhald_RVK/uppg202012island.csv") |> 
    filter(tegund2 == "Launakostnaður") |> 
    group_by(samtala0, samtala1, samtala2, samtala3, ar, arsfjordungur) |> 
    summarise(raun = sum(raun),
              .groups = "drop") |> 
    arrange(desc(raun))


d_19 <- read_csv2("Datasets/Bokhald_RVK/uppgj201912island.csv", locale = locale(encoding = "ISO-8859-1")) |> 
    filter(tegund2 == "Launakostnaður") |> 
    group_by(samtala0, samtala1, samtala2, samtala3, ar, arsfjordungur) |> 
    summarise(raun = sum(raun),
              .groups = "drop") |> 
    arrange(desc(raun))

d_18 <- read_csv2("Datasets/Bokhald_RVK/uppg201812island.csv", locale = locale(encoding = "ISO-8859-1")) |> 
    filter(tegund0 == "Laun og launatengd gjöld") |> 
    group_by(samtala0, samtala1, samtala2, samtala3, ar, arsfjordungur) |> 
    summarise(raun = sum(raun),
              .groups = "drop") |> 
    arrange(desc(raun))

d_17 <- read_csv2("Datasets/Bokhald_RVK/rvk201712.csv", locale = locale(encoding = "ISO-8859-1")) |> 
    filter(xtgr1 == "Launakostnaður") |> 
    mutate(xeining4 = ifelse(xeining5 == "Velferðarsvið", xþjon1, xeining4),
           xeining3 = ifelse(xeining5 == "Velferðarsvið", xþjon2, xeining3),
           xeining2 = ifelse(xeining5 == "Velferðarsvið", xþjon3, xeining2)) |> 
    group_by(samtala1 = xeining5, samtala2 = xeining4, samtala3 = xeining3, samtala0 = xeining1, samtala4 = xeining2,
             ar, arsfjordungur = ar_fjordungur) |> 
    summarise(raun = sum(raun),
              .groups = "drop") |>  
    arrange(desc(raun))


d_16 <- read_csv2("Datasets/Bokhald_RVK/rvk2016.csv", locale = locale(encoding = "ISO-8859-1")) |> 
    filter(XTGR1 == "Launakostnaður") |> 
    mutate(XEINING4 = ifelse(XEINING5 == "Velferðarsvið", XÞJON1, XEINING4),
           XEINING3 = ifelse(XEINING5 == "Velferðarsvið", XÞJON2, XEINING3),
           XEINING2 = ifelse(XEINING5 == "Velferðarsvið", XÞJON3, XEINING2)) |> 
    group_by(samtala1 = XEINING5, samtala2 = XEINING4, samtala3 = XEINING3, samtala0 = XEINING1, samtala4 = XEINING2,
             ar = AR, arsfjordungur = AR_FJORDUNGUR) |> 
    summarise(raun = sum(RAUN),
              .groups = "drop") |> 
    arrange(desc(raun))


d_15 <- read_csv2("Datasets/Bokhald_RVK/rvk2015.csv", locale = locale(encoding = "ISO-8859-1")) |> 
    filter(XTGR1 == "Launakostnaður") |> 
    mutate(XEINING4 = ifelse(XEINING5 == "Velferðarsvið", XÞJON1, XEINING4),
           XEINING3 = ifelse(XEINING5 == "Velferðarsvið", XÞJON2, XEINING3),
           XEINING2 = ifelse(XEINING5 == "Velferðarsvið", XÞJON3, XEINING2)) |> 
    group_by(samtala1 = XEINING5, samtala2 = XEINING4, samtala3 = XEINING3, samtala0 = XEINING1, samtala4 = XEINING2,
             ar = AR, arsfjordungur = AR_FJORDUNGUR) |> 
    summarise(raun = sum(RAUN),
              .groups = "drop") |>
    arrange(desc(raun))

d_14 <- read_csv2("Datasets/Bokhald_RVK/rvk2014.csv", locale = locale(encoding = "ISO-8859-1")) |> 
    filter(XTGR1 == "Launakostnaður") |> 
    mutate(XEINING4 = ifelse(XEINING5 == "Velferðarsvið", XÞJON1, XEINING4),
           XEINING3 = ifelse(XEINING5 == "Velferðarsvið", XÞJON2, XEINING3),
           XEINING2 = ifelse(XEINING5 == "Velferðarsvið", XÞJON3, XEINING2)) |> 
    group_by(samtala1 = XEINING5, samtala2 = XEINING4, samtala3 = XEINING3, samtala0 = XEINING1, samtala4 = XEINING2,
             ar = AR, arsfjordungur = AR_FJORDUNGUR) |> 
    summarise(raun = sum(RAUN),
              .groups = "drop") |>
    arrange(desc(raun))

d <- bind_rows(
    d_22,
    d_21,
    d_20,
    d_19,
    d_18,
    d_17,
    d_16,
    d_15,
    d_14
) |> 
    ungroup() |> 
    mutate(dags = ymd(str_c(ar, c("01", "04", "07", "10")[arsfjordungur], "-01"))) |> 
    mutate(across(starts_with("samtala"), str_to_lower))