#### D1: Create Table 2 ####
## Author: Dr Peter King (p.king1@leeds.ac.uk)
## Last change: 08/09/2025
## Changes:
# - Thanks chatgpt for tidying this code


# ******************************************************************
# Replication Information: ####
# ******************************************************************


# ─ Session info ───────────────────────────────────────────────────────────────
#  setting  value
#  version  R version 4.4.1 (2024-06-14 ucrt)
#  os       Windows 11 x64 (build 22631)
#  system   x86_64, mingw32
#  ui       RStudio
#  language (EN)
#  collate  English_United Kingdom.utf8
#  ctype    English_United Kingdom.utf8
#  tz       Europe/London
#  date     2025-03-28
#  rstudio  2023.06.2+561 Mountain Hydrangea (desktop)
#  pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────
#  ! package        * version date (UTC) lib source
#    cellranger       1.1.0   2016-07-27 [1] CRAN (R 4.4.0)
#    cli              3.6.3   2024-06-21 [1] CRAN (R 4.4.1)
#    codetools        0.2-20  2024-03-31 [1] CRAN (R 4.4.1)
#    colorspace       2.1-0   2023-01-23 [1] CRAN (R 4.4.0)
#    data.table     * 1.15.4  2024-03-30 [1] CRAN (R 4.4.0)
#    distributional * 0.4.0   2024-02-07 [1] CRAN (R 4.4.0)
#    dplyr          * 1.1.4   2023-11-17 [1] CRAN (R 4.4.0)
#    fansi            1.0.6   2023-12-08 [1] CRAN (R 4.4.0)
#    farver           2.1.2   2024-05-13 [1] CRAN (R 4.4.0)
#    forcats        * 1.0.0   2023-01-29 [1] CRAN (R 4.4.0)
#    generics         0.1.3   2022-07-05 [1] CRAN (R 4.4.0)
#    ggdist         * 3.3.2   2024-03-05 [1] CRAN (R 4.4.0)
#    ggplot2        * 3.5.1   2024-04-23 [1] CRAN (R 4.4.0)
#    glue             1.7.0   2024-01-09 [1] CRAN (R 4.4.0)
#    gtable           0.3.5   2024-04-22 [1] CRAN (R 4.4.0)
#    here           * 1.0.1   2020-12-13 [1] CRAN (R 4.4.0)
#    hms              1.1.3   2023-03-21 [1] CRAN (R 4.4.0)
#    labeling         0.4.3   2023-08-29 [1] CRAN (R 4.4.0)
#    lattice          0.22-6  2024-03-20 [1] CRAN (R 4.4.1)
#    lifecycle        1.0.4   2023-11-07 [1] CRAN (R 4.4.0)
#    lubridate      * 1.9.3   2023-09-27 [1] CRAN (R 4.4.0)
#    magrittr       * 2.0.3   2022-03-30 [1] CRAN (R 4.4.0)
#    MASS             7.3-61  2024-06-13 [1] CRAN (R 4.4.1)
#    Matrix           1.7-0   2024-04-26 [1] CRAN (R 4.4.1)
#    microbenchmark * 1.4.10  2023-04-28 [1] CRAN (R 4.4.0)
#    multcomp         1.4-25  2023-06-20 [1] CRAN (R 4.4.0)
#    munsell          0.5.1   2024-04-01 [1] CRAN (R 4.4.0)
#    mvtnorm          1.2-5   2024-05-21 [1] CRAN (R 4.4.0)
#    patchwork      * 1.2.0   2024-01-08 [1] CRAN (R 4.4.0)
#    pillar           1.9.0   2023-03-22 [1] CRAN (R 4.4.0)
#    pkgconfig        2.0.3   2019-09-22 [1] CRAN (R 4.4.0)
#    purrr          * 1.0.2   2023-08-10 [1] CRAN (R 4.4.0)
#    R6               2.5.1   2021-08-19 [1] CRAN (R 4.4.0)
#    ragg             1.3.2   2024-05-15 [1] CRAN (R 4.4.0)
#    RColorBrewer   * 1.1-3   2022-04-03 [1] CRAN (R 4.4.0)
#    Rcpp           * 1.0.12  2024-01-09 [1] CRAN (R 4.4.0)
#  D RcppParallel   * 5.1.7   2023-02-27 [1] CRAN (R 4.4.0)
#    RcppZiggurat   * 0.1.6   2020-10-20 [1] CRAN (R 4.4.0)
#    readr          * 2.1.5   2024-01-10 [1] CRAN (R 4.4.0)
#    readxl         * 1.4.3   2023-07-06 [1] CRAN (R 4.4.0)
#    Rfast          * 2.1.0   2023-11-09 [1] CRAN (R 4.4.0)
#    rlang            1.1.4   2024-06-04 [1] CRAN (R 4.4.0)
#    rprojroot        2.0.4   2023-11-05 [1] CRAN (R 4.4.0)
#    rstudioapi       0.16.0  2024-03-24 [1] CRAN (R 4.4.0)
#    sandwich         3.1-0   2023-12-11 [1] CRAN (R 4.4.0)
#    scales           1.3.0   2023-11-28 [1] CRAN (R 4.4.0)
#    sessioninfo    * 1.2.2   2021-12-06 [1] CRAN (R 4.4.2)
#    stringi          1.8.4   2024-05-06 [1] CRAN (R 4.4.0)
#    stringr        * 1.5.1   2023-11-14 [1] CRAN (R 4.4.0)
#    survival         3.7-0   2024-06-05 [1] CRAN (R 4.4.1)
#    systemfonts      1.1.0   2024-05-15 [1] CRAN (R 4.4.0)
#    textshaping      0.4.0   2024-05-24 [1] CRAN (R 4.4.0)
#    TH.data          1.1-2   2023-04-17 [1] CRAN (R 4.4.0)
#    tibble         * 3.2.1   2023-03-20 [1] CRAN (R 4.4.0)
#    tidyr          * 1.3.1   2024-01-24 [1] CRAN (R 4.4.0)
#    tidyselect       1.2.1   2024-03-11 [1] CRAN (R 4.4.0)
#    tidyverse      * 2.0.0   2023-02-22 [1] CRAN (R 4.4.0)
#    timechange       0.3.0   2024-01-18 [1] CRAN (R 4.4.0)
#    tzdb             0.4.0   2023-05-12 [1] CRAN (R 4.4.0)
#    utf8             1.2.4   2023-10-22 [1] CRAN (R 4.4.0)
#    vctrs            0.6.5   2023-12-01 [1] CRAN (R 4.4.0)
#    withr            3.0.0   2024-01-16 [1] CRAN (R 4.4.0)
#    zoo              1.8-12  2023-04-13 [1] CRAN (R 4.4.0)
# 
#  [1] C:/Users/earpkin/AppData/Local/Programs/R/R-4.4.1/library
# 
#  D ── DLL MD5 mismatch, broken installation.


# ******************************************************************
# Setup Environment: ####
# ******************************************************************


## Libraries here: ------------------------------------------------------------
## Setting up libraries in order of use in the script
library(dplyr)
library(tidyr)
library(readxl)
library(here)
rm(list = ls())


# ******************************************************************************
#### Section 1: Data Importing ####
# ******************************************************************************


Data <- here("MarketData", "AllCropData_V2.xlsx") %>%
  read_xlsx(sheet = "Sheet1") %>%
  data.frame()


# ******************************************************************************
#### Section 2: Data Cleaning ####
# ******************************************************************************


# Long form
Data_long <- Data %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    values_to = "Value"
  ) %>%
  mutate(
    Year = as.integer(sub("^X", "", Year)),
    Value = na_if(Value, ". ."),
    Value = as.numeric(Value)
  ) %>%
  filter(Year >= 2011, Year <= 2021)

# Crop-specific mapping for price + sprays
measure_map <- tribble(
  ~Crop,    ~Measure,                 ~NewMeasure,
  "Wheat",  "Milling wheat real",     "Price",
  "Wheat",  "MillingSprayCostsReal",  "Sprays",
  "OSR",    "PriceReal",              "Price",
  "OSR",    "WinterSpraysCostsReal",  "Sprays",
  "Barley", "Malting barley real",    "Price",
  "Barley", "MaltingSpraysCostsReal", "Sprays"
)


# ******************************************************************************
#### Section 3: Data organising ####
# ******************************************************************************



# Keep only the variables of interest + standardise
Table2 <- Data_long %>%
  filter(
    Crop %in% c("Wheat", "Barley", "OSR"),
    Measure %in% c("Area", "Yield", "Volume") |
      Measure %in% measure_map$Measure
  ) %>%
  left_join(measure_map, by = c("Crop", "Measure")) %>%
  mutate(FinalMeasure = coalesce(NewMeasure, Measure)) %>%
  group_by(Crop, FinalMeasure) %>%
  summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = FinalMeasure, values_from = Value) %>%
  mutate(
    Crop = factor(Crop, levels = c("Barley", "OSR", "Wheat")),
    # Round all numeric values to 2 dp
    across(c(Area, Price, Volume, Yield), ~ round(.x, 2)),
    Sprays = ifelse(!is.na(Sprays),
                    paste0("£", formatC(Sprays, format = "f", digits = 2),
                           "/per hectare"),
                    NA)
  ) %>%
  arrange(Crop) %>%
  dplyr::select(Crop, Area, Price, Volume, Yield, Sprays)


# ******************************************************************************
#### Section 4: Data export ####
# ******************************************************************************


## Export, fwrite much better
Table2 %>% 
  data.frame() %>% 
  fwrite(sep = ",", 
         here("Output/Tables", 
              "Table2_MarketDataSummary.txt"))


