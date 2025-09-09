#### DRUID D1: Create Figure 2 ###############
# Function: To plot the ChangeLostyieldPerHectare for Barley/OSR/Wheat
# Author: Dr Peter King (p.king1@Leeds.ac.uk)
# Last Edited: 09/09/2025
# Change:
## - Split out from the _THreeCrops_ script to standalone


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


library(here)
library(magrittr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(data.table)
library(Rfast)
library(ggdist)
library(janitor)
library(distributional)
library(patchwork) ## Any issues try: devtools::install_github("thomasp85/patchwork")


# ******************************************************************************
#### Section 1: Data Importing ####
# ******************************************************************************


# LPH ******************************************************************************
Wheat_LPH <- here("Output/LoopData", 
                  "Wheat_ThresholdSA_V2_LPH.csv") %>% 
  fread() %>% 
  data.frame()
Barley_LPH <- here("Output/LoopData", 
                   "Barley_ThresholdSA_V2_LPH.csv") %>% 
  fread() %>% 
  data.frame()
OSR_LPH <- here("Output/LoopData", 
                "OSR_ThresholdSA_V2_LPH.csv") %>% 
  fread() %>% 
  data.frame()


Data_LPH <- rbind(Wheat_LPH,
                  Barley_LPH,
                  OSR_LPH)


# ******************************************************************************
#### Section X: Plot setup ####
# ******************************************************************************


## Specify once here for consistency
TextSize <- 14
TextFamily <- "sans"
TextType <- element_text(size = TextSize,
                         colour = "black",
                         family = TextFamily)


## Define all colours here for ease
PlotColours <- c(RColorBrewer::brewer.pal(n = 9, name = "Blues")[c(6, 8)], "black")





# *****************************************************************************
#### Section X: Create Figure 2 ####
# *****************************************************************************


## Plot LPH
Figure4 <- 
  Data_LPH %>% 
  dplyr::filter(Type == "ET") %>% 
  ggplot(aes(
    x = NE %>% as.numeric(),
    group = Density %>% as.factor()
  )) +
  
  geom_line(aes(y = Means, 
                color = Density %>% as.factor()),
            linewidth = 1) +
  
  geom_point(aes(y = Means, 
                 color = Density %>% as.factor())) +
  
  geom_ribbon(
    aes(
      y = Means,
      ymin = Ymin,
      ymax = Ymax,
      fill = Density %>% as.factor()
    ),
    outline.type = "both",
    alpha = 0.2
  ) +
  
  theme_bw() +
  
  facet_grid(Crop ~ Threshold, 
             labeller = as_labeller(c(Barley = "Barley", 
                                      OSR = "OSR", 
                                      Wheat = "Wheat",
                                      
                                      "1" = "Lowest",
                                      "2.5" = "Low",
                                      "5" = "Default",
                                      "7.5" = "High",
                                      "10" = "Highest"
             )),
             space = "free_y",
             scales = "free_y") +
  
  geom_hline(yintercept = 0) +
  
  scale_color_manual(name = "Pest density",
                     labels = c("Low", "Medium", "High"),
                     values = PlotColours) +
  
  scale_fill_manual(name = "Pest density",
                    labels = c("Low", "Medium", "High"),
                    values = PlotColours) +
  
  scale_x_continuous(
    name = "Natural enemy presence as percentage.",
    breaks = seq.int(from = 0, to = 1, by = 0.25),
    labels  = seq.int(from = 0, to = 1, by = 0.25) %>%
      sprintf("%.2f", .)
  )  +
  
  scale_y_continuous(name = "Mean lost yield\nper hectare ",
                     breaks = seq.int(from = 0, to = 500, by = 100),
                     labels = scales::label_currency(prefix = "£")) +
  
  theme(
    strip.background = element_rect(fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "bottom",
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "white"),
    legend.text = TextType,
    strip.text = TextType,
    text = TextType,
    axis.title.x = TextType,
    axis.text.y = TextType,
    axis.title.y = TextType,
    axis.text.x = element_text(
      angle = 45,
      hjust = 0.75,
      size = TextSize,
      colour = "black",
      family = TextFamily
    ),
    panel.spacing.x = unit(0.75, "cm"),
    panel.spacing.y = unit(0.75, "cm")
  )



# *****************************************************************************
#### Section X: Export Figure 2 ####
# *****************************************************************************


Figure4 %>% 
  ggsave(
    device = "png",
    filename = paste0(here(), 
                      "/Output/Figures/",
                      "Figure4_LPH_ThresholdSA_ThreeCrops.png"), 
    width = 25,
    height = 15,
    units = "cm",
    dpi = 500
  )

