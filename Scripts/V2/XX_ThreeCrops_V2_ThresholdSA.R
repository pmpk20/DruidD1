#### DRUID D1: Intro plotting of OSR data  ###############
# Function: To do sensitivity analysis with OSR data
# Author: Dr Peter King (p.king1@Leeds.ac.uk)
# Last Edited: 16/06/2025
# Change:
## - This version does sensitivity analysis
## - R1 wanted changed colours, which I define as PlotColours


# *****************************************************************************
#### Section 0: Setting up ####
## NOTES: This reports (a) system information and (b) imports packages.
# *****************************************************************************



## sessionInfo()---------------------------------------------------------------
# R version 4.3.1 (2023-06-16 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 11 x64 (build 22631)
# Matrix products: default
# locale:
#   [1] LC_COLLATE=English_United Kingdom.utf8
# [2] LC_CTYPE=English_United Kingdom.utf8
# [3] LC_MONETARY=English_United Kingdom.utf8
# [4] LC_NUMERIC=C
# [5] LC_TIME=English_United Kingdom.utf8
# time zone: Europe/London
# tzcode source: internal
# other attached packages:
# [1] patchwork_1.1.3.9000 distributional_0.3.2 ggdist_3.3.1
# [4] Rfast_2.1.0          RcppParallel_5.1.7   RcppZiggurat_0.1.6
# [7] Rcpp_1.0.11          data.table_1.15.0    RColorBrewer_1.1-3
# [10] lubridate_1.9.2      forcats_1.0.0        stringr_1.5.0
# [13] dplyr_1.1.3          purrr_1.0.2          readr_2.1.4
# [16] tidyr_1.3.0          tibble_3.2.1         tidyverse_2.0.0
# [19] ggplot2_3.4.4        readxl_1.4.3         magrittr_2.0.3
# [22] here_1.0.1
# [1] gtable_0.3.4      compiler_4.3.1    tidyselect_1.2.0  parallel_4.3.1
# [5] scales_1.3.0      R6_2.5.1          generics_0.1.3    munsell_0.5.0
# [9] rprojroot_2.0.4   pillar_1.9.0      tzdb_0.4.0        rlang_1.1.1
# [13] utf8_1.2.3        stringi_1.7.12    timechange_0.2.0  cli_3.6.1
# [17] withr_2.5.2       grid_4.3.1        rstudioapi_0.15.0 hms_1.1.3
# [21] lifecycle_1.0.4   vctrs_0.6.3       glue_1.6.2        farver_2.1.1
# [25] cellranger_1.1.0  fansi_1.0.4       colorspace_2.1-0  tools_4.3.1
# [29] pkgconfig_2.0.3


## Libraries here: ------------------------------------------------------------
## Setting up libraries in order of use in the script
library(here)
library(magrittr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(data.table)
library(Rfast)
library(ggdist)
library(distributional)


## Only run these if not already installed
# install.packages("devtools")
# devtools::install_github("thomasp85/patchwork")
library(patchwork)


# ******************************************************************************
#### Section 1: Data Importing ####
# ******************************************************************************


# CLPH ******************************************************************************
Wheat_CLPH <- here("Output/LoopData", 
                 "Wheat_ThresholdSA_V2_CLPH.csv") %>% 
  fread() %>% 
  data.frame()
Barley_CLPH <- here("Output/LoopData", 
                 "Barley_ThresholdSA_V2_CLPH.csv") %>% 
  fread() %>% 
  data.frame()
OSR_CLPH <- here("Output/LoopData", 
              "OSR_ThresholdSA_V2_CLPH.csv") %>% 
  fread() %>% 
  data.frame()


Data_CLPH <- rbind(Wheat_CLPH,
                   Barley_CLPH,
                   OSR_CLPH)

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

# YL ******************************************************************************
Wheat_YL <- here("Output/LoopData", 
                   "Wheat_ThresholdSA_V2_YL.csv") %>% 
  fread() %>% 
  data.frame()
Barley_YL <- here("Output/LoopData", 
                    "Barley_ThresholdSA_V2_YL.csv") %>% 
  fread() %>% 
  data.frame()
OSR_YL <- here("Output/LoopData", 
                 "OSR_ThresholdSA_V2_YL.csv") %>% 
  fread() %>% 
  data.frame()


Data_YL <- rbind(Wheat_YL,
                   Barley_YL,
                   OSR_YL)


# ******************************************************************************
#### Plot setup ####
# ******************************************************************************


## Specify once here for consistency
TextSize <- 14
TextFamily <- "sans"
TextType <- element_text(size = TextSize,
                         colour = "black",
                         family = TextFamily)


## Define all colours here for ease
PlotColours <- c(RColorBrewer::brewer.pal(n = 9, name = "Blues")[c(6, 8)], "black")




# ******************************************************************************
#### Plot creation ####
# ******************************************************************************



## Plot LPH
LPH_Plot <- 
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




# ******************************************************************************
#### Plot export ####
# ******************************************************************************


## Export here
LPH_Plot %>% 
  ggsave(
    device = "png",
    filename = paste0(here(), 
                      "/Output/Figures/",
                      "ThreeCrops_ThresholdSA_V2_LPH_Plot.png"), 
    width = 25,
    height = 15,
    units = "cm",
    dpi = 500
  )




# ******************************************************************************
#### OLD ####
# ******************************************************************************



# ## Plot Yield without AS
# YL_Plot <- 
#   Data_YL %>% 
#   dplyr::filter(Type == "ET") %>% 
#   ggplot(aes(x = NE %>% as.numeric(),
#              group = Density %>% as.factor())) +
#   
#   geom_line(aes(y = Means, 
#                 color = Density %>% as.factor()),
#             linewidth = 1) +
#   
#   geom_point(aes(y = Means, 
#                  color = Density %>% as.factor())) +
#   
#   geom_ribbon(
#     aes(
#       y = Means,
#       ymin = Ymin,
#       ymax = Ymax,
#       fill = Density %>% as.factor()
#     ),
#     outline.type = "both",
#     alpha = 0.2
#   ) +
#   
#   theme_bw() +
#   
#   facet_grid(Crop ~ Threshold, 
#              scales = "free_y") +
#   
#   geom_hline(yintercept = 0, alpha = 0.25, linetype = "dashed") +
#   
#   scale_color_manual(name = "Pest density",
#                      values = c("#C6DBEF", "#4292C6", "black")) +
#   
#   scale_fill_manual(name = "Pest density",
#                     values = c("#C6DBEF", "#4292C6", "black")) +
#   
#   scale_x_continuous(
#     name = "Natural enemy presence as percentage.",
#     breaks = seq.int(from = 0, to = 1, by = 0.25),
#     labels  = seq.int(from = 0, to = 1, by = 0.25) %>%
#       sprintf("%.2f", .)
#   )  +
#   
#   
#   scale_y_continuous(
#     name = "Average annual lost yield per hectare",
#     breaks = seq.int(from = 0, to = 25, by = 5),
#     labels = paste0(seq.int(
#       from = 0, to = 25, by = 5
#     ), "%")
#     # labels = scales::percent
#   ) +
#   
#   
#   theme(
#     strip.background = element_rect(fill = "white"),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     legend.position = "bottom",
#     legend.background = element_blank(),
#     legend.box.background = element_rect(colour = "white"),
#     legend.text = TextType,
#     strip.text = TextType,
#     text = TextType,
#     axis.text.x = TextType,
#     axis.text.y = TextType,
#     axis.title.y = TextType,
#     axis.title.x = TextType
#   ) 
# 
# ## Export here
# YL_Plot %>% 
#   ggsave(
#     device = "png",
#     filename = paste0(here(), 
#                       "/Output/Figures/",
#                       "ThreeCrops_ThresholdSA_V2_YL_Plot.png"), 
#     width = 25,
#     height = 15,
#     units = "cm",
#     dpi = 500
#   )



# ******************************************************************************
#### Change in loss per hectare setup 
# ******************************************************************************



# ## Plot Yield without AS
# CLPH_Plot <- 
#   
#   Data_CLPH %>% 
#   dplyr::filter(Type == "ET") %>% 
#   ggplot(aes(x = NE %>% as.numeric(),
#              group = Density %>% as.factor())) +
#   
#   geom_line(aes(y = Means, 
#                 color = Density %>% as.factor()),
#             linewidth = 1) +
#   
#   geom_point(aes(y = Means, 
#                  color = Density %>% as.factor())) +
#   
#   geom_ribbon(
#     aes(
#       y = Means,
#       ymin = Ymin,
#       ymax = Ymax,
#       fill = Density %>% as.factor()
#     ),
#     outline.type = "both",
#     alpha = 0.2
#   ) +
#   
#   theme_bw() +
#   
#   facet_grid(Crop ~ Threshold, 
#              scales = "free_y") +
#   
#   geom_hline(yintercept = 0) +
#   
#   scale_color_manual(name = "Pest density",
#                      values = c("#C6DBEF", "#4292C6", "black")) +
#   
#   scale_fill_manual(name = "Pest density",
#                     values = c("#C6DBEF", "#4292C6", "black")) +
#   
#   scale_y_continuous(name = "Change in lost yield against 100% presence",
#                      labels = scales::label_currency(prefix = "£")) +
#   
#   scale_x_continuous(
#     name = "Natural enemy presence as percentage.",
#     breaks = seq.int(from = 0, to = 1, by = 0.25),
#     labels  = seq.int(from = 0, to = 1, by = 0.25) %>%
#       sprintf("%.2f", .)
#   )  +
#   
#   guides(fill = "none") + ## changed FALSE to NONE
#   
#   theme(
#     strip.background = element_rect(fill = "white"),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     legend.position = "bottom",
#     legend.background = element_blank(),
#     legend.box.background = element_rect(colour = "white"),
#     legend.text = TextType,
#     strip.text = TextType,
#     text = TextType,
#     axis.text.x = TextType,
#     axis.text.y = TextType,
#     axis.title.y = TextType,
#     axis.title.x = TextType
#   ) 
# 
# 
# 
# 
# ## Export here
# CLPH_Plot %>% 
#   ggsave(
#     device = "png",
#     filename = paste0(here(), 
#                       "/Output/Figures/",
#                       "ThreeCrops_ThresholdSA_V2_CLPH_Plot.png"), 
#     width = 25,
#     height = 15,
#     units = "cm",
#     dpi = 500
#   )
# 

