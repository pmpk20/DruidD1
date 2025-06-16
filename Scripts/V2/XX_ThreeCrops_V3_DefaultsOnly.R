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


Wheat <- here("Output/LoopData", 
              "Wheat_Defaults_V3.csv") %>% 
  fread() %>% 
  data.frame()


Barley <- here("Output/LoopData", 
               "Barley_Defaults_V3.csv") %>% 
  fread() %>% 
  data.frame()


OSR <- here("Output/LoopData", 
            "OSR_Defaults_V3.csv") %>% 
  fread() %>% 
  data.frame()


# ******************************************************************************
#### Section 2: Merge all ####
# ******************************************************************************


## Common colnames here
Headers <- 
  c(
    "CLPH_Density",    
    "CLPH_NE",         
    "CLPH_Type",       
    "CLPH_Means",      
    "CLPH_SD",         
    "CLPH_Ymin",       
    "CLPH_Ymax",       
    "CLPH_Threshold",  
    "CLPH_Insecticide",
    
    "CLPH_Percent_Density",    
    "CLPH_Percent_NE",         
    "CLPH_Percent_Type",       
    "CLPH_Percent_Means",      
    "CLPH_Percent_SD",         
    "CLPH_Percent_Ymin",       
    "CLPH_Percent_Ymax",       
    "CLPH_Percent_Threshold",  
    "CLPH_Percent_Insecticide",
    
    
    "LPH_Density",     
    "LPH_NE",          
    "LPH_Type",        
    "LPH_Means",       
    "LPH_SD",          
    "LPH_Ymin",        
    "LPH_Ymax",        
    "LPH_Threshold",   
    "LPH_Insecticide", 
    "YL_Density",      
    "YL_NE",           
    "YL_Type",         
    "YL_Means",        
    "YL_SD",           
    "YL_Ymin",         
    "YL_Ymax",         
    "YL_Threshold",    
    "YL_Insecticide",  
    "Crop" 
  )



## Set here
colnames(Wheat) <- Headers
colnames(Barley) <- Headers
colnames(OSR) <- Headers


## append rows here
Data <- rbind(Wheat,
              Barley,
              OSR)



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


## Filter for non-AS fields as AS doesn't change with NEs
Data_NoAS <- Data %>% dplyr::filter(YL_Type != "AS")


# ******************************************************************************
#### PERCENT CLPH ####
# ******************************************************************************



## Plot Yield without AS
CLPH_Plot_Percent <- 
  
  Data_NoAS %>% 
  ggplot(aes(x = CLPH_Percent_NE %>% as.numeric(),
             group = CLPH_Percent_Density %>% as.factor())) +
  
  geom_line(aes(y = CLPH_Percent_Means, 
                color = CLPH_Percent_Density %>% as.factor()),
            linewidth = 1) +
  
  geom_point(aes(y = CLPH_Percent_Means, 
                 color = CLPH_Percent_Density %>% as.factor())) +
  
  geom_ribbon(
    aes(
      y = CLPH_Percent_Means,
      ymin = CLPH_Percent_Ymin,
      ymax = CLPH_Percent_Ymax,
      fill = CLPH_Percent_Density %>% as.factor()
    ),
    outline.type = "both",
    alpha = 0.2
  ) +
  
  theme_bw() +
  
  facet_grid(CLPH_Percent_Type ~ Crop, 
             labeller = as_labeller(c(Barley = "Barley", 
                                      OSR = "OSR", 
                                      Wheat = "Wheat",
                                      "ET" = "Economic Threshold", 
                                      "NS" = "Never Spray")),
             scales = "free_y") +
  
  geom_hline(yintercept = 0) +
  
  scale_color_manual(name = "Pest density",
                     values = PlotColours) +
  
  scale_fill_manual(name = "Pest density",
                    values = PlotColours) +
  
  scale_y_continuous(
    name = "Average annual percentage lost yield per hectare compared to 100% NEs",
    labels = scales::percent_format(scale = 1, 
                                    suffix = "%")) +
  
  scale_x_continuous(
    name = "Natural enemy presence as percentage.",
    breaks = seq.int(from = 0, to = 1, by = 0.25),
    labels  = seq.int(from = 0, to = 1, by = 0.25) %>%
      sprintf("%.2f", .)
  )  +
  
  # guides(fill = "none") + ## changed FALSE to NONE
  
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
    axis.text.x = TextType,
    axis.text.y = TextType,
    axis.title.y = TextType,
    axis.title.x = TextType,
    panel.spacing.x = unit(0.75, "cm"),
    panel.spacing.y = unit(0.75, "cm")
  ) 




## Export here
CLPH_Plot_Percent %>% 
  ggsave(
    device = "png",
    filename = paste0(here(), 
                      "/Output/Figures/",
                      "CLPH_Percent_ThreeCrops_V3.png"), 
    width = 25,
    height = 15,
    units = "cm",
    dpi = 500
  )


## Display data for summary sentences:
Data %>% 
  dplyr::filter(CLPH_NE == 0.9 &
                  CLPH_Density %in% c(5.0, 10.0),
                CLPH_Type == "NS") %>% 
  dplyr::group_by(Crop) %>% 
  dplyr::mutate(CLPH_Means %>% round(2)) %>% 
  dplyr::select(CLPH_Means) %>% 
  dplyr::summarise(paste0("£", 
                          max(CLPH_Means) %>% round(2) %>% abs(), 
                          "ph - £", 
                          min(CLPH_Means) %>% round(2) %>% abs(),
                          "ph"))

# ******************************************************************************
#### Raw CLPH ####
# ******************************************************************************



## Plot Yield without AS
CLPH_Plot <- 
  
  Data_NoAS %>% 
  ggplot(aes(x = CLPH_NE %>% as.numeric(),
             group = CLPH_Density %>% as.factor())) +
  
  geom_line(aes(y = CLPH_Means, 
                color = CLPH_Density %>% as.factor()),
            linewidth = 1) +
  
  geom_point(aes(y = CLPH_Means, 
                 color = CLPH_Density %>% as.factor())) +
  
  geom_ribbon(
    aes(
      y = CLPH_Means,
      ymin = CLPH_Ymin,
      ymax = CLPH_Ymax,
      fill = CLPH_Density %>% as.factor()
    ),
    outline.type = "both",
    alpha = 0.2
  ) +
  
  theme_bw() +
  
  facet_wrap(CLPH_Type ~ Crop, 
             labeller = as_labeller(c(Barley = "Barley", 
                                      OSR = "OSR", 
                                      Wheat = "Wheat",
                                      "ET" = "Economic Threshold", 
                                      "NS" = "Never Spray")),
             scales = "free_y") +
  
  geom_hline(yintercept = 0) +
  
  scale_color_manual(name = "Pest density",
                     labels = c("Low", "Medium", "High"),
                     values = PlotColours) +
  
  scale_fill_manual(name = "Pest density",
                    labels = c("Low", "Medium", "High"),
                    values = PlotColours) +
  
  scale_y_continuous(
    name = "Average annual percentage lost yield per hectare compared to 100% NEs",
    labels = scales::label_currency(prefix = "£")) +
  
  scale_x_continuous(
    name = "Natural enemy presence as percentage.",
    breaks = seq.int(from = 0, to = 1, by = 0.25),
    labels  = seq.int(from = 0, to = 1, by = 0.25) %>%
      sprintf("%.2f", .)
  )  +
  
  # guides(fill = "none") + ## changed FALSE to NONE
  
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
    axis.text.x = TextType,
    axis.text.y = TextType,
    axis.title.y = TextType,
    axis.title.x = TextType,
    panel.spacing.x = unit(0.75, "cm"),
    panel.spacing.y = unit(0.75, "cm")
  ) 




## Export here
CLPH_Plot %>% 
  ggsave(
    device = "png",
    filename = paste0(here(), 
                      "/Output/Figures/",
                      "CLPH_ThreeCrops_V3.png"), 
    width = 25,
    height = 15,
    units = "cm",
    dpi = 500
  )


## Display data for summary sentences:
Data %>% 
  dplyr::filter(CLPH_NE == 0.9 &
                  CLPH_Density %in% c(5.0, 10.0),
                CLPH_Type == "NS") %>% 
  dplyr::group_by(Crop) %>% 
  dplyr::mutate(CLPH_Means %>% round(2)) %>% 
  dplyr::select(CLPH_Means) %>% 
  dplyr::summarise(paste0("£", 
                          max(CLPH_Means) %>% round(2) %>% abs(), 
                          "ph - £", 
                          min(CLPH_Means) %>% round(2) %>% abs(),
                          "ph"))





Figure2_Draft <- 
  Data_NoAS %>% 
  ggplot(aes(x = CLPH_NE %>% as.numeric(),
             group = CLPH_Density %>% as.factor())) +
  
  geom_line(aes(y = CLPH_Means, 
                color = CLPH_Density %>% as.factor()),
            linewidth = 1) +
  
  geom_point(aes(y = CLPH_Means, 
                 color = CLPH_Density %>% as.factor())) +
  
  geom_ribbon(
    aes(
      y = CLPH_Means,
      ymin = CLPH_Ymin,
      ymax = CLPH_Ymax,
      fill = CLPH_Density %>% as.factor()
    ),
    outline.type = "both",
    alpha = 0.2
  ) +
  
  theme_bw() +
  
  facet_grid(Crop ~ CLPH_Type, 
             labeller = as_labeller(c(Barley = "Barley", 
                                      OSR = "OSR", 
                                      Wheat = "Wheat",
                                      "ET" = "Economic Threshold", 
                                      "NS" = "Never Spray")),
             scales = "free_y") +
  
  geom_hline(yintercept = 0) +
  
  scale_color_manual(name = "Pest density",
                     labels = c("Low", "Medium", "High"),
                     values = PlotColours) +
  
  scale_fill_manual(name = "Pest density",
                    labels = c("Low", "Medium", "High"),
                    values = PlotColours) +
  
  scale_y_continuous(
    name = "Average annual percentage lost yield per hectare\ncompared to 100% NEs",
    labels = scales::label_currency(prefix = "£")) +
  
  scale_x_continuous(
    name = "Natural enemy presence as percentage.",
    breaks = seq.int(from = 0, to = 1, by = 0.25),
    labels  = seq.int(from = 0, to = 1, by = 0.25) %>%
      sprintf("%.2f", .)
  )  +
  
  # guides(fill = "none") + ## changed FALSE to NONE
  
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
    axis.text.x = element_text(size = TextSize,
                               colour = "black",
                               family = TextFamily,
                               angle = 30,
                               hjust = 0.75),
    axis.text.y = TextType,
    axis.title.y = TextType,
    axis.title.x = TextType,
    panel.spacing.x = unit(0.75, "cm"),
    panel.spacing.y = unit(0.75, "cm"),
    strip.text.y.right = element_text(size = TextSize,
                                      colour = "black",
                                      family = TextFamily,
                                      angle = 0)
  ) 


Figure2_Draft %>% 
  ggsave(
    device = "png",
    filename = paste0(here(), 
                      "/Output/Figures/",
                      "CLPH_Plot_ThreeCrops_V3_Draft.png"), 
    width = 25,
    height = 15,
    units = "cm",
    dpi = 500
  )


# ******************************************************************************
#### Headline LPH ####
# ******************************************************************************



## Plot Yield without AS
LPH_Plot <- Data %>% 
  ggplot(aes(x = LPH_NE %>% as.numeric(),
             group = LPH_Density %>% as.factor())) +
  
  geom_line(aes(y = LPH_Means, 
                color = LPH_Density %>% as.factor()),
            linewidth = 1) +
  
  geom_point(aes(y = LPH_Means, 
                 color = LPH_Density %>% as.factor())) +
  
  geom_ribbon(
    aes(
      y = LPH_Means,
      ymin = LPH_Ymin,
      ymax = LPH_Ymax,
      fill = LPH_Density %>% as.factor()
    ),
    outline.type = "both",
    alpha = 0.2
  ) +
  
  theme_bw() +
  
  facet_wrap(LPH_Type ~ Crop, 
             labeller = as_labeller(c(Barley = "Barley", 
                                      OSR = "OSR", 
                                      Wheat = "Wheat",
                                      "AS" = "Always Spray",
                                      "ET" = "Economic Threshold", 
                                      "NS" = "Never Spray")),
             scales = "free_y") +
  
  geom_hline(yintercept = 0) +
  
  scale_color_manual(name = "Pest density",
                     labels = c("Low", "Medium", "High"),
                     values = PlotColours) +
  
  scale_fill_manual(name = "Pest density",
                    labels = c("Low", "Medium", "High"),
                    values = PlotColours) +
  
  scale_y_continuous(name = "Mean lost yield\nper hectare ",
                     breaks = seq.int(from = 0, to = 500, by = 100),
                     labels = scales::label_currency(prefix = "£")) +
  
  scale_x_continuous(
    name = "Natural enemy presence as percentage.",
    breaks = seq.int(from = 0, to = 1, by = 0.25),
    labels  = seq.int(from = 0, to = 1, by = 0.25) %>%
      sprintf("%.2f", .)
  )  +
  
  # guides(fill = "none") + ## changed FALSE to NONE
  
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
    axis.text.x = TextType,
    axis.text.y = TextType,
    axis.title.y = TextType,
    axis.title.x = TextType,
    panel.spacing.x = unit(0.75, "cm"),
    panel.spacing.y = unit(0.75, "cm")
  ) 


## Export here
LPH_Plot %>% 
  ggsave(
    device = "png",
    filename = paste0(here(), 
                      "/Output/Figures/",
                      "LPH_Plot_ThreeCrops_V3.png"), 
    width = 25,
    height = 15,
    units = "cm",
    dpi = 500
  )


## Display data for summary sentences:
Data %>% 
  dplyr::filter(CLPH_NE == 0.9 &
                  CLPH_Density %in% c(5.0, 10.0),
                CLPH_Type == "NS") %>% 
  dplyr::group_by(Crop) %>% 
  dplyr::mutate(CLPH_Means %>% round(2)) %>% 
  dplyr::select(CLPH_Means) %>% 
  dplyr::summarise(paste0("£", 
                          max(CLPH_Means) %>% round(2) %>% abs(), 
                          "ph - £", 
                          min(CLPH_Means) %>% round(2) %>% abs(),
                          "ph"))




#### ALTERNATIVE PLOT # ******************************************************************************


Figure3_Draft <- Data %>% 
  ggplot(aes(x = LPH_NE %>% as.numeric(),
             group = LPH_Density %>% as.factor())) +
  
  geom_line(aes(y = LPH_Means, 
                color = LPH_Density %>% as.factor()),
            linewidth = 1) +
  
  geom_point(aes(y = LPH_Means, 
                 color = LPH_Density %>% as.factor())) +
  
  geom_ribbon(
    aes(
      y = LPH_Means,
      ymin = LPH_Ymin,
      ymax = LPH_Ymax,
      fill = LPH_Density %>% as.factor()
    ),
    outline.type = "both",
    alpha = 0.2
  ) +
  
  theme_bw() +
  
  facet_grid(Crop ~ LPH_Type, 
             labeller = as_labeller(c(Barley = "Barley", 
                                      OSR = "OSR", 
                                      Wheat = "Wheat",
                                      "AS" = "Always Spray",
                                      "ET" = "Economic Threshold", 
                                      "NS" = "Never Spray")),
             scales = "free_y") +
  
  geom_hline(yintercept = 0) +
  
  scale_color_manual(name = "Pest density",
                     labels = c("Low", "Medium", "High"),
                     values = PlotColours) +
  
  scale_fill_manual(name = "Pest density",
                    labels = c("Low", "Medium", "High"),
                    values = PlotColours) +
  
  scale_y_continuous(name = "Mean lost yield\nper hectare (£ph)",
                     breaks = seq.int(from = 0, to = 500, by = 100),
                     labels = scales::label_currency(prefix = "£")) +
  
  scale_x_continuous(
    name = "Natural enemy presence as percentage.",
    breaks = seq.int(from = 0, to = 1, by = 0.25),
    labels  = seq.int(from = 0, to = 1, by = 0.25) %>%
      sprintf("%.2f", .)
  )  +
  
  # guides(fill = "none") + ## changed FALSE to NONE
  
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
    axis.text.x = element_text(size = TextSize,
                               colour = "black",
                               family = TextFamily,
                               angle = 30,
                               hjust = 0.75),
    axis.text.y = TextType,
    axis.title.y = TextType,
    axis.title.x = TextType,
    panel.spacing.x = unit(0.75, "cm"),
    panel.spacing.y = unit(0.75, "cm"),
    strip.text.y.right = element_text(size = TextSize,
                                      colour = "black",
                                      family = TextFamily,
                                      angle = 0)
  ) 



## Export here
Figure3_Draft %>% 
  ggsave(
    device = "png",
    filename = paste0(here(), 
                      "/Output/Figures/",
                      "LPH_ThreeCrops_V3_Draft.png"), 
    width = 25,
    height = 15,
    units = "cm",
    dpi = 500
  )


# ******************************************************************************
#### Yield loss ####
# ******************************************************************************



## Plot Yield without AS
YL_Plot <- 
  Data_NoAS %>% 
  ggplot(aes(x = YL_NE %>% as.numeric(),
             group = YL_Density %>% as.factor())) +
  
  geom_line(aes(y = YL_Means, 
                color = YL_Density %>% as.factor()),
            linewidth = 1) +
  
  geom_point(aes(y = YL_Means, 
                 color = YL_Density %>% as.factor())) +
  
  geom_ribbon(
    aes(
      y = YL_Means,
      ymin = YL_Ymin,
      ymax = YL_Ymax,
      fill = YL_Density %>% as.factor()
    ),
    outline.type = "both",
    alpha = 0.2
  ) +
  
  theme_bw() +
  
  facet_wrap(YL_Type ~ Crop, 
             labeller = as_labeller(c(Barley = "Barley", 
                                      OSR = "OSR", 
                                      Wheat = "Wheat",
                                      "ET" = "Economic Threshold", 
                                      "NS" = "Never Spray")),
             scales = "free_y") +
  
  geom_hline(yintercept = 0, 
             alpha = 0.25, 
             linetype = "dashed") +
  
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
  

scale_y_continuous(
  name = "Average annual lost yield per hectare",
  labels = scales::percent_format(scale = 1, 
                                  suffix = "%")
) +
  
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
    axis.text.x = TextType,
    axis.text.y = TextType,
    axis.title.y = TextType,
    axis.title.x = TextType,
    panel.spacing.x = unit(0.75, "cm"),
    panel.spacing.y = unit(0.75, "cm")
    # strip.text.y.right = element_text(size = TextSize,
    #                                   colour = "black",
    #                                   family = TextFamily,
    #                                   angle = 0)
  ) 


## Export here
YL_Plot %>% 
  ggsave(
    device = "png",
    filename = paste0(here(), 
                      "/Output/Figures/",
                      "YL_ThreeCrops_V3.png"), 
    width = 25,
    height = 15,
    units = "cm",
    dpi = 500
  )







## Display data for summary sentences:
Data %>% 
  dplyr::filter(YL_NE == 0.9 &
                  YL_Density %in% c(5.0, 10.0),
                YL_Type == "NS") %>% 
  dplyr::group_by(Crop) %>% 
  dplyr::mutate(YL_Means %>% round(2)) %>% 
  dplyr::select(YL_Means) %>% 
  dplyr::summarise(paste0(min(YL_Means) %>% round(2) %>% abs(), 
                          "% - ", 
                          max(YL_Means) %>% round(2) %>% abs(),
                          "%"))





Figure1_Draft <- 
Data_NoAS %>% 
  ggplot(aes(x = YL_NE %>% as.numeric(),
             group = YL_Density %>% as.factor())) +
  
  geom_line(aes(y = YL_Means, 
                color = YL_Density %>% as.factor()),
            linewidth = 1) +
  
  geom_point(aes(y = YL_Means, 
                 color = YL_Density %>% as.factor())) +
  
  geom_ribbon(
    aes(
      y = YL_Means,
      ymin = YL_Ymin,
      ymax = YL_Ymax,
      fill = YL_Density %>% as.factor()
    ),
    outline.type = "both",
    alpha = 0.2
  ) +
  
  theme_bw() +
  
  facet_grid(Crop ~ YL_Type, 
             labeller = as_labeller(c(Barley = "Barley", 
                                      OSR = "OSR", 
                                      Wheat = "Wheat",
                                      "ET" = "Economic Threshold", 
                                      "NS" = "Never Spray")),
             scales = "free_y") +
  
  geom_hline(yintercept = 0, 
             alpha = 0.25, 
             linetype = "dashed") +
  
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
  
  
  scale_y_continuous(
    name = "Average annual lost yield per hectare",
    labels = scales::percent_format(scale = 1, 
                                    suffix = "%")
  ) +
  
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
    axis.text.x = element_text(size = TextSize,
                               colour = "black",
                               family = TextFamily,
                               angle = 30,
                               hjust = 0.75),
    axis.text.y = TextType,
    axis.title.y = TextType,
    axis.title.x = TextType,
    panel.spacing.x = unit(0.75, "cm"),
    panel.spacing.y = unit(0.75, "cm"),
    strip.text.y.right = element_text(size = TextSize,
                                      colour = "black",
                                      family = TextFamily,
                                      angle = 0))


## Export here
Figure1_Draft %>% 
  ggsave(
    device = "png",
    filename = paste0(here(), 
                      "/Output/Figures/",
                      "YL_ThreeCrops_V3_Draft.png"), 
    width = 25,
    height = 15,
    units = "cm",
    dpi = 500
  )


# ******************************************************************************
#### END OF SCRIPT ####
# ******************************************************************************