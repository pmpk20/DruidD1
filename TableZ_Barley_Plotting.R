#### DRUID D1: Intro plotting of Barley data  ###############
# Function: To plot Barley data
# Author: Dr Peter King (p.king1@Leeds.ac.uk)
# Last Edited: 12/10/23


# *****************************************************************************
#### Section 0: Setting up ####
## NOTES: This is just importing packages.
# *****************************************************************************



## sessionInfo()---------------------------------------------------------------
# R version 4.3.1 (2023 - 06 - 16 ucrt)
# Platform:x86_64 - w64 - mingw32 / x64 (64 - bit)
# Running under:Windows 10 x64 (build 19045)
#
# Matrix products:default
#
#
# locale:[1] LC_COLLATE = English_United Kingdom.utf8  LC_CTYPE = English_United Kingdom.utf8
# [3] LC_MONETARY = English_United Kingdom.utf8 LC_NUMERIC = C
# [5] LC_TIME = English_United Kingdom.utf8
#
# time zone:Europe / London
# tzcode source:internal
#
# attached base packages:[1] stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:[1] RColorBrewer_1.1 - 3 lubridate_1.9.2    forcats_1.0.0      stringr_1.5.0      dplyr_1.1.2
# [6] purrr_1.0.1        readr_2.1.4        tidyr_1.3.0        tibble_3.2.1       tidyverse_2.0.0
# [11] ggplot2_3.4.2      readxl_1.4.2       here_1.0.1         magrittr_2.0.3
#
# loaded via a namespace (and not attached):[1] utf8_1.2.3        generics_0.1.3    stringi_1.7.12    lattice_0.21 -
#   8    hms_1.1.3
# [6] grid_4.3.1        timechange_0.2.0  cellranger_1.1.0  rprojroot_2.0.3   Matrix_1.5 -
#   4.1
# [11] processx_3.8.1    pkgbuild_1.4.2    ps_1.7.5          mgcv_1.8 - 42       fansi_1.0.4
# [16] scales_1.2.1      cli_3.6.1         rlang_1.1.1       crayon_1.5.2      munsell_0.5.0
# [21] splines_4.3.1     withr_2.5.0       tools_4.3.1       tzdb_0.4.0        colorspace_2.1 -
#   0
# [26] vctrs_0.6.3       R6_2.5.1          lifecycle_1.0.3   pkgconfig_2.0.3   callr_3.7.3
# [31] pillar_1.9.0      gtable_0.3.3      glue_1.6.2        tidyselect_1.2.0  rstudioapi_0.14
# [36] farver_2.1.1      nlme_3.1 - 162      labeling_0.4.2    compiler_4.3.1    prettyunits_1.1.1


rm(list = ls())
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

# ******************************************************************************
#### Section 1: Data Importing ####
## Here we import all the crop data and isolate the Barley estimates
# ******************************************************************************


Data <-
  here("MarketData","AllCropData_V1.xlsx") %>% readxl::read_xlsx(sheet = "Sheet1") %>% data.frame()


Barley <- Data[Data$Crop == "Barley", ] 


# ******************************************************************************
#### Section 2: Cleaning and trimming data ####
## We clean the data, drop missing years and name the variables we need
# ******************************************************************************


## Trim years column names
Years <- Barley[3:51] %>% colnames() %>% gsub(pattern = "X", replacement = "") %>% as.numeric()
colnames(Barley) <- c("Crop", "Measure", Years)

## Drop some incomplete years
Barley_Trimmed <- Barley[, c(1:2, 41:51)] 

Barley_Trimmed_DF <- as.data.frame(x = t(Barley_Trimmed[, 3:13]), stringsAsFactors = FALSE)
colnames(Barley_Trimmed_DF) <- Barley_Trimmed$Measure

Barley_Trimmed_DF$Area %<>% as.numeric()
Barley_Trimmed_DF$Yield %<>% as.numeric()
Barley_Trimmed_DF$Volume %<>% as.numeric()
Barley_Trimmed_DF$ValueReal %<>% as.numeric()
Barley_Trimmed_DF$`Malting barley real` %<>% as.numeric()
Barley_Trimmed_DF$Area_Total <- Barley_Trimmed_DF$Area * 1000


## These are used later
Area <- Barley_Trimmed_DF$Area_Total
Price <- Barley_Trimmed_DF$`Malting barley real`
Yield <- Barley_Trimmed_DF$Yield
Value_PH <- Price * Yield 


## For the table we're going to use the latest year data for now
## Hence 2021 data
Column_Area <- Area[11]
Column_Yield <- Yield[11]
Column_Price <- Price[11]


Variable_Area <- Area
Variable_Yield <- Yield
Variable_Price <- Price
Variable_Years <- Barley_Trimmed_DF %>% rownames() %>% as.numeric()


# ******************************************************************************
#### Section 3: Define initial variable values ####
## So we use the low/med/high scenarios from Zhang et al. 2018
## We need the following variables:
# area, price, yield [all from the market data above]
## aphids, density, NEs, yield loss [calculated below]
# ******************************************************************************



## Aphids  *****************************************************************

## Aphid density for low/medium/high levels
Aphids_Low <- 5
Aphids_Medium <- 7.5
Aphids_High  <- 10



## NEs  *****************************************************************

## Presence/absence of NEs
## Easier to use one value but this sequence makes a column for the table
# NE <- seq.int(from = 0, to = 1, by = 0.25)

## Adding a slot for 34% reduction figure
NE <- c(0.01, seq.int(from = 0.1, to = 1, by = 0.1))
Column_NE <- rep(
  NE,
  times = 3
)


## Corresponds to: c("Low", "Medium", "High"),
Column_Density <- rep(
  c(5, 7.5, 10),
  each = Column_NE %>% unique() %>% length()
)



# ******************************************************************************
#### Section 4: Useful functions ####
## We clean the data, drop missing years and name the variables we need
# ******************************************************************************



## Define here for later
## Lots of gsub to fix formatting
Transformer <- function(Input) {
  Input %>%
    round(2) %>% 
    as.numeric() %>%
    format(trim = TRUE, 
           nsmall = 2,
           big.mark = ",") %>%
    paste0("£", .)
}



## Makes the input report zero if negative valued
Constrainer <- function(Input) {
  ifelse(Input < 0,
         0,
         Input) 
}



## Calculate and format the percentage change in LPH
Summarise_LPH_PCT <- function(Data, New, Old) {
  
  Input <- ((Data[, New] - Data[, Old])/Data[, Old]) %>% 
    data.frame() %>% 
    summarise_all(list(~ str_c(
      mean(.) %>% round(2) %>% sprintf("%.2f",. ),
      "% (", 
      sd(.) %>% round(2) %>% sprintf("%.2f",. ), 
      "%)"))) 
  
  return(Input %>% as.character())
  
}



## For given changes in variables this reports mean, median, sd and quantiles
Summarise_LPH_All <- function(Data, New, Old) {
  
  Input <- ((Data[, New] - Data[, Old])) %>% 
    data.frame() %>% 
    summarise(across(everything(), list(
      "Mean" = mean, 
      "Median" = median, 
      "SD" = sd, 
      "Y0" = ~quantile(., 0.05), 
      "Y25" = ~quantile(., 0.25),
      "Y75" = ~quantile(., 0.75), 
      "Y100" = ~quantile(., 0.95))))
  
  return(Input %>% data.frame())
  
}



# ******************************************************************************
#### Section 5: Estimate REA value ####
## Using data from Schmidt et al. 2004
## The REA parameter is important for the interaction of Aphids:NEs
# ******************************************************************************


## Setup  *****************************************************************

# If you have the standard error (SE) and 
# want to compute the standard deviation (SD) from it, 
# simply multiply it by the square root of the sample size.
SampleSize <- 72 ## number of plots they report
SEtoSD <- sqrt(SampleSize)
DistSize <- 1000


# S.Avenae: 59.1	±	13.12		9.1	±	3.92
# R.Padi: 8.7	±	3.05		23.1± 	6.46
# Total before: 67.8 + 16.07 
# Total  after: 32.2 + 10.38 


## Control *****************************************************************
## Write down the parameters from the REA ##
## then generate a distribution ##
Control_Mean <- 67.8
Control_SE <- 16.07
Control_SD <- Control_SE %>% multiply_by(SEtoSD)
Control_Distribution <- rnorm(n = DistSize, mean = Control_Mean, sd = Control_SD) 


## Treatment *****************************************************************

Treatment_Mean <- 32.2
Treatment_SE <- 10.38
Treatment_SD <- Treatment_SE %>% multiply_by(SEtoSD)
Treatment_Distribution <- rnorm(n = DistSize, mean = Treatment_Mean, sd = Treatment_SD) 

## PCT Change  ****************************************************************

## Calculate mean change from the entire distribution ##
REA_Old <- 0.4749263 ## (83-366)/366
REA_New <- (((Treatment_Distribution - Control_Distribution) / Control_Distribution)) %>% abs()


## Now constraint to [0, 1]
REA_New <- ifelse(REA_New > 1, 
                  1, 
                  ifelse(REA_New < 0 , 
                         0 , 
                         REA_New))


# ******************************************************************************
#### Section 6: Initialise loop vectors ####
# ******************************************************************************


ColLength <- Column_NE %>% length()
YearLength <- Variable_Years %>% length()


## Nrow = 15: 5 levels of NEs * 3 scenarios for aphids
## Ncol = 6: Mean +  SD for 3 variables: YL, LPH, LT
Estimates <- matrix(0, nrow = ColLength, ncol = 6) %>% data.frame()


## Filling in per the annual loops
Annual_YL <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_YL_Raw <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_YL_Mean <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_YL_SD <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
## These vectors store data on changes in LPH 
Annual_YL_ChangeMeans <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_YL_ChangeSDs <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_YL_Change_Quantile_Y0 <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_YL_Change_Quantile_Y25 <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_YL_Change_Quantile_Y50 <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_YL_Change_Quantile_Y75 <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_YL_Change_Quantile_Y100 <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
YieldLosss_Total_RawData <- matrix(0, 
                                   nrow = DistSize, 
                                   ncol = ColLength) %>% data.frame()



## These vectors store data on changes in LPH 
Annual_LPH <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_ChangeMeans <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_ChangeSDs <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Change_Quantile_Y0 <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Change_Quantile_Y25 <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Change_Quantile_Y50 <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Change_Quantile_Y75 <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Change_Quantile_Y100 <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
LossPerHectare_Total_RawData <- matrix(0, 
                                       nrow = DistSize, 
                                       ncol = ColLength) %>% data.frame()




## Store changes in loss total
Annual_LT <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LT_Raw <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LT_Mean <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LT_SD <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LT_ChangeMeans <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LT_ChangeSDs <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LT_Change_Quantile_Y0 <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LT_Change_Quantile_Y25 <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LT_Change_Quantile_Y50 <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LT_Change_Quantile_Y75 <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LT_Change_Quantile_Y100 <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
LossTotal_Total_RawData <- matrix(0, 
                                  nrow = DistSize, 
                                  ncol = ColLength) %>% data.frame()




## Initialise values here
Threshold <- 5
Insecticide_PH <- 282
Field_NS <- 0.007
Field_ET <- 0.37
Field_AS <- 1 - Field_NS - Field_ET


# ******************************************************************************
#### Section 4A: Loop ####
# ******************************************************************************

## Loop through each density of aphids and report a distribution of estimates ##
## then report mean (SD) from that distribution ##


for (Year in 1:length(Variable_Years)) {
  for (i in 1:length(Column_NE)) {
    
    
    ## Defining pests  *****************************************************************
    ## This is the central calculation
    ## For each value of aphids and NEs, we simulate the response
    Dist <- c(Column_Density[i] * (1 - (REA_New * Column_NE[i])))
    
    
    ## Correct false zeroes AND convert to aphid days
    Dist <- Dist %>% ifelse(. == 0,
                            0.001,
                            .) %>% multiply_by(10)
    
    
    ## YIELD LOSS  *****************************************************************
    ## We now calculate the yield loss for each value
    YieldLoss_NS <- 12.70 %>%
      multiply_by(Dist) %>% 
      raise_to_power(0.65) %>% 
      divide_by(100) %>% 
      ifelse(is.infinite(.), 0, .) %>% 
      ifelse(. < 0, 0, .)
    
    
    YieldLoss_AS <- 0 
    
    
    YieldLoss_ET <- ifelse(Dist >= (Threshold * 10), 0, Dist) %>%  ## so only loss if density > threshold 
      multiply_by(12.70) %>% ## 4.5ln(AphidDensity)
      raise_to_power(0.65) %>% 
      divide_by(100) %>% ## get as percentage change
      ifelse(is.infinite(.), 0, .) %>% ## infs if log(0) so check for that
      ifelse(. < 0, 0.0000001, .) ## change negatives to zeroes
    
    
    
    
    ## LOSS PER HECTARE  *****************************************************************
    ## Now calculate loss per hectare and total
    ## for every possibility
    LossPerHectare_NS <- (Variable_Yield[Year] * Variable_Price[Year]) *  YieldLoss_NS
    
    
    LossPerHectare_ET <- ifelse(
      YieldLoss_ET == 0,
      Insecticide_PH,
      ifelse(
        YieldLoss_ET == 0.0000001,
        0,
        YieldLoss_ET %>% multiply_by(Variable_Yield[Year] * Variable_Price[Year])))
    
    
    ## Moving this bit here to correct the placeholder bug
    YieldLoss_ET <- YieldLoss_ET %>% ifelse(. == 0.0000001, 0, .)
    YieldLoss_Total <- 
      (YieldLoss_NS * Field_NS) + 
      (YieldLoss_ET * Field_ET) + 
      (YieldLoss_AS * Field_AS)
    YieldLosss_Total_RawData[, i] <- YieldLoss_Total
    
    
    LossPerHectare_AS <- (Variable_Yield[Year] * Variable_Price[Year])  %>% ## per hectare value
      multiply_by(YieldLoss_AS) %>% ## lost yield percent times value
      add(Insecticide_PH) ## plus cost of sprays
    
    
    # LossPerHectare <- (Variable_Yield[Year] * Variable_Price[Year]) *  YieldLoss_Total
    
    LossPerHectare <- 
      (LossPerHectare_NS * Field_NS) + 
      (LossPerHectare_ET * Field_ET) + 
      (LossPerHectare_AS * Field_AS)
    LossPerHectare_Total_RawData[, i] <- LossPerHectare
    
    
    ## TOTAL LOSS  *****************************************************************
    ## Now calculate loss per hectare and total
    LossTotal_NS <- (Variable_Area[Year] * LossPerHectare_NS)
    
    LossTotal_AS <- (Variable_Area[Year] * LossPerHectare_AS)
    
    
    LossTotal_ET <- (Variable_Area[Year] * LossPerHectare_ET)
    
    LossTotal_Total <- (LossTotal_NS * Field_NS) + 
      (LossTotal_ET * Field_ET) + 
      (LossTotal_AS * Field_AS)
    LossTotal_Total_RawData[, i] <- LossTotal_Total
    
    
    
    ## FILL COLUMNS  *****************************************************************
    ## Furnish each column with mean and sd of each distribution
    Estimates[i, 1] <- YieldLoss_Total %>% mean(na.rm = TRUE)
    Estimates[i, 2] <- YieldLoss_Total %>% sd(na.rm = TRUE)
    
    Estimates[i, 3] <- LossPerHectare %>% mean(na.rm = TRUE)
    Estimates[i, 4] <- LossPerHectare %>% sd(na.rm = TRUE)
    
    Estimates[i, 5] <- LossTotal_Total %>% mean(na.rm = TRUE)
    Estimates[i, 6] <- LossTotal_Total %>% sd(na.rm = TRUE)

    
    ## OUTPUT NICELY: YIELD  *****************************************************************
    Output_Yield <- paste0(
      Estimates[, 1] %>% Constrainer() %>% multiply_by(100) %>% round(2) %>% sprintf("%.2f",. ) %>% paste0(., "%"),
      " (",
      Estimates[, 2] %>% Constrainer() %>% multiply_by(100) %>% round(2) %>% sprintf("%.2f",. ),
      ")"
    )
    
    Output_Yield_Raw <- Estimates[, 1] %>% multiply_by(100)
    
    
    
    ## OUTPUT NICELY: LPH  *****************************************************************
    Output_LPH <- paste0(
      Estimates[, 3] %>% Constrainer() %>% round(2) %>% sprintf("%.2f",. ),
      " (",
      Estimates[, 4] %>% Constrainer() %>% round(2) %>% sprintf("%.2f",. ),
      ")"
    )
    
    Output_LPH_Raw_Mean <- Estimates[, 3]
    Output_LPH_Raw_SD <- Estimates[, 4]
    
    
    
    ## OUTPUT NICELY: LT  *****************************************************************
    Output_LT <- paste0(
      Estimates[, 5] %>% 
        Constrainer() %>% 
        divide_by(1000000) %>% 
        Transformer() %>% 
        paste0(., "m"),
      " (",
      Estimates[, 6] %>% 
        Constrainer() %>% 
        divide_by(1000000) %>% 
        Transformer() %>% 
        paste0(., "m"),
      ")"
    )
    
    Output_LT_Raw <- Estimates[, 5]

  }
  
  Annual_YL[, Year] <- Output_Yield
  Annual_YL_Raw[, Year] <- Output_Yield_Raw
  
  Annual_LPH[, Year] <- Output_LPH
  Annual_LPH_Raw[, Year] <- Output_LPH_Raw_Mean
  
  Annual_LT[, Year] <- Output_LT
  Annual_LT_Raw[, Year] <- Output_LT_Raw
  

  ## Left for all baselines
  ## Right for all rows
  ## Trying to avoid actual numbers to allow us to flexibly define numbers
  Looper <- data.frame(
    "Left" = c(ColLength / 3 ,
               ColLength / 3 * 2, 
               ColLength) %>% rep(each = YearLength) , 
    "Right" = seq.int(from = 1, to = ColLength, by = 1)
  )
  
  
  for (Row in 1:nrow(Looper)){
    
    
    Output_LPH <- Summarise_LPH_All(LossPerHectare_Total_RawData, 
                                Looper[Row, 1], 
                                Looper[Row, 2])
    
    
    ## Store output
    Annual_LPH_ChangeMeans[Row, Year] <- Output_LPH["._Mean"] %>% as.numeric()
    Annual_LPH_ChangeSDs[Row, Year] <- Output_LPH["._SD"] %>% as.numeric()
    
    Annual_LPH_Change_Quantile_Y0[Row, Year] <- Output_LPH["._Y0"] %>% as.numeric()
    Annual_LPH_Change_Quantile_Y25[Row, Year] <- Output_LPH["._Y25"] %>% as.numeric()
    Annual_LPH_Change_Quantile_Y50[Row, Year] <- Output_LPH["._Median"] %>% as.numeric()
    Annual_LPH_Change_Quantile_Y75[Row, Year] <- Output_LPH["._Y75"] %>% as.numeric()
    Annual_LPH_Change_Quantile_Y100[Row, Year] <- Output_LPH["._Y100"] %>% as.numeric()
  }
  
}

# ******************************************************************************
#### Section 8: LABEL OUTPUTS ####
# ******************************************************************************


## Compile formatted outputs *************************************************
colnames(Annual_YL) <- Variable_Years
colnames(Annual_LPH) <- Variable_Years
colnames(Annual_LT) <- Variable_Years


## Label raw data  *************************************************
colnames(Annual_YL_Raw) <- Variable_Years
colnames(Annual_LPH_Raw) <- Variable_Years
colnames(Annual_LT_Raw) <- Variable_Years


## Label raw data  *************************************************
colnames(Annual_YL_Mean) <- Variable_Years
# colnames(Annual_YL_SD) <- Variable_Years
colnames(Annual_LT_Mean) <- Variable_Years
# colnames(Annual_LT_SD) <- Variable_Years


colnames(Annual_LPH_ChangeMeans) <- Variable_Years
colnames(Annual_LPH_ChangeSDs) <- Variable_Years



# ******************************************************************************
#### Section X1: Plot everything ####
# ******************************************************************************



## Plot Change vs NE facet by density and grouped by year 
Test_LPH_AllYears <- cbind(
  "Density" = Column_Density,
  "NE" = Column_NE,
  "Means" = Annual_LPH_ChangeMeans %>% as.matrix() %>% rowmeans(),
  "SD" = Annual_LPH_ChangeSDs %>% as.matrix() %>% rowmeans())



Test_LPH_AllYears_Quantiles <- cbind(
  "Density" = Column_Density,
  "NE" = Column_NE,
  "Y0" = Annual_LPH_Change_Quantile_Y0 %>% as.matrix() %>% rowmeans(),
  "Y25" = Annual_LPH_Change_Quantile_Y25 %>% as.matrix() %>% rowmeans(),
  "Y50" = Annual_LPH_Change_Quantile_Y50 %>% as.matrix() %>% rowmeans(),
  "Y75" = Annual_LPH_Change_Quantile_Y75 %>% as.matrix() %>% rowmeans(),
  "Y100" = Annual_LPH_Change_Quantile_Y100 %>% as.matrix() %>% rowmeans()) %>% 
  data.frame()


PlotData <- Test_LPH_AllYears_Quantiles %>% data.frame() %>% pivot_longer(cols = Y0:Y100, names_to = "variable")


## Histinterval plots
Test_LPH_AllYears %>% data.frame() %>%
  ggplot(
    aes(
      y = NE %>% as.factor(),
      xdist = dist_normal(Means, SD) %>% dist_truncated(upper = 0),
      group = Density %>% as.factor(),
      fill = Density %>% as.factor()
    )
  ) +
  stat_histinterval(normalize = "groups",
                    position = position_dodge(width = 1.1)) +
  theme_bw() +
  geom_vline(xintercept = 0) +
  scale_fill_manual(name = "Initial pest density in aphids/t",
                    values = RColorBrewer::brewer.pal(n = 9, 
                                                      name = "Blues")[c(5, 7, 9)], 
                    guide = guide_legend(reverse = TRUE)) +
  scale_x_continuous(name = "Change in lost yield against 100% presence",
                     breaks = seq.int(from = -250, to = 100, by = 25)) +
  scale_y_discrete(
    name = "Natural enemy presence as percentage.",
    breaks = seq.int(from = 0, to = 1, by = 0.1),
    labels  = seq.int(from = 0, to = 1, by = 0.1) %>%
      sprintf("%.2f", .)
  ) 




PlotData <- Test_LPH_AllYears %>% data.frame() 

## Long line plot
PlotData %>% 
  ggplot(
    aes(
      y = NE %>% as.numeric(),
      x = Means %>% as.numeric(),
      group = Density %>% as.factor(),
      colour = Density %>% as.factor()
    )
  ) +
  geom_line() + geom_point() +
  theme_bw() +
  geom_vline(xintercept = 0) +
  scale_fill_brewer(palette = "Blues", name = "Pest density") +
  scale_x_continuous(name = "Change in lost yield against 100% presence",
                     breaks = seq.int(from = -120, to = 0, by = 10)) +
  scale_y_continuous(
    name = "Natural enemy presence as percentage.",
    breaks = seq.int(from = 0, to = 1, by = 0.1),
    labels  = seq.int(from = 0, to = 1, by = 0.1) %>%
      sprintf("%.2f", .)
  )  +
  coord_flip()



## Stat_smooth method
### using linetype = 0 to hide loess line
Example1 <- ggplot(PlotData,
       aes(Means, NE, group = Density, colour = Density %>% as.factor()))+
  geom_smooth(orientation = "y", se = TRUE, linetype = 0) +
  geom_point() +
  geom_line() +
  theme_bw() +
  geom_vline(xintercept = 0) +
  scale_colour_manual(name = "Pest density", values = RColorBrewer::brewer.pal(n = 9, name = "Blues")[c(5, 7, 9)]) +
  scale_x_continuous(name = "Change in lost yield against 100% presence",
                     breaks = seq.int(from = -120, to = 0, by = 10)) +
  scale_y_continuous(
    name = "Natural enemy presence as percentage.",
    breaks = seq.int(from = 0, to = 1, by = 0.1),
    labels  = seq.int(from = 0, to = 1, by = 0.1) %>%
      sprintf("%.2f", .)
  )  +
  coord_flip() +
  ggtitle("Barley")


ggsave(
  Example1,
  device = "jpeg",
  filename = paste0(here(), "/Output/Figures/FigureZ_Barley_Plotting_Example1.jpeg"),
  width = 20,
  height = 15,
  units = "cm",
  dpi = 250
)


PlotData %>% 
  data.frame() %>% 
  fwrite(sep = ",",
         here("Output/Tables",
              "TableZ_Barley_PlotData.csv"))


## Modify upper bound to not exceed zero
PlotData$Ymax <- (PlotData$Means + PlotData$SD) %>% ifelse(. > 0, 0, .)
PlotData$Ymin <- (PlotData$Means - PlotData$SD) %>% ifelse(. > 0, 0, .)



Example2 <- PlotData %>% 
  ggplot(
    aes(
      x = NE %>% as.numeric(),
      group = Density %>% as.factor())) + 
  
  geom_line(aes(y = Means, color = Density %>% as.factor()), linewidth = 1) +
  
  geom_point(aes(y = Means, color = Density %>% as.factor())) + 
  
  geom_ribbon(aes(y = Means, 
                  ymin = Ymin, 
                  ymax = Ymax, 
                  fill = Density %>% as.factor()), 
              outline.type = "both", alpha= 0.2) +
  
  theme_bw() +
  
  geom_vline(xintercept = 0) +
  
  scale_color_manual(name = "Pest density", 
                     values = c("#C6DBEF", "#4292C6", "black")) +
  
  
  scale_fill_manual(name = "Pest density", 
                    values = c("#C6DBEF", "#4292C6", "black")) +
  
  scale_y_continuous(name = "Change in lost yield against 100% presence",
                     breaks = seq.int(from = -120, to = 0, by = 10)) +
  
  scale_x_continuous(
    name = "Natural enemy presence as percentage.",
    breaks = seq.int(from = 0, to = 1, by = 0.1),
    labels  = seq.int(from = 0, to = 1, by = 0.1) %>%
      sprintf("%.2f", .)
  )  +
  
  ggtitle("Barley: threshold at 5")


ggsave(
  Example2,
  device = "jpeg",
  filename = paste0(here(), "/Output/Figures/FigureZ_Barley_T5_Plotting_Example2.jpeg"),
  width = 20,
  height = 15,
  units = "cm",
  dpi = 250
)

# ******************************************************************************
#### Section 6B: Exporting the table ####
# ******************************************************************************



TableZ %>% data.frame() %>% fwrite(sep = "#",
                                   here("Output/Tables", 
                                        "TableZ_OutputSummaryForBarley.txt"))


