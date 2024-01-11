
# ******************************************************************************
#### Section 1: Estimating crop specific tables ####
# ******************************************************************************

## Wheat
here("Scripts", "TableZ_Wheat_Corrected.R") %>% source()

## Barley
here("Scripts", "TableZ_Barley_Corrected.R") %>% source()

## OSR
here("Scripts", "TableZ_OSR_Corrected.R") %>% source()



# ******************************************************************************
#### Section 2: Importing crop specific data ####
# ******************************************************************************

## Import Wheat
Wheat <- here("Output/Tables",
              "TableZ_OutputSummaryForWheat.txt") %>% 
  fread(sep = "#") %>% 
  data.frame()


## Import Barley
Barley <- here("Output/Tables",
               "TableZ_OutputSummaryForBarley.txt") %>% 
  fread(sep = "#") %>% 
  data.frame()


## Import OSR
OSR <- here("Output/Tables",
            "TableZ_OutputSummaryForOSR.txt") %>% 
  fread(sep = "#") %>% 
  data.frame()



# ******************************************************************************
#### Section 3: Creating new table ####
# ******************************************************************************


BreakTable_2 <- rbind(
  "Wheat", 
  Wheat, 
  "Barley",
  Barley, 
  "OSR",
  OSR
) 


## Output all data by AS/NS/ET
BreakTable_2 %>% data.frame() %>% fwrite(sep = "#",
                                         here("Output/Tables", 
                                              "TableZ_OutputSummaryForThreeCrops.txt"))


