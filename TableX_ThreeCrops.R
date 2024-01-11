
# ******************************************************************************
#### Section 1: Estimating crop specific tables ####
# ******************************************************************************

## Wheat
here("Scripts", "TableX_Wheat_NewLPHCalcs.R") %>% source()

## Barley
here("Scripts", "TableX_Barley_NewLPHCalcs.R") %>% source()

## OSR
here("Scripts", "TableX_OSR_NewLPHCalcs.R") %>% source()



# ******************************************************************************
#### Section 2: Importing crop specific data ####
# ******************************************************************************

## Import Wheat
Wheat <- here("Output/Tables",
              "TableX_OutputSummaryForWheat.txt") %>% 
  fread(sep = "#") %>% 
  data.frame()

## Import Barley
Barley <- here("Output/Tables",
               "TableX_OutputSummaryForBarley.txt") %>% 
  fread(sep = "#") %>% 
  data.frame()


## Import OSR
OSR <- here("Output/Tables",
                  "TableX_OutputSummaryForOSR.txt") %>% 
  fread(sep = "#") %>% 
  data.frame()



# ******************************************************************************
#### Section 3: Creating new table ####
# ******************************************************************************


BreakTable_1 <- cbind(
  Wheat[, 1:3],
  Wheat$YL..Weighted.,
  Barley$YL..Weighted.,
  OSR$YL..Weighted.,
  
  Wheat$LPH..Weighted.,
  Barley$LPH..Weighted.,
  OSR$LPH..Weighted.,
  
  Wheat$PCT.Change.in.LPH.vs.1,
  Barley$PCT.Change.in.LPH.vs.1,
  OSR$PCT.Change.in.LPH.vs.1,
  
  Wheat$LT..Weighted.,
  Barley$LT..Weighted.,
  OSR$LT..Weighted.
)

BreakTable_1 %>% View()

## Output all data by AS/NS/ET
BreakTable_1 %>% data.frame() %>% fwrite(sep = "#",
                                   here("Output/Tables", 
                                        "TableX_OutputSummaryForThreeCrops.txt"))


