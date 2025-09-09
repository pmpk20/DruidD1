# DruidD1

#### Title:

*Economic valuation of pest regulation benefits provided by arthropods in the UK.*

#### Keywords:

Economic valuation, production functions, pest regulation, natural enemies, insects, insecticides.

#### Paper co-authors:

Peter King (Leeds), Theresa Robinson (JNCC), Charlotte Howard (Reading), Tom Breeze (Reading), Martin Dallimer (Imperial). Correspondence: p.king1\@leeds.ac.uk

#### **Funding:**

This work was supported by the Natural Environment Research Council Grant/Award Number: NE/V006916/1 This work is part of task D1 of work package D for the DRUID project: <https://environment.leeds.ac.uk/geography-research-river-basin-processes-management/dir-record/research-projects/1656/drivers-and-repercussions-of-uk-insect-declines-druid>

#### Last change: 08/09/2025

#### Status: Accepted for publication at ***Ecosystem Services***

#### Organisation

-   Use *00_AllScriptsHere.R* to sequentially estimate all data + figures in the manuscript.
-   This script is numbered in order of manusript appearance
-   The production function script themselves are pretty long and complex, outputting multiple tables/figures
-   In-text, we only use the figures for losses per hectare (LPH), change in LPH (CLPH), and yield losses (YL).
-   "\_InsecticdeSA" refers to the sensitivity analysis around insecticide costs
-   "\_ThresholdSA" refers to the sensitivity analysis around insecticide thresholds


````

## List files recursively with nice tree
# > fs::dir_tree(path = ".")
# .
# ├── 00_AllScriptsHere.R
# ├── DruidD1.Rproj
# ├── MarketData
# │   ├── AllCropData_V2.xlsx
# │   ├── DEFRAData_AUK-Chapter7-14jul22.ods
# │   ├── Nix Input Data.xlsx
# │   └── NIX sprays costs 2011 2021.xlsx
# ├── Output
# │   ├── Figures
# │   │   ├── Factsheet_Plot_V3.png
# │   │   ├── Figure2_CLPH_ThreeCrops.png
# │   │   ├── Figure3_LPH_ThreeCrops.png
# │   │   ├── Figure4_LPH_ThresholdSA_ThreeCrops.png
# │   │   ├── Figure5_LPH_InsecticideSA_ThreeCrops.png
# │   ├── LoopData
# │   │   ├── Barley_Defaults_V3.csv
# │   │   ├── Barley_InsecticideSA_V3_CLPH.csv
# │   │   ├── Barley_InsecticideSA_V3_LPH.csv
# │   │   ├── Barley_InsecticideSA_V3_YL.csv
# │   │   ├── Barley_ThresholdSA_V2_CLPH.csv
# │   │   ├── Barley_ThresholdSA_V2_LPH.csv
# │   │   ├── Barley_ThresholdSA_V2_YL.csv
# │   │   ├── OSR_Defaults_V3.csv
# │   │   ├── OSR_InsecticideSA_V3_CLPH.csv
# │   │   ├── OSR_InsecticideSA_V3_LPH.csv
# │   │   ├── OSR_InsecticideSA_V3_YL.csv
# │   │   ├── OSR_ThresholdSA_V2_CLPH.csv
# │   │   ├── OSR_ThresholdSA_V2_LPH.csv
# │   │   ├── OSR_ThresholdSA_V2_YL.csv
# │   │   ├── Wheat_Defaults_V3.csv
# │   │   ├── Wheat_InsecticideSA_V3_CLPH.csv
# │   │   ├── Wheat_InsecticideSA_V3_LPH.csv
# │   │   ├── Wheat_InsecticideSA_V3_YL.csv
# │   │   ├── Wheat_ThresholdSA_V2_CLPH.csv
# │   │   ├── Wheat_ThresholdSA_V2_LPH.csv
# │   │   └── Wheat_ThresholdSA_V2_YL.csv
# │   └── Tables
# │       ├── Table2_MarketDataSummary.txt
# ├── README.md
# ├── Scripts
# │   ├── Figures
# │   │   ├── 05_Figure2_ThreeCrops_CLPH.R
# │   │   ├── 06_Figure3_ThreeCrops_LPH.R
# │   │   ├── 13_Figure4_ThresholdSA_ThreeCrops_LPH.R
# │   │   ├── 18_Figure5_InsecticideSA_ThreeCrops_LPH.R
# │   │   ├── 19_FigureB1_ThreeCrops_YL.R
# │   │   └── XX_FactsheetPlot_V3.R
# │   ├── Setup
# │   │   ├── 02_Barley_DefaultsOnly.R
# │   │   ├── 03_OSR_DefaultsOnly.R
# │   │   ├── 04_Wheat_DefaultsOnly.R
# │   │   ├── 07_ThreeCrops_DefaultsOnly.R
# │   │   ├── 08_ThreeCrops_HeadlineChanges.R
# │   │   └── SensitivityAnalysis
# │   │       ├── InsecticideCosts
# │   │       │   ├── 14_Barley_InsecticideSA.R
# │   │       │   ├── 15_OSR_InsecticideSA.R
# │   │       │   ├── 16_Wheat_InsecticideSA.R
# │   │       │   └── 17_ThreeCrops_InsecticideSA.R
# │   │       └── Thresholds
# │   │           ├── 09_Barley_ThresholdSA.R
# │   │           ├── 10_OSR_ThresholdSA.R
# │   │           ├── 11_Wheat_ThresholdSA.R
# │   │           └── 12_ThreeCrops_ThresholdSA.R
# │   ├── Tables
# │   │   └── 01_Table2_OutputMarketData.R

````







```         
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
```
