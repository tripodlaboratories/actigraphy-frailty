# actigraphy-frailty
Repository to reproduce results for frailty prediction from actigraphy data. This project aims to use machine learning techniques to build predictive models of frailty (frailty trait scale, fried frailty score, and rockwood frailty index) from activity data derived from wearable devices. 



results included in repo have been generated using R 4.1.2 and xgboost 1.5.2.1 on a macOS 12.3.1 system.

# Repo Structure

```bash
├── LICENSE
├── README.md
├── data
│   ├── actigraph_export       <- Data exports from actigraphy wearables
│   │   ├── BatchSleepExportDetails(2020-04-24_02-04-48).csv
│   │   ├── hi_DailyDetailed.csv
│   │   ├── hi_HourlyDetailed.csv
│   │   └── hi_Variables.csv
│   ├── BD STANFORD 20_06_01.xlsx   <- frailty and related risk data
│   ├── DataDescription.xlsx        <- descriptions of "BD STANFORD 20_06_01.xlsx" fields
│   └── organized_data.rda          <- post-processed data
├── figures       <- Main figures included with the publication reproduced using included scripts
├── results       <- Results files on each predictive task (not included to save space)
└── scripts       <- Scripts used to process data, create analysis results, and produce figures
    ├── data_processing.R        <- from raw data creates organized_data.rda
    ├── analysis.R.              <- generates xgboost predictive models and some univariate analysis (results saved to results folder)
    └── figures.R.               <- produces main figures from the previously generated results files
```

# Reproducing Results

To reproduce results one simply needs to update the ```datapath``` object in the three scripts and run them in the following order:

1. data_preprocessing.R
2. analysis.R
3. figures.R

