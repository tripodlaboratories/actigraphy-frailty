# actigraphy-frailty
repository to reproduce results for frailty prediction from actigraphy data

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
├── figures       <- Main figures included in the publication reproduced using included scripts
├── results       <- Results files for each predictive task (not included in repo to save space)
└── scripts       <- Scripts used to process data, create analysis results, and produce figures
    ├── data_processing.R        <- from raw data creates organized_data.rda for easier analysis
    ├── analysis.R.              <- generates xgboost predictive models and some univariate analysis (results saved in results folder)
    └── figures.R.               <- produces main figures from the previously generated results files
```

