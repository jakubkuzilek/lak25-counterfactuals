# LAK25: Personalised Recommendations for At-Risk Students: A Counterfactual Explanations Approach

This repository contains the code and data for the paper "Personalised Recommendations for At-Risk Students: A Counterfactual Explanations Approach" submitted to LAK25 conference.

# Project structure

- `data/`: contains the data used in the experiments.
- `src/`: contains the codes for creating the features and performing the clustering.
- `viewer`: contains the code for the web shiny-based dashboard enabling exploration of the counterfactual explanations results.

# How to run the code in src/

1. Clone the repository
2. Install the required packages by running the following command:
```r
install.packages(c("tidyverse", "factoextra", "magrittr", "readr", "tibble"))
```
3. Open the file 00_clustering.Rmd using RStudio and run the code.

# How to run the code in viewer/

1. Clone the repository
2. Install the required packages by running the following command:
```r
install.packages(c("shiny", "shinydashboard", "tidyverse"))
```
3. Open the file app.R using RStudio and run the app.