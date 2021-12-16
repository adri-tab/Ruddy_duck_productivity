
# Ruddy_duck_recruitment_rate

Objective : Recruitment rate estimation using count data

## Data source

Data are formatted in the Ruddy_duck_data repository : https://github.com/adri-tab/Ruddy_duck_data

## Repository organisation

Two main files in the Code folder : 

the R file 
  - it calls the data file from the Data folder
  - it is the model that infers from count data the estimations of various parameters
  - it produces the figures for the manuscript in the Output folder

the Rmarkdown file
  - it calls the figures from the Output folder
  - it generates the pdf file for the manuscript in the Code folder

## How to run code

  - Import locally the repository
  - Open Ruddy_duck_recruitment_rate.Rproj in Rstudio
  - Open then Model_recruitment_rate.R to run or modify the model
  - Open Draft_ruddy_duck_recruitment rate.Rmd to run or modify the draft