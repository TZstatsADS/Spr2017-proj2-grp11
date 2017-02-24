# Project: NYC Open Data
### Data folder

The data directory contains data used in the analysis. This is treated as read only; 

sitc3digit-year.csv : data from [data.gov](http://www.census.gov/foreign-trade/statistics/country/sitc/index.html) and we kept only the yearly import and export data of coffee, tea, cocoa, chocolate and spice(ID from 71 to 75).  

mydata.csv : can be generated using data_cleaning.R file in lib folder 

mydata_wRegions.csv : is mydata.csv with 2 added columns, storing data of region types. Can be generated using data.clean.BW.Rmd file in the lib folder  

country_cleaned.csv : all the country and their export value to US in terms of the 5 commodities

exchange_rate.csv : exchange rate of every country in the previous dataset  

CPI.csv : consumer price index of each country




