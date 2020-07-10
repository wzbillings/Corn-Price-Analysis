# Regression Model of USDA Corn prices
I did a multiple linear regression analysis on some data from the USDA Feed Grains Database for MATH 375: Statistical Methods II at Western Carolina University. This repo contains all of the files, including data, CLEAN data that I wanted to use, R script for analysis, and my final presentation.

## Description of contents.

### data
* clean_data: contains the rectangular .csv file with all of the data I wanted to use. All NA values are values that were blank or were not recorded in the original data. All data were transposed into columns starting at the appropriate year and cleaned as necessary.
* raw_data: contains the .xls spreadsheet for the Feed Grains Database which I originally downloaded from data.gov. This data is VERY messy and was not close to being in a tidy state ready for analysis. Also contains the .xls spreadsheet split up into component .csv files, but these look really bad. Finally, contains the NASA temperature anomaly data.

### analysis
* DataDocumentation.txt: a .txt file containing a rough codebook for the data; basically descriptions of all the variables was written down here. I wasn't involved in data collection and didn't speak to the USDA so this is as close as I have to a codebook for the excel sheet.
* Project3Script.R: this script does all of the data analysis. Not really written reproducibly but this was done before I made a conscious effort to have a reproducible workflow.

### presentation
* images: contains all images used in the slides.
* keynote presentation: the original presentation as created in the MacOS keynote software.
* pdf presentation: the actual .pdf file I presented for my project.

### coming soon
* reorganization
* analysis writeup
* weighted least squares? :O
