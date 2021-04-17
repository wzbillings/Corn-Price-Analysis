# Modeling USDA Corn Prices with Generalized Least Squares Regression
Zane Billings

## Intro
This began as a project for Statistical Methods II (MATH 375) at Western Carolina University in 2019. The project, with only a few minor fixes, is essentially left as-is in the Archive directory. Since then, I decided to take a few hours and work on this again, to see if I remembered what I learned, and also to re-do the project now that (in 2021) I think I'm better at coding and statstics.

## Modeling Strategy

## Description of Contents
- Data: this directory contains all of the data used in this project.
  - Raw: contains the raw excel file downloaded from the USDA website.
  - Processed: contains the Rds and CSV files produced by the data cleaning script.
- Code: this directory contains all .R code files used in data cleaning and analysis.
  - `Raw-Data-Downloading.R`: this script downloads the excel file of the raw data from the USDA website to the Data/Raw directory.
  - `Data-Cleaning.R`: this script imports the relevant sheets from the raw excel file, extracts and cleans the relevant data, joins together data from across streets, and saves the final cleaned data to CSV and Rds formats in the Data/Processed directory.
- Figures: this directory will contain all figures and visualizations from the analysis.
- Models: this directory will contain all models created from the analysis saved as .Rds files.
- Manuscript: a write-up of the final model results will be contained here.

