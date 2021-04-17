###
# Excel data cleaning script
# Zane Billings
# The USDA feed grains data needs a lot of cleaning work.
###

library(here)
library(readxl)
library(stringr)
library(zoo)
library(dplyr)
library(janitor)

# Path to data file
pth <- here::here("Data", "Raw-Data", "FeedGrainsAllYears.xls")

# List of data to grab by sheet

################################################################################
# Clean sheet 01

sheet1 <- 
  readxl::read_excel(
    path = pth,
    sheet = "FGYearbookTable01-Full",
    skip = 2,
    col_names = c("commodity", "year", "acreage", "harvest", "production", 
                  "yield", "weighted_avg_farm_price", "loan_rate")
  ) %>% 
  dplyr::mutate(commodity = zoo::na.locf(commodity)) %>%
  dplyr::filter(commodity == "Corn") %>%
  dplyr::select(-commodity) %>%
  janitor::remove_empty(which = "rows")

################################################################################
## FGYearbookTable02-Full cols C - N

################################################################################
## FGYearbooKTable09-Full col O

################################################################################
## FGYearbookTable10-Full col O (rows are in groups)
### broiler-feed ratio
### market egg-feed ratio
### hog-corn ratio
### milk-feed ratio
### steer and heifer-corn ratio
### turkey-feed ratio
################################################################################
## FGYearbookTable18-Full col O (grouped)
### corn grain export
### corn total export

################################################################################
## FGYearbookTable20-Full col O (grouped)
### corn grain import
### corn total import

################################################################################
## FGYearbookTable28-Full col O
### produce price index
### rail car loadings

################################################################################
# End of script
