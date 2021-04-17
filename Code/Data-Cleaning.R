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
library(tidyr)

# Path to data file
pth <- here::here("Data", "Raw-Data", "FeedGrainsAllYears.xls")

################################################################################
# Helper functions
get_corn_rows_only <- function(.data, filter_com = "Corn") {
  .data %>%
    dplyr::mutate(commodity = zoo::na.locf(commodity)) %>%
    dplyr::filter(commodity == filter_com) %>%
    dplyr::select(-commodity) %>%
    janitor::remove_empty(which = "rows")
}

################################################################################
# cleaning sheet FGYearbookTable01-Full all cols

dat1 <- 
  readxl::read_excel(
    path = pth,
    sheet = "FGYearbookTable01-Full",
    skip = 2,
    col_names = c("commodity", "year", "acreage", "harvest", "production", 
                  "yield", "weighted_avg_farm_price", "loan_rate")
  ) %>% 
  get_corn_rows_only()

################################################################################
## cleaning sheet FGYearbookTable02-Full cols C - N

dat2 <- 
  readxl::read_excel(
    path = pth,
    sheet = "FGYearbookTable02-Full",
    skip = 3,
    col_names = c("commodity", "year", "beginning_stocks", 
                  "supply_production", "supply_imports", "supply_total",
                  "dis_food_alc_ind", "dis_feed_use", "dis_domestic",
                  "dis_exports", "dis_total", "ending_stocks")
  ) %>% 
  get_corn_rows_only()

################################################################################
## cleaning sheet FGYearbooKTable09-Full col O

dat3 <- 
  readxl::read_excel(
    path = pth,
    sheet = "FGYearbookTable09-Full",
    skip = 1
  ) %>%
  dplyr::select(
    commodity = `Commodity and\nmkt year 1/`,
    year = `...2`,
    wt_avg_farmer_price = `Wt avg 2/`
  ) %>%
  get_corn_rows_only(filter_com = "Corn\n(dollars per bushel)")

################################################################################
## cleaning sheet FGYearbookTable15-Full col O

dat4 <-
  readxl::read_excel(
    path = pth,
    sheet = "FGYearbookTable15-Full",
    skip = 1
  ) %>%
  dplyr::select(
    ratio_type = `Ratio and\nmkt yr 1/`,
    year = `...2`,
    avg_ratio = `Avg 2/`
  ) %>%
  dplyr::mutate(ratio_type = zoo::na.locf(ratio_type)) %>%
  filter(!is.na(year) & !is.na(avg_ratio)) %>%
  tidyr::pivot_wider(
    names_from = ratio_type,
    values_from = avg_ratio
  ) %>%
  dplyr::rename(
    broiler_feed = `Broiler-feed\n3/ 4/`,
    market_egg_feed = `Market egg-feed\n3/ 5/`,
    hog_corn = `Hog-corn\n3/ 6/`,
    milk_feed = `Milk-feed\n3/ 7/`,
    steer_heifer_corn = `Steer and heifer-corn\n3/ 8/`,
    turkey_feed = `Turkey-feed\n3/ 9/`
  )

################################################################################
## cleaning sheet FGYearbookTable18-Full col O

dat5 <-
  readxl::read_excel(
    path = pth,
    sheet = "FGYearbookTable18-Full",
    skip = 1
  ) %>%
  dplyr::select(
    export_type = `Export and mkt yr 1/`,
    year = `...2`,
    annual = `Annual`
  ) %>%
  dplyr::mutate(export_type = zoo::na.locf(export_type)) %>%
  dplyr::filter(export_type %in% c("Corn grain", "Corn total 2/")) %>% 
  filter(!is.na(year) & !is.na(annual)) %>%
  tidyr::pivot_wider(
    names_from = export_type,
    values_from = annual
  ) %>%
  dplyr::rename(
    annual_exported_corn_grain = "Corn grain",
    annual_exported_corn_total = "Corn total 2/"
  )

################################################################################
## cleaning sheet FGYearbookTable20-Full
### corn grain import
### corn total import

################################################################################
## cleaning sheet FGYearbookTable28-Full
### produce price index
### rail car loadings

################################################################################
# End of script
