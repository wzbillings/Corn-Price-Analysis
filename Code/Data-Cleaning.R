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
pth <- here::here("Data", "Raw", "FeedGrainsAllYears.xls")

################################################################################
# Helper functions

years_to_numeric <- function(.data) {
  .data %>%
    dplyr::mutate(year = as.integer(stringr::str_sub(year, start = 1, end = 4)))
}

################################################################################
# cleaning sheet FGYearbookTable01-Full all cols

dat01 <- 
  readxl::read_excel(
    path = pth,
    sheet = "FGYearbookTable01-Full",
    skip = 2,
    col_names = c("commodity", "year", "acreage", "harvest", "production", 
                  "yield", "weighted_avg_farm_price", "loan_rate")
  ) %>%
  dplyr::mutate(commodity = zoo::na.locf(commodity)) %>%
  # If any element in non-commodity columns is not NA, keep that row.
  dplyr::filter(if_any(!commodity, ~ !is.na(.x))) %>%
  years_to_numeric() %>%
  # Not all data is in terms of commodity, so make these all separate variables.
  tidyr::pivot_wider(
    names_from = commodity,
    values_from = !c(commodity, year)
  )

################################################################################
## cleaning sheet FGYearbookTable02-Full

dat02 <- 
  readxl::read_excel(
    path = pth,
    sheet = "FGYearbookTable02-Full",
    skip = 3,
    col_names = c("commodity", "year", "beginning_stocks", 
                  "supply_production", "supply_imports", "supply_total",
                  "dis_food_alc_ind", "dis_feed_use", "dis_domestic",
                  "dis_exports", "dis_total", "ending_stocks")
  ) %>%
  dplyr::mutate(commodity = zoo::na.locf(commodity)) %>%
  dplyr::filter(if_any(!commodity, ~ !is.na(.x))) %>%
  years_to_numeric() %>%
  dplyr::mutate(commodity = ifelse(commodity == "Coarse grains 5/", "coarse_grains", commodity)) %>%
  # Not all data is in terms of commodity, so make these all separate variables.
  tidyr::pivot_wider(
    names_from = commodity,
    values_from = !c(commodity, year)
  )

################################################################################
# Sheets 03--08 are all quarterly so will not be cleaned unless necessary.

################################################################################
## cleaning sheet FGYearbooKTable09, annual column only.

dat09 <- 
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
  dplyr::mutate(commodity = zoo::na.locf(commodity)) %>%
  dplyr::filter(if_any(!commodity, ~ !is.na(.x))) %>%
  years_to_numeric() %>%
  tidyr::pivot_wider(
    names_from = commodity,
    values_from = wt_avg_farmer_price,
    id_cols = year
  ) %>%
  dplyr::rename(
    wt_avg_farmer_price_per_bushel_corn = `Corn\n(dollars per bushel)`,
    wt_avg_farmer_price_per_bushel_sorghum = `Sorghum\n(dollars per bushel)`,
    wt_avg_farmer_price_per_hundredweight_sorghum = `Sorghum\n(dollars per hundredweight)`
  )

################################################################################
## sheet 10 TO-DO

################################################################################
## sheet 11 TO-DO

################################################################################
## sheets 12, 13, 14: maybe skip for now, unsure.

################################################################################
## cleaning sheet FGYearbookTable15-Full, annual column only

dat15 <-
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
  ) %>%
  years_to_numeric()

################################################################################
## cleaning sheet FGYearbookTable18-Full col O

dat18 <-
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
  ) %>%
  years_to_numeric()

################################################################################
## sheet 16-19 TO-DO

################################################################################
## cleaning sheet FGYearbookTable20-Full

dat20 <-
  readxl::read_excel(
    path = pth,
    sheet = "FGYearbookTable20-Full",
    skip = 1
  ) %>%
  dplyr::select(
    import_type = `Import and mkt yr 1/`,
    year = `...2`,
    annual = `Annual`
  ) %>%
  dplyr::mutate(import_type = zoo::na.locf(import_type)) %>%
  dplyr::filter(if_any(!import_type, ~ !is.na(.x))) %>%
  tidyr::pivot_wider(
    names_from = import_type,
    values_from = annual
  ) %>%
  dplyr::rename(
    annual_imported_corn_grain = "Corn grain",
    annual_imported_corn_total = "Corn total 2/",
    annual_imported_sorghum_total = "Sorghum total 3/"
  ) %>%
  years_to_numeric()

################################################################################
## sheets 21-27 TO-DO

################################################################################
## cleaning sheet FGYearbookTable28-Full

dat28 <-
  readxl::read_excel(
    path = pth,
    sheet = "FGYearbookTable28-Full",
    skip = 1
  ) %>%
  dplyr::select(
    index_type = `Year`, # Column names got imported wrong here -_-
    year = `...2`,
    annual = `CY Jan-Dec`
  ) %>%
  dplyr::mutate(index_type = zoo::na.locf(index_type)) %>%
  filter(!is.na(year) & !is.na(annual)) %>%
  tidyr::pivot_wider(
    names_from = index_type,
    values_from = annual
  ) %>%
  dplyr::rename(
    annual_producer_price_index = "Producer price index, line-haul railroads, all products (1984=100)",
    annual_grain_rail_car_loadings = "Rail car loadings, grain (1,000 rail cars) /1"
  )
  
################################################################################
# Join data sets together

## Intialize dataframe with dat01 for joining in loop.
fg_dat <- dat01

## Make list of existing dataset names. This allows more sheets to be cleaned
##  later if necessary without changing this code.
sheets <- ls()[startsWith(ls(), "dat")]

## Join all of these datasets together by year.
## Index starts at 2 to ignore dat01.
for (i in 2:length(sheets)) {
  fg_dat <- dplyr::left_join(
    x = fg_dat,
    y = get(sheets[[i]]), 
    by = "year"
  )
}

## Clean names before exporting dataset
fg_dat <- janitor::clean_names(fg_dat)

saveRDS(
  object = fg_dat,
  file = here::here("Data", "Processed", "Clean-Data.Rds")
)

write.csv(
  x = fg_dat,
  file = here::here("Data", "Processed", "Clean-Data.csv")
)

################################################################################
# End of script
