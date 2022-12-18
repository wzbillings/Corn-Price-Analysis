###
# Excel data cleaning script
# Zane Billings
# The USDA feed grains data needs a lot of cleaning work.
###

box::use(
  here,
  readxl,
  stringr,
  dplyr,
  janitor,
  tidyr
)

# Path to data file
pth <- here::here("Data", "Raw", "FeedGrainsAllYears.xls")

################################################################################
# Helper functions ####

years_to_numeric <- function(.col) {
  # Convert a variable in the form character "2016/17" -> integer "2016".
  out <-
    .col |>
    stringr::str_sub(start = 1L, end = 4L) |>
    as.integer()

  return(out)
}

################################################################################
# cleaning sheet FGYearbookTable01-Full all cols ####

dat01 <-
  readxl::read_excel(
    path = pth,
    sheet = "FGYearbookTable01-Full",
    skip = 2,
    col_names = c("commodity", "year", "acreage", "harvest", "production",
                  "yield", "weighted_avg_farm_price", "loan_rate")
  ) |>
  tidyr::fill(commodity, .direction = "down") |>
  # If any element in non-commodity columns is not NA, keep that row.
  # I.e. drop rows that are all NA, except for commodity which
  # might have message rows in it which no data that need to be dropped.
  dplyr::filter(if_any(!commodity, ~ !is.na(.x))) |>
  # Convert years to a numeric format
  dplyr::mutate(year = years_to_numeric(year))

################################################################################
# cleaning sheet FGYearbookTable02-Full ####

dat02 <-
  readxl::read_excel(
    path = pth,
    sheet = "FGYearbookTable02-Full",
    skip = 3,
    col_names = c("commodity", "year", "beginning_stocks",
                  "supply_production", "supply_imports", "supply_total",
                  "dis_food_alc_ind", "dis_feed_use", "dis_domestic",
                  "dis_exports", "dis_total", "ending_stocks")
  ) |>
  tidyr::fill(commodity, .direction = "down") |>
  dplyr::filter(if_any(!commodity, ~ !is.na(.x))) |>
  dplyr::mutate(year = years_to_numeric(year)) |>
  dplyr::mutate(
    commodity = ifelse(
      commodity == "Coarse grains 5/",
      "coarse_grains",
      commodity)
  )

################################################################################
# Sheets 03--08 are all quarterly so will not be cleaned unless necessary.

################################################################################
# cleaning sheet FGYearbooKTable09, annual column only. ####

dat09 <-
  readxl::read_excel(
    path = pth,
    sheet = "FGYearbookTable09-Full",
    skip = 1
  ) |>
  dplyr::select(
    commodity = `Commodity and\nmkt year 1/`,
    year = `...2`,
    wt_avg_farmer_price = `Wt avg 2/`
  ) |>
  tidyr::fill(commodity, .direction = "down") |>
  dplyr::filter(if_any(!commodity, ~ !is.na(.x))) |>
  tidyr::separate(
    commodity,
    into = c("commodity", "price_units"),
    sep = "\n"
  ) |>
  dplyr::mutate(
    year = years_to_numeric(year),
    # Remove parentheses from units
    units = stringr::str_remove_all(price_units, pattern = "[\\(\\)]")
  )

################################################################################
## sheet 10 TO-DO

################################################################################
## sheet 11 TO-DO

################################################################################
## sheets 12, 13, 14: maybe skip for now, unsure.

################################################################################
# cleaning sheet FGYearbookTable15-Full, annual column only ####

dat15 <-
  readxl::read_excel(
    path = pth,
    sheet = "FGYearbookTable15-Full",
    skip = 1
  ) |>
  dplyr::select(
    ratio_type = `Ratio and\nmkt yr 1/`,
    year = `...2`,
    avg_ratio = `Avg 2/`
  ) |>
  tidyr::fill(ratio_type, .direction = "down") |>
  dplyr::filter(!is.na(year) & !is.na(avg_ratio)) |>
  tidyr::separate(
    ratio_type,
    into = c("ratio_type", NA),
    sep = "\n"
  ) |>
  dplyr::mutate(
    year = years_to_numeric(year),
    ratio_type = stringr::str_replace(ratio_type, "-", " to "),
    ratio_type = paste(ratio_type, "ratio")
  ) |>
  tidyr::pivot_wider(
    names_from = ratio_type,
    values_from = avg_ratio
  ) |>
  janitor::clean_names()

################################################################################
# cleaning sheet FGYearbookTable18-Full ####

dat18 <-
  readxl::read_excel(
    path = pth,
    sheet = "FGYearbookTable18-Full",
    skip = 1
  ) |>
  dplyr::select(
    export_type = `Export and mkt yr 1/`,
    year = `...2`,
    annual = `Annual`
  ) |>
  tidyr::fill(export_type, .direction = "down") |>
  dplyr::filter(export_type %in% c("Corn grain", "Corn total 2/")) |>
  dplyr::filter(!is.na(year) & !is.na(annual)) |>
  tidyr::pivot_wider(
    names_from = export_type,
    values_from = annual
  ) |>
  dplyr::rename(
    annual_exported_corn_grain = "Corn grain",
    annual_exported_corn_total = "Corn total 2/"
  ) |>
  dplyr::mutate(year = years_to_numeric(year))

################################################################################
## sheet 16-19 TO-DO

################################################################################
# cleaning sheet FGYearbookTable20-Full ####

dat20 <-
  readxl::read_excel(
    path = pth,
    sheet = "FGYearbookTable20-Full",
    skip = 1
  ) |>
  dplyr::select(
    import_type = `Import and mkt yr 1/`,
    year = `...2`,
    annual = `Annual`
  ) |>
  tidyr::fill(import_type, .direction = "down") |>
  dplyr::filter(dplyr::if_any(!import_type, ~ !is.na(.x))) |>
  tidyr::pivot_wider(
    names_from = import_type,
    values_from = annual
  ) |>
  dplyr::rename(
    annual_imported_corn_grain = "Corn grain",
    annual_imported_corn_total = "Corn total 2/",
    annual_imported_sorghum_total = "Sorghum total 3/"
  ) |>
  dplyr::mutate(year = years_to_numeric(year))

################################################################################
## sheets 21-27 TO-DO

################################################################################
# cleaning sheet FGYearbookTable28-Full ####

dat28 <-
  readxl::read_excel(
    path = pth,
    sheet = "FGYearbookTable28-Full",
    skip = 1
  ) |>
  dplyr::select(
    index_type = `Year`, # Column names got imported wrong here -_-
    year = `...2`,
    annual = `CY Jan-Dec`
  ) |>
  tidyr::fill(index_type, .direction = "down") |>
  dplyr::filter(!is.na(year) & !is.na(annual)) |>
  tidyr::pivot_wider(
    names_from = index_type,
    values_from = annual
  ) |>
  dplyr::rename(
    annual_producer_price_index =
      "Producer price index, line-haul railroads, all products (1984=100)",
    annual_grain_rail_car_loadings =
      "Rail car loadings, grain (1,000 rail cars) /1"
  )

################################################################################
# Join data sets together ####

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
