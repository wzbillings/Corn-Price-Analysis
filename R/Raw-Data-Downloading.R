###
# Raw data downloading script
# Zane Billings
# This script downloads the current version of the USDA feed grains database.
#  Unfortunately the USDA changes this link for every version of the database
#  and it is AFAIK impossible to automate without scraping the webpage which
#  seems like an awful lot of trouble.
# So if you want to update the data you need to check
#  https://www.ers.usda.gov/data-products/feed-grains-database/feed-grains-yearbook-tables.aspx
#  to get the updated link.
###

box::use(here)

# Current path as of 2021-04-16.
pth <- paste0("https://www.ers.usda.gov/webdocs/DataFiles/50048/",
              "Feed%20Grains%20Yearbook%20Tables-All%20Years.xls?v=4138.8")

# Output file name
out <- here::here("Data", "Raw-Data", "FeedGrainsAllYears.xls")

# Download excel file to raw data repo.
download.file(
  url = pth,
  destfile = out
)
