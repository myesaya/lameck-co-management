# ============================================================================
# IMPORT SPSS DATA AND EXPORT TO EXCEL (name + label in two header rows)
# ============================================================================

library(tidyverse)
library(janitor)
library(haven)
library(openxlsx)
library(here)

# 1. Create output directories
if(!dir.exists(here::here("tables")))  dir.create(here::here("tables"),  recursive = TRUE)
if(!dir.exists(here::here("figures"))) dir.create(here::here("figures"), recursive = TRUE)

# 2. Import SPSS data, clean names, convert labelled -> factor
vwaza <- read_sav(
  here::here("data/lameck.sav")
) |>
  clean_names() |>
  mutate(across(where(is.labelled), as_factor)) |>
  mutate(id = row_number())

# 3. Build the two header rows
var_names  <- names(vwaza)
var_labels <- map_chr(vwaza, ~ {
  lab <- attr(.x, "label")
  if (is.null(lab) || length(lab) == 0 || is.na(lab) || lab == "") "" else lab
})

# 4. Write Excel: row 1 = names, row 2 = labels, row 3+ = data
wb <- createWorkbook()
addWorksheet(wb, "data")

writeData(wb, "data", t(var_names),  startRow = 1, colNames = FALSE)
writeData(wb, "data", t(var_labels), startRow = 2, colNames = FALSE)
writeData(wb, "data", vwaza,        startRow = 3, colNames = FALSE)

# Optional styling so the two header rows stand out
header_style <- createStyle(textDecoration = "bold", fgFill = "#D9E1F2", border = "bottom")
addStyle(wb, "data", header_style, rows = 1:2,
         cols = 1:ncol(vwaza), gridExpand = TRUE, stack = TRUE)

saveWorkbook(wb, here::here("data/lameck.xlsx"), overwrite = TRUE)