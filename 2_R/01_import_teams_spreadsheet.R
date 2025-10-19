
## Type   :  PhD Project
## Auteur :  Maxime Marini
## Topic  :  STABLE ISOTOPES CLARIFY HOW FORAGING NICHES IS PARTITIONNED WITHIN MIGRATORY SHOREBIRDS IN THE HUNTER ESTUARY
## Main   :  Import and filter blood samples info
## Created:  2025 October 


# 1 - Packages ----

library(dplyr)
library(here)
library(gt)

# 2 - Settings ----

# Set WD
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 

# 3 - Import & Load data ----
tryCatch({
  write.csv(
    readxl::read_excel("C:/Users/c3541851/The University of Newcastle/StudentGroupPhD - Louise Williams and Mattea Taylor - General/Bloods and feathers records/List of bird blood samples_last update_Aug25.xlsx",
                       sheet = "Data sheet"),
    file.path(here::here("1_data", "spreadsheets"), paste0(Sys.Date(), "-blood-teams.sheet", ".csv")),
    row.names = FALSE
  )
}, error = function(e) {
  write.csv(
    readxl::read_excel("C:/Users/marin/The University of Newcastle/StudentGroupPhD - Louise Williams and Mattea Taylor - General/Bloods and feathers records/List of bird blood samples_last update_Aug25.xlsx",
                       sheet = "Data sheet"),
    file.path(here::here("1_data", "spreadsheets"), paste0(Sys.Date(), "-blood-teams.sheet", ".csv")),
    row.names = FALSE
  )
})

spreadsheets_blood <- read.csv(here::here("1_data", "spreadsheets", paste0(Sys.Date(), "-blood-teams.sheet.csv")))

# 4 - Filter & Sort data ----

spreadsheets_blood <- spreadsheets_blood %>%
  filter(Blood. == "Y")  %>%
  rename(under_50ul = "X.50.uL",
        ul_50 = "X.50.uL.1",
        ul_75 = "X.75.uL",
        ul_100 = "X.100.uL",
        above_50ul = "X.100uL") %>%
  mutate(under_50ul = ifelse(is.na(under_50ul), FALSE, TRUE),
         ul_50 = ifelse(is.na(ul_50), FALSE, TRUE),
         ul_75 = ifelse(is.na(ul_75), FALSE, TRUE),
         ul_100 = ifelse(is.na(ul_100), FALSE, TRUE),
         above_50ul = ifelse(is.na(above_50ul), FALSE, TRUE))

blood_stock <- spreadsheets_blood %>%
  group_by(Species) %>%
  summarise(
    under_50ul = sum(under_50ul, na.rm = TRUE),
    ul_50      = sum(ul_50, na.rm = TRUE),
    ul_75      = sum(ul_75, na.rm = TRUE),
    ul_100     = sum(ul_100, na.rm = TRUE),
    above_50ul = sum(above_50ul, na.rm = TRUE),
    total      = n()  )

# 5 - Samples in stock table ----

# Publication format
stock_samples <- blood_stock %>%
  gt() %>%
  tab_header(title = paste0("Blood samples in stock - ", Sys.Date())) %>%
  opt_table_font(font = "Times New Roman") %>% 
  # Font
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels()) %>%
  tab_options(table.font.size = pct(90),
              heading.title.font.size = px(16))

# Save
gtsave( data = stock_samples,
        filename = here::here("docs", "figures", "blood_stock.jpg"))

