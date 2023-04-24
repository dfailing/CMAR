# Load required packages
library(here)
library(tidyverse)


# Create subdirectories in case they're missing
if (!file.exists(here("data-raw"))) dir.create(here("data-raw"))
if (!file.exists(here("data"))) dir.create(here("data"))
if (!file.exists(here("data-export"))) dir.create(here("data-export"))


# Generate a list of filenames
dir_raw <- here("data-raw")
dir_data <- here("data")
dir_export <- here("data-export")

csv_filenames <- list.files(dir_raw, pattern = ".csv$")
variable_names <- c()

for (filename in csv_filenames) {
  variable_name <- filename %>% 
    str_replace(".csv", "") %>% 
    str_replace_all("-", "_") %>%
    paste0("results_", .)
  
  variable_names <- c(variable_names, variable_name)
  
  data <- read_csv(here(dir_raw, filename))
  
  data %>%
    select(-any_of(c("Note", "Bib", "UltraSignup"))) %>%
    arrange(`Overall Place`) %>%
    group_by(Gender) %>%
    mutate(`Gender Place` = row_number(), .after = `Overall Place`) %>%
    ungroup() %>%
    assign(variable_name, ., envir = parent.frame())
}

merged_data <- lapply(variable_names, get) %>%
  bind_rows()

# Course Records
# TODO: Finish course records
course_records <- merged_data %>%
  filter(!Distance %in% c(5, 6, 50)) %>% 
  mutate(Gender = case_when(Gender == "M" ~ "Male", Gender == "F" ~ "Female", TRUE ~ "")) %>%
  group_by(Event, Gender) %>%
  arrange(`Finish Time`) %>%
  slice_min(`Finish Time`, n = 3) %>%
  mutate(Rank = dense_rank(`Finish Time`)) %>%
  ungroup() %>%
  arrange(-Distance, desc(Gender), `Finish Time`) %>%
  select(Event, Gender, Rank, First, Last, Age, Time = `Finish Time`, Year)

# Mileage Totals
mileage_totals <- merged_data %>%
  # TODO: Select the UUID column too for grouping
  select(First, Last, Gender, City, State, Year, Distance) %>% 
  group_by(Last, First, Gender) %>% 
  arrange(-Year) %>%
  # Overwrite City, State with values from most recent race result
  mutate(City = City[which.max(Year)],
         State = State[which.max(Year)]) %>%
  mutate(`Finishes` = n_distinct(Year),
         `Total Miles` = sum(Distance), .after = State) %>%
  pivot_wider(names_from = Year, values_from = Distance) %>%
  ungroup() %>%
  arrange(-`Total Miles`, Last, First)

# All Results
all_results <- merged_data %>%
  select(Year, Event, Place = `Overall Place`, First, Last, City, State, Age, Gender, GP = `Gender Place`, Time = `Finish Time`) %>% 
  arrange(-Year, Event, Place)

# Export Data
write_csv(course_records, here("data", "course_records.csv"))
write_csv(mileage_totals, here("data", "mileage_totals.csv"))
write_csv(all_results, here("data", "all_results.csv"))

