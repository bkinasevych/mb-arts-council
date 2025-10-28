library(sf)
library(ggrepel)
library(janitor)
library(tidyverse)
library(rnaturalearth)

# get manitoba boundary shape
mb_boundary_map <- ne_states(country = "canada",
                             returnclass = "sf") |> 
  filter(name == "Manitoba") |> 
  st_transform(crs = 4326) |> 
  st_make_valid() |> 
  st_union()

# get mb economic regions shapefiles
mb_econ_regions_sf <- read_sf("mb_economic-regions-shape-files") |> 
  clean_names() |> 
  st_transform(crs = 4326)

mb_econ_regions_sf <- mb_econ_regions_sf |> 
  mutate(economic_r = case_when(
    economic_r == "Winnipeg" ~ "Capital Region",
    economic_r == "Southwest" ~ "Westman",
    economic_r == "Southeast" ~ "Eastman",
    economic_r == "South Central" ~ "Pembina Valley",
    economic_r == "Parkland" ~ "Parklands",
    economic_r == "North Central" ~ "Central Plains",
    economic_r == "North" ~ "Norman",
    economic_r == "Interlake" ~ "Interlake"
  ))

# import regions from survey

data <- read_rds("data/survey_data_clean.rds")
regions <- read_csv("data/mb-region-fsa-table.csv") |> 
  select(-dup)



mac_fsa_data <- data |>
  select(id, postal_code) |> 
  mutate(postal_code = str_replace_all(postal_code, " ", ""),
         postal_code = str_to_upper(postal_code),
         fsa = str_sub(postal_code, start = 1, end = 3)) |> 
    filter(!is.na(fsa),
           str_starts(fsa, "R")) |> 
  select(id, fsa)


#link region to mac_fsa_df
#randomly assign regions with multiple occurences of FSA

mac_fsa_data_linked <- mac_fsa_data |>
  mutate(.row_id = row_number()) |> 
  left_join(regions, by = "fsa", relationship = "many-to-many") |> 
  group_by(.row_id) |> 
  slice_sample(n = 1)

mac_fsa_data_grouped <- mac_fsa_data_linked |> 
  group_by(region) |> 
  count()
  
# link counts to shapefile

mb_econ_regions_sf <-mb_econ_regions_sf |> 
  left_join(mac_fsa_data_grouped,
            by = c("economic_r" = "region"))



ggplot() +
  geom_sf(data = mb_boundary_map,
          fill = "grey98",
          colour = "grey50",
          linewidth = 0.4) +
  geom_sf(data = mb_econ_regions_sf,
          (aes(fill = n)),
                          colour = NA) +
  scale_fill_gradient(trans = "reverse") +
  theme_void()





