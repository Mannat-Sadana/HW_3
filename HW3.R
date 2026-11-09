# HW3
# This file contains my code and explanations for all three problems. I used ggplot2 for the graphs and sf for the spatial analysis.

# SETUP

packages <- c("ggplot2", "dplyr", "tidyr", "readr", "sf",
              "stringr", "tibble", "viridis")

missing <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing) > 0) {
  install.packages(missing)
}

invisible(lapply(packages, library, character.only = TRUE))

# Create folders for the output files.
out_dir <- "HW3_output"
dir.create(out_dir, showWarnings = FALSE)
dir.create(file.path(out_dir, "plots"), showWarnings = FALSE)
dir.create(file.path(out_dir, "data"), showWarnings = FALSE)

# This function saves my plots.
save_plot <- function(plot, filename, width = 9, height = 6) {
  ggsave(
    file.path(out_dir, "plots", filename),
    plot,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
}


# PROBLEM 1

# I used the built-in mpg and diamonds datasets from ggplot2.

vehicles <- ggplot2::mpg
gems <- ggplot2::diamonds


# PART A

# This graph compares engine displacement and highway MPG. I colored the points by drive type and used separate panels to make it easier to compare the different drive types.

p1a <- ggplot(vehicles, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~drv) +
  labs(
    title = "Engine Size and Highway MPG by Drive Type",
    x = "Engine displacement (litres)",
    y = "Highway fuel economy (mpg)",
    color = "Drive type"
  ) +
  theme_bw()

print(p1a)

save_plot(p1a, "P1A_engine_highway_by_drive.png", 10, 5.5)

# I used facet_wrap to create a separate panel for each drive type. The regression lines help show the relationship between engine size and highway fuel economy.


# PART B

# First, I made a bar graph showing the number of cars in each group. The fill color represents the number of cylinders.

p1b_count <- ggplot(
  vehicles,
  aes(x = drv, fill = factor(cyl))
) +
  geom_bar() +
  labs(
    title = "Cylinder Counts for Each Drive Type",
    x = "Drive type",
    y = "Count",
    fill = "Number of cylinders"
  ) +
  theme_bw()

print(p1b_count)

save_plot(p1b_count, "P1B_counts_by_drive.png", 7, 5)

# Next, I changed the bars to proportions. This makes it easier to compare the groups even if they have different numbers of cars.

p1b_prop <- ggplot(
  vehicles,
  aes(x = drv, fill = factor(cyl))
) +
  geom_bar(position = "fill") +
  labs(
    title = "Cylinder Proportions by Drive Type",
    x = "Drive type",
    y = "Proportion",
    fill = "Number of cylinders"
  ) +
  theme_bw()

print(p1b_prop)

save_plot(p1b_prop, "P1B_proportions_by_drive.png", 7, 5)

# The first graph shows the actual counts, while the second graph shows the proportions within each drive type.


# PART C

# I removed missing carat values and kept diamonds with carat values greater than 0 and up to 2.00.

gems_small <- gems %>%
  filter(!is.na(carat), carat > 0, carat <= 2.00)

retained_n <- nrow(gems_small)
excluded_n <- nrow(gems) - retained_n

cat("\nProblem 1C\n")
cat("Records retained:", retained_n, "\n")
cat("Records excluded:", excluded_n, "\n")

# First histogram with a smaller bin width.

p_fine <- ggplot(gems_small, aes(x = carat)) +
  geom_histogram(
    binwidth = 0.01,
    boundary = 0,
    closed = "right",
    color = "white"
  ) +
  coord_cartesian(xlim = c(0, 2)) +
  labs(
    title = "Diamond Carat Histogram (Bin Width = 0.01)",
    x = "Carat",
    y = "Count"
  ) +
  theme_bw()

print(p_fine)

save_plot(p_fine, "P1C_histogram_fine_001.png", 9, 5)

# Second histogram with a larger bin width.

p_coarse <- ggplot(gems_small, aes(x = carat)) +
  geom_histogram(
    binwidth = 0.5,
    boundary = 0,
    closed = "right",
    color = "white"
  ) +
  coord_cartesian(xlim = c(0, 2)) +
  labs(
    title = "Diamond Carat Histogram (Bin Width = 0.5)",
    x = "Carat",
    y = "Count"
  ) +
  theme_bw()

print(p_coarse)

save_plot(p_coarse, "P1C_histogram_coarse_05.png", 9, 5)

# I also checked how many diamonds have exactly 0.99 and 1.00 carats.

exact_carat_counts <- gems %>%
  filter(carat %in% c(0.99, 1.00)) %>%
  count(carat, name = "n_diamonds") %>%
  complete(
    carat = c(0.99, 1.00),
    fill = list(n_diamonds = 0)
  )

print(exact_carat_counts)

write_csv(
  exact_carat_counts,
  file.path(out_dir, "data", "P1C_exact_carat_counts.csv")
)

write_csv(
  tibble(
    records_retained = retained_n,
    records_excluded = excluded_n,
    subset_definition = "Nonmissing carat greater than 0 and up to 2.00"
  ),
  file.path(out_dir, "data", "P1C_retained_excluded.csv")
)

# The smaller bin width shows more detail in the distribution. The larger bin width combines more values and makes the overall distribution easier to see. I used the same filtered data for both graphs.


# PROBLEMS 2 AND 3: DATA COLLECTION AND CLEANING

# For these problems, I chose Chicago, Illinois. I used Chicago Public Library locations as my facilities and Chicago community areas as my study areas.

points_url <- paste0(
  "https://data.cityofchicago.org/resource/",
  "x8fc-8rcq.geojson?$limit=5000"
)

boundary_url <- paste0(
  "https://data.cityofchicago.org/resource/",
  "igwz-8jzy.geojson?$limit=5000"
)

# Download the two datasets.

sites_raw <- st_read(points_url, quiet = TRUE)
areas_raw <- st_read(boundary_url, quiet = TRUE)

# Save copies of the original data.

st_write(
  sites_raw,
  file.path(out_dir, "data", "libraries_raw.geojson"),
  delete_dsn = TRUE,
  quiet = TRUE
)

st_write(
  areas_raw,
  file.path(out_dir, "data", "community_areas_raw.geojson"),
  delete_dsn = TRUE,
  quiet = TRUE
)


# Clean the library point data.

sites_wgs <- st_transform(sites_raw, 4326)

site_geom_ok <- !st_is_empty(sites_wgs) &
  st_geometry_type(sites_wgs) == "POINT"

coordinates <- st_coordinates(sites_wgs)

sites_valid <- sites_wgs[
  site_geom_ok &
    !is.na(coordinates[, 1]) &
    !is.na(coordinates[, 2]),
]

# Find the column that contains the library names.

name_candidates <- c(
  "branch_",
  "branch",
  "name",
  "facility",
  "facility_name"
)

name_field <- name_candidates[
  name_candidates %in% names(sites_valid)
][1]

if (is.na(name_field)) {
  stop(
    "Could not identify the library name column. ",
    "Check names(sites_raw) and update name_candidates."
  )
}

site_names <- as.character(sites_valid[[name_field]])

site_names[
  is.na(site_names) | trimws(site_names) == ""
] <- "Unnamed library"

sites_valid$site_id <- sprintf(
  "LIB%03d",
  seq_len(nrow(sites_valid))
)

sites_valid$facility_name <- site_names

# I changed the coordinate system to EPSG:26916. This coordinate system uses meters, which is important for calculating distances and buffer areas.

metric_crs <- 26916

sites_m <- st_transform(sites_valid, metric_crs)

# Check for duplicate coordinates.

coords <- st_coordinates(sites_m)

coord_key <- paste(
  round(coords[, 1], 3),
  round(coords[, 2], 3),
  sep = "_"
)

duplicate_coordinate_rows <- sum(duplicated(coord_key))

# Create a small report showing the cleaning results.

cleaning_report <- tibble(
  raw_records = nrow(sites_raw),
  retained_records = nrow(sites_m),
  removed_records = nrow(sites_raw) - nrow(sites_m),
  duplicate_coordinate_rows_retained = duplicate_coordinate_rows,
  coordinate_system = "EPSG:26916",
  distance_units = "meters"
)

print(cleaning_report)

write_csv(
  cleaning_report,
  file.path(out_dir, "data", "P2_cleaning_report.csv")
)

st_write(
  sites_m,
  file.path(out_dir, "data", "sites_m.gpkg"),
  layer = "sites_m",
  delete_dsn = TRUE,
  quiet = TRUE
)

write_csv(
  st_drop_geometry(sites_valid),
  file.path(out_dir, "data", "cleaned_library_locations.csv")
)

# Record the sources and date of the downloaded data.

data_sources <- tibble(
  dataset = c(
    "Chicago Public Library locations",
    "Chicago community area boundaries"
  ),
  publisher = c(
    "City of Chicago Data Portal",
    "City of Chicago Data Portal"
  ),
  source_page = c(
    "https://data.cityofchicago.org/d/x8fc-8rcq",
    "https://data.cityofchicago.org/d/igwz-8jzy"
  ),
  download_url = c(points_url, boundary_url),
  access_date = as.character(Sys.Date()),
  format = c("GeoJSON", "GeoJSON")
)

write_csv(
  data_sources,
  file.path(out_dir, "data", "data_sources.csv")
)


# PROBLEM 2: BUFFERS

# I created 500 meter and 1,000 meter buffers around each library. I used st_union to combine overlapping buffers so that overlapping areas are not counted multiple times.

buf500 <- st_union(st_buffer(sites_m, dist = 500))
buf1000 <- st_union(st_buffer(sites_m, dist = 1000))

# Map showing the library locations.

p_sites <- ggplot() +
  geom_sf(
    data = sites_m,
    color = "firebrick",
    size = 1.5
  ) +
  labs(
    title = "Public Library Locations in Chicago",
    subtitle = paste(
      nrow(sites_m),
      "retained library records"
    ),
    caption = paste(
      "Source: City of Chicago Data Portal.",
      "Accessed on the date the script was run."
    ),
    x = "Easting (meters)",
    y = "Northing (meters)"
  ) +
  theme_bw()

print(p_sites)

save_plot(p_sites, "P2_library_sites.png", 8, 8)

# Create a map of the combined 500 meter buffers.

buffer500_sf <- st_sf(
  radius_m = 500,
  geometry = buf500
)

p_buffers <- ggplot() +
  geom_sf(
    data = buffer500_sf,
    fill = "#E69F00",
    alpha = 0.40,
    color = NA
  ) +
  geom_sf(
    data = sites_m,
    color = "black",
    size = 1.1
  ) +
  labs(
    title = "500 Meter Buffers Around Chicago Libraries",
    caption = "Overlapping buffers have been combined.",
    x = "Easting (meters)",
    y = "Northing (meters)"
  ) +
  theme_bw()

print(p_buffers)

# Create a map of the combined 1,000 meter buffers.

buffer1000_sf <- st_sf(
  radius_m = 1000,
  geometry = buf1000
)

p_buffers1000 <- ggplot() +
  geom_sf(
    data = buffer1000_sf,
    fill = "#56B4E9",
    alpha = 0.40,
    color = NA
  ) +
  geom_sf(
    data = sites_m,
    color = "black",
    size = 1.1
  ) +
  labs(
    title = "1,000 Meter Buffers Around Chicago Libraries",
    caption = "Overlapping buffers have been combined.",
    x = "Easting (meters)",
    y = "Northing (meters)"
  ) +
  theme_bw()

print(p_buffers1000)

save_plot(p_buffers1000, "P2_combined_buffers_1000m.png", 8, 8)

save_plot(p_buffers, "P2_combined_buffers.png", 8, 8)

# Calculate the total area covered by each set of buffers. I converted square meters to square kilometers.

buffer_summary <- tibble(
  radius_m = c(500, 1000),
  union_area_km2 = c(
    as.numeric(st_area(buf500)),
    as.numeric(st_area(buf1000))
  ) / 1e6
)

print(buffer_summary)

# The 1,000 meter buffers should cover a larger combined area than the 500 meter buffers.

stopifnot(
  buffer_summary$union_area_km2[2] >=
    buffer_summary$union_area_km2[1]
)

write_csv(
  buffer_summary,
  file.path(out_dir, "data", "buffer_summary.csv")
)

# Save both buffer geometries.

st_write(
  st_sf(radius_m = 500, geometry = buf500),
  file.path(out_dir, "data", "buffer_500m.gpkg"),
  delete_dsn = TRUE,
  quiet = TRUE
)

st_write(
  st_sf(radius_m = 1000, geometry = buf1000),
  file.path(out_dir, "data", "buffer_1000m.gpkg"),
  delete_dsn = TRUE,
  quiet = TRUE
)

# The buffers show the areas within walking-distance ranges of the library locations. The union area gives the total covered area without counting overlaps more than once.

cat("\nProblem 2 - Buffer results\n")

cat(
  "The total 500 meter buffer area is",
  round(buffer_summary$union_area_km2[1], 2),
  "square kilometers.\n"
)

cat(
  "The total 1,000 meter buffer area is",
  round(buffer_summary$union_area_km2[2], 2),
  "square kilometers.\n"
)


# PROBLEM 3: COMMUNITY AREA COUNTS AND DENSITY

# I used the community area boundaries to organize the libraries. First, I cleaned the boundary data and changed it to the same coordinate system as the library locations.

areas_wgs <- st_transform(areas_raw, 4326)

areas_ok <- !st_is_empty(areas_wgs) &
  st_geometry_type(areas_wgs) %in%
    c("POLYGON", "MULTIPOLYGON")

areas_valid <- st_make_valid(areas_wgs[areas_ok, ])

# Find the ID and name columns for the community areas.

area_id_candidates <- c(
  "area_numbe",
  "area_num_1",
  "area_num",
  "area_id",
  "community"
)

area_name_candidates <- c(
  "community",
  "community_area",
  "name",
  "area_name"
)

id_field <- area_id_candidates[
  area_id_candidates %in% names(areas_valid)
][1]

area_name_field <- area_name_candidates[
  area_name_candidates %in% names(areas_valid)
][1]

if (is.na(id_field) || is.na(area_name_field)) {
  stop(
    "Could not identify community area ID/name columns. ",
    "Check names(areas_raw) and update the candidate lists."
  )
}

areas_valid$district_id <- as.character(
  areas_valid[[id_field]]
)

areas_valid$district_name <- as.character(
  areas_valid[[area_name_field]]
)

areas_m <- st_transform(areas_valid, metric_crs)

areas_m <- areas_m %>%
  select(district_id, district_name, geometry)

# Calculate the size of each community area in square kilometers.

areas_m$area_km2 <- as.numeric(st_area(areas_m)) / 1e6


# Check how many community areas each library matches.

intersections <- st_intersects(sites_m, areas_m)

match_n <- lengths(intersections)

# I only assign a library to an area when it matches exactly one area. This avoids counting the same library in multiple areas.

site_match_audit <- tibble(
  site_id = sites_m$site_id,
  facility_name = sites_m$facility_name,
  n_area_matches = match_n,
  matched_district_id = vapply(
    intersections,
    function(z) {
      if (length(z) == 1) {
        areas_m$district_id[z]
      } else {
        NA_character_
      }
    },
    character(1)
  ),
  matched_district_name = vapply(
    intersections,
    function(z) {
      if (length(z) == 1) {
        areas_m$district_name[z]
      } else {
        NA_character_
      }
    },
    character(1)
  )
)

print(site_match_audit %>% count(n_area_matches, name = "n_sites"))

write_csv(
  site_match_audit,
  file.path(out_dir, "data", "site_match_audit.csv")
)

# Count the facilities that match exactly one community area.

assigned <- site_match_audit %>%
  filter(n_area_matches == 1) %>%
  count(
    matched_district_id,
    matched_district_name,
    name = "n_sites"
  )

# Join the counts to the community area data. I kept areas with zero libraries so that all areas are included.

district_counts <- areas_m %>%
  st_drop_geometry() %>%
  select(district_id, district_name, area_km2) %>%
  left_join(
    assigned,
    by = c(
      "district_id" = "matched_district_id",
      "district_name" = "matched_district_name"
    )
  ) %>%
  mutate(
    n_sites = replace_na(n_sites, 0L),
    sites_per_km2 = n_sites / area_km2
  ) %>%
  select(
    district_id,
    district_name,
    n_sites,
    area_km2,
    sites_per_km2
  ) %>%
  arrange(desc(n_sites))

print(district_counts)

stopifnot(all(district_counts$area_km2 > 0))

stopifnot(
  sum(district_counts$n_sites) == sum(match_n == 1)
)

write_csv(
  district_counts,
  file.path(out_dir, "data", "district_counts.csv")
)

st_write(
  areas_m,
  file.path(out_dir, "data", "community_areas_m.gpkg"),
  layer = "community_areas_m",
  delete_dsn = TRUE,
  quiet = TRUE
)

# I calculated density by dividing the number of libraries by the size of each community area in square kilometers. This helps compare areas that have different sizes.


# PART 3 MAPS

# Join the counts and density values to the map data.

areas_plot <- areas_m %>%
  left_join(
    district_counts %>%
      select(district_id, n_sites, sites_per_km2),
    by = "district_id"
  )

areas_plot$n_sites[is.na(areas_plot$n_sites)] <- 0

areas_plot$sites_per_km2[
  is.na(areas_plot$sites_per_km2)
] <- 0

map_extent <- st_bbox(areas_m)

map_caption <- paste0(
  "Source: City of Chicago Data Portal. ",
  "Coordinate system: EPSG:26916. ",
  "Sites unassigned because they matched zero or multiple areas: ",
  sum(match_n != 1),
  "."
)

# Map showing the number of libraries in each community area.

p_count <- ggplot(areas_plot) +
  geom_sf(
    aes(fill = n_sites),
    color = "white",
    linewidth = 0.15
  ) +
  scale_fill_viridis_c(
    name = "Libraries",
    option = "C"
  ) +
  coord_sf(
    xlim = c(map_extent[1], map_extent[3]),
    ylim = c(map_extent[2], map_extent[4])
  ) +
  labs(
    title = "Library Counts by Chicago Community Area",
    caption = map_caption,
    x = "Easting (meters)",
    y = "Northing (meters)"
  ) +
  theme_bw()

print(p_count)

save_plot(p_count, "P3_facility_counts_by_area.png", 9, 8)

# Map showing the number of libraries per square kilometer.

p_density <- ggplot(areas_plot) +
  geom_sf(
    aes(fill = sites_per_km2),
    color = "white",
    linewidth = 0.15
  ) +
  scale_fill_viridis_c(
    name = "Libraries per km2",
    option = "B"
  ) +
  coord_sf(
    xlim = c(map_extent[1], map_extent[3]),
    ylim = c(map_extent[2], map_extent[4])
  ) +
  labs(
    title = "Library Density by Chicago Community Area",
    caption = map_caption,
    x = "Easting (meters)",
    y = "Northing (meters)"
  ) +
  theme_bw()

print(p_density)

save_plot(p_density, "P3_facility_density_by_area.png", 9, 8)


# PROBLEM 3: RESULTS AND EXPLANATION

# I checked how many libraries matched zero, one, or multiple areas. Only libraries that matched exactly one area were included in the final community area counts.

cat("\nProblem 3 - Results\n")

cat(
  "Libraries matching zero areas:",
  sum(match_n == 0),
  "\n"
)

cat(
  "Libraries matching exactly one area:",
  sum(match_n == 1),
  "\n"
)

cat(
  "Libraries matching multiple areas:",
  sum(match_n > 1),
  "\n"
)

# Find the community area with the most libraries.

most_libraries <- district_counts %>%
  slice_max(n_sites, n = 1, with_ties = TRUE)

cat("\nCommunity area(s) with the most libraries:\n")
print(most_libraries %>% select(district_name, n_sites))

# Find the community area with the highest library density.

highest_density <- district_counts %>%
  slice_max(sites_per_km2, n = 1, with_ties = TRUE)

cat("\nCommunity area(s) with the highest library density:\n")
print(
  highest_density %>%
    select(district_name, sites_per_km2)
)

# The count map shows where the libraries are located in terms of total numbers. The density map also considers the size of each community area, so the two maps can look different. Areas with more libraries do not always have the highest density, because some community areas are much larger than others.


# FINAL SUMMARY

summary_lines <- c(
  "HW3 RUN SUMMARY",
  paste("Run date:", Sys.Date()),
  "Study area: Chicago, Illinois",
  "Facilities: Chicago Public Library locations",
  paste("Raw facility records:", nrow(sites_raw)),
  paste("Retained facility records:", nrow(sites_m)),
  paste("Removed facility records:", nrow(sites_raw) - nrow(sites_m)),
  paste("Duplicate-coordinate rows retained:", duplicate_coordinate_rows),
  "Coordinate system: EPSG:26916",
  "Distance units: meters",
  "Area units in district table: square kilometers",
  paste(
    "500 meter union area (km2):",
    round(buffer_summary$union_area_km2[1], 4)
  ),
  paste(
    "1,000 meter union area (km2):",
    round(buffer_summary$union_area_km2[2], 4)
  ),
  paste("Sites matching zero areas:", sum(match_n == 0)),
  paste("Sites matching exactly one area:", sum(match_n == 1)),
  paste("Sites matching multiple areas:", sum(match_n > 1)),
  paste("Total assigned sites:", sum(district_counts$n_sites)),
  paste(
    "Area with the most facilities:",
    paste(most_libraries$district_name, collapse = ", ")
  ),
  paste(
    "Area with the highest facility density:",
    paste(highest_density$district_name, collapse = ", ")
  )
)

writeLines(
  summary_lines,
  file.path(out_dir, "HW3_run_summary.txt")
)

cat("\n----------------------------------\n")
cat("HW3 FINISHED\n")
cat("Check the HW3_output folder for your plots, tables, and results.\n")
cat("----------------------------------\n")