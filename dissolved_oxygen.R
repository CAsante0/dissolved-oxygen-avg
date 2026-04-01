#!/usr/bin/env Rscript

# Load libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
  library(lubridate)
})

# --- DATA UNPACKING ---
# The calling script passes the JSON dictionary as the first command line argument
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) stop("No JSON configuration provided.")

# Parse the JSON string into an R list
# Expected JSON structure: {"input_path": "/path/to/csvs", "output_path": "/path/to/save"}
config <- fromJSON(args[1])
print(config) # For debugging: confirm we received the expected configuration

# Clearly defining variables for intermediate/advanced analysis users
input_folderpath <- config$input_path
output_directory_path <- config$output_path

# --- VALIDATION ---
if (!dir.exists(input_folderpath)) stop("Input directory does not exist.")
if (!dir.exists(output_directory_path)) dir.create(output_directory_path, recursive = TRUE)

# --- FILE DISCOVERY ---
# Search recursively for filenames matching the specific pattern
all_files <- list.files(
  path = input_folderpath,
  pattern = "ERDDAP_fishbot_realtime_dissolved-oxygen-count_.*\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)

if (length(all_files) == 0) {
  message("No files found matching the naming pattern. Exiting.")
  quit(save = "no")
}

# --- FILENAME PARSING ---
# Create a reference table of all files and their target months
file_registry <- data.frame(full_path = all_files) %>%
  mutate(
    file_name = basename(full_path),
    # Extract date (YYYY-MM-DD) from the filename pattern
    date_string = str_extract(file_name, "\\d{4}-\\d{2}-\\d{2}"),
    # Create a Year-Month grouping variable
    month_id = substr(date_string, 1, 7) 
  ) %>%
  filter(!is.na(month_id))

# Identify which months we need to process
unique_months <- unique(file_registry$month_id)

# --- MAIN PROCESSING LOOP ---
# Using a standard for-loop for transparency and ease of debugging
for (current_month in unique_months) {
  
  # Define the output file name
  plot_filename <- paste0("DO_Heatmap_", current_month, ".png")
  plot_destination <- file.path(output_directory_path, plot_filename)
  
  # SKIP logic: Check if this map was already generated
  if (file.exists(plot_destination)) {
    message(paste("Skipping", current_month, "- File already exists."))
    next
  }
  
  message(paste("Processing month:", current_month))
  
  # Filter the registry for files belonging only to the current month loop
  files_to_read <- file_registry %>% 
    filter(month_id == current_month) %>% 
    pull(full_path)
  
  # Read and combine all daily CSVs for this month
  # We use map_df for a clean row-bind, but read_csv is standard tidyverse
  monthly_data <- files_to_read %>%
    map_df(~read_csv(.x, show_col_types = FALSE))
  
  if (nrow(monthly_data) == 0) next
  
  # --- DATA ANALYSIS ---
  # Calculate the average dissolved oxygen count per coordinate
  summary_stats <- monthly_data %>%
    group_by(lat, lon) %>%
    summarize(
      avg_dissolved_oxygen = mean(dissolved_oxygen_count, na.rm = TRUE),
      observation_count = n(),
      .groups = "drop"
    )
  
  # --- VISUALIZATION ---
  heatmap_plot <- ggplot(summary_stats, aes(x = lon, y = lat, fill = avg_dissolved_oxygen)) +
    geom_tile() +
    scale_fill_viridis_c(option = "mako") +
    labs(
      title = paste("Average Dissolved Oxygen Count:", current_month),
      subtitle = paste("Aggregated from", length(files_to_read), "daily records"),
      x = "Longitude",
      y = "Latitude",
      fill = "Avg Count"
    ) +
    theme_minimal()
  
  # Export the plot
  ggsave(
    filename = plot_destination, 
    plot = heatmap_plot, 
    width = 10, 
    height = 8, 
    dpi = 300
  )
}

message("Process complete.")