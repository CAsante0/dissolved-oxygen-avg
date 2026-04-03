# heatmap_template.R

library(tidyverse)
library(jsonlite)
library(lubridate)

# The API requires this specific function name to execute
run_analysis <- function(context) {

  print("Context received:")
  print(context)
  # --- PATH RESOLUTION ---
  # These come from the 'mnts' list defined in api.R
  input_folderpath      <- context$mounts$input
  output_directory_path <- context$mounts$output
  
  message(paste("Starting Heatmap Generation. Input:", input_folderpath))
  
  # --- FILE DISCOVERY ---
  all_files <- list.files(
    path = input_folderpath,
    pattern = "ERDDAP_fishbot_realtime_dissolved-oxygen-count_.*\\.csv$",
    full.names = TRUE,
    recursive = TRUE
  )
  
  if (length(all_files) == 0) {
    stop("No files found matching the naming pattern in the input mount.")
  }
  
  # --- FILENAME PARSING ---
  file_registry <- data.frame(full_path = all_files) %>%
    mutate(
      file_name = basename(full_path),
      date_string = str_extract(file_name, "\\d{4}-\\d{2}-\\d{2}"),
      month_id = substr(date_string, 1, 7) 
    ) %>%
    filter(!is.na(month_id))

  unique_months <- unique(file_registry$month_id)

  # --- PROCESSING LOOP ---
  for (current_month in unique_months) {
    
    plot_filename <- paste0("DO_Heatmap_", current_month, ".png")
    plot_destination <- file.path(output_directory_path, plot_filename)
    
    # Idempotency Check
    if (file.exists(plot_destination)) {
      message(paste("Skipping", current_month, "- Map exists."))
      next
    }
    
    message(paste("Processing month:", current_month))
    
    # Aggregate Data
    monthly_data <- file_registry %>% 
      filter(month_id == current_month) %>% 
      pull(full_path) %>%
      map_df(~read_csv(.x, show_col_types = FALSE))
    
    if (nrow(monthly_data) == 0) next
    
    summary_stats <- monthly_data %>%
      group_by(latitude, longitude) %>%
      summarize(
        avg_dissolved_oxygen = mean(dissolved_oxygen_count, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Plotting
    heatmap_plot <- ggplot(summary_stats, aes(x = longitude, y = latitude, fill = avg_dissolved_oxygen)) +
      geom_tile() +
      scale_fill_viridis_c(option = "mako") +
      labs(
        title = paste("Average Dissolved Oxygen:", current_month),
        subtitle = paste("Job ID:", context$job_id),
        x = "Longitude", y = "Latitude"
      ) +
      theme_minimal()
    
    ggsave(plot_destination, plot = heatmap_plot, width = 10, height = 8)
  }
  
  message("Heatmap generation sequence finalized.")
}
print("Script loaded successfully. Awaiting execution.")