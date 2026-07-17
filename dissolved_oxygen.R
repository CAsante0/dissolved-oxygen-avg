# heatmap_template.R

library(tidyverse)
library(jsonlite)
library(lubridate)

# The API requires this specific function name to execute
run_analysis <- function(context) {

  message("\n========================================================")
  message("[DIAGNOSTIC] Dissolved Oxygen Plot Process Initiated")
  message("========================================================")
  
  # --- STEP 1: CONTEXT VALIDATION ---
  message("[STEP 1/6] Validating incoming context object...")
  if (missing(context) || is.null(context)) {
    stop("[FATAL] Context object is completely missing or NULL.")
  }
  
  print("Dissolved Oxygen Plot: Input Parameters received:")
  print(context)
  
  # Extract paths
  input_folderpath      <- context$input
  output_directory_path <- context$output
  job_id                <- context$job_id %||% "UNKNOWN_JOB"
  
  # --- STEP 2: PATH & PERMISSION CHECKS ---
  message("[STEP 2/6] Verifying directory paths and access rights...")
  
  message(paste("-> Target Input Path: ", input_folderpath))
  if (is.null(input_folderpath) || input_folderpath == "") {
    stop("[FATAL] Input folderpath is empty or NULL in context.")
  }
  if (!dir.exists(input_folderpath)) {
    stop(paste("[FATAL] Input directory does not exist or is inaccessible:", input_folderpath))
  }
  
  message(paste("-> Target Output Path:", output_directory_path))
  if (is.null(output_directory_path) || output_directory_path == "") {
    stop("[FATAL] Output directory path is empty or NULL in context.")
  }
  if (!dir.exists(output_directory_path)) {
    message("[WARN] Output directory missing. Attempting to create it now...")
    dir.create(output_directory_path, recursive = TRUE)
    if (!dir.exists(output_directory_path)) {
      stop(paste("[FATAL] Failed to create output directory:", output_directory_path))
    }
  }

# --- STEP 3: ROBUST FILE DISCOVERY ---
  message("[STEP 3/6] Scanning input directory for CSV files...")
  
  # FIXED: Removed 'max.depth = 1' and replaced with standard non-recursive list
  root_contents <- list.files(input_folderpath, recursive = FALSE)
  message(paste("-> Total items found in root of input mount:", length(root_contents)))
  if (length(root_contents) > 0) {
    message(paste("-> Sample items in root:", paste(head(root_contents, 5), collapse = ", ")))
  }

  # Executing ultra-robust discovery: 
  all_files <- list.files(
    path = input_folderpath,
    pattern = "\\.csv$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  
  message(paste("-> Total matching CSV files discovered:", length(all_files)))
  
  if (length(all_files) == 0) {
    stop(paste("[FATAL] Zero CSV files were found matching '\\.csv$' (case-insensitive) inside:", input_folderpath))
  }

  # Executing ultra-robust discovery: 
  # - Uses '\\.csv$' to eliminate regular expression interpretation confusion
  # - Uses 'ignore.case = TRUE' to catch '.CSV', '.csv', '.Csv', etc.
  all_files <- list.files(
    path = input_folderpath,
    pattern = "\\.csv$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  
  message(paste("-> Total matching CSV files discovered:", length(all_files)))
  
  if (length(all_files) == 0) {
    stop(paste("[FATAL] Zero CSV files were found matching '\\.csv$' (case-insensitive) inside:", input_folderpath))
  }
  
  # --- STEP 4: FILENAME PARSING & REGISTRY ---
  message("[STEP 4/6] Parsing filenames for date patterns...")
  
  file_registry <- data.frame(full_path = all_files, stringsAsFactors = FALSE) %>%
    mutate(
      file_name = basename(full_path),
      date_string = str_extract(file_name, "\\d{4}-\\d{2}-\\d{2}"),
      month_id = substr(date_string, 1, 7) 
    )
  
  # Diagnostic overview of parsing success
  total_scanned <- nrow(file_registry)
  file_registry <- file_registry %>% filter(!is.na(month_id))
  total_valid <- nrow(file_registry)
  
  message(paste("-> Scanned files:", total_scanned, "| Successfully parsed dates:", total_valid))
  
  if (total_valid == 0) {
    stop("[FATAL] Found CSV files, but NONE of their filenames contain a valid 'YYYY-MM-DD' date string format.")
  }

  unique_months <- unique(file_registry$month_id)
  message(paste("-> Identified unique months to process:", paste(unique_months, collapse = ", ")))

  # --- STEP 5: PROCESSING LOOP WITH ERROR TRAPPING ---
  message("[STEP 5/6] Entering processing loop...")
  
  for (current_month in unique_months) {
    message(paste("\n--- Processing Month:", current_month, "---"))
    
    plot_filename <- paste0("DO_Heatmap_", current_month, ".png")
    plot_destination <- file.path(output_directory_path, plot_filename)
    
    # Idempotency Check
    if (file.exists(plot_destination)) {
      message(paste("  [INFO] Skipping", current_month, "- Target heatmap image already exists."))
      next
    }
    
    # Isolate targets
    target_files <- file_registry %>% 
      filter(month_id == current_month) %>% 
      pull(full_path)
    
    message(paste("  [INFO] Aggregating", length(target_files), "files for this month..."))
    
    # Catch structural schema or read errors per file safely
    monthly_data <- tryCatch({
      target_files %>% map_df(~read_csv(.x, show_col_types = FALSE))
    }, error = function(e) {
      message(paste("  [ERROR] Failed to read CSV datasets for month", current_month, ":", e$message))
      return(NULL)
    })
    
    if (is.null(monthly_data) || nrow(monthly_data) == 0) {
      message("  [WARN] Dataset empty or unreadable for this block. Moving to next month.")
      next
    }
    
    # Verify column layout requirement
    required_cols <- c("latitude", "longitude", "dissolved_oxygen_count")
    missing_cols <- setdiff(required_cols, colnames(monthly_data))
    
    if (length(missing_cols) > 0) {
      message(paste("  [ERROR] Missing required columns:", paste(missing_cols, collapse = ", ")))
      message(paste("  [DEBUG] Available columns were:", paste(colnames(monthly_data), collapse = ", ")))
      next
    }
    
    # Generate Stats
    summary_stats <- monthly_data %>%
      group_by(latitude, longitude) %>%
      summarize(
        avg_dissolved_oxygen = mean(dissolved_oxygen_count, na.rm = TRUE),
        .groups = "drop"
      )
    
    message(paste("  [INFO] Rendering plot with", nrow(summary_stats), "calculated grid points..."))
    
    # Plotting
    heatmap_plot <- ggplot(summary_stats, aes(x = longitude, y = latitude, fill = avg_dissolved_oxygen)) +
      geom_tile() +
      scale_fill_viridis_c(option = "mako") +
      labs(
        title = paste("Average Dissolved Oxygen:", current_month),
        subtitle = paste("Job ID:", job_id),
        x = "Longitude", y = "Latitude"
      ) +
      theme_minimal()
    
    # Safe save check
    save_status <- tryCatch({
      ggsave(plot_destination, plot = heatmap_plot, width = 10, height = 8)
      TRUE
    }, error = function(e) {
      message(paste("  [ERROR] Failed writing file to disk:", e$message))
      FALSE
    })
    
    if (save_status) {
      message(paste("  [SUCCESS] Saved map to:", plot_destination))
    }
  }
  
  # --- STEP 6: FINALIZATION ---
  message("\n========================================================")
  message("[STEP 6/6] Heatmap generation sequence finalized successfully.")
  message("========================================================\n")
}

print("Script loaded successfully. Awaiting execution.")