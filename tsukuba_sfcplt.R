library(tidyverse)
library(plotly)
library(htmlwidgets)
library(akima)
library(tidyr)
library(tibble)

# -------------------------------------------------------------------------
# Part 2: Data Loading and Preparation
# -------------------------------------------------------------------------
file_path <- "tsukuba_shiki.csv"
if (!file.exists(file_path)) {
  stop(paste(file_path, "not found."))
}

full_data <- read_csv(file_path, show_col_types = FALSE)

# Convert date data to Date type and extract year
full_data$date <- as.Date(full_data$date)
full_data$year <- as.numeric(format(full_data$date, "%Y"))

# Select required columns and remove rows with NA values
climate_vars <- na.omit(full_data[, c("year", "sunshine", "temperature", "precipitation")])

# ★★★ Apply log10(log10(precipitation + 1) + 1) transformation ★★★
climate_vars$log_precip <- log10(climate_vars$precipitation + 1)
climate_vars$double_log_precip <- log10(climate_vars$log_precip + 1)

# -------------------------------------------------------------------------
# Part 3: Create Animation Data (with akima Interpolation)
# -------------------------------------------------------------------------
start_year <- min(climate_vars$year)
end_year <- max(climate_vars$year) - 1

cat(paste0("Generating and interpolating animation data from ", start_year, " to ", end_year, "...\n"))

# Define a regular grid resolution
grid_resolution_x <- 40
grid_resolution_y <- 40

# Loop through each year, interpolate a surface, and store it
list_of_frames <- map(start_year:end_year, function(current_start_year) {

  cat(paste0("Processing ", current_start_year, "-", current_start_year + 1, "\n"))
  target_years <- c(current_start_year, current_start_year + 1)

  window_data <- climate_vars %>%
    filter(year %in% target_years)

  # Use akima::interp to create an interpolated surface
  if (nrow(window_data) > 5) {
    interp_result <- try(
      interp(
        x = window_data$sunshine,
        y = window_data$temperature,
        z = window_data$double_log_precip, # ★★★ Use double_log_precip for interpolation
        duplicate = "mean",
        nx = grid_resolution_x,
        ny = grid_resolution_y,
        linear = TRUE 
      ),
      silent = TRUE
    )

    if (inherits(interp_result, "try-error")) {
      cat(paste0("  Skipping ", current_start_year, " due to interpolation error.\n"))
      return(NULL)
    }

    # Convert the (matrix) result from interp into a long data frame
    z_matrix <- interp_result$z
    rownames(z_matrix) <- interp_result$x
    colnames(z_matrix) <- interp_result$y

    frame_data <- as.data.frame(z_matrix) %>%
      rownames_to_column(var = "sunshine") %>%
      pivot_longer(
        cols = -sunshine,
        names_to = "temperature",
        # ★★★ Use a new name for the smoothed double-log value ★★★
        values_to = "double_log_precip_smooth" 
      ) %>%
      mutate(
        sunshine = as.numeric(sunshine),
        temperature = as.numeric(temperature),
        frame = current_start_year
      )

    return(frame_data)

  } else {
    cat(paste0("  Skipping ", current_start_year, " due to insufficient data.\n"))
    return(NULL)
  }
})

# Combine all frames into one large data frame and remove NA values
animation_data <- bind_rows(list_of_frames) %>%
  na.omit()

cat("Data generation complete.\n")

# -------------------------------------------------------------------------
# Part 4: Create Interactive Plot with Plotly
# -------------------------------------------------------------------------
# Fix axis and color ranges for the entire period
xlim <- range(climate_vars$sunshine)
ylim <- range(climate_vars$temperature)
# ★★★ Use the double-log smoothed data for z-axis and color limits ★★★
zlim <- range(animation_data$double_log_precip_smooth)
cmin <- min(zlim)
cmax <- max(zlim)

cat("Generating interactive 3D plot...\n")

fig <- plot_ly(
  data = animation_data,
  x = ~sunshine,
  y = ~temperature,
  # ★★★ Use the double-log smoothed data for z-axis ★★★
  z = ~double_log_precip_smooth,
  frame = ~frame,
  type = "mesh3d",
  # ★★★ Use the double-log smoothed data for intensity ★★★
  intensity = ~double_log_precip_smooth,
  colorscale = "Viridis",
  showscale = TRUE,
  cmin = cmin,
  cmax = cmax,
  # ★★★ Update colorbar title ★★★
  colorbar = list(title = "log10(log10(precip+1)+1)")
) %>%
  animation_slider(
    currentvalue = list(
      prefix = "Period: ",
      suffix = " (2-year period)",
      font = list(color="black")
    )
  ) %>%
  layout(
    title = "Time Series Climate Surface Changes in Tsukuba (2-year periods)",
    scene = list(
      xaxis = list(title = "Sunshine (h)", range = xlim),
      yaxis = list(title = "Temperature (℃)", range = ylim),
      # ★★★ Update z-axis title ★★★
      zaxis = list(title = "Precipitation (mm, log-log scale)", range = zlim),
      aspectmode = "cube"
    )
  )

# -------------------------------------------------------------------------
# Part 5: Save Plot as HTML file
# -------------------------------------------------------------------------
output_filename <- "climate_surfaceplot.html"
saveWidget(fig, output_filename, selfcontained = TRUE)

cat(paste0("\nProcess complete. '", output_filename, "' has been created.\n"))
cat("Please open this HTML file in a web browser to view the interactive plot.\n")


