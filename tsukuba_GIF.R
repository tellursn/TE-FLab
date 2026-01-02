
library(tidyverse)
library(rgl)
library(magick)


file_path <- "tsukuba_shiki1.csv" # load file
if (!file.exists(file_path)) {
  stop(paste(file_path, "not found"))
}

full_data <- read_csv(file_path)

full_data$date <- as.Date(full_data$date)
full_data$year <- as.numeric(format(full_data$date, "%Y"))
climate_vars <- na.omit(full_data[, c("year", "sunshine", "temperature", "precipitation")])

if (!dir.exists("gif_frames")) {
  dir.create("gif_frames")
}


start_year <- min(climate_vars$year)
end_year <- max(climate_vars$year) - 1

cat(paste0("Create attractors from", start_year, "to", end_year, "\n"))

xlim <- range(climate_vars$sunshine)
ylim <- range(climate_vars$temperature)
zlim <- range(climate_vars$precipitation)


open3d()
par3d(windowRect = c(0, 0, 800, 800))
bg3d("white")

for (year in start_year:end_year) {

  current_years <- c(year, year + 1, year + 2)

  window_data <- climate_vars %>%
    filter(year %in% current_years)

  frame_file <- file.path("gif_frames", sprintf("frame_%04d.png", year))

clear3d()
  plot3d(
    x = window_data$sunshine,
    y = window_data$temperature,
    z = window_data$precipitation,
    type = "l",
    col = "blue",
    size = 3,
    axes = TRUE,
    xlab = "sunshine (h)",
    ylab = "temperture (℃)",
    zlab = "precipitation (mm)",
    main = paste0(year, " - ", year + 2),
    xlim = xlim, ylim = ylim, zlim = zlim,
    font.main = 2
  )

  view3d(theta = 60, phi = 40, fov = 60)
  rgl.snapshot(filename = frame_file)

  cat(paste(year, "frame was created\n"))
}

close3d()

cat("\nAll frames generated．Creating GIFs...\n")

frame_files <- list.files("gif_frames", pattern = "frame_.*\\.png$", full.names = TRUE)
frame_files <- sort(frame_files)

animation <- image_read(frame_files) %>%

  image_animate(fps = 5) # frame rate

image_write(animation, "tsukuba_climate.gif")

unlink("gif_frames", recursive = TRUE)

cat("\ncompleted．'tsukuba_climate.gif' was created．\n")

