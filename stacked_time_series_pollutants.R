# Libraries

library(tidyverse)
library(janitor)
library(plyr)
library(scales)
library(cowplot)

# Set directories
main_dir <- "~"
setwd(main_dir)
data_dir <- file.path(main_dir, "~")
out_dir <- file.path(main_dir, "~")

# Read in data
data <- read.csv(file.path(data_dir, "~")) 
colnames(data) <- make_clean_names(colnames(data))

metadata <- read.csv(file.path(data_dir, "~"))
metadata <- metadata %>%
  select(aqs_sitecode, local_site_name, county_name) %>%
  mutate(aqs_sitecode = str_pad(as.character(aqs_sitecode), width = 9, side = "left", pad = "0"))

# event_dates <- read.csv(file.path(data_dir, "event_dates.csv"))
# event_dates$date_local <- as.Date(event_dates$date, format = "%m/%d/%Y")
# event_dates$date <- NULL

# percentile_matrix <- read.csv(file.path(main_dir, "figures_and_tables/event_site_percentile_matrix.csv"))
# colnames(percentile_matrix) <- make_clean_names(colnames(percentile_matrix))
# percentile_matrix <- percentile_matrix %>%
#   select(aqs_sitecode, id) %>%
#   distinct() %>%
#   mutate(site_type = "Exceeding Site",
#          aqs_sitecode = str_pad(as.character(aqs_sitecode), width = 9, side = "left", pad = "0"))

data_filt <- data %>%
  select(aqs_sitecode, parameter, sample_duration, sample_frequency, date_local, time_local, sample_measurement) %>%
  mutate(aqs_sitecode = str_pad(as.character(aqs_sitecode), side = "left", pad = "0", width = 9),
         datetime = paste(date_local, time_local),
         datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M", tz = "UTC"),
         date_local = as.Date(date_local)) %>%
  left_join(metadata) %>%
  filter(sample_duration == "1 HOUR",
         parameter %in% c("Carbon monoxide", "Nitrogen dioxide (NO2)", "PM10 Total 0-10um STP", "PM2.5 - Local Conditions")) %>%
  mutate(date_local = as.Date(date_local))

data_filt$datetime <- as.POSIXct(data_filt$datetime, format = "%Y-%m-%d %H:%M", tz = "UTC")

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
cb_colors <- c("#000000", gg_color_hue(n = length(unique(data_filt$local_site_name))))
alpha_scale <- c(0.5, rep(0.75, length(unique(data_filt$local_site_name))))
lw_scale <- c(0.5, rep(0.75, length(unique(data_filt$local_site_name))))
site_order <- c("Non Exceeding Site", unique(data_filt$local_site_name))

co_label <- "1-hr CO (ppm)"
nox_label <- expression(paste("1-hr ", NO[2], " (ppb)"))
pm25_1hr_label <- expression(paste("1-hr ", PM[2.5], " (", mu*g/m^3, ")", sep = ""))
pm10_1hr_label <- expression(paste("1-hr ", PM[10], " (", mu*g/m^3, ")", sep = ""))

# plotting 

for (i in unique(event_dates$id)) {
  # time information
  dates <- event_dates[event_dates$id == i,]
  
  # file name 
  if (length(dates$date) > 1) {
    x_limits <- seq.Date(from = dates$date[1] - 4, to = dates$date[length(dates$date)] + 4, by = "day")
    out_name <- paste0("co_no2_pm_", 
                       gsub(pattern = "-", replacement = "_", as.character(dates$date[1])), 
                       "_to_", 
                       as.character(format(dates$date[length(dates$date)], "%d")), 
                       ".png")
  } else {
    x_limits <- seq.Date(from = dates$date[1] - 4, to = dates$date[length(dates$date)] + 4, by = "day")
    out_name <- paste0("co_no2_pm_", 
                       gsub(pattern = "-", replacement = "_", as.character(dates$date[1])), 
                       ".png")
  }
  
  ptile_matrix_temp <- percentile_matrix %>%
    filter(id == i)
  
  df <- data_filt %>%
    filter(between(x = date_local, left = min(x_limits), right = max(x_limits))) %>%
    mutate(id = i) %>%
    left_join(ptile_matrix_temp) %>%
    mutate(label = ifelse(is.na(site_type), "Non Exceeding Site", local_site_name))

  # shading data frame
  shading <- df %>% 
    filter(date_local %in% dates$date) %>% 
    select(datetime, date_local) %>% 
    distinct() %>%
    group_by(date_local) %>%
    reframe(xmin = min(datetime),
            xmax = max(datetime),
            ymin = -Inf,
            ymax = Inf) %>%
    ungroup()
  
  legend <- get_legend(
    ggplot(data = df, 
           mapping = aes(x = datetime, y = sample_measurement, group = local_site_name, color = label, alpha = label, linewidth = label)) +
      geom_line() +
      scale_color_manual(values = cb_colors, breaks = site_order) + 
      scale_alpha_manual(values = alpha_scale, breaks = site_order) +
      scale_linewidth_manual(values = lw_scale, breaks = site_order) +
      theme_bw() +
      theme(legend.position = "right",
            legend.title = element_blank())
  )
  
  # CO
  max_co <- df %>%
    filter(parameter == "Carbon monoxide") %>%
    pull(sample_measurement) %>%
    max(na.rm = T)
  
  co <- ggplot(data = df, aes(x = datetime, y = sample_measurement, group = local_site_name, color = label, alpha = label, linewidth = label)) +
    geom_rect(data = shading, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, group = date_local), fill = "gray", alpha = 0.4, show.legend = F, inherit.aes = F) +
    geom_line(data = df %>%
                filter(parameter == "Carbon monoxide",
                       label == "Non Exceeding Site"),
              show.legend = F) +
    geom_line(data = df %>%
                filter(parameter == "Carbon monoxide",
                       label != "Non Exceeding Site"),
              show.legend = F) +
    scale_x_datetime(breaks = "24 hours", name='Date/Time', labels=label_date('%m/%d %H:%M'), expand = c(0,0)) +
    scale_y_continuous(limits = c(-0.01, max_co), breaks = seq(0, max_co, 0.2)) +
    ylab(co_label) +
    scale_color_manual(values = cb_colors, breaks = site_order) + 
    scale_alpha_manual(values = alpha_scale, breaks = site_order) +
    scale_linewidth_manual(values = lw_scale, breaks = site_order) +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          panel.grid.minor = element_blank()) 
  co
  
  # NOx
  max_nox <- df %>%
    filter(parameter == "Nitrogen dioxide (NO2)") %>%
    pull(sample_measurement) %>%
    max(na.rm = T) %>%
    round_any(10, f = ceiling)
  
  nox <- ggplot(data = df, aes(x = datetime, y = sample_measurement, group = local_site_name, color = label, alpha = label, linewidth = label)) +
    geom_rect(data = shading, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, group = date_local), fill = "gray", alpha = 0.4, show.legend = F, inherit.aes = F) +
    geom_line(data = df %>%
                filter(parameter == "Nitrogen dioxide (NO2)",
                       label == "Non Exceeding Site"),
              show.legend = F) +
    geom_line(data = df %>%
                filter(parameter == "Nitrogen dioxide (NO2)",
                       label != "Non Exceeding Site"),
              show.legend = F) +
    scale_x_datetime(breaks = "24 hours", name='Date/Time', labels=label_date('%m/%d %H:%M'), expand = c(0,0)) +
    scale_y_continuous(limits = c(0, max_nox), breaks = seq(0, max_nox, 10)) +    
    ylab(nox_label) +
    scale_color_manual(values = cb_colors, breaks = site_order) + 
    scale_alpha_manual(values = alpha_scale, breaks = site_order) +
    scale_linewidth_manual(values = lw_scale, breaks = site_order) +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          panel.grid.minor = element_blank())  
  nox
    
  # pm10 hourly
  
  max_pm10hr <- df %>%
    filter(parameter == "PM10 Total 0-10um STP") %>%
    pull(sample_measurement) %>%
    max(na.rm = T) %>%
    round_any(10, f = ceiling)
  
  if (max_pm10hr <= 30) {
    step_pm10 <- 5
  } else if (max_pm10hr > 30 & max_pm10hr <= 100) {
    step_pm10 <- 10
  } else if (max_pm10hr > 100 & max_pm10hr <= 150) {
    step_pm10 <- 20
  } else if (max_pm10hr > 150 & max_pm10hr <= 300) {
    step_pm10 <- 35
  } else if (max_pm10hr > 300 & max_pm10hr <= 500) {
    step_pm10 <- 50
  } else if (max_pm10hr > 500 & max_pm10hr <= 1000) {
    step_pm10 <- 100
  } else if (max_pm10hr > 1000 & max_pm10hr <= 2000) {
    step_pm10 <- 200
  } else if (max_pm10hr > 2000 & max_pm10hr <= 4000) {
    step_pm10 <- 500
  } else {
    step_pm10 <- 1000
  }
  
  pm10hr <- ggplot(data = df, aes(x = datetime, y = sample_measurement, group = local_site_name, color = label, alpha = label, linewidth = label)) +
    geom_rect(data = shading, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, group = date_local), fill = "gray", alpha = 0.4, show.legend = F, inherit.aes = F) +
    geom_line(data = df %>%
                filter(parameter == "PM10 Total 0-10um STP",
                       label == "Non Exceeding Site"),
              show.legend = F) +
    geom_line(data = df %>%
                filter(parameter == "PM10 Total 0-10um STP",
                       label != "Non Exceeding Site"),
              show.legend = F) +
    scale_x_datetime(breaks = "24 hours", name='Date/Time', labels=label_date('%m/%d %H:%M'), expand = c(0,0)) +
    scale_y_continuous(limits = c(0, max_pm10hr), breaks = seq(0, max_pm10hr, step_pm10)) +
    ylab(pm10_1hr_label) +
    scale_color_manual(values = cb_colors, breaks = site_order) + 
    scale_alpha_manual(values = alpha_scale, breaks = site_order) +
    scale_linewidth_manual(values = lw_scale, breaks = site_order) +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          panel.grid.minor = element_blank())  
  pm10hr
  
  # pm25 hourly
  
  max_pm25hr <- df %>%
    filter(parameter == "PM2.5 - Local Conditions") %>%
    pull(sample_measurement) %>%
    max(na.rm = T) %>%
    round_any(10, f = ceiling)
  
  if (max_pm25hr <= 30) {
    step_pm25 <- 5
  } else if (max_pm25hr > 30 & max_pm25hr <= 100) {
    step_pm25 <- 10
  } else if (max_pm25hr > 100 & max_pm25hr <= 150) {
    step_pm25 <- 20
  } else if (max_pm25hr > 150 & max_pm25hr <= 300) {
    step_pm25 <- 35
  } else if (max_pm25hr > 300 & max_pm25hr <= 500) {
    step_pm25 <- 50
  } else {
    step_pm25 <- 100
  }
  
  pm25hr <- ggplot(data = df, aes(x = datetime, y = sample_measurement, group = local_site_name, color = label, alpha = label, linewidth = label)) +
    geom_rect(data = shading, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, group = date_local), fill = "gray", alpha = 0.4, show.legend = F, inherit.aes = F) +
    geom_line(data = df %>%
                filter(parameter == "PM2.5 - Local Conditions",
                       label == "Non Exceeding Site"),
              show.legend = F) +
    geom_line(data = df %>%
                filter(parameter == "PM2.5 - Local Conditions",
                       label != "Non Exceeding Site"),
              show.legend = F) +
    scale_x_datetime(breaks = "24 hours", name='Date/Time', labels=label_date('%m/%d'), expand = c(0,0)) +
    scale_y_continuous(limits = c(0, max_pm25hr), breaks = seq(0, max_pm25hr, step_pm25)) +
    ylab(pm25_1hr_label) +
    scale_color_manual(values = cb_colors, breaks = site_order) + 
    scale_alpha_manual(values = alpha_scale, breaks = site_order) +
    scale_linewidth_manual(values = lw_scale, breaks = site_order) +
    theme_bw() 
  
  pm25hr
  
  # combine
  
  combined_plot <- plot_grid(co, 
                             nox, 
                             pm10hr, 
                             pm25hr,
                             ncol = 1, align = "v", rel_heights = c(1,1,1,1.1))
  
  combined_plot <- plot_grid(combined_plot, legend, ncol = 2, align = "h", rel_widths = c(1, 0.3))
  
  combined_plot
  
  ggsave(filename = out_name, plot = combined_plot, device = "png", path = out_dir,
         width = 11, height = 9, dpi = 800, units = "in")
  
}
