# Libraries

library(tidyverse)
library(janitor)
library(plyr)
library(scales)

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


# Filter and pivot data
data_pivot <- data %>%
  filter(parameter %in% c("PM2.5 - Local Conditions", "PM10 Total 0-10um STP"),
         sample_duration == "1 HOUR") %>%
  mutate(date_local = as.Date(date_local),
         aqs_sitecode = str_pad(as.character(aqs_sitecode), width = 9, side = "left", pad = "0"),
         parameter = case_when(parameter == "PM2.5 - Local Conditions" ~ "pm25",
                               parameter == "PM10 Total 0-10um STP" ~ "pm10")) %>%
  select(aqs_sitecode, parameter, date_local, time_local, sample_measurement) %>%
  group_by(aqs_sitecode) %>%
  pivot_wider(names_from = parameter, values_from = sample_measurement, values_fn = mean) %>%
  ungroup() %>%
  mutate(pm25_pm10_ratio = pm25/pm10,
         datetime = paste(date_local, time_local),
         year = as.numeric(format(date_local, "%Y")))

data_pivot$pm25_pm10_ratio <- ifelse(data_pivot$pm25_pm10_ratio == Inf, NA_real_, data_pivot$pm25_pm10_ratio)
data_pivot$pm25_pm10_ratio <- ifelse(data_pivot$pm25_pm10_ratio == -Inf, NA_real_, data_pivot$pm25_pm10_ratio)
data_pivot$datetime <- as.POSIXct(data_pivot$datetime, format = "%Y-%m-%d %H:%M", tz = "UTC")
data_pivot$hour = as.numeric(format(data_pivot$datetime, "%H"))
data_pivot$month = as.numeric(format(data_pivot$datetime, "%m"))

data_pivot <- left_join(data_pivot, metadata)

# Loop for each unique ID
for (i in unique(event_dates$id)) {
  # time information
  dates <- event_dates[event_dates$id == i,]
  year <- unique(as.numeric(format(dates$date, "%Y")))
  month <- unique(as.numeric(format(dates$date, "%m")))
  months <- unique(c(month - 1, month, month + 1))
  month_name <- month.abb[c(months[1], months[length(months)])]
  month_label <- paste(month_name[1], month_name[2], sep = "-")
  
  # filter data
  df <- data_pivot %>% filter(month %in% months)
  
  # calculate statistics
  ratio_stats <- df %>%
    group_by(local_site_name, hour) %>%
    reframe(pm25_avg = mean(pm25, na.rm = T),
            pm10_avg = mean(pm10, na.rm = T),
            ratio_avg = mean(pm25_pm10_ratio, na.rm = T),
            ratio_avg1 = pm25_avg/pm10_avg,
            ratio_fifth_pct = quantile(pm25_pm10_ratio, 0.05, na.rm =T ),
            ratio_nf_pct = quantile(pm25_pm10_ratio, 0.95, na.rm = T),
            parameter = paste0(month_label, " Avg. Ratio")) %>%
    ungroup()
  
  df <- left_join(df, ratio_stats) %>%
    filter(!is.na(ratio_avg))
  
  # file name 
  if (length(dates$date) > 1) {
    x_limits <- seq.Date(from = dates$date[1] - 1, to = dates$date[length(dates$date)] + 1, by = "day")
    out_name <- paste0("pm25_pm10_ratio_", 
                       gsub(pattern = "-", replacement = "_", as.character(dates$date[1])), 
                       "_to_", 
                       as.character(format(dates$date[length(dates$date)], "%d")), 
                       ".png")
  } else {
    x_limits <- seq.Date(from = dates$date[1] - 1, to = dates$date[length(dates$date)] + 1, by = "day")
    out_name <- paste0("pm25_pm10_ratio_", 
                       gsub(pattern = "-", replacement = "_", as.character(dates$date[1])), 
                       ".png")
  }
  
  # set axis limits
  df_total <- data.frame()
  for (s in unique(df$local_site_name)) {
    df_temp <- df %>%
      filter(local_site_name == s, date_local %in% x_limits) %>%
      dplyr::mutate(n_hours = sum(!is.na(pm25_pm10_ratio)),
                    max_hours = n(),
                    pct_comp = n_hours/max_hours)
    df_total <- rbind(df_temp, df_total)
    rm(df_temp)
  }
  df <- df_total %>% 
    filter(pct_comp >= 0.75)
  
  max_ratio <- round_any(max(df$pm25_pm10_ratio, na.rm = T), 0.1, f = ceiling)
  max_95th <- round_any(max(df$ratio_nf_pct, na.rm = T), 0.1, f = ceiling)
  x_limits <- c(min(df$datetime), max(df$datetime))
  
  # y-axis breaks
  y_limits <- ifelse(max_ratio > max_95th, max_ratio, max_95th) 
  if (y_limits > 3) {
    breaks <- 0.6
  } else if (y_limits <= 3 & y_limits > 2) {
    breaks <- 0.4
  } else {
    breaks <- 0.2
  }
  
  # set shading variables
  cval = "#e67e00"
  linetype_values <- c("Seasonal Hourly Avg." = "dashed",
                       "Event Period" = "solid")
  color_values <- c("Seasonal Avg." = cval)
  fill_values <- c("Seasonal 5th-95th %ile" = cval)
  
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
  
  # plot
  ratio <- ggplot() +
    geom_rect(data = shading, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, group = date_local), fill = "gray", alpha = 0.4, show.legend = F) +
    geom_ribbon(data = df, aes(x = datetime, ymin = ratio_fifth_pct, ymax = ratio_nf_pct, fill = "Seasonal 5th-95th %ile"), alpha = 0.3) +
    geom_line(data = df, aes(x = datetime, y = pm25_pm10_ratio, linetype = "Event Period"), color = "#e67e00") +
    geom_line(data = df, aes(x = datetime, y = ratio_avg, linetype = "Seasonal Hourly Avg."), color = cval) +
    ylab(expression(PM[2.5]/PM[10])) +
    scale_x_datetime(limits = x_limits, breaks = "12 hours", name = "Date/Time", labels = label_date("%m/%d %H:%M", tz = "UTC"), expand = c(0.01, 0.01)) +
    scale_y_continuous(limits = c(-0.05, y_limits), breaks = seq(0, y_limits, by = breaks), sec.axis = dup_axis()) +
    scale_color_manual(values = color_values) +
    scale_fill_manual(values = fill_values) +
    scale_linetype_manual(values = linetype_values) +
    labs(caption = paste0("Data: ", month_label)) +
    facet_wrap(~local_site_name, ncol = 2, scales = "fixed") +
    theme_bw() +
    theme(text = element_text(size = 14),
          plot.title = element_text(hjust = 0.5),
          legend.title = element_blank(),
          legend.direction= "horizontal",
          strip.background = element_blank(),
          legend.position = "top",
          axis.text.x = element_text(angle = 90, vjust = 0.25))
   ratio
  ggsave(filename = out_name, plot = ratio, device = "png", path = out_dir,
         width = 8 + length(dates$date), height = 2 + length(unique(df$aqs_sitecode)), dpi = 800, units = "in")
}
