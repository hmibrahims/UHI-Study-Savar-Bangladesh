library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(stringr)

# ---------------------------
# 0) Read files
# ---------------------------
era5_scene <- read_csv("ERA5Land_t2m_scene_matched_ALLBLOCKS.csv", show_col_types = FALSE)
analysis_block <- read_csv("SUHI_ERA5_block_analysis.csv", show_col_types = FALSE)

# ---------------------------
# 1) Standardise year_group in era5_scene to match analysis_block
#    era5_scene has "2000-01"; analysis_block has "2000_2001"
# ---------------------------
fix_year_group <- function(x) {
  x <- str_trim(as.character(x))
  ifelse(
    str_detect(x, "^\\d{4}_\\d{4}$"),
    x,
    ifelse(
      str_detect(x, "^\\d{4}-\\d{2}$"),
      {
        start <- as.integer(str_sub(x, 1, 4))
        suf   <- as.integer(str_sub(x, 6, 7))
        end   <- as.integer(paste0(str_sub(x, 1, 2), sprintf("%02d", suf)))
        paste0(start, "_", end)
      },
      x
    )
  )
}

era5_scene <- era5_scene %>%
  mutate(year_group = fix_year_group(year_group))

# ---------------------------
# 2) ERA5 parity table (BLOCK LEVEL)  ✅ main output
# ---------------------------
parity_table <- analysis_block %>%
  select(year_group, n_scenes, ERA5_mean, ERA5_sd, ERA5_p25, ERA5_p50, ERA5_p75) %>%
  arrange(year_group)

print(parity_table)
write_csv(parity_table, "STEP3_ERA5_parity_table.csv")

# Dot plot of ERA5 mean by block (simple, clean)
p_dot <- ggplot(parity_table, aes(x = year_group, y = ERA5_mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ERA5_mean - ERA5_sd, ymax = ERA5_mean + ERA5_sd), width = 0.15) +
  labs(x = "Year block", y = "ERA5-Land t2m at overpass (°C)",
       title = "ERA5 overpass-time air temperature by block (mean ± SD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("STEP3_ERA5_mean_by_block.png", p_dot, width = 9, height = 5, dpi = 300)

# ---------------------------
# 3) Scene-level ERA5 distribution by block (BOXPLOT) ✅ main output
# ---------------------------
p_box <- ggplot(era5_scene, aes(x = year_group, y = era5_t2m_c)) +
  geom_boxplot(outlier.alpha = 0.4) +
  labs(x = "Year block", y = "ERA5-Land t2m at overpass (°C)",
       title = "Scene-level ERA5 overpass-time air temperature by block") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("STEP3_ERA5_boxplot_scene_level.png", p_box, width = 9, height = 5, dpi = 300)

# ---------------------------
# 4) Temporal sampling balance (MONTH + HOUR) ✅ bias screening
#    Uses UTC date/time fields from your ERA5 scene table
# ---------------------------

# Build a UTC datetime from your columns
# If your file already has date_utc + hour_utc + minute_utc, this will work.
era5_scene <- era5_scene %>%
  mutate(
    date_utc = as.Date(date_utc),
    dt_utc = as.POSIXct(date_utc) + hours(hour_utc) + minutes(minute_utc),
    month = month(dt_utc),
    hour = hour(dt_utc)
  )

# Month counts by block (Apr–Sep should dominate)
month_counts <- era5_scene %>%
  count(year_group, month) %>%
  arrange(year_group, month)

write_csv(month_counts, "STEP3_month_counts_by_block.csv")
print(month_counts)

# Hour counts by block (should be similar across blocks)
hour_counts <- era5_scene %>%
  count(year_group, hour) %>%
  arrange(year_group, hour)

write_csv(hour_counts, "STEP3_hour_counts_by_block.csv")
print(hour_counts)

# Optional: plot month distribution (counts)
p_month <- ggplot(month_counts, aes(x = factor(month), y = n, fill = year_group)) +
  geom_col(position = "dodge") +
  labs(x = "Month (UTC)", y = "Number of scenes",
       title = "Retained-scene month distribution by block (bias check)") +
  theme_minimal()

ggsave("STEP3_month_distribution_by_block.png", p_month, width = 10, height = 5, dpi = 300)

# Optional: plot hour distribution (counts)
p_hour <- ggplot(hour_counts, aes(x = factor(hour), y = n, fill = year_group)) +
  geom_col(position = "dodge") +
  labs(x = "Hour (UTC)", y = "Number of scenes",
       title = "Retained-scene hour distribution by block (bias check)") +
  theme_minimal()

ggsave("STEP3_hour_distribution_by_block.png", p_hour, width = 10, height = 5, dpi = 300)

# ---------------------------
# 5) Quick “red flag” checks (prints)
# ---------------------------
cat("\n=== Quick checks ===\n")
cat("ERA5 mean range across blocks (°C): ",
    round(min(parity_table$ERA5_mean, na.rm=TRUE), 2), " to ",
    round(max(parity_table$ERA5_mean, na.rm=TRUE), 2), "\n")

cat("Scenes per block:\n")
print(parity_table %>% select(year_group, n_scenes))

