# ============================================================
# RQ1 — Springer-style figures (serif font, clean theme)
# April–July (not Apr–Sep)
# Reads: Savar_AreaTable_km2_*.csv under Results (recursive)
# Writes: tables + figures to Results/RQ1_outputs_springer/
# ============================================================

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(ggplot2)

# ----------------------------
# 0) EDIT ONLY THIS LINE
# ----------------------------
BASE_PATH <- "/Users/ibrahimkhalil/WorkSpace/Projects & Work/UHI-Study-Savar-Bangladesh/Results"

# ----------------------------
# 1) Output folders
# ----------------------------
OUT_DIR   <- file.path(BASE_PATH, "RQ1_outputs_springer")
TABLE_DIR <- file.path(OUT_DIR, "tables")
FIG_DIR   <- file.path(OUT_DIR, "figures")
dir.create(TABLE_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIG_DIR,   showWarnings = FALSE, recursive = TRUE)

# ----------------------------
# 2) Find all AreaTable CSVs
# ----------------------------
files <- list.files(
  path = BASE_PATH,
  pattern = "^Savar_AreaTable_km2_.*\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)
if (length(files) == 0) stop("No Savar_AreaTable_km2_*.csv found inside Results folder.")

# ----------------------------
# 3) Read + standardize
# ----------------------------
read_one <- function(fp) {
  df <- read_csv(fp, show_col_types = FALSE)
  year_block <- str_extract(basename(fp), "\\d{4}_\\d{4}")
  if (is.na(year_block)) stop("Could not extract year block from: ", basename(fp))
  
  keep_cols <- intersect(names(df), c("Class", "Area_km2", "Percent_of_total"))
  df %>%
    select(all_of(keep_cols)) %>%
    mutate(year_group = year_block)
}

area_all <- bind_rows(lapply(files, read_one))

# Enforce correct block order (edit if you have different)
block_levels <- c("2000_2001","2005_2006","2010_2011","2015_2016","2020_2021","2024_2025")

area_ts <- area_all %>%
  filter(Class %in% c("Urban","Vegetation","Water","Other","Valid_data","NoData","Total")) %>%
  mutate(
    year_group = factor(year_group, levels = block_levels),
    year_label = str_replace(as.character(year_group), "_", "–")
  ) %>%
  arrange(year_group, Class)

# Save long master table
write_csv(area_ts, file.path(TABLE_DIR, "RQ1_area_timeseries_long.csv"))

# Wide km² table (Urban/Veg/Water)
area_wide_km2 <- area_ts %>%
  filter(Class %in% c("Urban","Vegetation","Water")) %>%
  select(year_label, Class, Area_km2) %>%
  pivot_wider(names_from = Class, values_from = Area_km2)

write_csv(area_wide_km2, file.path(TABLE_DIR, "Table_RQ1_LandCover_Area_km2.csv"))

# Wide % table (Urban/Veg/Water)
area_wide_pct <- area_ts %>%
  filter(Class %in% c("Urban","Vegetation","Water")) %>%
  select(year_label, Class, Percent_of_total) %>%
  pivot_wider(names_from = Class, values_from = Percent_of_total)

write_csv(area_wide_pct, file.path(TABLE_DIR, "Table_RQ1_LandCover_Percent.csv"))

# ============================================================
# 4) Springer-like plot theme (serif + clean axes)
# ============================================================

# Use a serif family (Times-like). If "Times New Roman" exists, great; otherwise it falls back to generic serif.
BASE_FAMILY <- "serif"

springer_theme <- function(base_size = 11, base_family = BASE_FAMILY) {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2, hjust = 0.5),
      plot.subtitle = element_text(size = base_size, hjust = 0.5),
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size),
      axis.line = element_line(linewidth = 0.6, colour = "black"),
      axis.ticks = element_line(linewidth = 0.6, colour = "black"),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# ============================================================
# 5) FIGURE F1 — Time-series (km²) Springer style
# ============================================================

p1 <- area_ts %>%
  filter(Class %in% c("Urban","Vegetation","Water")) %>%
  mutate(year_label = factor(year_label, levels = str_replace(block_levels, "_", "–"))) %>%
  ggplot(aes(x = year_label, y = Area_km2, group = Class, linetype = Class)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2, shape = 21, fill = "white", stroke = 0.8) +
  labs(
    title = "Land-cover area change (km²) in Savar (2000–2025)",
    subtitle = "April–July | Consistent index-based classification",
    x = "Year block",
    y = "Area (km²)"
  ) +
  springer_theme(base_size = 11) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

ggsave(
  filename = file.path(FIG_DIR, "F1_Springer_LandCover_Area_TimeSeries_km2.png"),
  plot = p1, width = 170/25.4, height = 90/25.4, dpi = 600, bg = "white"
)

# ============================================================
# 6) FIGURE F2 — Composition (% stacked bars) Springer style
# (Stacked AREA is harder to read in journals; stacked BARS are often cleaner.)
# ============================================================

p2 <- area_ts %>%
  filter(Class %in% c("Urban","Vegetation","Water")) %>%
  mutate(year_label = factor(year_label, levels = str_replace(block_levels, "_", "–"))) %>%
  ggplot(aes(x = year_label, y = Percent_of_total, fill = Class)) +
  geom_col(width = 0.75, colour = "black", linewidth = 0.2) +
  labs(
    title = "Land-cover composition (% of total area)",
    subtitle = "April–July | Urban, vegetation, and water shares across time blocks",
    x = "Year block",
    y = "Share of total area (%)"
  ) +
  springer_theme(base_size = 11) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

ggsave(
  filename = file.path(FIG_DIR, "F2_Springer_LandCover_Composition_percent.png"),
  plot = p2, width = 170/25.4, height = 90/25.4, dpi = 600, bg = "white"
)

# ============================================================
# 7) OPTIONAL: Valid vs NoData transparency figure (Springer style)
# ============================================================

p3 <- area_ts %>%
  filter(Class %in% c("Valid_data","NoData")) %>%
  mutate(
    year_label = factor(year_label, levels = str_replace(block_levels, "_", "–")),
    Class = factor(Class, levels = c("Valid_data","NoData"))
  ) %>%
  ggplot(aes(x = year_label, y = Area_km2, fill = Class)) +
  geom_col(width = 0.75, colour = "black", linewidth = 0.2) +
  labs(
    title = "Data coverage per block (Valid vs NoData)",
    subtitle = "April–July | Comparability across blocks",
    x = "Year block",
    y = "Area (km²)"
  ) +
  springer_theme(base_size = 11) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

ggsave(
  filename = file.path(FIG_DIR, "F3_Springer_Valid_vs_NoData_km2.png"),
  plot = p3, width = 170/25.4, height = 90/25.4, dpi = 600, bg = "white"
)

cat("\nDONE ✅ Springer-style outputs saved to:\n", OUT_DIR, "\n")
cat("Tables ->", TABLE_DIR, "\n")
cat("Figures ->", FIG_DIR, "\n")

