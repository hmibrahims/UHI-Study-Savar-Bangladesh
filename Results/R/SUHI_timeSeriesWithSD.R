# ============================================================
# RQ3 — SUHI Mean + SD in ONE figure (Springer-style)
# Input:  SUHI_Summary.csv (metrics as rows, blocks as columns)
# Output: F11_SUHI_Mean_and_SD_OnePanel.png (600 dpi)
# Style: Times New Roman, B/W, open circles, solid vs dashed
# Axis:  Y fixed to 2–6 °C
# ============================================================

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(ggplot2)

# ----------------------------
# 1) PATHS
# ----------------------------
BASE_PATH <- "/Users/ibrahimkhalil/WorkSpace/Projects & Work/UHI-Study-Savar-Bangladesh/Results/RQ3_Outputs"

IN_FILE <- file.path(BASE_PATH, "SUHI_Summary.csv")
OUT_DIR <- file.path(BASE_PATH, "RQ3_outputs_springer", "figures")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(IN_FILE)) stop("❌ Input file not found: ", IN_FILE)

# ----------------------------
# 2) READ + RESHAPE (WIDE MATRIX → TIDY)
# ----------------------------
raw <- read_csv(IN_FILE, show_col_types = FALSE)

# Pivot longer (Column1 = metric names; other columns = blocks)
long <- raw %>%
  pivot_longer(
    cols = -Column1,
    names_to = "block_header",
    values_to = "value"
  ) %>%
  mutate(
    year_block = str_extract(block_header, "\\d{4}/\\d{4}"),
    year_label = str_replace(year_block, "/", "–")
  ) %>%
  filter(!is.na(year_block))

# Standardize metric keys
metric_key <- function(x){
  x %>%
    str_replace_all("°C", "") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim() %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("_+$", "")
}

long <- long %>% mutate(metric = metric_key(Column1))

# Pivot wider so each metric becomes a column
tidy <- long %>%
  select(year_block, year_label, metric, value) %>%
  pivot_wider(names_from = metric, values_from = value)

# Rename to expected columns from your SUHI_Summary structure
# Your summary includes: suhi_mean and suhi_sd
if (!all(c("suhi_mean", "suhi_sd") %in% names(tidy))) {
  stop("❌ Could not find required columns 'suhi_mean' and 'suhi_sd' after reshaping.\n",
       "Available columns: ", paste(names(tidy), collapse = ", "))
}

tidy <- tidy %>%
  rename(
    Mean_SUHI_p75_C   = suhi_mean,
    SUHI_p75_StdDev_C = suhi_sd
  )

# Enforce chronological order
block_levels <- c("2000/2001","2005/2006","2010/2011","2015/2016","2020/2021","2024/2025")
label_levels <- str_replace(block_levels, "/", "–")

tidy <- tidy %>%
  mutate(
    year_block = factor(year_block, levels = block_levels),
    year_label = factor(year_label, levels = label_levels)
  ) %>%
  arrange(year_block)

# Long format for two-line plot
plot_df <- tidy %>%
  select(year_label, Mean_SUHI_p75_C, SUHI_p75_StdDev_C) %>%
  pivot_longer(cols = c(Mean_SUHI_p75_C, SUHI_p75_StdDev_C),
               names_to = "Metric", values_to = "Value") %>%
  mutate(
    Metric = recode(Metric,
                    Mean_SUHI_p75_C = "Mean SUHI (p75)",
                    SUHI_p75_StdDev_C = "SUHI SD (p75)")
  )

# ----------------------------
# 3) SPRINGER THEME (Times New Roman)
# ----------------------------
BASE_FAMILY <- "Times New Roman"  # if it fails, change to "Times"

theme_springer <- theme_classic(base_size = 16, base_family = BASE_FAMILY) +
  theme(
    plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.text  = element_text(size = 16),
    axis.line  = element_line(linewidth = 1.2),
    axis.ticks = element_line(linewidth = 1.2)
  )

# Line types: Mean solid, SD dashed
lt_values <- c("Mean SUHI (p75)" = "solid",
               "SUHI SD (p75)"   = "dashed")

# ----------------------------
# 4) FIGURE: Mean + SD (one panel)
# ----------------------------
p11 <- ggplot(plot_df, aes(x = year_label, y = Value, group = Metric, linetype = Metric)) +
  geom_line(color = "black", linewidth = 1.2) +
  geom_point(shape = 21, fill = "white", color = "black", size = 3.5, stroke = 1.2) +
  scale_linetype_manual(values = lt_values) +
  coord_cartesian(ylim = c(2, 6)) +
  labs(
    title = "SUHI intensity and spatial heterogeneity in Savar (2000–2025)",
    subtitle = "April–July | SUHI derived from p75 LST composites",
    x = "Year block",
    y = expression("SUHI ("*degree*C*")")
  ) +
  theme_springer

# Save PNG only (600 dpi)
ggsave(
  filename = file.path(OUT_DIR, "F11_SUHI_Mean_and_SD_OnePanel_600dpi.png"),
  plot = p11,
  width = 12, height = 6, dpi = 600
)

cat("\n✅ DONE: Figure saved as PNG (600 dpi)\n")
cat("🖼️ ", file.path(OUT_DIR, "F11_SUHI_Mean_and_SD_OnePanel_600dpi.png"), "\n")
print(tidy)
print(p11)
