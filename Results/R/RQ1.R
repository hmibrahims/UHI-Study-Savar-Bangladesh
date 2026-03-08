# ============================================================
# RQ2 — LST dynamics by land-cover type (Springer-ready)
# Reads all Savar_Table1_SUHI_p75_*.csv (Apr–Jul)
# Produces:
#   - Master time-series table (Urban vs Rural vegetation LST p75)
#   - Figure 6: Urban vs Vegetation mean p75 LST (line chart)
#   - Figure 7: ΔLST = Urban − Vegetation (bar/point chart)
# ============================================================
install.packages("extrafont")
library(extrafont)
font_import(prompt = FALSE)
loadfonts(device = "pdf")

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(ggplot2)

# ----------------------------
# 1) SET BASE PATH
# ----------------------------
BASE_PATH <- "/Users/ibrahimkhalil/WorkSpace/Projects & Work/UHI-Study-Savar-Bangladesh/Results"

OUT_DIR <- file.path(BASE_PATH, "RQ2_outputs_springer")
FIG_DIR <- file.path(OUT_DIR, "figures")
TAB_DIR <- file.path(OUT_DIR, "tables")

dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)

# ----------------------------
# 2) FIND ALL BLOCK FILES
# ----------------------------
files <- list.files(
  path = BASE_PATH,
  pattern = "^Savar_Table1_SUHI_p75_\\d{4}_\\d{4}_AprJul\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)

if (length(files) == 0) stop("❌ No Savar_Table1_SUHI_p75 AprJul CSV files found")

# ----------------------------
# 3) READ & STANDARDISE EACH FILE
# ----------------------------
read_block <- function(fp) {
  df <- read_csv(fp, show_col_types = FALSE) %>% slice(1)
  
  year_block <- str_extract(basename(fp), "\\d{4}_\\d{4}")
  if (is.na(year_block)) stop("Year block not found in filename: ", fp)
  
  # Required columns check
  needed <- c("UrbanMean_LST_p75_C", "RuralVegMean_LST_p75_C")
  missing <- setdiff(needed, names(df))
  if (length(missing) > 0) {
    stop("Missing columns in: ", basename(fp), " -> ", paste(missing, collapse = ", "))
  }
  
  tibble(
    year_block = year_block,
    year_label = str_replace(year_block, "_", "–"),
    UrbanMean_LST_p75_C = as.numeric(df$UrbanMean_LST_p75_C),
    RuralVegMean_LST_p75_C = as.numeric(df$RuralVegMean_LST_p75_C)
  )
}

lst_table <- bind_rows(lapply(files, read_block))

# Enforce chronological order (same as RQ1)
block_levels <- c(
  "2000_2001", "2005_2006", "2010_2011",
  "2015_2016", "2020_2021", "2024_2025"
)

lst_table <- lst_table %>%
  mutate(
    year_block = factor(year_block, levels = block_levels),
    year_label = factor(year_label, levels = str_replace(block_levels, "_", "–"))
  ) %>%
  arrange(year_block) %>%
  mutate(
    Delta_LST_C = UrbanMean_LST_p75_C - RuralVegMean_LST_p75_C
  )

# ----------------------------
# 4) SAVE MASTER TABLE
# ----------------------------
write_csv(
  lst_table,
  file.path(TAB_DIR, "Table_RQ2_LST_TimeSeries_Urban_vs_Veg_p75_AprJul.csv")
)

# ----------------------------
# 5) PLOT STYLE (match your sample figure look)
# ----------------------------
theme_sample <- theme_classic(base_size = 16, base_family = "Times New Roman") +
  theme(
    plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    axis.line = element_line(linewidth = 1.2),
    axis.ticks = element_line(linewidth = 1.2)
  )

# ----------------------------
# 6) FIGURE 6 — Urban vs Vegetation mean p75 LST
# ----------------------------
lst_long <- lst_table %>%
  select(year_label, UrbanMean_LST_p75_C, RuralVegMean_LST_p75_C) %>%
  pivot_longer(
    cols = c(UrbanMean_LST_p75_C, RuralVegMean_LST_p75_C),
    names_to = "Class",
    values_to = "LST_p75_C"
  ) %>%
  mutate(
    Class = recode(Class,
                   UrbanMean_LST_p75_C = "Urban",
                   RuralVegMean_LST_p75_C = "Vegetation")
  )

p6 <- ggplot(lst_long, aes(x = year_label, y = LST_p75_C, group = Class, linetype = Class)) +
  geom_line(color = "black", linewidth = 1.2) +
  geom_point(shape = 21, fill = "white", color = "black", size = 3.5, stroke = 1.2) +
  scale_linetype_manual(values = c("Urban" = "solid", "Vegetation" = "dotted")) +
  labs(
    title = "Land-surface temperature dynamics in Savar (2000–2025)",
    subtitle = "April–July | p75 LST composite | Consistent index-based classification",
    x = "Year block",
    y = expression("Mean p75 LST ("*degree*C*")")
  ) +
  theme_sample

ggsave(
  filename = file.path(FIG_DIR, "F6_LST_p75_Urban_vs_Vegetation_AprJul.png"),
  plot = p6, width = 12, height = 6, dpi = 300
)

# ----------------------------
# 7) FIGURE 7 — ΔLST (Urban − Vegetation)
# ----------------------------
p7 <- ggplot(lst_table, aes(x = year_label, y = Delta_LST_C)) +
  geom_col(fill = "white", color = "black", linewidth = 1.1) +
  geom_point(shape = 21, fill = "white", color = "black", size = 3.5, stroke = 1.2) +
  labs(
    title = "Urban–vegetation LST contrast in Savar (2000–2025)",
    subtitle = "ΔLST = Urban mean p75 LST − Vegetation mean p75 LST (April–July)",
    x = "Year block",
    y = expression(Delta*"LST ("*degree*C*")")
  ) +
  theme_sample

ggsave(
  filename = file.path(FIG_DIR, "F7_DeltaLST_Urban_minus_Vegetation_p75_AprJul.png"),
  plot = p7, width = 12, height = 6, dpi = 300
)

cat("\n✅ DONE: RQ2 table + Fig 6 + Fig 7 created\n")
cat("📁 Tables:", TAB_DIR, "\n")
cat("📁 Figures:", FIG_DIR, "\n")

