library(dplyr)
library(readr)
library(ggplot2)

# ---------------------------
# 4A) Read merged table
# ---------------------------
df <- read_csv(
  "SUHI_ERA5_block_analysis.csv",
  locale = locale(encoding = "Latin1"),
  show_col_types = FALSE
)

# ---------------------------
# 4B) Rename columns (FIXED names)
# ---------------------------
df2 <- df %>%
  rename(
    rural_mean_c = `Rural_Mean Â¡C`,
    urban_mean_c = `Urban_Mean Â¡C`,
    suhi_mean_c  = `SUHI_Mean Â¡C`,
    suhi_sd      = SUHI_SD,
    urban_km2    = Urban_Area_km_,
    veg_km2      = Vagetated_Area_km_,
    water_km2    = Waterbody_km_,
    other_km2    = Others_km_,
    total_km2    = Total_Area_km_
  ) %>%
  mutate(year_group = as.character(year_group))

print(df2 %>% select(year_group, suhi_mean_c, ERA5_mean, urban_km2, n_scenes))

# Save cleaned version (optional)
write_csv(df2, "STEP4_clean_block_table.csv")

# =========================================================
# STEP 4C — FIGURE 1 (MANDATORY): SUHI vs ERA5
# =========================================================
p1 <- ggplot(df2, aes(x = ERA5_mean, y = suhi_mean_c, label = year_group)) +
  geom_point(size = 3) +
  geom_text(nudge_y = 0.12) +
  labs(
    x = "ERA5-Land mean t2m at overpass (°C)",
    y = "Mean SUHI (p75, °C)",
    title = "Step 4: SUHI versus background air temperature (ERA5 control)"
  ) +
  theme_minimal()

ggsave("STEP4_SUHI_vs_ERA5_scatter.png", p1, width = 8, height = 5, dpi = 300)

# =========================================================
# STEP 4D — MODEL 1 (CORE): ERA5-controlled SUHI
# =========================================================
m1 <- lm(suhi_mean_c ~ ERA5_mean, data = df2)
print(summary(m1))

df2 <- df2 %>%
  mutate(
    suhi_fitted_era5 = fitted(m1),
    suhi_resid_era5  = resid(m1)
  )

write_csv(df2, "STEP4_SUHI_ERA5_with_residuals.csv")

print(df2 %>% select(year_group, suhi_mean_c, ERA5_mean, suhi_fitted_era5, suhi_resid_era5))

# =========================================================
# STEP 4E — FIGURE 2 (STRONG): Residuals by block
#          (Shows whether 2015/16 remains anomalous after ERA5 control)
# =========================================================
p2 <- ggplot(df2, aes(x = year_group, y = suhi_resid_era5)) +
  geom_col() +
  labs(
    x = "Year block",
    y = "SUHI residual after ERA5 control (°C)",
    title = "Step 4: ERA5-controlled SUHI residuals by block"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("STEP4_SUHI_residuals_by_block.png", p2, width = 8, height = 5, dpi = 300)

# ---------------------------
# 4F) Optional strong test: SUHI ~ ERA5 + Urban area
# ---------------------------
m2 <- lm(suhi_mean_c ~ ERA5_mean + urban_km2, data = df2)
print(summary(m2))

df2 <- df2 %>%
  mutate(suhi_resid_era5_urban = resid(m2))

p3 <- ggplot(df2, aes(x = year_group, y = suhi_resid_era5_urban)) +
  geom_col() +
  labs(
    x = "Year block",
    y = "Residual SUHI after ERA5 + urban area control (°C)",
    title = "Step 4 (optional): Residual SUHI after controlling for ERA5 and urban area"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("STEP4_residuals_ERA5_plus_urban.png", p3, width = 8, height = 5, dpi = 300)

# ---------------------------
# 4G) Key ranking: which block remains most anomalous?
# ---------------------------
cat("\n=== Residual ranking (largest positive = most anomalous after ERA5 control) ===\n")
print(df2 %>% arrange(desc(suhi_resid_era5)) %>% select(year_group, suhi_mean_c, ERA5_mean, suhi_resid_era5))

##
df2 %>% arrange(desc(suhi_resid_era5)) %>% select(year_group, suhi_resid_era5)

