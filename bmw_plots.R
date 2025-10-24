# bmw_plots.R
# Create 10 tidyverse plots from Kaggle's BMW dataset (bmw.csv)
# Usage:
#   install.packages(c("tidyverse", "scales"))
#   source("bmw_plots.R")   # saves PNGs into ./plots and a combined PDF ./bmw_plots.pdf

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})

# -------- Params --------
csv_path <- "bmw.csv"  # change if needed
out_dir  <- "plots"
pdf_out  <- "bmw_plots.pdf"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -------- Load & clean --------
bmw <- read_csv(csv_path, show_col_types = FALSE) |>
  mutate(
    model = stringr::str_trim(model),
    transmission = as.factor(transmission),
    fuelType = as.factor(fuelType),
    year = as.integer(year),
    tax = as.numeric(tax),
    mileage = as.numeric(mileage),
    price = as.numeric(price),
    mpg = as.numeric(mpg),
    engineSize = as.numeric(engineSize)
  )

# Helper: top N models by frequency
top_models <- bmw |>
  count(model, sort = TRUE) |>
  slice_head(n = 10) |>
  pull(model)

bmw_top <- bmw |>
  mutate(model_top = if_else(model %in% top_models, model, "Other")) |>
  mutate(model_top = fct_reorder(model_top, price, median, .desc = TRUE))

# A minimal theme
theme_bmw <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 6)),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.title.x = element_text(margin = margin(t = 8)),
    legend.position = "right"
  )

fmt_pounds <- label_number_si(prefix = "£", accuracy = 1)

# -------- Plot 1: Price distribution --------
p1 <- bmw |>
  ggplot(aes(price)) +
  geom_histogram(bins = 50) +
  labs(title = "BMW Price Distribution",
       x = "Price (GBP)", y = "Count") +
  scale_x_continuous(labels = fmt_pounds) +
  theme_bmw

# -------- Plot 2: Price by top model (boxplot) --------
p2 <- bmw_top |>
  filter(model_top != "Other") |>
  ggplot(aes(model_top, price)) +
  geom_boxplot(outlier.alpha = 0.3) +
  coord_flip() +
  labs(title = "Price by Model (Top 10)",
       x = "Model", y = "Price (GBP)") +
  scale_y_continuous(labels = fmt_pounds) +
  theme_bmw

# -------- Plot 3: Price vs. Mileage (log mileage) --------
p3 <- bmw |>
  mutate(mileage_k = mileage / 1000) |>
  ggplot(aes(mileage_k, price)) +
  geom_point(alpha = 0.25) +
  geom_smooth(se = FALSE, method = "loess") +
  labs(title = "Price vs. Mileage",
       subtitle = "With LOESS smooth",
       x = "Mileage (thousands of miles)", y = "Price (GBP)") +
  scale_y_continuous(labels = fmt_pounds) +
  theme_bmw

# -------- Plot 4: Price vs. Year --------
p4 <- bmw |>
  ggplot(aes(year, price)) +
  geom_jitter(width = 0.15, alpha = 0.15) +
  geom_smooth(se = FALSE, method = "loess") +
  labs(title = "Price vs. Year of Registration",
       x = "Year", y = "Price (GBP)") +
  scale_y_continuous(labels = fmt_pounds) +
  theme_bmw

# -------- Plot 5: Price vs. Engine Size (color fuel) --------
p5 <- bmw |>
  ggplot(aes(engineSize, price, color = fuelType)) +
  geom_point(alpha = 0.35) +
  geom_smooth(se = FALSE, method = "loess") +
  labs(title = "Price vs. Engine Size by Fuel Type",
       x = "Engine Size (L)", y = "Price (GBP)", color = "Fuel") +
  scale_y_continuous(labels = fmt_pounds) +
  theme_bmw

# -------- Plot 6: MPG by Fuel Type (violin + box) --------
p6 <- bmw |>
  filter(!is.na(mpg)) |>
  ggplot(aes(fuelType, mpg)) +
  geom_violin(trim = TRUE, alpha = 0.6) +
  geom_boxplot(width = 0.15, outlier.alpha = 0.25) +
  labs(title = "MPG by Fuel Type",
       x = "Fuel Type", y = "MPG") +
  theme_bmw

# -------- Plot 7: Transmission share by Fuel (stacked bars) --------
p7 <- bmw |>
  count(fuelType, transmission, name = "n") |>
  group_by(fuelType) |>
  mutate(pct = n / sum(n)) |>
  ggplot(aes(fuelType, pct, fill = transmission)) +
  geom_col(position = "stack") +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "Transmission Share within Each Fuel Type",
       x = "Fuel Type", y = "Share of Listings", fill = "Transmission") +
  theme_bmw

# -------- Plot 8: Road Tax vs. Engine Size --------
p8 <- bmw |>
  ggplot(aes(engineSize, tax)) +
  geom_jitter(width = 0.02, height = 0.02, alpha = 0.25) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, bs = "cs")) +
  labs(title = "Annual Road Tax vs. Engine Size",
       x = "Engine Size (L)", y = "Tax (GBP)") +
  theme_bmw

# -------- Plot 9: Mileage by Year (boxplots) --------
p9 <- bmw |>
  ggplot(aes(factor(year), mileage)) +
  geom_boxplot(outlier.alpha = 0.25) +
  labs(title = "Mileage by Registration Year",
       x = "Year", y = "Mileage (miles)") +
  theme_bmw

# -------- Plot 10: Price by Transmission across Years (facets) --------
p10 <- bmw |>
  mutate(year = factor(year)) |>
  ggplot(aes(year, price)) +
  geom_boxplot(outlier.alpha = 0.15) +
  facet_wrap(~ transmission, ncol = 2, scales = "free_y") +
  labs(title = "Price by Year, Faceted by Transmission",
       x = "Year", y = "Price (GBP)") +
  scale_y_continuous(labels = fmt_pounds) +
  theme_bmw

# -------- Save PNGs --------
plots <- list(
  "01_price_hist.png" = p1,
  "02_price_by_model_top10.png" = p2,
  "03_price_vs_mileage.png" = p3,
  "04_price_vs_year.png" = p4,
  "05_price_vs_engine_fuel.png" = p5,
  "06_mpg_by_fuel.png" = p6,
  "07_transmission_share_by_fuel.png" = p7,
  "08_tax_vs_engine.png" = p8,
  "09_mileage_by_year.png" = p9,
  "10_price_by_year_transmission_facet.png" = p10
)

iw <- 9; ih <- 6; idpi <- 150
walk2(names(plots), plots, ~ ggsave(file.path(out_dir, .x), .y, width = iw, height = ih, dpi = idpi))

# -------- Combined PDF --------
pdf(pdf_out, width = 10, height = 6.5)
walk(plots, print)
dev.off()

message("Saved PNGs to: ", normalizePath(out_dir))
message("Saved combined PDF to: ", normalizePath(pdf_out))
