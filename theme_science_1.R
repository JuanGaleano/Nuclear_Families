# ============================================================
# theme_science(): Unified ggplot2 theme matching the existing
# style of FIG_1 (map), FIG_2 (regional bar chart), and
# FIG_3 (faceted bubble/slope chart) — educational gradients
# paper (Cherlin / Galeano / Esteve / Pesando)
# ============================================================
# This is NOT a generic minimalist journal theme. It codifies
# the look you already have across the 3 main-text figures so
# the 3 annex figures match exactly:
#   - sans-serif, generously sized text
#   - light gray major gridlines (kept, not stripped)
#   - no panel border / no top-right axis lines
#   - gray facet strips with thin black border, bold text
#   - bottom-anchored legends, bold title, no border
#   - a shared diverging palette (pos/neg gradient) and a
#     shared qualitative palette (6 regions)
# ============================================================

library(ggplot2)

# ------------------------------------------------------------
# Core theme
# ------------------------------------------------------------
# grid: "both" (Fig 3 style), "x" (Fig 2 style), "y", or "none"
# ------------------------------------------------------------
theme_science <- function(base_size = 12,
                           base_family = "Helvetica",
                           legend_position = "bottom",
                           grid = "both") {

  half_line <- base_size / 1

  grid_x <- if (grid %in% c("both", "x")) {
    element_line(color = "grey85", linewidth = 0.3)
  } else element_blank()

  grid_y <- if (grid %in% c("both", "y")) {
    element_line(color = "grey85", linewidth = 0.3)
  } else element_blank()

  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      # ---- Text ----
      text = element_text(color = "black"),
      plot.title = element_text(
        size = rel(1.05), face = "bold", hjust = 0,
        margin = margin(b = half_line)
      ),
      plot.subtitle = element_text(
        size = rel(0.9), color = "grey30", hjust = 0,
        margin = margin(b = half_line)
      ),
      plot.caption = element_text(
        size = rel(0.7), color = "grey40", hjust = 0,
        margin = margin(t = half_line)
      ),

      axis.title = element_text(size = rel(0.95), color = "black"),
      axis.title.x = element_text(margin = margin(t = half_line * 0.8)),
      axis.title.y = element_text(margin = margin(r = half_line * 0.8), angle = 90),
      axis.text = element_text(size = rel(0.85), color = "black"),

      # ---- Axis lines: bottom + left only, no box ----
      axis.line.x = element_line(color = "black", linewidth = 0.3),
      axis.line.y = element_line(color = "black", linewidth = 0.3),
      axis.ticks = element_line(color = "black", linewidth = 0.3),
      axis.ticks.length = unit(2, "pt"),

      # ---- Panel ----
      panel.background = element_blank(),
     # panel.border = element_blank(),
      panel.grid.major.x = grid_x,
      panel.grid.major.y = grid_y,
      panel.grid.minor = element_blank(),
      panel.spacing = unit(1.1, "lines"),

      # ---- Facet strips (matches Fig 3 exactly) ----
      strip.background = element_rect(fill = "grey88", color = "black", linewidth = 0.3),
      strip.text = element_text(
        size = rel(0.95), face = "bold", color = "black",
        margin = margin(t = 4, b = 4)
      ),

      # ---- Legend ----
      legend.position = legend_position,
      legend.title = element_text(size = rel(0.9), face = "bold"),
      legend.text = element_text(size = rel(0.85)),
      legend.key = element_blank(),
      legend.background = element_blank(),
      legend.margin = margin(t = -2),
      legend.box.spacing = unit(0.4, "lines"),

      # ---- Overall plot ----
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(t = 5, r = 10, b = 5, l = 5)
    )
}

# ------------------------------------------------------------
# Map variant (for FIG_1-style choropleths): no axes, no grid,
# but same fonts / legend style so the map matches the rest
# of the set.
# ------------------------------------------------------------
theme_science_map <- function(base_size = 12,
                               base_family = "Helvetica",
                               legend_position = "bottom") {
  theme_void(base_size = base_size, base_family = base_family) +
    theme(
      text = element_text(color = "black"),
      legend.position = legend_position,
      legend.title = element_text(size = rel(1.0), face = "bold", hjust = 0.5),
      legend.text = element_text(size = rel(0.9)),
      legend.key = element_blank(),
      legend.background = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
    )
}

# ------------------------------------------------------------
# Shared palettes
# ------------------------------------------------------------

# Diverging: positive / negative gradient (map + bar chart)
# Sampled to match FIG_1 / FIG_2 red-blue pairing.
science_diverging <- c(
  "Negative" = "#B2182B",
  "Positive" = "#1F5C99",
  "NA"       = "#C9C9C9"
)

scale_fill_science_diverging <- function(...) {
  ggplot2::scale_fill_manual(values = science_diverging, ...)
}

# Qualitative: 6 world regions, matched to FIG_3 hues.
# Named vector -> stable mapping regardless of factor order,
# and reusable across all 6 figures (main + annex).
science_regions <- c(
  "Europe and North America" = "#E24E42",  # red
  "Latin America"            = "#C9AA2C",  # olive/gold
  "MENA"                     = "#4C9F4C",  # green
  "South Asia"               = "#3EC1C9",  # cyan
  "Southeast Asia and China" = "#4F8FE0",  # blue
  "Sub-Saharan Africa"       = "#D6428F"   # magenta
)

scale_color_science_region <- function(...) {
  ggplot2::scale_color_manual(values = science_regions, ...)
}

scale_fill_science_region <- function(...) {
  ggplot2::scale_fill_manual(values = science_regions, ...)
}

# ------------------------------------------------------------
# Standard Science figure export dimensions
# ------------------------------------------------------------
# single-column: width = 89 mm
# 1.5-column:    width = 120 mm
# double-column: width = 183 mm  (use this for Fig 3, it's 3-wide faceted)
#
# ggsave("fig3.pdf", plot = p, width = 183, height = 110,
#        units = "mm", dpi = 600, device = cairo_pdf)
# ------------------------------------------------------------

# ------------------------------------------------------------
# Example: reproducing FIG_2's style (regional bar chart)
# ------------------------------------------------------------
if (FALSE) {
  df2 <- data.frame(
    region = factor(names(science_regions)[c(1,5,3,2,6,4)],
                     levels = rev(names(science_regions)[c(1,5,3,2,6,4)])),
    gradient = c(0.06, 0.055, 0.035, -0.005, -0.07, -0.16),
    sign = c("Positive","Positive","Positive","Negative","Negative","Negative")
  )

  ggplot(df2, aes(x = gradient, y = region, fill = sign)) +
    geom_col(width = 0.7) +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
    scale_fill_science_diverging(guide = "none") +
    labs(x = "Population-weighted mean gradient", y = NULL) +
    theme_science(grid = "x")
}

# ------------------------------------------------------------
# Example: reproducing FIG_3's style (faceted bubble/slope plot)
# ------------------------------------------------------------
if (FALSE) {
  set.seed(1)
  df3 <- data.frame(
    region = rep(names(science_regions), each = 10),
    year = rep(1980:2015, length.out = 60),
    gradient = rnorm(60, 0, 0.2),
    population = runif(60, 1, 100)
  )

  ggplot(df3, aes(year, gradient, color = region, size = population)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "navy", linewidth = 0.4) +
    geom_point(alpha = 0.4) +
    scale_color_science_region(guide = "none") +
    scale_size_area(max_size = 12, name = "Population") +
    facet_wrap(~region, nrow = 2) +
    labs(x = "Year", y = "Educational gradient") +
    theme_science(grid = "both")
}
