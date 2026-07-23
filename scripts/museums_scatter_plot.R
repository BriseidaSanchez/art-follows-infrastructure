
# ══════════════════════════════════════════════════════════════════
#  BIENNALES POSTER — VIZ 2: SCATTER PLOT
#  Log museums (x) vs biennales (y) · color per regin

#  files:
#    - biennales.csv
#    - number_museums.csv
#
#  install.packages(c("tidyverse","showtext","ggrepel","glue"))
# ══════════════════════════════════════════════════════════════════

library(tidyverse)
library(showtext)
library(ggrepel)

# ── Fonts ──────────────────────────────────────────────────────────
font_add_google("Anton",    "anton")
font_add_google("DM Sans",  "dm_sans")
showtext_auto()
showtext_opts(dpi = 320)

# ══════════════════════════════════════════════════════════════════
#  DESIGN SYSTEM 
# ══════════════════════════════════════════════════════════════════

# ── FONDO — 
BG <- "#E8EEF4"    

TITLE_C   <- "#1A1A2E"
BODY_C    <- "#444444"
CAPTION_C <- "#888888"
GRID_C    <- "#D0D0D8"


REGION_COLORS <- c(
  "Western Europe" = "#4207F5",   # azul eléctrico
  "Eastern Europe" = "#00B0FF",   # azul cielo ← intercambio con Africa
  "North America"  = "#E9339E",   # fucsia
  "Latin America"  = "#7B16F5",   # violeta oscuro ← intercambio con Asia
  "Caribbean"      = "#F50057",   # rosa shock
  "Asia"           = "#FF3D00",   # rojo fuego ← intercambio con LatAm
  "Middle East"    = "#FFD600",   # amarillo solar
  "Africa"         = "#00BFA5",   # teal ← intercambio con E.Europe
  "Oceania"        = "#00C853"    # verde neón
)

# ══════════════════════════════════════════════════════════════════
#  DATA PREP 
# ══════════════════════════════════════════════════════════════════

raw <- read_csv("data/biennales.csv", show_col_types = FALSE)

country_col <- names(raw)[16]
region_col  <- names(raw)[17]

iso_lookup <- tribble(
  ~country_name,                ~iso,
  "USA",                        "USA",
  "Germany",                    "DEU",
  "Australia",                  "AUS",
  "China",                      "CHN",
  "Brazil",                     "BRA",
  "Russia",                     "RUS",
  "UK",                         "GBR",
  "France",                     "FRA",
  "South Korea",                "KOR",
  "Canada",                     "CAN",
  "Japan",                      "JPN",
  "Romania",                    "ROU",
  "Italy",                      "ITA",
  "Finland",                    "FIN",
  "Poland",                     "POL",
  "Latvia",                     "LVA",
  "Turkey",                     "TUR",
  "Greece",                     "GRC",
  "Taiwan",                     "TWN",
  "Belgium",                    "BEL",
  "Argentina",                  "ARG",
  "Sweden",                     "SWE",
  "Denmark",                    "DNK",
  "India",                      "IND",
  "Indonesia",                  "IDN",
  "Macedonia",                  "MKD",
  "Bolivia",                    "BOL",
  "Norway",                     "NOR",
  "Austria",                    "AUT",
  "Pakistan",                   "PAK",
  "Thailand",                   "THA",
  "Netherlands",                "NLD",
  "Slovenia",                   "SVN",
  "Serbia",                     "SRB",
  "Czech Republic",             "CZE",
  "Estonia",                    "EST",
  "Jamaica",                    "JAM",
  "Ireland",                    "IRL",
  "Portugal",                   "PRT",
  "Switzerland",                "CHE",
  "Bulgaria",                   "BGR",
  "Ecuador",                    "ECU",
  "Senegal",                    "SEN",
  "Chile",                      "CHL",
  "United Arab Emirates",       "ARE",
  "Lithuania",                  "LTU",
  "Costa Rica",                 "CRI",
  "Bangladesh",                 "BGD",
  "New Zealand",                "NZL",
  "Mongolia",                   "MNG",
  "Singapore",                  "SGP",
  "Iceland",                    "ISL",
  "Cameroon",                   "CMR",
  "Mexico",                     "MEX",
  "Algeria",                    "DZA",
  "Spain",                      "ESP",
  "Morocco",                    "MAR",
  "Palestine",                  "PSE",
  "Ukraine",                    "UKR",
  "Israel",                     "ISR",
  "Hungary",                    "HUN",
  "Afghanistan",                "AFG",
  "Andorra",                    "AND",
  "Egypt",                      "EGY",
  "Cyprus",                     "CYP",
  "Nigeria",                    "NGA",
  "Armenia",                    "ARM",
  "Malaysia",                   "MYS",
  "Philippines",                "PHL",
  "Guatemala",                  "GTM",
  "Albania",                    "ALB",
  "Mali",                       "MLI",
  "Haiti",                      "HTI",
  "Congo",                      "COD",
  "Uganda",                     "UGA",
  "Bermuda",                    "BMU",
  "Nepal",                      "NPL",
  "Lithuania, Estonia, Latvia", "LTU",
  "Monterrey",                  "MEX"
)

region_lookup <- tribble(
  ~country_name,   ~region_correct,
  "Mexico",        "Latin America",
  "Monterrey",     "Latin America",
  "Macedonia",     "Eastern Europe",
  "Brazil",        "Latin America",
  "Greece",        "Western Europe",
  "Pakistan",      "Asia",
  "Russia",        "Eastern Europe",
  "Jamaica",       "Caribbean",
  "Bermuda",       "Caribbean",
  "Haiti",         "Caribbean"
)

# Country name labels for plot
country_labels <- tribble(
  ~iso,   ~label,
  "USA",  "United States",
  "DEU",  "Germany",
  "AUS",  "Australia",
  "CHN",  "China",
  "GBR",  "United Kingdom",
  "JPN",  "Japan",
  "ITA",  "Italy",
  "FRA",  "France",
  "ROU",  "Romania",
  "LVA",  "Latvia",
  "TWN",  "Taiwan",
  "FIN",  "Finland",
  "BRA",  "Brazil",
  "RUS",  "Russia",
  "KOR",  "South Korea",
  "CAN",  "Canada",
  "POL",  "Poland",
  "BEL",  "Belgium",
  "GRC",  "Greece",
  "ARG",  "Argentina",
  "SWE",  "Sweden",
  "DNK",  "Denmark",
  "IND",  "India",
  "AUT",  "Austria",
  "NLD",  "Netherlands",
  "ESP",  "Spain",
  "MEX",  "Mexico",
  "HUN",  "Hungary",
  "TUR",  "Turkey",
  "CHE",  "Switzerland"
)

# Aggregate biennales
biennales_agg <- raw |>
  rename(country_name = all_of(country_col),
         region_raw   = all_of(region_col)) |>
  filter(!region_raw %in% c("online", "Antarctica"), !is.na(region_raw)) |>
  left_join(iso_lookup,    by = "country_name") |>
  left_join(region_lookup, by = "country_name") |>
  mutate(region = coalesce(region_correct, region_raw)) |>
  filter(!is.na(iso)) |>
  group_by(iso, region) |>
  summarise(n_biennales = n(), .groups = "drop")

# Join museums
museums <- read_csv("data/number_museums.csv", show_col_types = FALSE) |>
  mutate(museums = if_else(iso == "NLD", 700L, as.integer(museums)))

df <- biennales_agg |>
  left_join(museums |> select(iso, museums), by = "iso") |>
  left_join(country_labels,                  by = "iso") |>
  filter(!is.na(museums), n_biennales >= 2) |>   # solo países con 2+ bienales
  mutate(log_museums = log10(museums))

# ── Identify outliers via residuals from linear model ──────────────
model     <- lm(n_biennales ~ log_museums, data = df)
df$resid  <- resid(model)
df$fitted <- fitted(model)

# Top 5 by biennales — always annotated
top5_iso <- df |> slice_max(n_biennales, n = 5) |> pull(iso)

# Outliers: residual > 1.2 or < -1.5 (above/below trend)
outlier_iso <- df |>
  filter(abs(resid) > 0.8 | iso %in% top5_iso) |>
  pull(iso)

df <- df |>
  mutate(
    annotate  = iso %in% outlier_iso,
    direction = case_when(
      resid >  1.2 ~ "above",   # more biennales than expected
      resid < -1.5 ~ "below",   # fewer biennales than expected
      TRUE         ~ "on_trend"
    )
  )

message("Points to annotate: ", sum(df$annotate))
message("Above trend: ",  sum(df$direction == "above"))
message("Below trend: ",  sum(df$direction == "below"))

# ══════════════════════════════════════════════════════════════════
#  PLOT
# ══════════════════════════════════════════════════════════════════

p <- ggplot(df, aes(x = log_museums, y = n_biennales)) +
  
  # Trend line first (behind points)
  geom_smooth(method  = "lm", se = FALSE, fullrange = FALSE,
              color   = "#333D47", fill = "#333D47",
              alpha   = 0.08, linewidth = 0.8,
              linetype = "solid") +
  
  # Bubbles — size = n_biennales, alpha by direction
  geom_point(data  = filter(df, direction == "below"),
             aes(color = region, size = n_biennales),
             alpha = 0.35) +
  
  geom_point(data  = filter(df, direction == "on_trend"),
             aes(color = region, size = n_biennales),
             alpha = 0.60) +
  
  geom_point(data  = filter(df, direction == "above"),
             aes(color = region, size = n_biennales),
             alpha = 0.90) +
  
  
  # Labels with ggrepel
  geom_text_repel(
    data          = filter(df, annotate, !is.na(label)),
    aes(label     = label),
    color         = TITLE_C,        # always dark — legible on any bg
    family        = "dm_sans",
    size          = 8,
    fontface      = "bold",
    box.padding   = 0.6,
    point.padding = 0.4,
    segment.size  = 0.3,
    segment.color = "#AAAAAA",
    segment.alpha = 0.6,
    max.overlaps  = 25,
    show.legend   = FALSE
  ) +
  
  scale_color_manual(values = REGION_COLORS, guide = "none") +
  
  scale_size_area(max_size = 24, guide = "none") +
  
  
  # X axis: log scale labels readable
  scale_x_continuous(
    name   = "NUMBER OF MUSEUMS",
    limits = c(log10(30), log10(11000)),
    breaks = log10(c(50, 100, 500, 1000, 5000, 10000)),
    labels = c("50", "100", "500", "1,000", "5,000", "10,000"), 
    #expand = expansion(mult = c(0, 0.05)),  # 0 a la izquierda, poco a la derecha
  ) +
  
  scale_y_continuous(
    name   = NULL,
    breaks = NULL,
    expand = expansion(mult = c(0.05, 0.18))
  ) +
  
  # Economist-style annotations
  annotate("text",
           x = log10(50), y = 8,
           label = "↑ More
than expected",
           family = "dm_sans", size = 8,
           color = "#333D47", fontface = "bold",
           hjust = 0, lineheight = 0.9) +
  
  annotate("text",
           x = log10(4000), y = 1.2,
           label = "↓ Fewer
than expected",
           family = "dm_sans", size = 8,
           color = "#888888", fontface = "bold",
           hjust = 0, lineheight = 0.9) +
  
  annotate("text",
           x = log10(175), y = 2,
           label = "expected →",
           family = "dm_sans", fontface = "italic", size = 7.5,
           color = "#888888", alpha = 1,
           hjust = 0, angle = 10.5) +
  
  labs(caption = NULL) +
  
  theme_minimal(base_family = "dm_sans") +
  theme(
    plot.background  = element_rect(fill = BG, color = NA),
    panel.background = element_rect(fill = BG, color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x      = element_line(color = GRID_C, linewidth = 0.5),
    
    axis.title   = element_text(family = "dm_sans", size = 20,
                                color  = TITLE_C, face = "bold"),
    axis.text.x  = element_text(family = "dm_sans", size = 16, color = BODY_C),
    axis.text.y  = element_blank(),
    axis.ticks        = element_line(color = BODY_C, linewidth = 0.4),
    axis.ticks.length = unit(0.25, "cm"),
    
    plot.margin  = margin(20, 20, 14, 20),
    
    legend.position   = "none" 
  )

#── Export ─────────────────────────────────────────────────────────
ggsave("images/museums-scatter-plot.png",
       plot = p, width = 16, height = 8, dpi = 320)

p
