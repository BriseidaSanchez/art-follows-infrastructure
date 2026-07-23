

# ══════════════════════════════════════════════════════════════════
#  BIENNALES POSTER — VIZ 1: STAR MAP
#  files: biennales.csv, number_museums.csv,
#            world_equalearth.geojson, country_centroids_ee.csv
# ══════════════════════════════════════════════════════════════════

library(tidyverse)
library(sf)
library(showtext)
library(scales)
library(glue)

# ── Fonts ──────────────────────────────────────────────────────────
font_add_google("Anton",           "anton")
font_add_google("Playfair Display","playfair")
font_add_google("DM Sans",         "dm_sans")
showtext_auto()
showtext_opts(dpi = 320)

# ══════════════════════════════════════════════════════════════════
#  DESIGN SYSTEM 
# ══════════════════════════════════════════════════════════════════

BG <- "#FFFFFF"   


# ── OCEAN (bg map) ────────────────────────────────────────
OCEAN   <- "#E8EEF4"  

# ── COUNTRIES (no data) ──────────────────────────────────────────────
# LAND_ND <- "#E0DCE0"   
LAND_ND <- "#E8E0D5" 
# ── BORDERS ──────────────────────────────────────────────
BORDER  <- "#FFFFFF"   

# ── FONT COLORS ────────────────────────────────────────────────────
TITLE_C   <- "#1A1A2E"   # Anton bold
ITALIC_C  <- "#E040A0"   # Playfair italic
BODY_C    <- "#444444"   # DM Sans
CAPTION_C <- "#888888"   # Sources/Notes

# ── Color scale (#museums) ────────────────────────────────────

MUSEUM_PAL <- c(
  "#EEF0F2",
  "#C5CBD1",
  "#8F99A3",
  "#5A6470",
  "#333D47",
  "#141C24"
)

# ── DOTS PER REGION ─────────────────

REGION_COLORS <- c(
  "Western Europe" = "#4207F5",   
  "Eastern Europe" = "#00B0FF",   
  "North America"  = "#E9339E",   
  "Latin America"  = "#7B16F5",   
  "Caribbean"      = "#F50057",   
  "Asia"           = "#FF3D00",   
  "Middle East"    = "#FFD600",   
  "Africa"         = "#00BFA5",   
  "Oceania"        = "#00C853"   ) 



# ══════════════════════════════════════════════════════════════════
#  STEP 1 — BIENNALES: raw → aggregated by country
# ══════════════════════════════════════════════════════════════════

raw <- read_csv("data/biennales.csv", show_col_types = FALSE)

country_col <- names(raw)[16]
region_col  <- names(raw)[17]
message("Country col: ", country_col, " | Region col: ", region_col)

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

message("Countries: ", nrow(biennales_agg),
        " | Biennales: ", sum(biennales_agg$n_biennales))

# ══════════════════════════════════════════════════════════════════
#  STEP 2 — JOIN centroids + museums to dots_data
# ══════════════════════════════════════════════════════════════════

centroids <- read_csv("data/country_centroids_ee.csv", show_col_types = FALSE)

museums <- read_csv("data/number_museums.csv", show_col_types = FALSE) |>
  mutate(museums = if_else(iso == "NLD", 700L, as.integer(museums)))

dots_data <- biennales_agg |>
  left_join(centroids,                       by = "iso",
            relationship = "one-to-one") |>
  left_join(museums |> select(iso, museums), by = "iso")

r_val <- cor(dots_data$n_biennales, log10(dots_data$museums),
             use = "complete.obs") |> round(2)
message("r = ", r_val)

# ══════════════════════════════════════════════════════════════════
#  STEP 3 — SHAPEFILE + museums to choropleth
# ══════════════════════════════════════════════════════════════════

world_sf <- st_read("data/world_equalearth.geojson", quiet = TRUE) |>
  left_join(museums |> select(iso, museums), by = c("iso_a3" = "iso")) |>
  mutate(museums_log = log10(museums))

# ══════════════════════════════════════════════════════════════════
#  PLOT
# ══════════════════════════════════════════════════════════════════

p <- ggplot() +
  
  geom_sf(data = world_sf, aes(fill = museums_log),
          color = BORDER, linewidth = 0.15) +
  
  scale_size_area(
    max_size = 16,
    breaks   = c(1, 6, 17),
    labels   = c("1", "6", "17"),
    name     = "BIENNALES",
    guide    = guide_legend(
      override.aes = list(color = "#555555", alpha = 0.85),
      ncol         = 3,
      keyheight    = unit(0.6, "cm"),
      keywidth     = unit(0.6, "cm"),
      label.theme  = element_text(family = "dm_sans", size = 11, color = "#1A1A2E")
    )
  ) +
  
  scale_color_manual(
    values = REGION_COLORS,
    name   = "REGION",
    guide  = guide_legend(
      override.aes = list(size = 3.5, alpha = 1),
      ncol = 1,
      keyheight    = unit(0.45, "cm"),
      keywidth     = unit(0.45, "cm"),
      label.theme  = element_text(family = "dm_sans", size = 9,
                                  color = "#1A1A2E", margin = margin(l = 2))
    )
  ) +
  
  scale_fill_gradientn(
    colors   = MUSEUM_PAL,
    na.value = LAND_ND,
    limits   = c(log10(4), log10(10000)),
    breaks   = c(1, 2, 3, 4),
    labels   = c("10", "100", "1k", "10k"),
    name     = "MUSEUMS",
    guide    = guide_colorbar(
      barwidth     = 0.5,
      barheight    = 7,
      title.hjust  = 0,
      ticks        = FALSE,
      frame.colour = "#DDDDDD"
    )
  ) +
  
  geom_point(data = dots_data,
             aes(x = lon_c, y = lat_c, size = n_biennales),
             color = "white", alpha = 0.3, shape = 16) +
  
  geom_point(data = dots_data,
             aes(x = lon_c, y = lat_c, size = n_biennales, color = region),
             alpha = 0.95, shape = 16) +
  
  coord_sf(expand = FALSE, clip="on") +
  
  
  labs(
    # Titles and subtitles manual
    title    = NULL,
    subtitle = NULL,
    # caption  = paste0(
    #   "Dot size = active biennales per country (2017–2018)  ·  ",
    #   "Background = museum count (Wikidata, log scale)\n"
    # )
  ) +
  
  theme_void(base_family = "dm_sans") +
  
  guides(
    size  = guide_legend(order = 1, ncol=3),   # BIENNALES
    color = guide_legend(order = 2),   # REGION 
    fill  = guide_colorbar(
      order = 3,
      title.theme = element_text(margin = margin(b = 16))
    )  # MUSEUMS 
  ) +
  theme(
    # OCEAN = plot.background
    # In Pages background = OCEAN
    plot.background  = element_rect(fill = OCEAN, color = NA),
    panel.background = element_rect(fill = OCEAN, color = NA),
    plot.caption = element_text(),
    # plot.caption = element_text(
    #   family     = "dm_sans", size = 10,   color = CAPTION_C,
    #   hjust      = 0, margin = margin(t = 6, l = 4), lineheight = 1.4
    # ),
    plot.margin       = margin(0, 0, 0, 0),
    
    # Legend
    legend.position   = c(0.01, 0.04),   
    legend.justification = c(0, 0),      
    legend.direction  = "vertical",
    legend.box        = "vertical",
    legend.box.just   = "left",
    legend.spacing    = unit(0.25, "cm"),
    
    # Background
    legend.background = element_rect(fill  = alpha("white", 0.1),
                                     color = NA),
    legend.margin     = margin(5, 8, 5, 8),
    legend.key        = element_rect(fill = NA, color = NA),
    
    legend.text  = element_text(family = "dm_sans", size = 15, color = "#1A1A2E"),
    legend.title = element_text(family = "dm_sans",   size = 17,face='bold',
                                color = "#1A1A2E", margin = margin(b = 6)),
    legend.spacing.y  = unit(0.15, "cm"),
    legend.spacing.x  = unit(0.2,  "cm")
  )
p
# ── Export ─────────────────────────────────────────────────────────

ggsave("/images/star-map.png",
       plot = p, width = 20, height = 12, dpi = 320)
# ══════════════════════════════════════════════════════════════════
#  VIZ 1B: EUROPE INSET MAP
#  Zoom over Europe
# ══════════════════════════════════════════════════════════════════

XMIN_EU <- -1050000
XMAX_EU <-  3200000
YMIN_EU <-  4000000   
YMAX_EU <-  7800000   

dots_europe <- dots_data |>
  filter(!is.na(lon_c), !is.na(lat_c)) |>
  filter(lon_c >= XMIN_EU, lon_c <= XMAX_EU,
         lat_c >= YMIN_EU, lat_c <= YMAX_EU)

p_inset <- ggplot() +
  
  # Choropleth — EXACT same palette and limits as main map
  geom_sf(data = world_sf, aes(fill = museums_log),
          color = BORDER, linewidth = 0.3) +
  
  scale_fill_gradientn(
    colors   = MUSEUM_PAL,
    na.value = LAND_ND,
    limits   = c(log10(4), log10(10000)),
    guide    = "none"
  ) +
  
  # Glow
  geom_point(data = dots_europe,
             aes(x = lon_c, y = lat_c, size = n_biennales),
             color = "white", alpha = 0.2, shape = 16) +
  
  # Dots — same colors, slightly larger for readability at small size
  geom_point(data = dots_europe,
             aes(x = lon_c, y = lat_c, size = n_biennales, color = region),
             alpha = 0.95, shape = 16) +
  
  scale_color_manual(values = REGION_COLORS, guide = "none") +
  scale_size_area(max_size = 18, guide = "none") +
  
  # Zoom
  coord_sf(
    xlim   = c(XMIN_EU, XMAX_EU),
    ylim   = c(YMIN_EU, YMAX_EU),
    expand = FALSE, clip="on"
  ) +
  
  labs(title = NULL, subtitle = NULL, caption = NULL) +
  
  theme_void() +
  theme(
    # Same background as main map — cohesion
    plot.background  = element_rect(fill = OCEAN, color = NA),
    panel.background = element_rect(fill = OCEAN, color = NA),
    plot.margin      = margin(0, 0, 0, 0)
  )

# Square export — adjust size as needed in Pages

ggsave("images/europe-inset-star-map.png",
       plot = p_inset, width = 8, height = 8, dpi = 320)



p
