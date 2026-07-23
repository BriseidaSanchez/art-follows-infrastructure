# ══════════════════════════════════════════════════════════════════
#  BIENNALES POSTER — VIZ 4: CITY BUBBLE CHART
#  Top 10 biennale cities · size = museums · color = region
#  "8 of 10 top biennale cities have 100+ museums"
#
#  Sources:
#  Museums: WCCF 2022 (worldcitiescultureforum.com/data)
#           Statista 2019 (Moscow), Beijing Gov 2024,
#           Est. official national stats (Athens, Bucharest,
#           São Paulo, Riga)
#  Biennales: Tifentale & Manovich (2020) Elsewhere Project
#
#  install.packages("packcircles")
# ══════════════════════════════════════════════════════════════════

library(tidyverse)
library(showtext)
library(packcircles)

font_add_google("Anton",   "anton")
font_add_google("DM Sans", "dm_sans")
showtext_auto()
showtext_opts(dpi = 320)

BG <- "#E8EEF4" 
TITLE_C <- "#1A1A2E"


REGION_COLORS <- c(
  "Western Europe" = "#4207F5",   
  "Eastern Europe" = "#00B0FF",   
  "North America"  = "#E9339E",   
  "Latin America"  = "#7B16F5",    
  "Caribbean"      = "#F50057",   
  "Asia"           = "#FF3D00",   
  "Middle East"    = "#FFD600",   
  "Africa"         = "#00BFA5",   
  "Oceania"        = "#00C853"    
)

# ── Data ───────────────────────────────────────────────────────────
df <- tribble(
  ~city,          ~region,           ~biennales, ~museums,
  "Moscow",       "Eastern Europe",  4,          261,
  "Taipei",       "Asia",            4,          131,
  "Riga",         "Eastern Europe",  4,           35,
  "New York",     "North America",   3,          140,
  "Bucharest",    "Eastern Europe",  3,           60,
  "Paris",        "Western Europe",  2,          297,
  "Beijing",      "Asia",            2,          226,
  "Athens",       "Western Europe",  2,          155,
  "Buenos Aires", "Latin America",   2,          132,
  "São Paulo",    "Latin America",   2,          120
) |>
  arrange(desc(museums)) |>  # largest first for better packing
  mutate(
    # Grey per city — renormalized to group range (more museums = darker)
    grey_color = colorRampPalette(c("#C5CBD1", "#141C24"))(100)[
      pmax(1, pmin(100, round(
        (museums - min(museums)) /
          (max(museums) - min(museums)) * 99) + 1))
    ],
    # Text color: dark on light bubbles, white on dark bubbles
    text_color = ifelse(museums < 80, "#1A1A2E", "white")
  )

# ── Circle packing ────────────────────────────────────────────────
packing <- circleProgressiveLayout(df$museums, sizetype = "area")

# Join — mutate id first, never inside select()
df_plot <- df |>
  mutate(id = row_number()) |>
  bind_cols(packing)

# Polygon vertices
verts <- circleLayoutVertices(packing, npoints = 80) |>
  left_join(
    df_plot |> select(id, region, museums, city, biennales, grey_color, text_color),
    by = "id"
  )

# ── Plot ───────────────────────────────────────────────────────────
p <- ggplot() +
  
  # Filled circles
  geom_polygon(
    data      = verts,
    aes(x = x, y = y, group = id, fill = grey_color),
    color     = BG,
    linewidth = 0.5,
    alpha     = 0.85
  ) +
  
  # City name — top half of circle
  geom_text(
    data     = df_plot,
    aes(x = x, y = y + radius*0.2, label = city),
    family   = "dm_sans",
    fontface = "bold",
    color    = df_plot$text_color,
    size     = 10,
    lineheight = 0.85
  ) +
  
  # Museum count — bottom half of circle
  geom_text(
    data     = df_plot,
    aes(x = x, y = y - radius * 0.35,
        label = paste0(museums)),
    family   = "dm_sans",
    fontface = "plain",
    color    = df_plot$text_color,
    size     = 10
  ) +
  
  scale_fill_identity(guide = "none") +
  
  coord_equal(clip = "off") +
  labs(x = NULL, y = NULL, caption = NULL) +
  
  theme_void() +
  theme(
    plot.background  = element_rect(fill = BG, color = NA),
    panel.background = element_rect(fill = BG, color = NA),
    plot.margin      = margin(12, 12, 12, 12)
  )

ggsave("images/city-bubbles.png",
       plot = p, width = 15, height = 15, dpi = 320)


