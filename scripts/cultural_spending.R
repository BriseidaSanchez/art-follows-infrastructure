
# ══════════════════════════════════════════════════════════════════
#  BIENNALES POSTER — VIZ 3B: GROUPED BAR CHART

#  Sources: OECD/G20 Cultural Report 2021 · Eurostat COFOG 2018
# ══════════════════════════════════════════════════════════════════

library(tidyverse)
library(showtext)
library(ggpattern)


font_add_google("Anton",   "anton")
font_add_google("DM Sans", "dm_sans")
showtext_auto()
showtext_opts(dpi = 320)

BG      <- "#E8EEF4"
TITLE_C <- "#1A1A2E"
BODY_C  <- "#555555"
GRID_C  <- "#D0D4DC"

# Two colors — 
COL_MUSEUMS <- "#AD1457" 
COL_SPEND   <- "#E8EEF4"   
COL_SPEND_BORDER <- "#1A1A2E"

# ══════════════════════════════════════════════════════════════════
#  DATA 
# ══════════════════════════════════════════════════════════════════

N_TOP <- 15

# Cultural spending — hardcoded external source (OECD/Eurostat)
# not in CSVs — join by iso
spending_lookup <- tribble(
  ~iso,   ~spend_usd,  ~country_label,
  "ITA",  188,         "Italy",
  "DEU",  392,         "Germany",
  "USA",  218,         "US",
  "GBR",  338,         "UK",
  "JPN",  112,         "Japan",
  "BRA",   28,         "Brazil",
  "FRA",  465,         "France",
  "RUS",   88,         "Russia",
  "POL",   95,         "Poland",
  "CHN",   49,         "China",
  "KOR",  195,         "S.Korea",
  "AUS",  287,         "Australia",
  "ROU",   42,         "Romania",
  "FIN",  510,         "Finland",
  "CAN",  241,         "Canada"
)

# ── Pipeline from CSVs ─────────────────────────────────────────────
iso_lookup <- tribble(
  ~country_name,                ~iso,
  "USA","USA","Germany","DEU","Australia","AUS","China","CHN",
  "Brazil","BRA","Russia","RUS","UK","GBR","France","FRA",
  "South Korea","KOR","Canada","CAN","Japan","JPN","Romania","ROU",
  "Italy","ITA","Finland","FIN","Poland","POL","Latvia","LVA",
  "Turkey","TUR","Greece","GRC","Taiwan","TWN","Belgium","BEL",
  "Argentina","ARG","Sweden","SWE","Denmark","DNK","India","IND",
  "Indonesia","IDN","Macedonia","MKD","Bolivia","BOL","Norway","NOR",
  "Austria","AUT","Pakistan","PAK","Thailand","THA","Netherlands","NLD",
  "Slovenia","SVN","Serbia","SRB","Czech Republic","CZE","Estonia","EST",
  "Jamaica","JAM","Ireland","IRL","Portugal","PRT","Switzerland","CHE",
  "Bulgaria","BGR","Ecuador","ECU","Senegal","SEN","Chile","CHL",
  "United Arab Emirates","ARE","Lithuania","LTU","Costa Rica","CRI",
  "Bangladesh","BGD","New Zealand","NZL","Mongolia","MNG",
  "Singapore","SGP","Iceland","ISL","Cameroon","CMR","Mexico","MEX",
  "Algeria","DZA","Spain","ESP","Morocco","MAR","Palestine","PSE",
  "Ukraine","UKR","Israel","ISR","Hungary","HUN","Afghanistan","AFG",
  "Andorra","AND","Egypt","EGY","Cyprus","CYP","Nigeria","NGA",
  "Armenia","ARM","Malaysia","MYS","Philippines","PHL","Guatemala","GTM",
  "Albania","ALB","Mali","MLI","Haiti","HTI","Congo","COD",
  "Uganda","UGA","Bermuda","BMU","Nepal","NPL",
  "Lithuania, Estonia, Latvia","LTU","Monterrey","MEX"
)

region_lookup <- tribble(
  ~country_name,  ~region_correct,
  "Mexico","Latin America","Monterrey","Latin America",
  "Macedonia","Eastern Europe","Brazil","Latin America",
  "Greece","Western Europe","Pakistan","Asia",
  "Russia","Eastern Europe","Jamaica","Caribbean",
  "Bermuda","Caribbean","Haiti","Caribbean"
)

raw <- read_csv("data/biennales.csv", show_col_types = FALSE)
country_col <- names(raw)[16]
region_col  <- names(raw)[17]

museums <- read_csv("data/number_museums.csv", show_col_types = FALSE) |>
  mutate(museums = if_else(iso == "NLD", 700L, as.integer(museums)))

biennales_agg <- raw |>
  rename(country_name = all_of(country_col),
         region_raw   = all_of(region_col)) |>
  filter(!region_raw %in% c("online","Antarctica"), !is.na(region_raw)) |>
  left_join(iso_lookup,    by = "country_name") |>
  left_join(region_lookup, by = "country_name") |>
  mutate(region = coalesce(region_correct, region_raw)) |>
  filter(!is.na(iso)) |>
  group_by(iso, region) |>
  summarise(n_biennales = n(), .groups = "drop") |>
  left_join(museums |> select(iso, museums), by = "iso")

# Join spending — only countries with spending data available
# Top N by museums, tiebreak by biennales
df <- biennales_agg |>
  inner_join(spending_lookup, by = "iso") |>
  arrange(desc(museums), desc(n_biennales)) |>
  slice_head(n = N_TOP) |>
  rename(biennales = n_biennales,
         country   = country_label) |>
  mutate(country = fct_inorder(country)) |>
  mutate(
    norm_museums  = (museums   - min(museums))   / (max(museums)   - min(museums)),
    norm_spend    = (spend_usd - min(spend_usd)) / (max(spend_usd) - min(spend_usd)),
    label_museums = ifelse(museums >= 1000,
                           paste0(round(museums/1000, 1), "K"),
                           as.character(museums)),
    label_spend   = paste0("$", spend_usd),
    country       = fct_inorder(country),
    # Grey per country renormalized to group range
    bar_color     = colorRampPalette(c("#8F99A3", "#141C24"))(100)[
      pmax(1, pmin(100, round(
        (museums - min(museums)) /
          (max(museums) - min(museums)) * 99) + 1))
    ]
  )

# Long format for grouped bars
# Long format — only museums for bars
df_bars <- df |>
  select(country, norm_museums, label_museums, bar_color) |>
  mutate(label = label_museums)

# Spending stays wide for line + points
df_line <- df |>
  select(country, norm_spend, label_spend) |>
  mutate(label = label_spend)

# ── Plot ───────────────────────────────────────────────────────────
p <- ggplot(df_bars, aes(x = country)) +
  
  # Bars — grey per country
  geom_col(aes(y = norm_museums, fill = bar_color),
           width = 0.65, alpha = 0.92) +
  
  scale_fill_identity() +
  
  # Spending line
  geom_line(data = df_line,
            aes(y = norm_spend, group = 1),
            color = "#FCBF49", linewidth = 1, alpha = 0.9) +
  
  # Spending points
  geom_point(data = df_line,
             aes(y = norm_spend),
             color = "#FCBF49", size = 3.5, alpha = 0.95) +
  

  # Museum bar labels — large bars above
  geom_label(
    data          = filter(df_bars, norm_museums > 0.12),
    aes(y = norm_museums, label = label),
    vjust         = -0.8,
    family        = "dm_sans",
    size          = 11.5,
    fontface      = "bold",
    color         = TITLE_C,
    fill          = alpha(BG, 0.80),
    label.size    = 0,
    label.padding = unit(0.15, "lines")
  ) +
  
  # Museum bar labels — small bars inside
  geom_label(
    data          = filter(df_bars, norm_museums <= 0.12),
    aes(y = norm_museums, label = label),
    vjust         = 1.4,
    family        = "dm_sans",
    size          = 11.5,
    fontface      = "bold",
    color         = TITLE_C,
    fill          = alpha(BG, 0.85),
    label.size    = 0,
    label.padding = unit(0.15, "lines")
  ) +

  
  # Spending line labels — above each point
  geom_label(
    data          = df_line,
    aes(y = norm_spend, label = label),
    vjust         = 0,
    family        = "dm_sans",
    size          = 10,
    fontface      = "bold",
    color         = TITLE_C,
    fill          = alpha("#FCBF49", 0.8),
    label.size    = 0,
    label.padding = unit(0.12, "lines")
  ) +
  
  
  scale_y_continuous(
    limits = c(-0.08, 1.35),
    breaks = NULL,
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(x = NULL, y = NULL, caption = NULL) +
  
  theme_minimal(base_family = "dm_sans") +
  theme(
    plot.background    = element_rect(fill = BG, color = NA),
    panel.background   = element_rect(fill = BG, color = NA),
    panel.grid         = element_blank(),
    axis.text.x  = element_text(family = "dm_sans", size = 24,
                                color = TITLE_C, face = "bold"),
    axis.text.y  = element_blank(),
    axis.ticks   = element_blank(),
    legend.position   = "top",
    legend.direction  = "horizontal",
    legend.spacing.x  = unit(0.8, "cm"),
    legend.background = element_rect(fill = NA, color = NA),
    plot.margin  = margin(12, 16, 12, 16)
  )
##save
ggsave("images/cultural-spending.png",
       plot = p, width = 25, height = 12.5, dpi = 320)




