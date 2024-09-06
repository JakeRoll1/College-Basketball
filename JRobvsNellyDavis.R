# Load in relevant packages
library(tidyverse)
library(rvest)
library(cbbdata)
library(gt)
library(gtExtras)
library(cbbplotR)
library(flextable)

# Log into cbbdata API created by Andrew Weatherman
cbbdata::cbd_login()

# Create different datasets based on the type of postseason game that was being played.
post <- cbd_torvik_player_game(year= 2024) %>%
  filter(player %in% c("Johnell Davis", "Jaxson Robinson")) %>%
  filter(type == "post")

R1 <- cbd_torvik_player_game(year= 2024) %>%
  filter(player %in% c("Johnell Davis", "Jaxson Robinson")) %>%
  filter(type == "conf_t", result == "W")

R2 <- cbd_torvik_player_game(year= 2024) %>%
  filter(player %in% c("Johnell Davis", "Jaxson Robinson")) %>%
  filter(type == "conf_t", result == "L")

# Combine the datasets and create a new column based on the game type that was being played
combined <- bind_rows(
  mutate(post, game_type = "post"),
  mutate(R1, game_type = "Conf. Tourney R1"),
  mutate(R2, game_type = "Conf. Tourney R2"))

# Select only the columns we want - observing mostly offensive impact within the 3 postseason games.
jaxjohn <- combined %>%
  select(player, min, usg, ortg, pts, ast, ts, efg, tov, to_pct)

# Create our GT table
jaxjohn %>%
  gt() %>%
  gt_theme_espn() %>%
  gt_set_font("Roboto") %>%
  
  # Change Column names
  cols_label(min = "Mins", player = "Players", usg = "Usage %", ortg = "Offensive Rating", pts = "Points", ast = "Assists", ts = "TS %", efg = "EFG %", tov = "TO", to_pct = "TO %") %>%
  
  # Create table header and subtitle to explain the goal.
  tab_header(title = md("**Jaxson Robinson vs. Johnell Davis**"),
             subtitle = "As the Kentucky-Arkansas rivarly gains more fuel, let's look at each team's perceived best player and their statistics from two Conference Tourney games and NCAA Round of 64") %>%
  
  # Create a row label to indicate which group of postseason data is being displayed.
  tab_row_group(
    label = md("**NCAA Round**<br>**of 64**"),
    rows = 1:2)%>%
  tab_row_group(
    label = md("**Conference Tourney**<br>**Game Two**"),
    rows = 5:6) %>%
  tab_row_group(
    label = md("**Conference Tourney**<br>**Game One**"),
    rows = 3:4) %>%
  
  # Color in cells based on the team's colors to indicate who had the advantage.
  gt_color_rows("usg", palette = c("#ffffff","#9D2235"))%>%
  gt_color_rows("ortg", palette = c("#ffffff","#0033A0"))%>%
  gt_color_rows("pts", palette = c("#ffffff","#9D2235"))%>%
  gt_color_rows("ast", palette = c("#ffffff","#9D2235"))%>%
  gt_color_rows("ts", palette = c("#ffffff","#9D2235"))%>%
  gt_color_rows("efg", palette = c("#ffffff","#9D2235"))%>%
  gt_color_rows("tov": "to_pct", palette = c("#ffffff","#0033A0")) %>%
  
  # Explain cell colors and overall trend in the source notes.
  tab_source_note(source_note = md("**Johnell Davis has overall statistical advantage - Could be explained by weaker opponents within Conf. Tourney**"))%>%
  tab_source_note(source_note = md("**Columns with blue = Statistic where Jaxson Robinson has higher average.**"))%>%
  tab_source_note(source_note = md("**Columns with red = Statistic where Johnell Davis has higher average.**")) %>%
  tab_source_note("Data by BartTorvik & CBBData - Table by @JakesBBN") %>%
  
  # Create a spanner tab to group our columns into main categories for easier viewing.
  tab_spanner(label = md("**Offensive Efficiency**"),
              columns = c("usg", "ortg", "pts", "ast"))%>%
  tab_spanner(label = md("**Shooting Efficiency**"),
              columns = c("ts", "efg"))%>%
  tab_spanner(label = md("**TO Rates**"),
              columns = c("tov", "to_pct")) %>%
  
  # Create column dividers for better clarity.
  gt_add_divider(player, include_labels = FALSE, color = 'black', weight = px(1.5)) %>%
  gt_add_divider(min, include_labels = FALSE, color = 'black', weight = px(1.5)) %>%
  gt_add_divider(usg, include_labels = FALSE, color = 'black', weight = px(1.5)) %>%
  gt_add_divider(ortg, include_labels = FALSE, color = 'black', weight = px(1.5)) %>%
  gt_add_divider(pts, include_labels = FALSE, color = 'black', weight = px(1.5)) %>%
  gt_add_divider(ast, include_labels = FALSE, color = 'black', weight = px(1.5)) %>%
  gt_add_divider(ts, include_labels = FALSE, color = 'black', weight = px(1.5)) %>%
  gt_add_divider(efg, include_labels = FALSE, color = 'black', weight = px(1.5)) %>%
  gt_add_divider(tov, include_labels = FALSE, color = 'black', weight = px(1.5)) %>%
  
  # Change background color.
  tab_options(table.background.color = "snow1") %>%
  
  # Space out Cells.
  opt_horizontal_padding(1.5) %>%
  
  # Make Column labels the color black.
  tab_style(locations = cells_column_labels(everything()), style = cell_text(color = "black"))