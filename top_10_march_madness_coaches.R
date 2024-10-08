# Load in relevant packages
library(tidyverse)
library(rvest)
library(cbbdata)
library(gt)
library(gtExtras)
library(cbbplotR)

# Log into cbbdata API created by Andrew Weatherman
cbbdata::cbd_login()

# View The top-10 coaches with the most March Madness wins since 2020.
cbbdata::cbd_torvik_ncaa_results(2020, 2024, type = "coach") %>%
  arrange(desc(wins)) %>%
  slice(1:10) 

# Define a team column since the above set does not already have one built in.
team <-  c("Connecticut",  "Gonzaga", "Houston", "Kansas", "Baylor", "USC", "Creighton", "UCLA", "Alabama", "Miami FL")

# Define our dataset and add the team column into the set.
cbb_coaches <- cbbdata::cbd_torvik_ncaa_results(2020, 2024, type = "coach") %>%
  arrange(desc(wins)) %>%
  slice(1:10) %>%
  mutate(team) %>%
  # Create a win percentage column from the column that used decimals.
  mutate(Win_p = w_percent*100) %>%
  # Add in the HTML column with logos from cbd_teams into our new dataset.
  left_join(cbd_teams() %>% select(team = common_team, logo)) 

# Create the gt table.
cbb_coaches %>%
  select(team, coach, wins, loss, Win_p, pase, s16, f4, champ) %>%
  gt_cbb_teams(team, include_name = FALSE)  %>%
  gt() %>%
  gt_theme_espn() %>%
  fmt_markdown() %>%
  cols_label(Win_p = "Win %", loss = "Losses", champ = "Titles") %>%
  gt_set_font("Roboto") %>%
  cols_align(columns = everything(), "left") %>%
  tab_header(
    title = "The Best Coaches in March Madness",
    subtitle = "The ten best coaches in March Madness since 2020; along with how they perform against their projections")  %>%
  tab_source_note("Data by Barttorvik & cbbdata - Table & Analysis by @JakesBBN") %>%
  gt_add_divider(team, include_labels = FALSE, color = 'black', weight = px(1.5)) %>% 
  gt_add_divider(coach, include_labels = FALSE, color = 'black', weight = px(1.5)) %>%
  gt_add_divider(wins, include_labels = FALSE, color = 'black', weight = px(1.5)) %>%
  gt_add_divider(pase, include_labels = FALSE, color = 'black', weight = px(1.5)) %>%
  gt_add_divider(loss, include_labels = FALSE, color = 'black', weight = px(1.5)) %>%
  gt_add_divider(Win_p, include_labels = FALSE, color = 'black', weight = px(1.5)) %>% 
  gt_add_divider(s16, include_labels = FALSE, color = 'black', weight = px(1.5)) %>%
  gt_add_divider(f4, include_labels = FALSE, color = 'black', weight = px(1.5)) %>%
  gt_hulk_col_numeric(pase)
