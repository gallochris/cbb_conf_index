# Login and load utilities
source(here::here("R/cbbdata_login.R"))

source(here::here("R/utils.R"))

# Fetch the conference only game types for this season 
conf_data <-
  cbbdata::cbd_torvik_game_stats(year = 2024, type = "conf")

# Add today's date to get the latest NET numbers - it's updated around 9 am EST each day
today_date <- format(Sys.Date(), "%Y-%m-%d")

# Load CSV from GitHub and update conferences 
net_up <-
  readr::read_csv(
    "https://raw.githubusercontent.com/andreweatherman/NCAA_NET_RANKINGS/main/complete_data.csv"
  ) |>
  dplyr::mutate(
    conf = dplyr::case_match(
      team,
      "Tarleton St." ~ "WAC",
      "Utah Tech" ~ "WAC",
      "St. Thomas" ~ "Sum",
      "UC San Diego" ~ "BW",
      "Bellarmine" ~  "ASun",
      "Queens" ~  "ASun",
      "Le Moyne" ~ "NEC",
      "Lindenwood" ~ "OVC",
      "Texas A&M Commerce" ~ "Slnd",
      "Southern Indiana" ~ "OVC",
      "Stonehill" ~ "NEC",
      team ~ conf
    )
  ) |>
  dplyr::mutate(conf = conf_name_lookup(conf))

# Isolate only the NET rankings for today and conference data 
team_net <- net_up |>
  dplyr::filter(date == today_date) |>
  dplyr::select(team, net, conf)

# Create the Conference records and quad data 
conf_records <- conf_data |>
  cbbdata::cbd_add_net_quad() |>
  dplyr::mutate(delta = abs(pts_scored - opp_pts)) |>
  dplyr::select(-net,-conf) |>
  dplyr::left_join(team_net, by = "team") |>
  dplyr::group_by(conf) |>
  dplyr::summarise(
    games_played = dplyr::n(),
    avg_net = mean(net),
    home_wins = sum(result == "W" & location == "H"),
    home_loss = sum(result == "L" & location == "H"),
    home_win_pct = (home_wins / (home_wins + home_loss)),
    avg_diff = mean(delta),
    q1 = sum(quad == "Quadrant 1") / games_played,
    q2 = sum(quad == "Quadrant 2") / games_played,
    q3 = sum(quad == "Quadrant 3") / games_played,
    q4 = sum(quad == "Quadrant 4") / games_played,
  ) |>
  dplyr::arrange(avg_net)

# Create the table for the conference records
by_conf_records <- conf_records |>
  dplyr::mutate(row_number = 1:dplyr::n()) |>
  dplyr::select(
    row_number,
    conf,
    games_played,
    home_win_pct,
    avg_net,
    q1,
    q2,
    q3,
    q4
  ) |>
  gt::gt() |>
  gt::cols_label(
    row_number = "",
    conf = "",
    games_played = "Games",
    home_win_pct = "Home Win %",
    avg_net = "Avg NET",
    q1 = "Q1%",
    q2 = "Q2%",
    q3 = "Q3%",
    q4 = "Q4%"
  ) |>
  gt::fmt_number(columns = c(avg_net), decimals = 1) |>
  gt::fmt_percent(columns = c(home_win_pct, q1, q2, q3, q4), decimals = 1) |>
  gt::tab_spanner(label = "Quadrants",
                  columns = c(avg_net, q1, q2, q3, q4)) |>
  gtExtras::gt_theme_dot_matrix() |>
  gt::cols_align(align = "left", columns = "conf") |>
  gtExtras::gt_hulk_col_numeric(columns = c(home_win_pct, q1, q2, q3, q4)) |>
  gtExtras::gt_hulk_col_numeric(columns = c(avg_net),
                                reverse = TRUE,) |>
  gt::tab_header(title = "NET Summary by Conferences",
                 subtitle = "Shows the home win percentage in conference games only, average NET ranking for all teams in one conference, and percentage of conference games in each quadrant.") |>
  gt::tab_source_note(source_note = "Bless your chart | data: cbbdata") |>
  gt::tab_options (
    source_notes.font.size = gt::px(10),
    row.striping.background_color = '#EEEEEE',
    table.font.size = gt::px(12),
    column_labels.text_transform = 'capitalize'
  ) |>
  gt::tab_style(style = list(gt::cell_borders(
    sides = c("left"),
    color = "#c1c1c1",
    weight = gt::px(2)
  )),
  locations = list(gt::cells_body(
    columns = c(avg_net, home_win_pct, q1, q2, q3, q4)
  ))) -> by_conf_table