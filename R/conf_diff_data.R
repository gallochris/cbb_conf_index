# Login and load utilities
source(here::here("R/cbbdata_login.R"))

source(here::here("R/utils.R"))

# Fetch the conference only game types for this season 
conf_data <-
  cbbdata::cbd_torvik_game_stats(year = 2024, type = "conf")

# Add NET rankings
net_up <-
  cbbdata::cbd_all_metrics() 

team_net <- net_up |>
  dplyr::select(team, net = net_rank)

# Retrieve only the conference data for teams for joining later
only_confs <- conf_data |>
  dplyr::select(conf, team_name = team) |>
  dplyr::distinct(team_name, .keep_all = TRUE) |>
  dplyr::mutate(conf = conf_name_lookup(conf))


# Determine the margin for conference only games
conf_margin <- conf_data |>
  dplyr::group_by(team) |>
  dplyr::summarise(
    wins = sum(result == "W"),
    loss = sum(result == "L"),
    delta = sum(pts - opp_pts),
    home_wins = sum(result == "W" & location == "H"),
    home_loss = sum(result == "L" & location == "H"),
    home_delta = sum((pts - opp_pts) * (location == "H")),
    away_wins = sum(result == "W" & location == "A"),
    away_loss = sum(result == "L" & location == "A"),
    away_delta = sum((pts - opp_pts) * (location == "A"))
  ) |>
  dplyr::arrange(-wins,-delta) |>
  dplyr::mutate(team_name = team) |>
  dplyr::relocate(team_name, .before = wins) |>
  dplyr::left_join(only_confs, by = "team_name") |>
  dplyr::relocate(conf, .before = team)  |>
  dplyr::left_join(team_net, by ="team") |> 
  dplyr::relocate(net, .before = wins)  |>
  cbbplotR::gt_cbb_teams(team, include_name = FALSE)

# Add a function to fetch the differentials and records by conference 
# Within the function build the table 
conf_deltas <- function(conf) {
  conf_margin |>
    dplyr::filter(conf == {
      {
        conf
      }
    }) |>
    dplyr::mutate(row_number = 1:dplyr::n()) |>
    dplyr::relocate(row_number, .before = team) |>
    gt::gt() |>
    gt::fmt_markdown(columns = c(team)) |>
    gt::cols_label(
      row_number = "",
      team_name = "",
      team = "",
      net = "NET",
      delta = "+/-",
      wins = "W",
      loss = "L",
      home_wins = "W",
      home_loss = "L",
      home_delta = "+/-",
      away_wins = "W",
      away_loss = "L",
      away_delta = "+/-"
    ) |>
    gt::tab_spanner(label = "Overall",
                    columns = c(wins, loss, delta)) |>
    gt::tab_spanner(label = "Home",
                    columns = c(home_wins, home_loss, home_delta)) |>
    gt::tab_spanner(label = "Away",
                    columns = c(away_wins, away_loss, away_delta)) |>
    gt::fmt(
      columns = c(delta, home_delta, away_delta),
      fns = function(x) {
        ifelse(x > 0, paste0("+", x), x)
      }
    ) |>
    gt::cols_hide(columns = c(conf)) |>
    gtExtras::gt_theme_dot_matrix() |>
    gt::cols_align(align = "left", columns = "team_name") |>
    gtExtras::gt_hulk_col_numeric(columns = c(delta, home_delta, away_delta)) |>
    gtExtras::gt_hulk_col_numeric(columns = c(net), reverse = TRUE) |>
    gt::tab_header(title = "2023-24 Conference Standings",
                   subtitle = "Win/loss and point differential by location in conference play only.") |>
    gt::tab_source_note(source_note = "Bless your chart | data: cbbdata + cbbplotR") |>
    gt::tab_options (
      source_notes.font.size = gt::px(10),
      row.striping.background_color = '#EEEEEE',
      table.font.size = gt::px(12),
      column_labels.text_transform = 'uppercase'
    ) |>
    gt::tab_style(style = list(
      gt::cell_borders(
        sides = c("left"),
        color = "#c1c1c1",
        weight = gt::px(2)
      )
    ),
    locations = list(gt::cells_body(
      columns = c(
        wins,
        loss,
        delta,
        home_wins,
        home_loss,
        home_delta,
        away_wins,
        away_loss,
        away_delta
      )
    ))) -> conf_table
  
  conf_table
}

# Cache the table with the function for filtering 
cached_conf_deltas <- memoise::memoise(conf_deltas)