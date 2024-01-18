# Login and load utilities
source(here::here("R/cbbdata_login.R"))

source(here::here("R/utils.R"))

# Fetch the non-conference only game type for this season 
non_con_data <-
  cbbdata::cbd_torvik_game_stats(year = 2024, type = "nc")

# Add today's date to get the latest NET numbers - it's updated around 9 am EST each day
# This isn't needed unless we load from the CSV: readr::read_csv(
# "https://raw.githubusercontent.com/andreweatherman/NCAA_NET_RANKINGS/main/complete_data.csv"
# ) 
# today_date <- format(Sys.Date(), "%Y-%m-%d")

# Load NET rankings from full metrics 
net_up <-
  cbbdata::cbd_all_metrics() |> 
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

# Isolate only the NET rankings and conference data 
team_net <- net_up |>
  dplyr::select(team, net = net_rank, conf)

# Create the Non-conference records and quad data 
non_con_records <- non_con_data |>
  cbbdata::cbd_add_net_quad() |>
  dplyr::select(-net,-conf) |> # drop these for easier joining 
  dplyr::left_join(team_net, by = "team") |>
  dplyr::group_by(conf) |>
  dplyr::summarise(
    games_played = dplyr::n(),
    avg_net = mean(net),
    q1_rec = paste0(sum(quad == "Quadrant 1" & result == "W"), "-",
                    sum(quad == "Quadrant 1" & result == "L")),
    q1 = sum(quad == "Quadrant 1") / games_played,
    q2_rec = paste0(sum(quad == "Quadrant 2" & result == "W"), "-",
                    sum(quad == "Quadrant 2" & result == "L")),
    q2 = sum(quad == "Quadrant 2") / games_played,
    q3_rec = paste0(sum(quad == "Quadrant 3" & result == "W"), "-",
                    sum(quad == "Quadrant 3" & result == "L")),
    q3 = sum(quad == "Quadrant 3") / games_played,
    q4_rec = paste0(sum(quad == "Quadrant 4" & result == "W"), "-",
                    sum(quad == "Quadrant 4" & result == "L")),
    q4 = sum(quad == "Quadrant 4") / games_played,
  ) |>
  dplyr::arrange(avg_net)

# Create the table for the conference records
by__non_con_records <- non_con_records |>
  dplyr::mutate(row_number = 1:dplyr::n()) |>
  dplyr::select(
    row_number,
    conf,
    games_played,
    avg_net,
    q1_rec,
    q2_rec,
    q3_rec,
    q4_rec,
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
    avg_net = "Avg NET",
    q1_rec = "Q1",
    q2_rec = "Q2",
    q3_rec = "Q3",
    q4_rec = "Q4",
    q1 = "Q1%",
    q2 = "Q2%",
    q3 = "Q3%",
    q4 = "Q4%"
  ) |>
  gt::fmt_number(columns = c(avg_net), decimals = 1) |>
  gt::fmt_percent(columns = c(q1, q2, q3, q4), decimals = 1) |>
  gt::tab_spanner(label = "Quadrant Record (W-L)",
                  columns = c(q1_rec, q2_rec, q3_rec, q4_rec)) |>
  gt::tab_spanner(label = "Percent of games in Quadrant",
                  columns = c(q1, q2, q3, q4)) |>
  gtExtras::gt_theme_dot_matrix() |>
  gt::cols_align(align = "left", columns = "conf") |>
  gtExtras::gt_hulk_col_numeric(columns = c(q1, q2, q3, q4)) |>
  gtExtras::gt_hulk_col_numeric(columns = c(avg_net),
                                reverse = TRUE,) |>
  gt::tab_header(title = "Non-Conference play NET summary by each conference",
                 subtitle = "Shows the quadrant record in non-conference games, average NET ranking for all teams in one conference, and percentage of non-conference games in each quadrant.") |>
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
    columns = c(avg_net, q1_rec, q2_rec, q3_rec, q4_rec, q1, q2, q3, q4)
  ))) -> by_non_con_table