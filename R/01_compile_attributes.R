# ==============================================================================
# Define functions
# ==============================================================================

#' Load data from path to object
#'
#' @param from Character. File path to data file
#' @param to Character. Name of object to contain data frame
#'
#' @importFrom fs path_file path_dir file_exists
#' @importFrom cli cli_abort
#' @importFrom rlang caller_env
#' @importFrom haven read_dta
#'
#' @noRd
load_data <- function(cases_df, from, to) {

  if (!fs::file_exists(from)) {

    file_name  <- fs::path_file(from)
    file_dir <- fs::path_dir(from)

    cli::cli_abort(
      c(
        "File does not exist",
        i = "Expected {file_name}.",
        x = "No such file in {file_dir}"
      ),
      call = rlang::caller_env()
    )

  }

  # load from disk
  df <- haven::read_dta(file = from)

  # filter
  df_filtered <- df |>
    dplyr::semi_join(
      y = cases_df,
      by = dplyr::join_by(interview__id, interview__key)
    )

  base::assign(
    x = to,
    value = df_filtered,
    envir = rlang::caller_env(n = 5)
  )

}

#' Create NA attribute object for variable
#'
#' @param df_name Character. Name of data frame.
#' @param attrib_name Character. Stub name of attribute
#' @param var_name Character. Variable name.
#'
#' @importFrom susoreview create_attribute
#' @importFrom glue glue
#' @importFrom haven is_tagged_na
#' @importFrom rlang data_sym caller_env
#'
#' @noRd
create_na_attrib_obj <- function(
  df_name,
  attrib_name,
  var_name
) {

  df <- base::get(
    x = df_name,
    envir = rlang::caller_env(n = 4)
  )

  attrib_df <- susoreview::create_attribute(
    df = df,
    condition = haven::is_tagged_na(!!rlang::data_sym(var_name), tag = "a"),
    attrib_name = glue::glue("{attrib_name}_na"),
    attrib_vars = glue::glue("{var_name}")
  )

  base::assign(
    x = glue::glue("attrib_{attrib_name}_na"),
    value = attrib_df,
    envir = rlang::caller_env(n = 4)
  )

}

create_dk_attrib_obj <- function(
  df_name,
  attrib_name,
  var_name,
  dk_val = 888
) {

  df <- base::get(
    x = df_name,
    envir = rlang::caller_env(n = 4)
  )

  attrib_df <- susoreview::create_attribute(
    df = df,
    condition = !!rlang::parse_quo(
      x = glue::glue("{var_name} == {dk_val}"),
      env = rlang::current_env()
    ),
    attrib_name = glue::glue("{attrib_name}_dk"),
    attrib_vars = glue::glue("{var_name}")
  )

  base::assign(
    x = glue::glue("attrib_{attrib_name}_dk"),
    value = attrib_df,
    envir = rlang::caller_env(n = 4)
  )

}

#' Create Boolean attribute object for variable
#'
#' @inheritParams create_na_attrib_obj
#'
#' @importFrom susoreview create_attribute
#' @importFrom rlang data_sym caller_env
#' @importFrom glue glue
#'
#' @noRd
create_bool_attrib_obj <- function(
  df_name,
  var_name,
  attrib_name
) {

  df <- base::get(
    x = df_name,
    envir = rlang::caller_env(n = 4)
  )

  attrib_df <- susoreview::create_attribute(
    df = df,
    condition = !!rlang::parse_quo(
      x = glue::glue("{var_name} == 1"),
      env = rlang::current_env()
    ),
    attrib_name = attrib_name,
    attrib_vars = var_name
  )

  base::assign(
    x = glue::glue("attrib_{attrib_name}"),
    value = attrib_df,
    envir = rlang::caller_env(n = 4)
  )

}

create_bool_attrib_obj_w_2_vars <- function(
  df_name,
  var1_name,
  var2_name,
  attrib_name
) {

  df <- base::get(
    x = df_name,
    envir = rlang::caller_env(n = 4)
  )

  attrib_df <- susoreview::create_attribute(
    df = df,
    condition = !!rlang::parse_quo(
      x = glue::glue("{var1_name} == 1 & {var2_name} == 0"),
      env = rlang::current_env()
    ),
    attrib_name = attrib_name,
    attrib_vars = glue::glue("({var1_name}|{var2_name})")
  )

  base::assign(
    x = attrib_name,
    value = attrib_df,
    envir = rlang::caller_env(n = 4)
  )

}

#' Create attributes
#'
#' @param proj_dir Character. Path to project root directory.
#' @param cases_df Data frame. Cases to review.
#' 
#' @return Data frame of attributes
create_attributes <- function(
  proj_dir,
  hhold_name,
  cases_df
) {

  # ============================================================================
  # Load data
  # ============================================================================

  files <- c(
    paste0(hhold_name, ".dta"),
    "SEC_C_GENERATOR.dta",
    "SEC_C_RECHAR_BA.dta",
    "Solar_C_Device.dta",
    "appliances.dta",
    "cookstoves.dta",
    "fuel_consumption.dta"
  )

  names <- c(
    "hholds",
    "generators",
    "batteries",
    "solar_systems",
    "appliances",
    "stoves",
    "fuels"
  )

  hhold_c_dir <- fs::path(proj_dir, "01_data", "01_hhold", "02_combined")

  purrr::walk2(
    .x = files,
    .y = names,
    .f = ~ load_data(
      cases_df = cases_df,
      from = fs::path(hhold_c_dir, .x),
      to = .y
    )
  )

  # ============================================================================
  # Create data frames of main one of a rostered systems
  # ============================================================================

  # ----------------------------------------------------------------------------
  # Ensure there is one household entry per file by joining
  # ----------------------------------------------------------------------------

  #' Ensure that there is at least 1 hobs per hhold
  #'
  #' @param df_name Character. Name of df in the main environment
  #' @param hhold_name Character. Name of the household df.
  #'
  #' @importFrom dplyr select left_join
  #' @importFrom rlang caller_env
  ensure_1_hh_obs <- function(df_name, hhold_name = "hholds") {

    df <- base::get(
      x = df_name,
      envir = rlang::caller_env(n = 1)
    )

    hholds <- base::get(
      x = hhold_name,
      envir = rlang::caller_env(n = 1)
    )

    df_w_hhold <- hholds |>
      dplyr::select(interview__id, interview__key) |>
      dplyr::left_join(
        y = df,
        by = c("interview__id", "interview__key")
      )

    base::assign(
      x = df_name,
      value = df_w_hhold,
      envir = rlang::caller_env(n = 5)
    )

  }

  item_lvl_df_names <- c(
    "generators",
    "batteries",
    "solar_systems",
    "appliances",
    "stoves",
    "fuels"
  )

  purrr::walk(
    .x = item_lvl_df_names,
    .f = ~ ensure_1_hh_obs(df_name = .x)
  )

  # ----------------------------------------------------------------------------
  # Electricity
  # ----------------------------------------------------------------------------

  main_generator <- generators |>
    # identify the main system as the one listed first
    dplyr::group_by() |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::ungroup()

  main_battery <- batteries |>
    # identify the main system as the one listed first
    dplyr::group_by() |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::ungroup()

  main_solar_system <- hholds |>
    # identify main solar system
    dplyr::select(
      interview__id, interview__key, SEC_C_Q141,
      # attributes of main system
      # hours of electricity
      SEC_C_Q148, SEC_C_Q149
    ) |>
    dplyr::left_join(
      solar_systems,
      by = c("interview__id", "interview__key")
    ) |>
    dplyr::filter(
      # if solar systems, one that is the main one
      Solar_C_Device__id == SEC_C_Q141 |
      # if no solar system, retain a row anyway
      is.na(Solar_C_Device__id)
    ) |>
    # select questions for which to generate attributes
    dplyr::select(
      interview__id, interview__key,
      # items powered
      SEC_C_Q122, SEC_C_Q123, SEC_C_Q124, dplyr::starts_with("SEC_C_Q125__"),
      # hours of electricity
      SEC_C_Q148, SEC_C_Q149,
      # hours used for lighting and other applications each day
      SEC_C_Q138
    )

  # ----------------------------------------------------------------------------
  # Cookstove
  # ----------------------------------------------------------------------------

  main_stove <- hholds |>
    # identify main cookstove
    dplyr::select(interview__id, SEC_G_Q04) |>
    # combine household and cookstove data
    dplyr::left_join(stoves, by = "interview__id") |>
    # select main stove from roster
    dplyr::filter(cookstoves__id == SEC_G_Q04)

  # ============================================================================
  # Access to electricity
  # ============================================================================

  # ----------------------------------------------------------------------------
  # Capacity
  # ----------------------------------------------------------------------------

  # main source unanswered
  attrib_main_source_na <- susoreview::create_attribute(
    df = hholds,
    condition = haven::is_tagged_na(SEC_C_Q162, tag = "a"),
    attrib_name = "main_source_na",
    attrib_vars = "SEC_C_Q162"
  )

  # questions unanswered that are needed to determine capacity of a solar system
  # NOTE: condition yields a FALSE for solar lantern and solar lighting,
  # where these questions are not asked, but will have a non-tagged NA

  key_solar_capacity_qs <- tibble::tribble(
    ~ df_name, ~ var_name, ~ attrib_name,
    # charge phone
    "main_solar_system",  "SEC_C_Q122", "solar_charge_phone",
    # power light bulb
    "main_solar_system",  "SEC_C_Q123", "solar_charge_lightbulb",
    # run radio
    "main_solar_system",  "SEC_C_Q124", "solar_charge_radio",
  )

  purrr::pwalk(
    .l = key_solar_capacity_qs,
    .f = create_na_attrib_obj
  )

  # power any other device
  attrib_solar_other_na <- main_solar_system |>
    dplyr::mutate(
      all_na = dplyr::if_all(
        .cols = dplyr::starts_with("SEC_C_Q125__"),
        .fns = ~ haven::is_tagged_na(.x, "a")
      )
    ) |>
    susoreview::create_attribute(
      condition = all_na == TRUE,
      attrib_name = "solar_charge_other_na",
      attrib_vars = "SEC_C_Q125"
    )

  # ----------------------------------------------------------------------------
  # Availability
  # ----------------------------------------------------------------------------

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Hours of electricity each day
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  hrs_per_day <- tibble::tribble(
    ~ attrib_name,          ~ var_name,   ~ df_name,
    "nat_grid_hrs_day",     "SEC_C_Q27",  "hholds",
    "mini_grid_hrs_day",    "SEC_C_Q65",  "hholds",
    # NOTE: pending answer from MTF team; how to identify main generator/battery
    # "generator_hrs_day",  "SEC_C_Q93",  "generators",
    # "battery_hrs_day",    "SEC_C_Q111", "batteries",
    "solar_hrs_day",        "SEC_C_Q148", "main_solar_system",
  )

  purrr::pwalk(
    .l = hrs_per_day,
    .f = create_na_attrib_obj
  )

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Hours of electricity each evening
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  hrs_per_evening <- tibble::tribble(
    ~ attrib_name,            ~ var_name,   ~ df_name,
    "nat_grid_hrs_night",     "SEC_C_Q28",  "hholds",
    "mini_grid_hrs_night",    "SEC_C_Q66",  "hholds"
    # ,
    # NOTE: pending answer from MTF team; how to identify main generator/battery
    # "generator_hrs_night",  "SEC_C_Q94",  "generators",
    # "battery_hrs_night",    "SEC_C_Q112", "batteries",
    # NOTE: confirm
  )

  purrr::pwalk(
    .l = hrs_per_evening,
    .f = create_na_attrib_obj
  )

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Connected to electricity
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  elec_sources <- tibble::tribble(
    ~ df_name,  ~ var_name,   ~ attrib_name,
    "hholds",   "SEC_C_Q02",  "uses_nat_grid",
    "hholds",   "SEC_C_Q40",  "uses_mini_grid",
    "hholds",   "SEC_C_Q78",  "uses_generator",
    "hholds",   "SEC_C_Q100",  "uses_batteries",
    "hholds",   "SEC_C_Q117",  "uses_solar",
  )

  purrr::pwalk(
    .l = elec_sources,
    .f = create_bool_attrib_obj
  )

  attrib_has_electricity <- susoreview::create_attribute(
    df = hholds,
    condition = (
      SEC_C_Q02 == 1 |
      SEC_C_Q40 == 1 |
      SEC_C_Q78 == 1 |
      SEC_C_Q100 == 1 |
      SEC_C_Q117 == 1
    ),
    attrib_name = "has_electricity",
    attrib_vars = 
      "(SEC_C_Q02|SEC_C_Q40|SEC_C_Q78|SEC_C_Q100|SEC_C_Q117)"
  )

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Own asset that uses electricity
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  attrib_owns_elec_asset <- 
    susoreview::create_attribute(
      df = dplyr::mutate(
        .data = hholds,
        # exclude battery-based assets
        # 5 - Torch/flashlight/lantern (using dry-cell battery)
        # 6 - Radio/CD Players/sound system (using dry-cell battery)
        # solar-based assets
        # 25 - Solar-based water heater
        # 36 - Solar-based water pump
        # other assets of unknown power source
        # 555 - Other, specify
        owns_elec_asset = dplyr::if_any(
          .cols = c(
            dplyr::matches("SEC_F_Q01__[1-4]$"),
            dplyr::matches("SEC_F_Q01__1[0-9]$"),
            dplyr::matches("SEC_F_Q01__(2[1-4]|2[6-9])$"),
            dplyr::matches("SEC_F_Q01__3[1-5]$")
          ),
          .fns = ~ .x == 1
        )
      ),
      condition = owns_elec_asset == TRUE,
      attrib_name = "owns_elec_asset",
      attrib_vars = "SEC_F_Q01"
    )

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Own asset with heavy and/or continuous draw
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  # collapse from assets roster
  # used in past 6 months: SEC_F_Q02b
  attrib_uses_big_draw_elec_asset <- appliances |>
    dplyr::mutate(
      uses_big_draw_elec_asset = 
      # is big-draw electric asset
      (appliances__id %in% c(
        10, # fridge
        1, # microwave
        14, # hair dryer
        16, # rice cooker
        18, # washing machine
        22, # air conditioning
        27, # electric hot pot/kettle
        30, 31, 32, # TV
        35 # electric water pump (non-solar)
      )) &
      # used in the past 6 months
      SEC_F_Q02b == 1
    ) |>
    dplyr::summarise(
      uses_big_draw_elec_asset = max(uses_big_draw_elec_asset, na.rm = TRUE),
      .by = c(interview__id, interview__key)
    ) |>
    susoreview::create_attribute(
      condition = uses_big_draw_elec_asset == 1,
      attrib_name = "uses_big_draw_elec_asset",
      attrib_vars = "SEC_F_Q01__(1[01468]|2[27]|3[0125])"
    )

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Household business's connection shared with household
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  biz_elec_sources <- tibble::tribble(
    ~ var1_name,     ~var2_name,      ~ attrib_name,
    "SEC_A2_Q33__1", "SEC_A2_Q36__1", "biz_hh_share_nat_grid",
    "SEC_A2_Q33__2", "SEC_A2_Q36__2", "biz_hh_share_mini_grid",
    "SEC_A2_Q33__3", "SEC_A2_Q36__3", "biz_hh_share_generator",
    "SEC_A2_Q33__7", "SEC_A2_Q36__7", "biz_hh_share_batteries",
  ) |>
  dplyr::mutate(df_name = "hholds")

  purrr::pwalk(
    .l = biz_elec_sources,
    .f = create_bool_attrib_obj_w_2_vars
  )

  attrib_biz_hh_share_solar <- susoreview::create_attribute(
    df = hholds,
    condition = (
      (SEC_A2_Q33__4 == 1 | SEC_A2_Q33__5 == 1 | SEC_A2_Q33__6 == 1) &
      (SEC_A2_Q36__4 != 1 & SEC_A2_Q36__5 != 1 & SEC_A2_Q36__6 != 1)
    ),
    attrib_name = "biz_hh_share_solar",
    attrib_vars = "SEC_A2_Q3[36]"
  )

  # ----------------------------------------------------------------------------
  # Reliability
  # ----------------------------------------------------------------------------

  blackout_nums <- tibble::tribble(
    ~ var_name,   ~ attrib_name,
    # grid
    "SEC_C_Q29",  "num_blackout_grid",
    "SEC_C_Q30a", "dur_hrs_blackout_grid",
    "SEC_C_Q30b", "dur_min_blackout_grid",
    # mini-grid
    "SEC_C_Q67",  "num_blackout_mini_grid",
    "SEC_C_Q68a", "dur_hrs_blackout_mini_grid",
    "SEC_C_Q68b", "dur_min_blackout_mini_grid",
  ) |>
  dplyr::mutate(df_name = "hholds")

  # unanswered
  purrr::pwalk(
    .l = blackout_nums,
    .f = create_na_attrib_obj
  )

  # DK
  purrr::pwalk(
    .l = blackout_nums,
    .f = create_dk_attrib_obj
  )

  # ----------------------------------------------------------------------------
  # Quality
  # ----------------------------------------------------------------------------

  critical_damage <- tibble::tribble(
    ~ var_name,   ~ attrib_name,            ~ df_name,
    "SEC_C_Q35",  "damage_equip_grid",      "hholds",
    "SEC_C_Q73",  "damage_equip_mini_grid", "hholds",
    "SEC_C_Q95",  "damage_equip_generator", "main_generator",
  )

  purrr::pwalk(
    .l = critical_damage,
    .f = create_na_attrib_obj
  )

  # ----------------------------------------------------------------------------
  # Affordability
  # ----------------------------------------------------------------------------

  # food
  attrib_num_food_items <- susoreview::count_vars(
    df = hholds,
    var_val = 1,
    var_pattern = "SEC_O_Q01__",
    attrib_name = "num_food_items",
    attrib_vars = "SEC_O_Q01"
  )

  # non-food, 1 month
  attrib_num_nf_1m <- susoreview::count_vars(
    df = hholds,
    var_val = 1,
    var_pattern = "SEC_O_One_Month__",
    attrib_name = "num_nf_1m",
    attrib_vars = "SEC_O_One_Month"
  )

  # non-food, 12 months
  attrib_num_non_food_12m <- susoreview::count_vars(
    df = hholds,
    var_val = 1,
    var_pattern = "SEC_O_Q12_Months__",
    attrib_name = "num_nf_12m",
    attrib_vars = "SEC_O_Q12_Months"
  )

  # ----------------------------------------------------------------------------
  # Formality
  # ----------------------------------------------------------------------------

  who_paid_params <- tibble::tribble(
    ~ var_name,   ~ attrib_name,
    "SEC_C_Q12",  "who_paid_grid",
    "SEC_C_Q48",  "who_paid_mini_grid"
  ) |>
  dplyr::mutate(df_name = "hholds")

  purrr::pwalk(
    .l = who_paid_params,
    .f = create_na_attrib_obj
  )

  # ----------------------------------------------------------------------------
  # Health and safety
  # ----------------------------------------------------------------------------

  death_injury <- tibble::tribble(
    ~ var_name,   ~ attrib_name,      ~ df_name,
    "SEC_C_Q162", "injury_main",      "hholds",
    "SEC_C_Q38", "injury_grid",       "hholds",
    "SEC_C_Q76", "injury_mini_grid",  "hholds",
    "SEC_C_Q98", "injury_generator",  "main_generator",
    "SEC_C_Q116", "injury_batteries", "main_battery",
  )

  purrr::pwalk(
    .l = death_injury,
    .f = create_na_attrib_obj
  )

  # ============================================================================
  # Access to modern cooking solution
  # ============================================================================

  # ----------------------------------------------------------------------------
  # Exposure
  # ----------------------------------------------------------------------------

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # fuel
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  attr_stove_fuel_na <- main_stove |>
    dplyr::mutate(
      stove_fuel_na = dplyr::if_all(
        .cols = dplyr::matches("SEC_G_Q16__"),
        .fns = ~ haven::is_tagged_na(.x, tag = "a")
      )
    ) |>
    susoreview::extract_attribute(
      var = stove_fuel_na,
      attrib_name = "stove_fuel_na",
      attrib_vars = "SEC_G_Q16__"
    )

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # stove design, ventillation
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  exposure_params <- tibble::tribble(
    ~ df_name,      ~ var_name,   ~ attrib_name,
    # stove type
    "main_stove",   "SEC_G_Q05", "stove_type",
    # where cook
    "main_stove",   "SEC_G_Q14", "where_cook",
  )

  purrr::pwalk(
    .l = exposure_params,
    .f = create_na_attrib_obj
  )

  # exhaust system
  attrib_exhaust_sys_na <- main_stove |>
    dplyr::mutate(
      exhaust_sys_na = dplyr::if_all(
        .cols = dplyr::matches("SEC_G_Q15__"),
        .fns = ~ haven::is_tagged_na(.x, tag = "a")
      )
    ) |>
    susoreview::extract_attribute(
      var = exhaust_sys_na,
      attrib_name = "exaust_sys_na",
      attrib_vars = "SEC_G_Q15"
    )

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # contact time
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  time_params  <- tibble::tribble(
    ~ df_name,      ~ var_name,     ~ attrib_name,
    # total time in kitchen
    "hholds",       "SEC_G_Q02",    "time_in_kitchen",
    # avg time per meal
    "main_stove",   "SEC_G_Q18",    "time_per_meal",
    # days used stove in last week
    "main_stove",   "SEC_G_Q19",    "days_use_stove",
    # time per day to boil water
    "main_stove",   "SEC_G_Q21",    "min_boil",
  )

  purrr::pwalk(
    .l = time_params,
    .f = create_na_attrib_obj
  )

  # time per day to cook
  time_cook <- tibble::tribble(
    ~ df_name,    ~ var_name,   ~ attrib_name,
    "main_stove", "SEC_G_Q22a", "min_cook_morn_na",
    "main_stove", "SEC_G_Q22b", "min_cook_aftn_na",
    "main_stove", "SEC_G_Q22c", "min_cook_even_na",
  )

  purrr::pwalk(
    .l = time_cook,
    .f = create_na_attrib_obj
  )

  # ----------------------------------------------------------------------------
  # Efficiency
  # ----------------------------------------------------------------------------

  # N/A ?

  # ----------------------------------------------------------------------------
  # Safety of primary cookstove
  # ----------------------------------------------------------------------------

  attrib_stove_safety_na <- main_stove |>
    susoreview::create_attribute(
      condition = haven::is_tagged_na(SEC_G_Q23, tag = "a"),
      attrib_name = "stove_safety_na",
      attrib_vars = "SEC_G_Q23"
    )

  # ----------------------------------------------------------------------------
  # Affordability
  # ----------------------------------------------------------------------------

  # NOTE: covered in connection affordability above

  # ----------------------------------------------------------------------------
  # Fuel availability
  # ----------------------------------------------------------------------------

  # N/A ?

  # ============================================================================
  # Assorted
  # ============================================================================

  # ----------------------------------------------------------------------------
  # Cookstove type used (in past 12 months)
  # ----------------------------------------------------------------------------

  stove_used <- tibble::tribble(
    ~ val,  ~ stove,
    5,      "lpg",
    6,      "nat_gas",
    7,      "elec",
  )

  main_stove_small <- main_stove |>
    dplyr::mutate(
      in_past_12m = dplyr::if_any(
        .cols = dplyr::matches("SEC_G_Q13__"),
        .fns = ~ .x == 1
      )
    )

  attrib_stove_used <- purrr::pmap(
    .l = stove_used,
    .f = ~ susoreview::create_attribute(
      df = main_stove_small,
      condition = !!rlang::parse_quo(
        x = glue::glue("SEC_G_Q05 == {..1} & in_past_12m == 1"),
        env = rlang::current_env()
      ),
      attrib_name = glue::glue("stove_used_{..2}"),
      attrib_vars = "(SEC_G_Q05|SEC_G_Q13)"
    )
  ) |>
  dplyr::bind_rows()

  # ----------------------------------------------------------------------------
  # Fuel used (in past 12 months)
  # ----------------------------------------------------------------------------

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # in at least one stove
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  create_stove_expr <- function(vals) {

    expr <- glue::glue("SEC_G_Q16__{vals} == 1")

    if (length(expr) > 1) {
      expr <- glue::glue_collapse(x = expr, sep = " & ")
    }

    return(expr)

  }

  stove_inputs <- tibble::tribble(
    ~ attrib_name,        ~ vals,
    "wood",               c(1,2),
    "charcoal",           3,
    "kerosene",           4,
    "coal",               5,
    "anim_waste",         6,
    "plant_biomass",      7,
    "sawdust",            8,
    "coal_briquette",     9,
    "biomass_briquette",  10,
    "processed_biomass",  11,
    "biogas",             12,
    "ethanol",            13,
    "garbage",            14
  ) |>
  dplyr::rowwise(attrib_name) |>
  dplyr::mutate(expr = create_stove_expr(vals = vals)) |>
  dplyr::ungroup()

  attrib_fuel_stove <- purrr::pmap(
    .l = stove_inputs,
    .f = ~ susoreview::any_obs(
      df = stoves,
      where = !!rlang::parse_expr(..3),
      attrib_name = glue::glue("fuel_stove_{..1}"),
      attrib_vars = "SEC_G_Q16"
    )
  ) |>
  dplyr::bind_rows()

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # overall
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  create_fuel_expr <- function(vals) {

    vals_txt <- glue::glue("c({glue::glue_collapse(vals, sep = ', ')})")
    expr <- glue::glue(
      "(fuel_consumption__id %in% {vals_txt}) & (SEC_I_Q03__2 == 1)"
    )

    return(expr)

  }

  fuel_inputs <- tibble::tribble(
    ~ attrib_name,        ~ vals,
    "lpg",                1,
    "wood",               c(2,3),
    "charcoal",           4,
    "kerosene",           5,
    "coal",               7,
    "anim_waste",         8,
    "plant_biomass",      9,
    "sawdust",            10,
    "coal_briquette",     11,
    "biomass_briquette",  12,
    "processed_biomass",  13,
    "biogas",             14,
    "ethanol",            15,
    "garbage",            16
  ) |>
  dplyr::rowwise(attrib_name) |>
  dplyr::mutate(expr = create_fuel_expr(vals = vals)) |>
  dplyr::ungroup()

  attrib_fuel_used <- purrr::pmap(
    .l = stove_inputs,
    .f = ~ susoreview::any_obs(
      df = stoves,
      where = !!rlang::parse_expr(x = ..3),
      attrib_name = glue::glue("fuel_cook_{..1}"),
      attrib_vars = "(SEC_I_Q02|SEC_I_Q03)"
    )
  ) |>
  dplyr::bind_rows()

  # ----------------------------------------------------------------------------
  # Assets powered by solar system
  # ----------------------------------------------------------------------------

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Powered by solar
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  solar_powered <- tibble::tribble(
    ~ attrib_name,  ~ var,
    "phone",        "SEC_C_Q122",
    "radio",        "SEC_C_Q124",
    "tv",           "SEC_C_Q125__1",
    "fan",          "SEC_C_Q125__2",
    "fridge",       "SEC_C_Q125__3",
  )

  attrib_powered_by_solar <- purrr::pmap(
    .l = solar_powered,
    .f = ~ susoreview::any_obs(
      df = solar_systems,
      where = !!rlang::parse_quo(
        x = glue::glue("{..2} == 1"),
        env = rlang::caller_env()
      ),
      attrib_name = glue::glue("powered_by_solar_{..1}"),
      attrib_vars = stringr::str_remove(
        string = ..2,
        pattern = "__[1-3]"
      )
    )
  )

  attrib_powered_by_solar_lights <- susoreview::any_obs(
    df = solar_systems,
    where = (SEC_C_Q123 > 0) & (SEC_C_Q123 != 888),
    attrib_name = "powered_by_solar_lights",
    attrib_vars = "SEC_C_Q123"
  )

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Owned by the household
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  create_asset_expr <- function(vals) {
    expr_txt <- glue::glue("c({glue::glue_collapse(vals, sep = ', ')})")
    return(expr_txt)
  }

  owned_assets <- tibble::tribble(
    ~ attrib_name,  ~ vals,
    # "phone_charger",        c(28, 29),
    "lights",       c(1:4),
    "radio",        c(5, 6),
    "tv",           c(30, 31, 32),
    "fan",          9,
    "fridge",       10,
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(expr = create_asset_expr(vals)) |>
  dplyr::ungroup()

  attrib_asset_owned <- purrr::pmap(
    .l = owned_assets,
    .f = ~ susoreview::any_obs(
      df = appliances,
      where = !!rlang::parse_quo(
        x = glue::glue("appliances__id %in% c({paste(..2, collapse = ', ')})"),
        env = rlang::current_env()
      ),
      attrib_name = glue::glue("asset_owned_{..1}"),
      attrib_vars = "SEC_F_Q01"
    )
  ) |>
  dplyr::bind_rows()

  # ----------------------------------------------------------------------------
  # Backup electricity source
  # ----------------------------------------------------------------------------

  attrib_is_backup_nat_grid <- susoreview::create_attribute(
    df = hholds,
    condition = SEC_C_Q159 == 1, # for lighting for solar
    attrib_name = "is_backup_nat_grid",
    attrib_vars = "SEC_C_Q159"
  )

  attrib_is_backup_mini_grid <- susoreview::create_attribute(
    df = hholds,
    condition = (
      # for national grid
      # first, lighting; then, main for electricity
      (SEC_C_Q31 == 1 | SEC_C_Q32 == 1) |
      # for solar
      (SEC_C_Q159 == 2)
    ),
    attrib_name = "is_backup_mini_grid",
    attrib_vars = "(SEC_C_Q31|SEC_C_Q32|SEC_C_Q159)"
  )

  attrib_is_backup_generator <- susoreview::create_attribute(
    df = hholds,
    condition = (
      # for national grid
      (SEC_C_Q31 == 2 | SEC_C_Q32 == 2) |
      # for mini-grid
      (SEC_C_Q69 == 1 | SEC_C_Q70 == 1) |
      # for solar
      (SEC_C_Q159 == 3)
    ),
    attrib_name = "is_backup_generator",
    attrib_vars = "(SEC_C_Q31|SEC_C_Q32|SEC_C_Q69|SEC_C_Q70|SEC_C_Q159)"
  )

  attrib_is_backup_solar <- susoreview::create_attribute(
    df = hholds,
    condition = (
      # for national grid
      (SEC_C_Q31 %in% c(3:5) | SEC_C_Q32 %in% c(3:5)) |
      # for mini-grid
      (SEC_C_Q69 %in% c(2:4) | SEC_C_Q70 %in% c(2:4))
    ),
    attrib_name = "is_backup_solar",
    attrib_vars = "(SEC_C_Q31|SEC_C_Q32|SEC_C_Q69|SEC_C_Q70)"
  )

  attrib_is_backup_batteries <- susoreview::create_attribute(
    df = hholds,
    condition = (
      # for national grid
      (SEC_C_Q31 == 6 | SEC_C_Q32 == 6) |
      # for mini-grid
      (SEC_C_Q69 == 5 | SEC_C_Q70 == 5) |
      # for solar
      (SEC_C_Q159 == 4)
    ),
    attrib_name = "is_backup_batteries",
    attrib_vars = "(SEC_C_Q31|SEC_C_Q32|SEC_C_Q69|SEC_C_Q70|SEC_C_Q159)"
  )

  # ============================================================================
  # Combine attributes
  # ============================================================================

  # combine all attribute data sets whose names match the pattern below
  attributes <- dplyr::bind_rows(mget(ls(pattern = "^attrib_")))

  return(attributes)

}
