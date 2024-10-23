#' Create issues from data attriubtes
#' 
#' @param attributes Data frame of attributes
#' 
#' @return Data frame of issues
#' 
#' @importFrom susoreview create_issue
#' @importFrom tibble tribble
#' @importFrom glue glue
create_issues <- function(attributes) {

  # ============================================================================
  # Access to electricity
  # ============================================================================

  # ----------------------------------------------------------------------------
  # Capacity
  # ----------------------------------------------------------------------------

  # main electricity source unanswered
  issue_main_source_na <- susoreview::create_issue(
    df_attribs = attributes,
    vars = "main_source_na",
    where = main_source_na == 1,
    type = 1,
    desc = "Main source unanswered",
    comment = paste0(
      "ERROR: Main electricity source of electricity left unanswered. ",
      "First, record an answer for question 162 in section C. ",
      "Then, resubmit the interview"
    )
  )

  # number of solar-powered lightbulbs unanswered
  issue_solar_charge_phone_na <- susoreview::create_issue(
    df_attribs = attributes,
    vars = "solar_charge_phone_na",
    where = solar_charge_phone_na == 1,
    type = 1,
    desc = "Number lightbulbs powered by solar unanswered",
    comment = paste0(
      "ERROR: Number of lightbulbs powered by the main solar unanswered. ",
      "First, locate the main solar system in section C. ",
      "Next, record an answer for question 123. ",
      "Then, resubmit the interview."
    )
  )

  # whether phone powered by solar unanswered
  solar_capacity_na <- tibble::tribble(
    ~ var_stub,   ~ noun,   ~ question,
    "phone",      "phone",  "122",
    "lightbulb",  "radio",  "123",
    "other",      "other appliances", "125",
  )

  issue_solar_charge_na <- purrr::pmap(
    .l = solar_capacity_na,
    .f = ~ susoreview::create_issue(
      df_attribs = attributes,
      vars = glue::glue("solar_charge_{..1}_na"),
      where = !!rlang::parse_quo(
        x = glue::glue("solar_charge_{..1}_na == 1"),
        env = rlang::current_env()
      ),
      type = 1,
      desc = glue::glue("Whether {..2} powered by solar"),
      comment = glue::glue(
        "ERROR: Whether {..2} powered by solar unanswered. ",
        "First, locate the main solar system in section C. ",
        "Next, record an answer for question {..3} ",
        "Then, resubmit the interview."
      )
    )
  ) |>
  dplyr::bind_rows()

  # ----------------------------------------------------------------------------
  # Availability
  # ----------------------------------------------------------------------------

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Hours of electricity each day
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  hrs_per_day_na <- tibble::tribble(
    ~ var_stub,           ~ noun,           ~ question,
    "nat_grid_hrs_day",   "national grid",  "27",
    "mini_grid_hrs_day",  "mini grid",      "65",
    "solar_hrs_day",      "main solar system",  "148",
  )

  issue_hrs_per_day_na <- purrr::pmap(
    .l = hrs_per_day_na,
    .f = ~ susoreview::create_issue(
      df_attribs = attributes,
      vars = glue::glue("{..1}_na"),
      where = !!rlang::parse_quo(
        x = glue::glue("{..1}_na == 1"),
        env = rlang::current_env()
      ),
      type = 1,
      desc = glue::glue("Hours per day available from {..2} unanswered"),
      comment = glue::glue(
        "ERROR: Hours per day available from {..2} unanswered.",
        "First, locate the main solar system in section C.",
        "Next, record an answer for question {..3} ",
        "Then, resubmit the interview.",
        .sep = " "
      )
    )
  ) |>
  dplyr::bind_rows()

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Hours of electricity each evening
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  hrs_per_night_na <- tibble::tribble(
    ~ var_stub,           ~ noun,           ~ question,
    "nat_grid_hrs_night",   "national grid",  "29",
    "mini_grid_hrs_night",  "mini grid",      "66",
  )

  issue_hrs_per_night_na <- purrr::pmap(
    .l = hrs_per_night_na,
    .f = ~ susoreview::create_issue(
      df_attribs = attributes,
      vars = glue::glue("{..1}_na"),
      where = !!rlang::parse_quo(
        x = glue::glue("{..1}_na == 1"),
        env = rlang::current_env()
      ),
      type = 1,
      desc = glue::glue("Hours per evening available from {..2} unanswered"),
      comment = glue::glue(
        "ERROR: Hours per evening available from {..2} unanswered",
        "First, locate the main solar system in section C. ",
        "Next, record an answer for question {..3} ",
        "Then, resubmit the interview."
      )
    )
  ) |>
  dplyr::bind_rows()

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Owns asset that uses electricity, but has no electricity access
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  issue_elect_asset_no_elec <- susoreview::create_issue(
    df_attribs = attributes,
    vars = c("owns_elec_asset", "has_electricity"),
    where = owns_elec_asset == 1 & has_electricity == 0, 
    type = 1,
    desc = "Owns electric appliance, but has no electricity",
    comment = paste(
      "ERROR: owns an electric appliance, but reports no access to electricity.",
      "Please check access to electricity sources in section C and ",
      "electric appliance ownership in section F.",
      sep = " "
    )
  )

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Own asset with heavy and/or continuous draw, but has no electricity access
  # - -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Business shares a connection with the household, but househol does not report
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  connections <- tibble::tribble(
    ~ var_stub,   ~ noun,
    "nat_grid",   "national grid",
    "mini_grid",   "mini grid",
    "generator",   "generator",
    "batteries",   "batteries",
  )

  issue_biz_shares_hh_not_report <- purrr::pmap(
    .l = connections,
    .f = ~ susoreview::create_issue(
      df_attribs = attributes,
      vars = c(glue::glue("biz_hh_share_{..1}", "uses_nat_{..1}")),
      where = !!rlang::parse_quo(
        x = glue::glue("biz_hh_share_{..1} == 1 & uses_{..1} == 0"),
        env = rlang::current_env()
      ),
      type = 1,
      desc = glue::glue(
        "Business shares {..2} connection with household, ",
        "but the household does not report it."
      ),
      comment = glue::glue(
        "ERROR: Business {..2} grid connection with household, ",
        "but the household does not report that connection. ",
        "Please check the business electricity access details in section A2",
        "and the household access details in section C."
      )
    )
  ) |>
  dplyr::bind_rows()

  # ----------------------------------------------------------------------------
  # Reliability
  # ----------------------------------------------------------------------------

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Number of outages
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  num_blackout_na <- tibble::tribble(
    ~ var_stub,           ~ noun,               ~ q,
    "num_blackout_grid",      "national grid",  "29",
    "num_blackout_mini_grid", "mini grid",      "67",
  )

  issue_num_blackouts <- purrr::pmap(
    .l = num_blackout_na,
    .f = ~ susoreview::create_issue(
      df_attribs = attributes,
      vars = glue::glue("{..1}_na"),
      where = !!rlang::parse_quo(
        x = glue::glue("{..1}_na == 1"),
        env = rlang::current_env()
      ),
      type = 1,
      desc = glue::glue("Number of {..2} outages not answered"),
      comment = glue::glue(
        "ERROR: Number of {..2} outages/blackouts not answered. ",
        "First, locate the {..2} sub-section in section C. ",
        "Next, record an answer for question {..3} ",
        "Then, resubmit the interview."
      )
    )
  ) |>
  dplyr::bind_rows()

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Duration of outages
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  dur_blackout_na <- tibble::tribble(
    ~ var_stub,                   ~ unit,     ~ noun,           ~ q,
    "dur_hrs_blackout_grid",      "hours",    "national grid",  "30a",
    "dur_min_blackout_grid",      "minutes",  "national grid",  "30a",
    "dur_min_blackout_mini_grid", "minutes",  "mini grid",      "68a",
    "dur_min_blackout_mini_grid", "minutes",  "mini grid",      "68b",
  )

  issue_dur_blackouts_na <- purrr::pmap(
    .l = dur_blackout_na,
    .f = ~ susoreview::create_issue(
      df_attribs = attributes,
      vars = glue::glue("{..1}_na"),
      where = !!rlang::parse_quo(
        x = glue::glue("{..1}_na == 1"),
        env = rlang::current_env()
      ),
      type = 1,
      desc = glue::glue("Duration in {..2} of {..3} outages not answered"),
      comment = glue::glue(
        "ERROR: Duration in {..2} of {..3} outages not answered",
        "First, locate the {..3} sub-section in section C. ",
        "Next, record an answer for question {..4} ",
        "Then, resubmit the interview.",
        .sep = " "
      )
    )
  ) |>
  dplyr::bind_rows()

  # ----------------------------------------------------------------------------
  # Quality
  # ----------------------------------------------------------------------------

  damage_na <- tibble::tribble(
    ~ var_stub,               ~ noun,               ~ q,
    "damage_equip_grid",      "national grid",      "35",
    "damage_equip_mini_grid", "mini grid",          "73",
    "damage_equip_generator", "generator",          "95",
  )

  issue_damage_na <- purrr::pmap(
    .l = damage_na,
    .f = ~ susoreview::create_issue(
      df_attribs = attributes,
      vars = glue::glue("{..1}_na"),
      where = !!rlang::parse_quo(
        x = glue::glue("{..1}_na == 1"),
        env = rlang::current_env()
      ),
      type = 1,
      desc = glue::glue("Damage by {..2} to appliances not answered"),
      comment = glue::glue(
        "ERROR: Damage by {..2} to appliances not answered.",
        "First, locate the {..2} sub-section in section C.",
        "Next, record an answer for question {..3}.",
        "Then, resubmit the interview.",
        .sep = " "
      )
    )
  ) |>
  dplyr::bind_rows()

  # ----------------------------------------------------------------------------
  # Affordability
  # ----------------------------------------------------------------------------

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # No food consumption
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  issue_no_food_cons <- susoreview::create_issue(
    df_attribs = attributes,
    vars = "num_food_items",
    where = num_food_items == 0,
    type = 1,
    desc = "No food consumption",
    comment = paste(
      "ERROR: No food consumption reported in the past 7 days.",
      "This is impossible.",
      "The household must have consumed some food in the past 7 days.",
      "If no food item in the questionnaire matches what the household reports,",
      "consider using 'Other food items'.",
      "First, navigate to section O.",
      "Next, readminister questions 1 - 4.",
      "Then, resubmit the interview.",
      sep = " "
    )
  )

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # No non-food consumption
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  issue_no_non_food_cons <- susoreview::create_issue(
    df_attribs = attributes,
    vars = c("num_nf_1m", "num_nf_12m"),
    where = (num_nf_1m + num_nf_12m) == 0,
    type = 1,
    desc = "No non-food consumption",
    comment = paste(
      "ERROR: No non-food consumption reported in the past 1 month or 12 months.",
      "This is extremely unlikely.",
      "The household must have consumed some-food item in past 12 months.",
      "If no non-food item matches what the household reports,",
      "consider using 'Other' item for either the 1 or 12-month recall.",
      "First, navigate to section O.",
      "Next, readminister the 1- and 12-month recall sections.",
      "Then, resubmit the interview.",
      sep = " "
    )
  )

  # ----------------------------------------------------------------------------
  # Formality
  # ----------------------------------------------------------------------------

  who_paid_na <- tibble::tribble(
    ~ var_name,           ~ noun,             ~ q,
    "who_paid_grid",      "national grid",    "12",
    "who_paid_min_grid",  "mini grid",        "48",
  )

  issue_who_paid_na <- purrr::pmap(
    .l = who_paid_na,
    .f = ~ susoreview::create_issue(
      df_attribs = attributes,
      vars = glue::glue("{..1}_na"),
      where = !!rlang::parse_quo(
        x = glue::glue("{..1}_na == 1"),
        env = rlang::current_env()
      ),
      type = 1,
      desc = glue::glue("Who paid for {..2} not answered"),
      comment = glue::glue(
        "ERROR: Who paid for {..2} not answered.",
        "First, locate the {..2} sub-section in section C. ",
        "Next, record an answer for question {..3} ",
        "Then, resubmit the interview.",
        .sep = " "
      )
    )
  ) |>
  dplyr::bind_rows()

  # ----------------------------------------------------------------------------
  # Health and safety
  # ----------------------------------------------------------------------------

  injury_na <- tibble::tribble(
    ~ var_name,           ~ noun,                     ~ q,
    "injury_grid",        "national grid",            "38",
    "injury_mini_grid",   "mini grid",                "76",
    "injury_generator",   "generator",                "98",
    "injury_batteries",   "batteries",                "116",
  )

  issue_injury_na <- purrr::pmap(
    .l = injury_na,
    .f = ~ susoreview::create_issue(
      df_attribs = attributes,
      vars = glue::glue("{..1}_na"),
      where = !!rlang::parse_quo(
        x = glue::glue("{..1}_na == 1"),
        env = rlang::current_env()
      ),
      type = 1,
      desc = glue::glue("Whether suffered injury/death from {..2} not answered"),
      comment = glue::glue(
        "ERROR: Whether suffered injury/death from {..2} not answered.",
        "First, locate the {..2} sub-section in section C.",
        "Next, record an answer for question {..3}.",
        "Then, resubmit the interview.",
        .sep = " "
      )
    )
  ) |>
  dplyr::bind_rows()

  # ============================================================================
  # Access to modern cooking solutions
  # ============================================================================

  # ----------------------------------------------------------------------------
  # Exposure
  # ----------------------------------------------------------------------------

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # fuel
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  issue_stove_fuel_na <- susoreview::create_issue(
    df_attribs = attributes,
    vars = "stove_fuel_na",
    where = stove_fuel_na == 1,
    type = 1,
    desc = "Fuel(s) used for main cooking stove not answered",
    comment = paste(
      "ERROR: Fuel(s) used for main cooking stove not answered.",
      "First, navigate to section G.",
      "Next, record an answer for question 16.",
      "Then, resubmit the interview.",
      sep = " "
    )
  )

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # stove design
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  issue_stove_type_na <- susoreview::create_issue(
    df_attribs = attributes,
    vars = "stove_type_na",
    where = stove_type_na == 1,
    type = 1,
    desc = "Type of main cooking stove not answered",
    comment = paste(
      "ERROR: Type of main cooking stove not answered.",
      "First, navigate to section G.",
      "Next, record an answer for question 5.",
      "Then, resubmit the interview.",
      sep = " "
    )
  )

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # ventillation
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  # where cook
  issue_where_cook_na <- susoreview::create_issue(
    df_attribs = attributes,
    vars = "where_cook_na",
    where = where_cook_na == 1,
    type = 1,
    desc = "Where normally cook not answered",
    comment = paste(
      "ERROR: Where normally cook not answered.",
      "First, navigate to section G.",
      "Next, record an answer for question 14.",
      "Then, resubmit the interview.",
      sep = " "
    )
  )

  # exhaust system
  issue_exhaust_sys_na <- susoreview::create_issue(
    df_attribs = attributes,
    vars = "exhaust_sys_na",
    where = exhaust_sys_na == 1,
    type = 1,
    desc = "What exhaust system not answered",
    comment = paste(
      "ERROR: What exhaust system not answered.",
      "First, navigate to section G.",
      "Next, record an answer for question 15.",
      "Then, resubmit the interview.",
      sep = " "
    )
  )

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # contact time
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  contact_params <- tibble::tribble(
    ~ var_name,           ~ q,  ~ desc,
    "time_in_kitchen_na", "2",  "Time spent in cooking space",
    "time_per_meal_na", "18",  "Time spent per meal",
    "days_use_stove_na", "19",  "Days in last week using the main cookstove",
    "min_boil_na", "21",  "Minutes per day boiling water with the main cookstove",
    "min_cook_morn_na", "22a",  "Minutes cooking in the morning with the main cookstove",
    "min_cook_aftn_na", "22b",  "Minutes cooking in the afternoon with the main cookstove",
    "min_cook_even_na", "22c",  "Minutes cooking in the evening with the main cookstove",
  )

  issue_contact_time_in_na <- purrr::pmap(
    .l = contact_params,
    .f = ~ susoreview::create_issue(
      df_attribs = attributes,
      vars = ..1,
      where = !!rlang::parse_quo(
        x = glue::glue("{..1} == 1"),
        env = rlang::current_env()
      ),
      type = 1,
      desc = glue::glue("{..3} not answered"),
      comment = glue::glue(
        "ERROR: {..3} not answered.",
        "First, navigate to section G.",
        "Next, record an answer for question {..2}",
        "Then, resubmit the interview.",
        .sep = " "
      )
    )
  ) |>
  dplyr::bind_rows()

  # ----------------------------------------------------------------------------
  # cookstove efficiency
  # ----------------------------------------------------------------------------

  # N/A ?

  # ----------------------------------------------------------------------------
  # safety
  # ----------------------------------------------------------------------------

  issue_cookstove_harm_na <- susoreview::create_issue(
    df_attribs = attributes,
    vars = "stove_safety_na",
    where = stove_safety_na == 1,
    type = 1,
    desc = "Harm/injury for main cook",
    comment = paste(
      "ERROR: What harm/injury from main stove not answered.",
      "First, navigate to section G.",
      "Next, record an answer for question 23.",
      "Then, resubmit the interview.",
      sep = " "
    )
  )

  # ----------------------------------------------------------------------------
  # affordability
  # ----------------------------------------------------------------------------

  # NOTE: covered in connection affordability above

  # ----------------------------------------------------------------------------
  # fuel availability
  # ----------------------------------------------------------------------------

  # ============================================================================
  # Assorted
  # ============================================================================

  # ----------------------------------------------------------------------------
  # Solar powers asset that is not owned
  # ----------------------------------------------------------------------------

  asset_params <- tibble::tribble(
    ~ var_name, ~ q,
    "lights",   "123",
    "radio",    "124",
    "tv",       "124",
    "fan",      "124",
    "fridge",   "124"
  )

  issue_asset_powered_not_owned <- purrr::pmap(
    .l = asset_params,
    .f = ~ susoreview::create_issue(
      df_attribs = attributes,
      vars = c(
        glue::glue("powered_by_solar_{..1}"),
        glue::glue("asset_owned_{..1}")
      ),
      where = !!rlang::parse_quo(
        x = glue::glue("powered_by_solar_{..1} == 1 & asset_owned_{..1} == 0"),
        env = rlang::current_env()
      ),
      type = 1,
      desc = glue::glue("Solar system powers {..1}, but not owned"),
      comment = glue::glue(
        "ERROR: Solar system powers {..1}, but no {..1} owned.",
        "This contradiction needs to be either explained or resolved.",
        "For what the solar system powers, navigate to section C question {..2}",
        "For what the household owns, navigate to section I, question 1.",
        .sep = " "
      )
    )
  ) |>
  dplyr::bind_rows()

  # ----------------------------------------------------------------------------
  # Fuel used in a stove, but not consumed
  # ----------------------------------------------------------------------------

  fuel_use_params <- tibble::tribble(
    ~ var_name,           ~ noun,
    "wood",               "wood (purchased or collected)",
    "charcoal",           "charcoal",
    "kerosene",           "kerosene",
    "coal",               "coal/lignite",
    "anim_waste",         "animal waste/dung",
    "plant_biomass",      "crop residue/plant biomass",
    "sawdust",            "sawdust",
    "coal_briquette",     "coal briquette",
    "biomass_briquette",  "biomass briquette",
    "processed_biomass",  "processed biomass (e.g., pellets, woodchips)",
    "biogas",             "biogas",
    "ethanol",            "ethanol",
    "garbage",            "garbage/plastic",
  )

  issue_fuel_stove_not_consumed <- purrr::pmap(
    .l = fuel_use_params,
    .f = ~ susoreview::create_issue(
      df_attribs = attributes,
      vars = c(
        glue::glue("fuel_stove_{..1}"),
        glue::glue("fuel_cook_{..1}")
      ),
      where = !!rlang::parse_quo(
        x = glue::glue("fuel_stove_{..1} == 1 & fuel_cook_{..1} == 0"),
        env = rlang::current_env()
      ),
      type = 1,
      desc = glue::glue("Use {..2} in cookstove, but not reported for cooking"),
      comment = glue::glue(
        "ERROR: Use {..2} in cookstove, but not reported for as fuel consumed",
        "for cooking.",
        "This contradiction needs to be either explained or resolved.",
        "For fuel used in cookstoves, navigate to section G, question 16 for the",
        "cookstove(s) using {..2} as a fuel.",
        "For fuels consumed overall, navigate to section I, where question 1",
        "captures the fuels consumed and question 3 identifies the use(s).",
        "for each fuel.",
        .sep = " "
      )
    )
  ) |>
  dplyr::bind_rows()

  # ----------------------------------------------------------------------------
  # Fuel consumed for cooking but not reported for a stove
  # ----------------------------------------------------------------------------

  issue_fuel_consumed_not_stove_not <- purrr::pmap(
    .l = fuel_use_params,
    .f = ~ susoreview::create_issue(
      df_attribs = attributes,
      vars = c(
        glue::glue("fuel_stove_{..1}"),
        glue::glue("fuel_cook_{..1}")
      ),
      where = !!rlang::parse_quo(
        x = glue::glue("fuel_cook_{..1} == 1 & fuel_stove_{..1} == 0"),
        env = rlang::current_env()
      ),
      type = 1,
      desc = glue::glue("Consume {..2} for cooking, but not reported in stove"),
      comment = glue::glue(
        "ERROR: Consume {..2} for cooking, but the fuel is not reported being",
        "in any cookstove.",
        "This contradiction needs to be either explained or resolved.",
        "For fuels consumed overall, navigate to section I, where question 1",
        "captures the fuels consumed and question 3 identifies the use(s).",
        "For fuel used in cookstoves, navigate to section G, question 16 for the",
        "cookstove(s) using {..2} as a fuel.",
        "for each fuel.",
        "For a listing of cookstoves used by the household, navigate to section",
        "G, question 3.",
        .sep = " "
      )
    )
  ) |>
  dplyr::bind_rows()

  # ----------------------------------------------------------------------------
  # Backup electricity source not reported as a source
  # ----------------------------------------------------------------------------

  source_params <- tibble::tribble(
    ~ var_stub,   ~ src_q,    ~ bkup_q,                 ~ noun,
    "nat_grid",   "2",        "159",                    "national grid",
    "mini_grid",  "40",       "31, 32, or 159",         "mini grid",
    "generator",  "78",       "31, 32, 69, 70, or 159", "generator",
    "batteries",  "100",      "31, 32, 69, 70, or 159", "batteries",
    "solar",      "117",      "31, 32, 69, or 70",      "solar system",
  )

  issue_backup_not_source <- purrr::pmap(
    .l = source_params,
    .f = ~ susoreview::create_issue(
      df_attribs = attributes,
      vars = c(
        glue::glue("is_backup_{..1}"),
        glue::glue("uses_{..1}")
      ),
      where = !!rlang::parse_quo(
        x = glue::glue("is_backup_{..1} == 1 & uses_{..1} == 0"),
        env = rlang::current_env()
      ),
      type = 1,
      desc = glue::glue("Reports {..4} as a backup, but not as a connection."),
      comment = glue::glue(
        "ERROR: Reports {..4} as a backup (for another connection),",
        "but fails to identify it not as an electricity connection.",
        "This contradiction needs to be resolved for explained.",
        "For backup connections, check questions {..3} in section C.",
        "For {..4} as a connection, check {..2}",
        .sep = " "
      )
    )
  ) |>
  dplyr::bind_rows()

  # =============================================================================
  # Combine all issues
  # =============================================================================

  # combine all issues
  issues <- dplyr::bind_rows(mget(ls(pattern = "^issue_")))

  return(issues)

}
