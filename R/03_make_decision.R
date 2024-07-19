create_decisions <- function(proj_dir, cases_to_review, issues) {

  # ===========================================================================
  # Set paths
  # ===========================================================================

  # data inputs
  data_dir <- fs::path(proj_dir, "01_data", "01_hhold")
  combined_dir <- fs::path(data_dir, "02_combined")
  derived_dir <- fs::path(data_dir, "03_derived")

  # decisions outputs
  output_dir <- fs::path(proj_dir, "02_recommendations", "01_hholds")

  # ===========================================================================
  # Set other parameters
  # ===========================================================================

  issue_types_to_reject <- c(1)

  # ===========================================================================
  # Ingest data
  # ===========================================================================

  # combined data
  comments <- haven::read_dta(fs::path(combined_dir, "interview__comments.dta"))
  suso_diagnostics <- haven::read_dta(
    fs::path(combined_dir, "interview__diagnostics.dta")
  )
  suso_errors <- haven::read_dta(
    fs::path(combined_dir, "interview__errors.dta")
  )

  # ===========================================================================
  # Add issues from interview metadata
  # ===========================================================================

  # ---------------------------------------------------------------------------
  # ... if questions left unanswered
  # ---------------------------------------------------------------------------

  # extract number of questions unanswered
  # use `interview__diagnostics` file rather than request stats from API
  interview_stats <- suso_diagnostics |>
    # rename to match column names from GET /api/v1/interviews/{id}/stats
    dplyr::rename(
      NotAnswered = n_questions_unanswered,
      WithComments = questions__comments,
      Invalid = entities__errors
    ) |>
    dplyr::select(
      interview__id, interview__key,
      NotAnswered, WithComments, Invalid
    )

  # add error if interview completed, but questions left unanswered
  # returns issues data supplemented with unanswered question issues
  issues_plus_unanswered <- susoreview::add_issue_if_unanswered(
    df_cases_to_review = cases_to_review,
    df_interview_stats = interview_stats,
    df_issues = issues,
    n_unanswered_ok = 0,
    issue_desc = "Questions left unanswered",
    issue_comment = glue::glue(
      "ERROR: The interview was marked as complete, but {NotAnswered}",
      "questions were left unanswered.",
      "Please answer these questions.",
      .sep = " "
    )
  )

  # ---------------------------------------------------------------------------
  # ... if any SuSo errors
  # ---------------------------------------------------------------------------

  # add issue if there are SuSo errors
  issues_plus_miss_and_suso <- susoreview::add_issues_for_suso_errors(
    df_cases_to_review = cases_to_review,
    df_errors = suso_errors,
    issue_type = 3,
    df_issues = issues_plus_unanswered
  )

  # ===========================================================================
  # Make decisions
  # ===========================================================================

  # check for comments
  # returns a data frame of cases that contain comments
  interviews_with_comments <- susoreview::check_for_comments(
    df_comments = comments,
    df_issues = issues_plus_miss_and_suso,
    df_cases_to_review = cases_to_review
  )

  # decide what action to take
  decisions <- susoreview::decide_action(
    df_cases_to_review = cases_to_review,
    df_issues = issues_plus_miss_and_suso,
    issue_types_to_reject = issue_types_to_reject,
    df_has_comments = interviews_with_comments,
    df_interview_stats = interview_stats
  )

  # add rejection messages
  to_reject <- decisions[["to_reject"]]

  to_reject <- susoreview::add_rejection_msgs(
    df_to_reject = to_reject,
    df_issues = issues_plus_miss_and_suso
  )

  # flag persistent issues
  revised_decisions <- susoreview::flag_persistent_issues(
    df_comments = comments,
    df_to_reject = to_reject
  )

  # ===========================================================================
  # Extract decisions into data representing them
  # ===========================================================================

  # ---------------------------------------------------------------------------
  # To reject
  # ---------------------------------------------------------------------------

  to_reject_ids <- revised_decisions[["to_reject"]] |>
    dplyr::select(interview__id) |>
    dplyr::left_join(cases_to_review, by = "interview__id")

  to_reject_issues <- to_reject_ids |>
    dplyr::left_join(
      issues_plus_miss_and_suso,
      by = c("interview__id", "interview__key")
    ) |>
    dplyr::filter(issue_type %in% c(issue_types_to_reject, 2)) |>
    dplyr::select(
      interview__id, interview__key, interview__status,
      dplyr::starts_with("issue_")
    )

  to_reject_api <- revised_decisions[["to_reject"]]

  # ---------------------------------------------------------------------------
  # To review
  # ---------------------------------------------------------------------------

  to_review_ids <- decisions[["to_review"]]

  to_review_issues <- to_review_ids |>
    dplyr::left_join(
      issues_plus_miss_and_suso,
      by = c("interview__id", "interview__key")
    ) |>
    dplyr::filter(issue_type %in% c(issue_types_to_reject, 4)) |>
    dplyr::select(
      interview__id, interview__key, interview__status,
      dplyr::starts_with("issue_")
    )

  to_review_api <- susoreview::add_rejection_msgs(
    df_to_reject = decisions[["to_review"]],
    df_issues = issues_plus_miss_and_suso
  )

  # ---------------------------------------------------------------------------
  # To follow up
  # ---------------------------------------------------------------------------

  to_follow_up_ids <- revised_decisions[["to_follow_up"]] |>
    dplyr::left_join(cases_to_review, by = "interview__id") |>
    dplyr::select(interview__id, interview__key)

  to_follow_up_issues <- revised_decisions[["to_follow_up"]] |>
    dplyr::left_join(issues_plus_miss_and_suso, by = "interview__id") |>
    dplyr::left_join(
      cases_to_review,
      by = c("interview__id", "interview__key")
    ) |>
    dplyr::select(
      interview__id, interview__key, interview__status,
      dplyr::starts_with("issue_")
    )

  to_follow_up_api <- revised_decisions[["to_follow_up"]]

  # ===========================================================================
  # Collect decisions in a named list
  # ===========================================================================

  decisions_list <- list(
    # to reject
    to_reject_ids = to_reject_ids,
    to_reject_issues = to_reject_issues,
    to_reject_api = to_reject_api,
    # to review
    to_review_ids = to_review_ids,
    to_review_issues = to_review_issues,
    to_review_api = to_review_api,
    # to follow up
    to_follow_up_ids = to_follow_up_ids,
    to_follow_up_issues = to_follow_up_issues,
    to_follow_up_api = to_follow_up_api
  )

  return(decisions_list)

}
