item_cvi <- function(data = NULL, expert_id = NULL, high_values = 1) {
  # Basic checks
  if (is.null(data) || is.null(expert_id)) {
    stop("You must provide a dataframe and the name of the column that identifies the experts.")
  }

  # Check if the expert_id column exists in the dataframe
  if (!expert_id %in% names(data)) {
    stop("The specified expert_id column is not present in the dataframe.")
  }

  # Select only the item columns (exclude the expert_id column)
  item_cols <- setdiff(names(data), expert_id)

  # Recode scores: high_values → 1, other values → 0, NA stays NA
  data_recoded <- data |>
    dplyr::mutate(dplyr::across(dplyr::all_of(item_cols), ~ dplyr::case_when(
      .x %in% high_values ~ 1,
      !.x %in% high_values & !is.na(.x) ~ 0,
      TRUE ~ NA_real_
    )))

  # Convert to long format and calculate CVI for each item
  result <- data_recoded |>
    tidyr::pivot_longer(cols = dplyr::all_of(item_cols),
                        names_to = "item",
                        values_to = "score") |>
    dplyr::group_by(item) |>
    dplyr::summarise(
      n_experts = sum(!is.na(score)),
      n_valid = sum(score, na.rm = TRUE),
      cvi = n_valid / n_experts,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      min_acceptable = dplyr::case_when(
        n_experts == 2 ~ 1,
        n_experts >= 3 & n_experts <= 5 ~ 0.83,
        n_experts >= 6 & n_experts <= 8 ~ 0.83,
        n_experts >= 9 ~ 0.78,
        TRUE ~ NA_real_
      ),
      acceptable = cvi >= min_acceptable
    )

  return(result)
}

