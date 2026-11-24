# -----------------------------------------------------------------------------
# functions.R
# Reusable functions for checking, cleaning, weighting and analysing a
# KoboToolbox household survey. Sourced by the numbered scripts and the app.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(survey)
})

# ---- Data-quality checks ------------------------------------------------------
# Every check returns rows in the same cleaning-log format:
# uuid | variable | old_value | issue | check_id

log_row <- function(uuid, variable, old_value, issue, check_id) {
  tibble(
    uuid = as.character(uuid),
    variable = variable,
    old_value = as.character(old_value),
    issue = issue,
    check_id = check_id
  )
}

# Duplicate submissions: keep the first, flag the rest.
check_duplicates <- function(df, id = "uuid") {
  dup <- df[duplicated(df[[id]]), ]
  if (nrow(dup) == 0) return(log_row(character(), character(), character(), character(), character()))
  log_row(dup[[id]], id, dup[[id]], "Duplicate submission", "duplicate")
}

# Interviews too short to have been done properly.
check_duration <- function(df, min_minutes = 15) {
  mins <- as.numeric(difftime(df$end, df$start, units = "mins"))
  bad <- df[!is.na(mins) & mins < min_minutes, ]
  bad_mins <- mins[!is.na(mins) & mins < min_minutes]
  log_row(bad$uuid, "duration_min", round(bad_mins, 1),
          paste0("Interview shorter than ", min_minutes, " minutes"), "duration")
}

# Numeric outliers using the interquartile-range rule (log scale for skewed data).
check_outliers <- function(df, vars, k = 3, log_scale = TRUE) {
  bind_rows(lapply(vars, function(v) {
    x <- df[[v]]
    xt <- if (log_scale) log1p(x) else x
    q <- quantile(xt, c(.25, .75), na.rm = TRUE)
    iqr <- q[2] - q[1]
    out <- !is.na(xt) & (xt < q[1] - k * iqr | xt > q[2] + k * iqr)
    log_row(df$uuid[out], v, x[out], "Statistical outlier (IQR rule)", "outlier")
  }))
}

# Logical inconsistencies between related answers.
check_logic <- function(df) {
  bind_rows(
    {
      bad <- df %>% filter(n_children_u5 > hh_size)
      log_row(bad$uuid, "n_children_u5", bad$n_children_u5,
              "More children under 5 than household members", "logic_u5")
    },
    {
      bad <- df %>% filter(pop_group == "host" & displaced_months > 0)
      log_row(bad$uuid, "displaced_months", bad$displaced_months,
              "Host household reports months of displacement", "logic_displaced")
    }
  )
}

# "Other, specify" answers that should be recoded to an existing option.
check_other <- function(df, var, other_var) {
  bad <- df %>% filter(.data[[var]] == "other", !is.na(.data[[other_var]]))
  log_row(bad$uuid, var, bad[[other_var]], "'Other' answer to review", "other")
}

# ---- Cleaning log -------------------------------------------------------------
# The log records a decision for every flag, so every change is traceable:
#   remove_survey   -> drop the interview
#   change_response -> replace old_value with new_value
#   blank_response  -> set the value to NA
#   no_action       -> keep as is (value was checked and is plausible)

#   remove_duplicate -> drop the extra copies of a submission, keep the first

apply_cleaning_log <- function(df, log) {
  if (any(log$change_type == "remove_duplicate")) df <- df[!duplicated(df$uuid), ]
  to_remove <- log %>% filter(change_type == "remove_survey") %>% pull(uuid) %>% unique()
  df <- df %>% filter(!uuid %in% to_remove)

  changes <- log %>% filter(change_type %in% c("change_response", "blank_response"))
  for (i in seq_len(nrow(changes))) {
    ch <- changes[i, ]
    rows <- df$uuid == ch$uuid
    if (!any(rows)) next
    old_class <- class(df[[ch$variable]])[1]
    new_val <- if (ch$change_type == "blank_response") NA else ch$new_value
    df[[ch$variable]][rows] <- switch(old_class,
      numeric = as.numeric(new_val),
      integer = as.integer(new_val),
      new_val
    )
  }
  df
}

# Check that the log was applied correctly (used as a final QA step).
verify_cleaning <- function(raw, clean, log) {
  removed <- unique(log$uuid[log$change_type == "remove_survey"])
  expected_n <- length(setdiff(unique(raw$uuid), removed))
  changed <- log %>% filter(change_type == "change_response", uuid %in% clean$uuid)
  applied <- vapply(seq_len(nrow(changed)), function(i) {
    identical(as.character(clean[[changed$variable[i]]][clean$uuid == changed$uuid[i]]),
              as.character(changed$new_value[i]))
  }, logical(1))
  stopifnot(
    "Duplicate uuids remain"            = !any(duplicated(clean$uuid)),
    "Removed surveys are still present" = !any(clean$uuid %in% removed),
    "Unexpected number of surveys"      = nrow(clean) == expected_n,
    "A change in the log was not applied" = all(applied)
  )
  invisible(TRUE)
}

# ---- Composite indicators -----------------------------------------------------

# WFP Food Consumption Score (7-day recall) with standard weights and the
# 21 / 35 thresholds.
add_fcs <- function(df) {
  df %>%
    mutate(
      fcs = fcs_cereals * 2 + fcs_pulses * 3 + fcs_veg * 1 + fcs_fruit * 1 +
        fcs_meat * 4 + fcs_dairy * 4 + fcs_sugar * 0.5 + fcs_oil * 0.5,
      fcs_cat = cut(fcs, c(-Inf, 21, 35, Inf),
                    labels = c("Poor", "Borderline", "Acceptable"), right = TRUE)
    )
}

# Reduced Coping Strategies Index (rCSI) with standard severity weights.
add_rcsi <- function(df) {
  df %>%
    mutate(
      rcsi = rcsi_less_preferred * 1 + rcsi_borrow * 2 + rcsi_limit_portions * 1 +
        rcsi_restrict_adults * 3 + rcsi_reduce_meals * 1,
      rcsi_high = rcsi >= 19
    )
}

add_indicators <- function(df) {
  improved_sources <- c("piped", "borehole", "protected_well", "water_trucking")
  df %>%
    add_fcs() %>%
    add_rcsi() %>%
    mutate(
      ind_fcs_poor_borderline = fcs_cat %in% c("Poor", "Borderline"),
      ind_rcsi_high = rcsi_high,
      ind_improved_water = water_source %in% improved_sources,
      ind_water_15l = (water_litres_day / hh_size) >= 15,
      ind_female_headed = hoh_gender == "female",
      ind_received_aid = received_aid_3m == "yes",
      ind_shelter_inadequate = shelter_type %in% c("buul", "tent", "none")
    )
}

indicator_labels <- c(
  ind_fcs_poor_borderline = "Poor or borderline food consumption (FCS)",
  ind_rcsi_high          = "High use of food coping strategies (rCSI >= 19)",
  ind_improved_water     = "Main water source is improved",
  ind_water_15l          = "At least 15 litres of water per person per day",
  ind_female_headed      = "Female-headed household",
  ind_received_aid       = "Received humanitarian assistance in last 3 months",
  ind_shelter_inadequate = "Living in a buul, tent or no shelter"
)

# ---- Sampling weights ---------------------------------------------------------
# Stratified two-stage cluster sample: strata = district x population group,
# PSUs = settlements / IDP sites. Design weight = households in stratum /
# households interviewed in stratum, so weighted totals match the population.

add_weights <- function(df, pop) {
  df %>%
    count(strata, name = "n_sampled") %>%
    left_join(pop, by = "strata") %>%
    mutate(weight = hh_population / n_sampled) %>%
    select(strata, weight) %>%
    right_join(df, by = "strata")
}

make_design <- function(df) {
  svydesign(ids = ~cluster_id, strata = ~strata, weights = ~weight,
            data = df, nest = TRUE)
}

# Weighted proportion with 95% CI for each indicator, overall and by group.
weighted_results <- function(design, indicators, by = NULL) {
  bind_rows(lapply(indicators, function(ind) {
    f <- as.formula(paste0("~I(as.numeric(", ind, "))"))
    if (is.null(by)) {
      est <- svymean(f, design, na.rm = TRUE)
      ci <- confint(est)
      tibble(group = "All", indicator = ind,
             estimate = as.numeric(coef(est)), low = ci[1, 1], high = ci[1, 2])
    } else {
      est <- svyby(f, as.formula(paste0("~", by)), design, svymean,
                   na.rm = TRUE, vartype = "ci")
      tibble(group = as.character(est[[by]]), indicator = ind,
             estimate = est[[2]], low = est$ci_l, high = est$ci_u)
    }
  })) %>%
    mutate(
      label = indicator_labels[indicator],
      across(c(estimate, low, high), ~ pmin(pmax(.x, 0), 1))
    )
}
