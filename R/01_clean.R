# -----------------------------------------------------------------------------
# 01_clean.R
# Extract -> check -> log decisions -> apply -> verify -> save clean data.
# Every change to the raw data is written to outputs/cleaning_log.csv.
# -----------------------------------------------------------------------------

source("R/functions.R")

raw <- read_csv("data/raw/kobo_export_synthetic.csv", show_col_types = FALSE,
                col_types = cols(.default = col_guess(), water_source_other = col_character()))

# 1. Run all checks -----------------------------------------------------------
flags <- bind_rows(
  check_duplicates(raw),
  check_duration(raw, min_minutes = 15),
  check_outliers(raw, c("water_litres_day", "income_last_month_usd", "hh_size")),
  check_logic(raw),
  check_other(raw, "water_source", "water_source_other")
)

# 2. Decide what to do with each flag ----------------------------------------
# In a live assessment these decisions come from follow-up with the field team.
# Here the rules are written down so every decision is reproducible.
recode_other <- function(txt) {
  t <- tolower(txt)
  case_when(
    grepl("berkad", t)                      ~ "unprotected_well",  # open rainwater reservoir
    grepl("donkey|cart", t)                 ~ "water_trucking",
    grepl("tap", t)                         ~ "piped",
    TRUE                                    ~ "other"
  )
}

log <- flags %>%
  mutate(
    change_type = case_when(
      check_id == "duplicate"                         ~ "remove_duplicate",
      check_id == "duration"                          ~ "remove_survey",
      check_id == "outlier" & variable == "hh_size"   ~ "no_action",       # large households are plausible
      check_id == "outlier"                           ~ "blank_response",
      check_id == "logic_u5"                          ~ "blank_response",
      check_id == "logic_displaced"                   ~ "change_response",
      check_id == "other"                             ~ "change_response"
    ),
    new_value = case_when(
      check_id == "logic_displaced" ~ "0",
      check_id == "other"           ~ recode_other(old_value),
      TRUE                          ~ NA_character_
    ),
    # "other" texts that match no existing option stay as "other".
    change_type = ifelse(check_id == "other" & new_value == "other", "no_action", change_type)
  )

# 3. Apply and verify -----------------------------------------------------------
clean <- apply_cleaning_log(raw, log)
verify_cleaning(raw, clean, log)

dir.create("data/clean", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)
write_csv(log, "outputs/cleaning_log.csv", na = "")
write_csv(clean, "data/clean/household_survey_clean.csv", na = "")

summary_tbl <- log %>% count(check_id, change_type, name = "n")
write_csv(summary_tbl, "outputs/cleaning_summary.csv")
message(sprintf("Raw submissions: %d | Clean surveys: %d | Log entries: %d",
                nrow(raw), nrow(clean), nrow(log)))
print(summary_tbl)
