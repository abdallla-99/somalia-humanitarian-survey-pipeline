# -----------------------------------------------------------------------------
# 02_weight_and_analyse.R
# Builds composite indicators, adds design weights and produces weighted
# estimates with 95% confidence intervals that account for strata and clusters.
# -----------------------------------------------------------------------------

source("R/functions.R")

clean <- read_csv("data/clean/household_survey_clean.csv", show_col_types = FALSE)
pop   <- read_csv("data/raw/population_frame_synthetic.csv", show_col_types = FALSE)

analysis <- clean %>%
  add_indicators() %>%
  add_weights(pop %>% select(strata, hh_population))

# Sanity check: weighted household totals must equal the population frame.
check <- analysis %>% group_by(strata) %>% summarise(weighted_hh = sum(weight)) %>%
  left_join(pop, by = "strata")
stopifnot(isTRUE(all.equal(check$weighted_hh, check$hh_population)))

design <- make_design(analysis)
inds <- names(indicator_labels)

results <- bind_rows(
  weighted_results(design, inds) %>% mutate(level = "Overall"),
  weighted_results(design, inds, by = "pop_group") %>% mutate(level = "Population group"),
  weighted_results(design, inds, by = "district") %>% mutate(level = "District"),
  weighted_results(design, inds, by = "strata") %>% mutate(level = "Stratum")
)

# Weighted vs unweighted: shows why weighting matters.
unweighted <- analysis %>%
  summarise(across(all_of(inds), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "indicator", values_to = "unweighted")
comparison <- results %>% filter(level == "Overall") %>%
  select(indicator, label, weighted = estimate) %>%
  left_join(unweighted, by = "indicator") %>%
  mutate(difference_pp = round(100 * (weighted - unweighted), 1))

write_csv(analysis, "data/clean/household_survey_analysis.csv", na = "")
write_csv(results, "outputs/weighted_results.csv")
write_csv(comparison, "outputs/weighted_vs_unweighted.csv")
saveRDS(list(results = results, analysis = analysis,
             cleaning_log = read_csv("outputs/cleaning_log.csv", show_col_types = FALSE)),
        "app/dashboard_data.rds")

message("Overall weighted estimates:")
print(results %>% filter(level == "Overall") %>%
        transmute(label, estimate = round(100 * estimate, 1),
                  ci = sprintf("%.1f-%.1f", 100 * low, 100 * high)), n = Inf)
