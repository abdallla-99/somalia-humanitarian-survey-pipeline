# -----------------------------------------------------------------------------
# 00_simulate_kobo_data.R
# Creates a SYNTHETIC household survey in the shape of a KoboToolbox export,
# plus a synthetic population frame. No real respondents are included.
#
# Design: 4 districts x 2 population groups (IDP / host) = 8 strata.
#         8 clusters (settlements or IDP sites) per stratum, 12 households each.
# Errors are planted on purpose (duplicates, rushed interviews, outliers,
# logic errors, "other" answers) so the cleaning step has real work to do.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({ library(dplyr); library(readr) })
set.seed(2026)

districts  <- c("Banadir", "Baidoa", "Kismayo", "Beledweyne")
groups     <- c("idp", "host")
clusters_per_stratum <- 8
hh_per_cluster <- 12

# Synthetic population frame (households per stratum).
pop <- expand.grid(district = districts, pop_group = groups, stringsAsFactors = FALSE) %>%
  mutate(
    strata = paste(district, pop_group, sep = "_"),
    hh_population = c(98000, 41000, 26000, 18000,     # IDP households
                      310000, 52000, 47000, 33000)    # host households
  )

frame <- pop %>%
  rowwise() %>%
  do(data.frame(
    district = .$district, pop_group = .$pop_group, strata = .$strata,
    cluster_id = rep(paste0(.$strata, "_C", sprintf("%02d", 1:clusters_per_stratum)),
                     each = hh_per_cluster),
    stringsAsFactors = FALSE
  )) %>%
  ungroup()

n <- nrow(frame)
is_idp <- frame$pop_group == "idp"
district_shift <- c(Banadir = 0, Baidoa = 0.6, Kismayo = 0.3, Beledweyne = 0.4)[frame$district]
cluster_effect <- rnorm(length(unique(frame$cluster_id)), 0, 0.35)[match(frame$cluster_id, unique(frame$cluster_id))]
vulnerability <- 0.9 * is_idp + district_shift + cluster_effect + rnorm(n, 0, 0.5)

pdays <- function(base) pmin(7, pmax(0, round(base + rnorm(n, 0, 1.2))))
pick_option <- function(options, probs_idp, probs_host) {
  vapply(is_idp, function(x) sample(options, 1, prob = if (x) probs_idp else probs_host), character(1))
}

start <- as.POSIXct("2026-10-05 07:30", tz = "Africa/Mogadishu") +
  sort(sample(0:(20 * 24 * 3600), n))
duration_min <- pmax(18, round(rnorm(n, 38, 9)))

hh_size <- pmax(1, round(rpois(n, 6.2 + 0.8 * is_idp)))

raw <- frame %>%
  mutate(
    uuid = sprintf("%08x-%04x-4%03x-a%03x-%012x",
                   sample.int(2^31 - 1, n), sample.int(65535, n), sample.int(4095, n),
                   sample.int(4095, n), sample.int(2^31 - 1, n)),
    start = start,
    end = start + duration_min * 60,
    enumerator_id = sprintf("ENUM%02d", sample(1:24, n, replace = TRUE)),
    hoh_gender = ifelse(runif(n) < 0.28 + 0.12 * is_idp, "female", "male"),
    hh_size = hh_size,
    n_children_u5 = pmin(hh_size - 1, rpois(n, 1.4 + 0.3 * is_idp)),
    displaced_months = ifelse(is_idp, sample(1:84, n, replace = TRUE), 0),
    water_source = pick_option(c("piped", "borehole", "protected_well", "water_trucking",
                          "unprotected_well", "surface_water", "other"),
                        c(.10, .22, .12, .25, .17, .09, .05),
                        c(.38, .25, .14, .06, .10, .04, .03)),
    water_source_other = NA_character_,
    water_litres_day = round(pmax(10, rlnorm(n, log(hh_size * (17 - 4 * vulnerability)), 0.35))),
    shelter_type = pick_option(c("permanent", "semi_permanent", "buul", "tent", "none"),
                        c(.03, .15, .62, .17, .03),
                        c(.48, .37, .12, .02, .01)),
    received_aid_3m = ifelse(runif(n) < plogis(-1.2 + 1.4 * is_idp + 0.3 * vulnerability), "yes", "no"),
    fcs_cereals = pdays(6.5), fcs_pulses = pdays(2.6 - 0.7 * vulnerability),
    fcs_veg = pdays(2.4 - 0.8 * vulnerability), fcs_fruit = pdays(1.1 - 0.5 * vulnerability),
    fcs_meat = pdays(1.6 - 0.7 * vulnerability), fcs_dairy = pdays(3.2 - 1.1 * vulnerability),
    fcs_sugar = pdays(5.5), fcs_oil = pdays(5.8 - 0.5 * vulnerability),
    rcsi_less_preferred = pdays(2.5 + 1.4 * vulnerability), rcsi_borrow = pdays(1.0 + 1.0 * vulnerability),
    rcsi_limit_portions = pdays(1.8 + 1.3 * vulnerability), rcsi_restrict_adults = pdays(0.6 + 0.9 * vulnerability),
    rcsi_reduce_meals = pdays(1.6 + 1.2 * vulnerability),
    income_last_month_usd = round(rlnorm(n, log(140 - 35 * pmin(vulnerability, 2.5)), 0.6))
  )

# ---- Plant realistic errors ---------------------------------------------------
others <- which(raw$water_source == "other")
raw$water_source_other[others] <- sample(c("berkad", "Berkad (rain water tank)", "bought from donkey cart",
                                           "mosque tap", "neighbour's tap"), length(others), replace = TRUE)

rushed <- sample(n, 14)
raw$end[rushed] <- raw$start[rushed] + sample(6:13, 14, replace = TRUE) * 60

out_rows <- sample(n, 6)
raw$water_litres_day[out_rows[1:3]] <- c(4000, 2500, 9000)
raw$income_last_month_usd[out_rows[4:6]] <- c(15000, 9800, 22000)

logic_rows <- sample(n, 5)
raw$n_children_u5[logic_rows] <- raw$hh_size[logic_rows] + sample(1:3, 5, replace = TRUE)
host_rows <- sample(which(!is_idp), 4)
raw$displaced_months[host_rows] <- sample(3:24, 4)

raw <- bind_rows(raw, raw[sample(n, 6), ])           # duplicate submissions
raw <- raw[sample(nrow(raw)), ]                       # Kobo exports are unordered

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
write_csv(raw, "data/raw/kobo_export_synthetic.csv", na = "")
write_csv(pop %>% select(strata, district, pop_group, hh_population), "data/raw/population_frame_synthetic.csv")
message("Wrote ", nrow(raw), " synthetic submissions to data/raw/")
