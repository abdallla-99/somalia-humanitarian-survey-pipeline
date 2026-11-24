# Runs the whole pipeline from the project root:  Rscript run_all.R
# Then open the dashboard with:  shiny::runApp("app")

source("R/00_simulate_kobo_data.R")
source("R/01_clean.R")
source("R/02_weight_and_analyse.R")
message("Done. Launch the dashboard with shiny::runApp('app')")
