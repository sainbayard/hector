# Goal: Run Hector, get outputs for temperature and sea level rise rate
# Compare to rate computed using Vermeer and Rahmstorff equations, which accounts
# for temperature change and ice melt
# Compare rates

library(dplyr)
library(hector)

# Pull our variables and scenarios of interest
inputdir <- system.file("input", package = "hector")
vars <- c(GLOBAL_TEMP(), SLR(), SL_RC())
scenarios <- c("rcp26", "rcp45", "rcp60", "rcp85")

# Function to run core and retrieve variables for each scenario
run_core <- function(scenario, vars) {
    ini <- file.path(inputdir, paste0("hector_", scenario, ".ini"))
    core <- newcore(ini)
    run(core)
    ed <- core$enddate
    fetchvars(core, 1949:ed, vars, scenario)
}

# Gather data
data <- lapply(scenarios, run_core, vars)
data <- bind_rows(data)

# V&R Constants
# Including ice melt
T0 <- -0.41 # K
a <- 0.56 # cm / year / K
b <- -4.9 # cm / K

# Derivative of temperature w.r.t. time
# Get temperature data from Hector run
temp <- data %>% filter(variable == "Tgav")

# Find dTdt - just take difference of temperature values since time step is 1 year
dTdt <- diff(temp$value)
# If a value appears in the year 1951, it represents the change from 1950 to 1951
# Need to note the empty first value
dTdt <- append(dTdt, NA, after = 0)

# Equation from V&R to find dHdt
# temp$value is Tgav
dHdt <- a*(temp$value - T0) + (b*dTdt)

# Organize data
SLR_data <- data %>%
    filter(variable == "sl_rc") %>%
    mutate(dHdt = dHdt) %>%
    # Difference between Hector and V&R (+: V&R > Hector)
    mutate(diff = dHdt - value) %>%
    rename(sl_rc = value) %>%
    select(-variable) %>%
    relocate(units, .after = diff) %>%
    slice(-1)

