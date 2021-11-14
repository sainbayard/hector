library(readr)
library(dplyr)

dat <- list()
dat[["rcp45"]] <- read_csv("~/Desktop/outputstream_rcp45.csv", skip = 1)
dat[["rcp45luc_pavement=1.0"]] <- read_csv("~/Desktop/outputstream_rcp45_luc1.0.csv", skip = 1)
dat[["rcp45luc_pavement=0.5"]] <- read_csv("~/Desktop/outputstream_rcp45_luc0.5.csv", skip = 1)
dat <- bind_rows(dat, .id = "which")

read_csv("~/Desktop/luc45.csv") %>%
    mutate(which = "rcp45", spinup = 0, variable = "luc_emissions") %>%
    select(which, spinup, year, variable, value = luc_emissions) ->
    luc

dat <- bind_rows(dat, luc)

library(ggplot2)
theme_set(theme_bw())
dat %>%
    filter(variable %in% c("npp", "Tgav", "veg_c", "luc_emissions")) %>%
    filter(!spinup) %>%
    ggplot(aes(year, value, color = which, size = which)) +
    geom_line() +
    scale_size_manual(values = c(1.5, 0.5, 0.5)) +
    facet_wrap(~variable, scales = "free")
