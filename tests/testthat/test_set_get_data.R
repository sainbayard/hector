context("Setting and getting data")

emissions <- c(
    EMISSIONS_BC(),
    EMISSIONS_C2F6(),
    EMISSIONS_CCL4(),
    EMISSIONS_CF4(),
    EMISSIONS_CFC11(),
    EMISSIONS_CFC113(),
    EMISSIONS_CFC114(),
    EMISSIONS_CFC115(),
    EMISSIONS_CFC12(),
    EMISSIONS_CH3BR(),
    EMISSIONS_CH3CCL3(),
    EMISSIONS_CH3CL(),
    EMISSIONS_CH4(),
    EMISSIONS_CO(),
    EMISSIONS_HALON1211(),
    EMISSIONS_HALON1301(),
    EMISSIONS_HALON2402(),
    EMISSIONS_HCFC141B(),
    EMISSIONS_HCFC142B(),
    EMISSIONS_HCFC22(),
    EMISSIONS_HFC125(),
    EMISSIONS_HFC134A(),
    EMISSIONS_HFC143A(),
    EMISSIONS_HFC227EA(),
    EMISSIONS_HFC23(),
    EMISSIONS_HFC245FA(),
    EMISSIONS_HFC32(),
    EMISSIONS_HFC4310(),
    EMISSIONS_N2O(),
    EMISSIONS_NMVOC(),
    EMISSIONS_NOX(),
    EMISSIONS_OC(),
    EMISSIONS_SF6(),
    EMISSIONS_SO2(),
    FFI_EMISSIONS(),
    LUC_EMISSIONS(),
    NAT_EMISSIONS_N2O()
)

# Select scenario, year, and variables
ini_file <- system.file("input", "hector_rcp45.ini", package = "hector")
comparisonn_vars  <- c(GLOBAL_TEMP(), RF_TOTAL())

# Change the emissions for this data and then check to make sure the
# change in emissions is regiestered by the Hector core and affects
# the proper output (temp and total rf).
year <- 1800

# Run a default Hector core and get some default output to use for comparison.
hc <- newcore(ini_file, suppresslogging = TRUE)
invisible(run(hc, year))
default_out <- fetchvars(hc, year, vars = comparisonn_vars)[["value"]]
shutdown(hc)

# Setting emissions variables
for (v in emissions) {

    # Set up the Hector core, a fresh core must be used to test each emission species individually.
    hc <- newcore(ini_file, suppresslogging = TRUE)

    # Use a random (positive) value here to make sure we are getting and setting
    # it consistently.
    val <- rexp(1, 0.2)
    unit <- getunits(v)
    test_that(paste0("Setting variable ", v, " works."), {
        expect_silent(setvar(hc, year, v, val, unit))
    })

    reset(hc)
    invisible(run(hc, year))

    test_that(paste0("Getting variable ", v, " works."), {
        expect_equal(fetchvars(hc, year, v)[["value"]], val)
    })

    new_out <- fetchvars(hc, year, vars = comparisonn_vars)[["value"]]
    test_that(paste0("Downstream changes from ", v, " works."), {
        # If this fails the test then it means that chaning the emissions
        # has no impact on the Tgav or total RF.
        expect_true(all(abs(new_out - default_out) > 0))
    })

}
