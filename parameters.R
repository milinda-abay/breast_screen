# Winter parameters

get_I <- function(cur_I = NULL) {

    if (is.null(cur_I)) {
        cur_I <- 0.005
    }

    new_value <- -1
    while (new_value < 0) {
        # we want the parameter value to be positive
        new_value <- cur_I + rnorm(n = 1, mean = 0, sd = 0.1)
    }

    new_value
}

get_CB <- function(cur_BC = NULL) {

    if (is.null(cur_BC)) {
        cur_BC <- 0.05
    }

    new_value <- -1
    while (new_value < 0) {
        # we use a beta distribution to manage the probabilities
        new_value <- cur_BC + rbeta(n = 1, 2, 2, ncp = 0) -.5
    }

    new_value
}


get_CR <- function(cur_CR = NULL, BC) {

    if (is.null(cur_CR)) {

        cur_CR <- 0.10
    }

    new_value <- -1
    while (new_value < 0) {

        new_value <- cur_CR + rbeta(n = 1, 2, 2, ncp = 0) - .5
    }

    new_value * (1 - BC)

}


get_EB <- function(cur_EB = NULL) {


    if (is.null(cur_EB)) {
        cur_EB <- 0.05
    }

    new_value <- -1
    while (new_value < 0) {
        # we want the parameter value to be positive
        new_value <- cur_EB + rnorm(n = 1, mean = 0, sd = 0.1)
    }

    new_value
}


get_ER <- function(cur_ER = NULL) {

    if (is.null(cur_ER)) {
        cur_ER <- 0.05
    }

    new_value <- -1
    while (new_value < 0) {
        # we want the parameter value to be positive
        new_value <- cur_ER + rnorm(n = 1, mean = 0, sd = 0.1)
    }

    new_value

}


get_ET <- function(cur_ET = NULL) {

    if (is.null(cur_ET)) {
        cur_ET <- 0.05
    }

    new_value <- -1
    while (new_value < 0) {
        # we want the parameter value to be positive
        new_value <- cur_ET + rnorm(n = 1, mean = 0, sd = 0.1)
    }

    new_value
}


get_KB <- function(cur_KB = NULL) {

    if (is.null(cur_KB)) {
        cur_KB <- 0.05
    }

    new_value <- -1
    while (new_value < 0) {
        # we want the parameter value to be positive
        new_value <- cur_KB + rnorm(n = 1, mean = 0, sd = 0.1)
    }

    new_value

}


get_KR <- function(cur_KR = NULL) {

    if (is.null(cur_KR)) {
        cur_KR <- 0.05
    }

    new_value <- -1
    while (new_value < 0) {
        # we want the parameter value to be positive
        new_value <- cur_KR + rnorm(n = 1, mean = 0, sd = 0.1)
    }

    new_value
}


get_KT <- function(cur_KT = NULL) {

    if (is.null(cur_KT)) {
        cur_KT <- 0.05
    }

    new_value <- -1
    while (new_value < 0) {
        # we want the parameter value to be positive
        new_value <- cur_KT + rnorm(n = 1, mean = 0, sd = 0.1)
    }

    new_value
}


get_testsn <- function(cur_testsn = NULL) {

    if (is.null(cur_testsn)) {
        cur_testsn <- 0.70
    }

    new_value <- -1
    while (new_value < 0 || new_value >= 1) {
        # we want the parameter value to be positive
        new_value <- cur_testsn + rnorm(n = 1, mean = 0, sd = 0.1)
    }
    new_value
}


get_testsp <- function(cur_testsp = NULL) {

    if (is.null(cur_testsp)) {
        cur_testsp <- 0.60
    }

    new_value <- -1
    while (new_value < 0 || new_value >= 1) {
        # we want the parameter value to be positive
        new_value <- cur_testsp + rnorm(n = 1, mean = 0, sd = 0.1)
    }
    new_value
}

get_POS  <- function(cur_POS = NULL) {

    if (is.null(cur_POS)) {

        cur_POS <- 0.4
    }

    new_value <- -1
    while (new_value < 0 || new_value >= 1) {

        new_value <- cur_POS + rnorm(n = 1, mean = 0, sd = 0.1)

    }

    new_value
       
}


get_PPC <- function(cur_PPC = NULL) {

    if (is.null(cur_PPC)) {

        cur_PPC <- 0.4
    }

    new_value <- -1
    while (new_value < 0 || new_value >= 1) {

        new_value <- cur_PPC + rnorm(n = 1, mean = 0, sd = 0.1)

    }

    new_value
}


find_calibration_data <- function(strategy= NULL, winter_index= NULL, winter_output=NULL) {

    dt_calibration <- data.table()

    
    for (each_year in 1:(strategy$cycles + 1)) {

        
        # loop over each year and create a data.table of data values
        x <- data.table(winter_index[each_year, 1,])

        # for now looking at the incidence(2) of cancer (8,9)
        y <- data.table(winter_output[each_year, 2,][8:9])

        x <- cbind(x, y)
        x[, cycle := each_year - 1]
        

        dt_calibration <- rbind(dt_calibration, x[, .('scr.detected' = sum(scr.detected), 'cln.detected'=sum(cln.detected)), by = .(cycle, state, year, age)])

    }

    
    # can easily group by the required dimensions.
    x <- dt_calibration[, .(scr.detected = sum(scr.detected), cln.detected = sum(cln.detected)), by = .(year = cycle)]
    x[, year := year+strategy$start_year]
}


find_empirical_data <- function(strategy = NULL, data_variables = NULL) {


    strategy$cycles
    strategy$start_year
    year_index <- strategy$start_year:(strategy$start_year + strategy$cycles)

    # drop the <= 15mm cancers (i.e. '1+')
    x <- ind4_cancer[screen != '1+', .(scr.detected = sum(calculated_cancer)), by = year]
    y <- ind7_cancer[year %in% year_index, .(cln.detected = sum(calculated_mean)), by = year]

    
    x <- merge(x, y, all = TRUE)

     # In ind7 'cln.detected' is in fact all cancers.
    x[, cln.detected := (cln.detected - scr.detected)]

    x
    
}
