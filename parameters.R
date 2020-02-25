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
        new_value <- cur_BC + rbeta(n = 1, 2, 2, ncp = 0)
    }

    new_value
}


get_CR <- function(cur_RC = NULL, BC) {

    if (is.null(cur_RC)) {
        cur_RC <- 0.10
    }

    new_value <- rbeta(n = 1, 2, 2, ncp = 0)

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



find_calibration_data <- function(winter_index, winter_output) {

    dt_calibration <- data.table()

    #todo - fix 1:11
    for (each_year in 1:11) {

        browser()

        x <- data.table(winter_index[each_year, 1,])
        y <- data.table(winter_output[each_year, 2,][8:9])

        x <- cbind(x,y)

        dt_calibration <- rbind(dt_calibration,x[, .(sum(scr.detected), sum(cln.detected)), by = .(state, year, age)])

    }

    browser()
    dt_calibration

}