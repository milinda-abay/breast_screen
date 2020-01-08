
library(lazyeval) # required
library(data.table) # required
library(hdf5r)
library(lhs)

state_names <- c('NoBC', 'mild.b', 'mild.r', 'mild.t', 'prog.b', 'prog.r', 'prog.t', 'sev.b', 'sev.r', 'sev.t',
                 'scr.detected', 'detected')


# create a HDF5
winter_h5 <- H5File$new("Winter.h5", mode = "w")

# source files
source("parameters.R")
source("object creator.R")
source("run_model.R")
source("calculate_complement.R")
source("perform_matrix_multiplication.R")
source("get_state_counts.R")
source("initialise_hfd5.R")
source('load_data.R')
source('mcmc.R')


object_creator <- create_objects(state_names)

#Create a list of values for the transition matrix
baseline_arglist <- object_creator$create_argument_list()
baseline_arglist$load_list('baseline_tmatrix')


#Create the transition matrix
baseline_matrix <- do.call(object_creator$define_transition, baseline_arglist$list_values())

# Create a list of model states
state_list <- object_creator$create_states(state_names, a=x, b=y, c=z)

# Initialise 
cur_I <- NULL
cur_BC <- NULL
cur_RC <- NULL
cur_EB <- NULL
cur_ER <- NULL
cur_ET <- NULL
cur_KB <- NULL
cur_KR <- NULL
cur_KT <- NULL
cur_SN <- NULL
cur_SP <- NULL
cur_POS <- NULL
cur_PPC = NULL

flow_cost <- rep(1,12)
state_cost <- rep(10, 12)
utility <- rep(1, 12)




# Creates an unevaluated set of parameters
parameters <- object_creator$define_parameters(I = get_I(cur_I),
                                              BC = get_BC(cur_BC),
                                              RC = get_RC(cur_RC,BC),
                                              TC = (1 - BC - RC),
                                              EB = get_EB(cur_EB),
                                              ER = get_ER(cur_ER),
                                              ET = get_ET(cur_ET),
                                              KB = get_KB(cur_KB),
                                              KR = get_KR(cur_KR),
                                              KT = get_KT(cur_KT),
                                              SN = get_testsn(cur_SN),
                                              SP = get_testsp(cur_SP),
                                              POS = get_POS(cur_POS),
                                              PPC = get_PPC(cur_PPC)
                                              )
# POS - probability of screening
# PPC - probability of presenting clinically


# Create and initialise inputs
winter_input_dt <- ind1_participation[, (state_names) := 0]
winter_input_dt[ , NoBC := calculated_mean]


# TODO - deparse the name of the arguments using (...) and create the property label. Test, treatment or xyz shouldn't
# matter, e.g. s2_strategey$properties$...

baseline_strategy <- object_creator$define_strategy(test = test,
                                                    my_name = "baseline",
                                                    states = state_list,
                                                    transition_matrix = baseline_matrix,
                                                    dsa = NULL,
                                                    mcmc = 'mcmc'
                                                    )



# TODO - define_initialisation is extremely bespoke. More thought must be given to make it
# a generic method
class(baseline_strategy)

start_year <- 2008
init <- winter_input_dt[l_period == start_year]
test <- 'MM'
cycles <- 8


output <- run_model(strategy = baseline_strategy, init= init, cycles = 8, inflow = TRUE)

x <- mcmc_runner(n_accepted = 100, parameters)



do_scenario <- function (list_of_tests, list_of_treatments) {

    do_test <- function(test) {

        do_treatment <- function(treatment) {

            # copies the scenario environment in order to evaluate test & treatment
            s2_strategy$properties$test$env <- environment()
            s2_strategy$properties$treatment$env <- environment()

            run_model(strategy = baseline_strategy, init = init, cycles = cycles)

        }

        lapply(list_of_treatments, do_treatment)

    }

    lapply(list_of_tests, do_test)

}

baseline_arglist <- object_creator$create_argument_list(list_values)
baseline_arglist$load_list("baseline")
baseline_matrix <- do.call(object_creator$define_transition, baseline_arglist$list_values())
baseline_state_list <- object_creator$create_states(state_names)


baseline_strategy <- object_creator$define_strategy(test = "",
                                                    treatment = "",
                                                    my_name = "BASELINE",
                                                    states = baseline_state_list,
                                                    transition_matrix = baseline_matrix,
                                                    dsa = dsa)




baseline_arglist$edit_list()

winter_h5[1]