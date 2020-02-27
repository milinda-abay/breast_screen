
library(lazyeval) # required
library(data.table) # required
library(hdf5r)
library(lhs)

state_names <- c('NoBC', 'mild.b', 'mild.r', 'mild.t', 'prog.b', 'prog.r', 'prog.t', 
                 'scr.detected', 'cln.detected')

# create a HDF5
if ('winter_h5' %in% ls() ) {
    winter_h5$close_all()
}
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

#Create an empty list of values for the transition matrix
baseline_arglist <- object_creator$create_argument_list()
baseline_arglist$load_list('baseline_tmatrix') # load an existing transition matrix
# baseline_arglist$edit_list()

#Create the transition matrix
baseline_matrix <- do.call(object_creator$define_transition, baseline_arglist$list_values())

# Create a list of model states
# state_list <- object_creator$create_states(state_names, a=x, b=y, c=z)

# Initialise 
#cur_I <- NULL
#cur_CB <- NULL
#cur_CR <- NULL
#cur_EB <- NULL
#cur_ER <- NULL
#cur_ET <- NULL
#cur_KB <- NULL
#cur_KR <- NULL
#cur_KT <- NULL
#cur_SN <- NULL
#cur_SP <- NULL


# Place holder values
flow_cost <- rep(1,9)
state_cost <- rep(10, 9)
utility <- rep(1, 9)

# Creates an unevaluated set of parameters
parameters <- object_creator$define_parameters(I = get_I(cur_I),
                                              CB = get_CB(cur_CB),
                                              CR = get_CR(cur_CR,CB),
                                              CT = (1 - CB - CR),
                                              EB = get_EB(cur_EB),
                                              ER = get_ER(cur_ER),
                                              ET = get_ET(cur_ET),
                                              KB = get_KB(cur_KB),
                                              KR = get_KR(cur_KR),
                                              KT = get_KT(cur_KT),
                                              SN = get_testsn(cur_SN),
                                              SP = get_testsp(cur_SP),
                                              SW = .1
                                              )



# Create and initialise inputs
winter_input_dt <- ind1_population[, (state_names) := 0]
winter_input_dt[ , NoBC := calculated_population]


# TODO - deparse the name of the arguments using (...) and create the property label. Test, treatment or xyz shouldn't
# matter, e.g. s2_strategey$properties$...

baseline_strategy <- object_creator$define_strategy(test = test,
                                                    my_name = "baseline",
                                                    cycles = 10,
                                                    start_year = 2008,
                                                    states = state_list,
                                                    transition_matrix = baseline_matrix,
                                                    parameters = parameters,
                                                    dsa = NULL,
                                                    mcmc = 'mcmc'
                                                    )



# TODO - define_initialisation is extremely bespoke. More thought must be given to make it
# a generic method
class(baseline_strategy)

start_year <- 2008
#cycles <- 9

init <- winter_input_dt[year == baseline_strategy$start_year]
test <- 'Mammogram' # just a placeholder property used to verify hdf5r group creation


x <- mcmc_runner(strategy = baseline_strategy, n_accepted = 20)


winter_h5$close_all()
#winter_h5 <- H5File$new("Winter.h5", mode = "w")

# output <- run_model(strategy = baseline_strategy, init = init, cycles = 10, inflow = TRUE)

#winter_input_dt[l_period == 2008]

#do_scenario <- function (list_of_tests, list_of_treatments) {

    #do_test <- function(test) {

        #do_treatment <- function(treatment) {

            ## copies the scenario environment in order to evaluate test & treatment
            #s2_strategy$properties$test$env <- environment()
            #s2_strategy$properties$treatment$env <- environment()

            #run_model(strategy = baseline_strategy, init = init, cycles = cycles)

        #}

        #lapply(list_of_treatments, do_treatment)

    #}

    #lapply(list_of_tests, do_test)

#}

#baseline_arglist <- object_creator$create_argument_list(list_values)
#baseline_arglist$load_list("baseline")
#baseline_matrix <- do.call(object_creator$define_transition, baseline_arglist$list_values())
#baseline_state_list <- object_creator$create_states(state_names)


#baseline_strategy <- object_creator$define_strategy(test = "",
                                                    #treatment = "",
                                                    #my_name = "BASELINE",
                                                    #states = baseline_state_list,
                                                    #transition_matrix = baseline_matrix,
                                                    #dsa = dsa)




#baseline_arglist$edit_list()

#winter_h5[1]