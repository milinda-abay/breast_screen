# The primary function run_model() calls to perform row by row transition calculations.
get_state_counts <- function(index_ds, output_ds, year, strategy, markov_cycle, dsa, a_run=NULL) {

    # collapsing the promise object 
    testing <- lazy_eval(strategy$properties$test)
    # treatment <- lazy_eval(strategy$properties$treatment)

    # Now access the HDF5 dataset
    DT <- setDT(index_ds[markov_cycle + 1, 1,])

    transition_matrix <- strategy$transition

    z <- nrow(DT)
    l <- sqrt(length(transition_matrix))

    # assign the current environment for evaluation. 
    # TODO - create a function for multiple parameters
    for (each in names(parameters)) {

        parameters[[each]]$env <- environment() # move evaluation of parameters to this environment
        assign(each, lazy_eval(parameters[[each]])) # generate a pair lists for each parameter

    }


    #flow_cost <- lazy_eval(unevaluated_flow_cost)
    #state_cost <- lazy_eval(unevaluated_state_cost)
    #utility <- param$utility

    
    this_env <- environment()

    if (dsa == TRUE) {

        # replace parameters  by values return form defined_dsa
        dsa_param <- dimnames(strategy$dsa$dsa)[[2]]
        dsa_value <- strategy$dsa$dsa[a_run,]

        lapply(dsa_param, function(p) {
                        
            assign(p, dsa_value[[p]], this_env)
            
        })
    }

    # write parameters to output_ds
    # TODO - replace hard coded parameter(s) and values
    
    if (markov_cycle == 0) {

        pn <- names(parameters)
        h5attr(output_ds, 'dsa') <- dsa
        lapply(pn, function(pn) {
            
            h5attr(output_ds, pn) <- eval(as.symbol(pn), this_env)
        })

    }


    # assign the current environment for evaluation. 
    for (i in 1:length(transition_matrix)) {

        transition_matrix[[i]]$env <- environment()

    }

    # Evaluates the transition matrix and insert a '-pi' place-holder for CMP.
    tM <- lazy_eval(transition_matrix, data = list(CMP = -pi))

    # Scalar values don't get evaluated into vectors
    # loop and manually expand to vectors
    for (i in 1:length(transition_matrix)) {

        if (length(tM[[i]]) == 1) {

            tM[[i]] <- rep(tM[[i]], times = z)
        }
    }

    # Manipulates the tM to calculate the CMP
    tM <- calculate_complement(tM, l, z)

    # Select the numeric state value columns in preparation for multiplication.

    print("PMM Start")
    print(Sys.time())
    # results <- perform_matrix_multiplication(output_ds, tM, l, z, markov_cycle, flow_cost, state_cost, utility, current_rows)
    perform_matrix_multiplication(output_ds, tM, l, z, markov_cycle, flow_cost, state_cost, utility)
    print("PMM End")
    print(Sys.time())

    NULL

}
