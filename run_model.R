
# Attempt at method dispatch
run_model <- function(strategy, ...) {

    UseMethod('run_model',strategy)

}


# The main model runtime loop 
run_model.default <- function(...,
                     parameters = NULL,
                     init = NULL,
                     cycles = NULL,
                     scenario_env = NULL,
                     method = NULL,
                     cost = NULL,
                     effect = NULL,
                     inflow = TRUE,
                     dsa = FALSE) {


    
    uneval_strategy_list <- list(...)
    strategy <- uneval_strategy_list[[1]] # assuming there is only one strategy!

    eval_strategy <- strategy

    year <- start_year # Initialise year with start_year
    markov_cycle <- 0 # Tracks the current cycle
    
    # A conditional flag used for inflows.
    my_name <- lazy_eval(strategy$properties$my_name)

    # fills index_ds and output_ds with initial values
    group <- initialise_hfd5(strategy = strategy)

    # Create names for datasets
    # TODO - move index_ds to strategy level group and pre-calculate it.
    # then all variations(scenarios) of the strategy can reference it.
    index_ds <- group[[2]][["index"]]
    output_ds <- group[[1]][['output']]


    while (markov_cycle < cycles) {

        #current_rows <- which(index_ds[]$cycle == markov_cycle)
        current_rows <- nrow(index_ds[markov_cycle+1,1,])

        writeLines(sprintf("\nCommencing Markov cycle %i", markov_cycle))
        writeLines(sprintf("Current number of populations in the working matrix is %i", current_rows))
        print(setDT(index_ds[markov_cycle+1,1,])[1:10, .N, by = .(age)])

        results <- get_state_counts(index_ds, output_ds, year, strategy, markov_cycle,dsa)

        # Update counters
        markov_cycle <- markov_cycle + 1
        year <- year + 1

        # Aging the population in the calculation object
        index_ds[markov_cycle+1, 1,] <- index_ds[markov_cycle,1,]
        index_ds[markov_cycle+1, 1,][, 'age'] <- index_ds[markov_cycle, 1,][, 'age'] + 1

        size <- nrow(index_ds[markov_cycle + 1,1,])

        # Inflows for next cycle. 

        if ((my_name == "S1" || my_name == "S0_1"
            || my_name == "S2" || my_name == "S0_12") && markov_cycle > 5) {

            inflow <- FALSE

        }

        if (inflow) {

            # TODO -  Need a check to ensure pop_inflow has data
            y <- year
            pop_inflow <- winter_input_dt[year == y,]
        }
        else {
            pop_inflow <- NULL # not convinced this is needed
        }

        
        if (inflow) {

            browser()
            # index test
            start <- nrow(index_ds[markov_cycle+1,1,]) + 1
            end <- start + nrow(pop_inflow) - 1
            x <- pop_inflow[, !..state_names]
            index_ds[markov_cycle + 1, 1, start:end] <- x

            # update state count in output_ds with new arrivals
            output_ds[markov_cycle + 1, 1, start:end] <- pop_inflow[, ..state_names]
           
        }

       
    }

    
}

# The main model runtime loop 
run_model.dsa <- function(...,
                     parameters = NULL,
                     init = NULL,
                     cycles = NULL,
                     scenario_env = NULL,
                     method = NULL,
                     cost = NULL,
                     effect = NULL,
                     inflow = TRUE,
                     dsa = TRUE) {

    
    uneval_strategy_list <- list(...)
    strategy <- uneval_strategy_list[[1]]

    eval_strategy <- strategy
    # The names test and treatment are specific to the model
    # TODO -  Find a generic method to extract the name value
    strategy_p1 <- lazy_eval(eval_strategy$properties$test)
    strategy_p2 <- lazy_eval(eval_strategy$properties$treatment)

    # A conditional flag used for inflows.
    my_name <- lazy_eval(strategy$properties$my_name)

    # dsa loop
    dsa_run <- 1:nrow(strategy$dsa$dsa)
    
    do_dsa <- function(a_run) {

        year <- start_year # Initialise year with start_year
        markov_cycle <- 0 # Tracks the current cycle

        # fills index_ds and output_ds with initial values
        group <- initialise_hfd5(strategy_p1, strategy_p2, my_name, a_run, strategy = strategy)

        # Create names for datasets
        # TODO - move index_ds to strategy level group and pre-calculate it.
        # then all variations(scenarios) of the strategy can reference it.
        index_ds <- group[[2]][["index"]]
        output_ds <- group[[1]][[paste('dsa',a_run, sep = '_')]]

        while (markov_cycle < cycles) {

            #current_rows <- which(index_ds[]$cycle == markov_cycle)
            current_rows <- nrow(index_ds[markov_cycle + 1, 1,])

            writeLines(sprintf("\nCommencing Markov cycle %i", markov_cycle))
            writeLines(sprintf("Current number of populations in the working matrix is %i", current_rows))
            print(setDT(index_ds[markov_cycle + 1, 1,])[1:10, .N, by = .(AGEP)])

            results <- get_state_counts(index_ds, output_ds, year, strategy, markov_cycle, dsa, a_run)

            # Update counters
            markov_cycle <- markov_cycle + 1
            year <- year + 1
            
            # Aging the population in the calculation object
            index_ds[markov_cycle + 1, 1,] <- index_ds[markov_cycle, 1,]
            index_ds[markov_cycle + 1, 1,][, 1] <- index_ds[markov_cycle, 1,][, 1] + 1
            
            size <- nrow(index_ds[markov_cycle + 1, 1,])

            # Inflows for next cycle. 

            if ((my_name == "S1" || my_name == "S0_1"
                || my_name == "S2" || my_name == "S0_12") && markov_cycle > 5) {

                inflow <- FALSE

            }

            if (inflow) {
                pop_inflow <- winter_input_dt[YARP == year,][, cycle := NA]
            }
            else {
                pop_inflow <- NULL # not convinced this is needed
            }


            if (inflow) {

                # index test
                start <- nrow(index_ds[markov_cycle + 1, 1,]) + 1
                end <- start + nrow(pop_inflow) - 1
                x <- pop_inflow[, !..state_names][, 1:7]
                index_ds[markov_cycle + 1, 1, start:end] <- x

                # update state count in output_ds with new arrivals
                output_ds[markov_cycle + 1, 1, start:end] <- pop_inflow[, ..state_names]

            }


        }

    }

    lapply(dsa_run, do_dsa)


}

run_model.mcmc <- function(...,
                     parameters = NULL,
                     init = NULL,
                     cycles = NULL,
                     scenario_env = NULL,
                     method = NULL,
                     cost = NULL,
                     effect = NULL,
                     inflow = TRUE,
                     dsa = FALSE,
                     mcmc = TRUE) {

    uneval_strategy_list <- list(...)
    strategy <- uneval_strategy_list[[1]]

    eval_strategy <- strategy

    year <- strategy$start_year # Initialise year with start_year
    markov_cycle <- 0 # Tracks the current cycle

    # A conditional flag used for inflows.
    my_name <- lazy_eval(strategy$properties$my_name)

    # fills index_ds and output_ds with initial values
    group <- initialise_hfd5(strategy = strategy)

    # Create names for datasets
    # TODO - move index_ds to strategy level group and pre-calculate it.
    # then all variations(scenarios) of the strategy can reference it.
    index_ds <- group[[2]][["index"]]
    output_ds <- group[[1]][['output']]


    while (markov_cycle < cycles) {

        #current_rows <- which(index_ds[]$cycle == markov_cycle)
        current_rows <- nrow(index_ds[markov_cycle , 1,])

        writeLines(sprintf("\nCommencing Markov cycle %i", markov_cycle))
        writeLines(sprintf("Current number of populations in the working matrix is %i", current_rows))
        print(setDT(index_ds[markov_cycle, 1,])[1:10, .N, by = .(age)])

        

        results <- get_state_counts(index_ds, output_ds, year, strategy, markov_cycle, dsa)

        # Update counters
        markov_cycle <- markov_cycle + 1
        year <- year + 1

        # Aging the population in the calculation object
        index_ds[markov_cycle + 1, 1,] <- index_ds[markov_cycle, 1,]
        index_ds[markov_cycle + 1, 1,][, 'age'] <- index_ds[markov_cycle, 1,][, 'age'] + 1

        size <- nrow(index_ds[markov_cycle + 1, 1,])

        # Inflows for next cycle. 

        if ((my_name == "S1" || my_name == "S0_1"
            || my_name == "S2" || my_name == "S0_12") && markov_cycle > 5) {

            inflow <- FALSE

        }

        if (inflow) {
            
            # TODO -  Need a check to ensure pop_inflow has data
            y <- year
            pop_inflow <- winter_input_dt[year == y,]

            if (nrow(pop_inflow) == 0) {

                pop_inflow <- NULL

            }

        }
        


        if (!is.null(pop_inflow)) {

            # browser()

            # index test
            start <- nrow(index_ds[markov_cycle + 1, 1,]) + 1
            end <- start + nrow(pop_inflow) - 1
            x <- pop_inflow[, !..state_names]
            index_ds[markov_cycle + 1, 1, start:end] <- x

            # update state count in output_ds with new arrivals
            output_ds[markov_cycle + 1, 1, start:end] <- pop_inflow[, ..state_names]

        }


    }

    group


}



