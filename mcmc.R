# This is the MCMC runner for winter

mcmc_runner <- function(strategy, n_accepted = 100) {

    init_params <- list()

    for (each in names(strategy$parameters)) {

        strategy$parameters[[each]]$env <- environment() # move evaluation of parameters to this environment
        assign(each, lazy_eval(strategy$parameters[[each]])) # generate a pair lists for each parameter
        init_params[[each]] <- eval(as.symbol(each)) # put the parameter value into a list

    }

    M <- metropolis_hastings(strategy = strategy, log_lh_func = my_log_lh_func, log_priors_func = my_log_priors_func, proposal_func = my_proposal_func, init_params = init_params, n_accepted = n_accepted)

}




metropolis_hastings <- function(strategy,
                                log_lh_func,
                                log_priors_func,
                                proposal_func,
                                init_params,
                                n_accepted,
                                max_iterations = 1e4) {


    # log_lh_func: a function returning the log-likelihood value. This is log( P(data | theta) )
    # priors_func: a function returning the joint prior log-likelihood of the parameters. This is  log( P(theta) )
    # proposal_func (or jumping function): a function returning a new parameter set, starting from another parameter set. P(theta' | theta)
    # init_params: a list containing the parameters. e.g list(beta=0.5, gamma=2.6)
    # n_accepted: number of accepted runs
    # max_iterations: maximum number of iterations allowed

    # preliminary check: max_iterations has to be >= n_accepted
    stopifnot(max_iterations >= n_accepted)


    # prepare storage for the results. columns will be named "log_lh", "accepted", "param1", "param2", ...
    results = data.frame(log_lh = double(), accepted = integer())
    for (param_name in names(init_params)) {
        results[[param_name]] = double()
    }

    # initialise counters
    count_iterations <- 0
    count_accepted <- 0

    # calculate the likelihoods of the initial parameter set
    current_params <- init_params # current_params is the last accepted set of paramaters 
    current_log_lh <- log_lh_func(strategy, init_params, count_iterations)
    current_log_prior <- log_priors_func(init_params)

    while (count_accepted < n_accepted) {

        if (count_iterations >= max_iterations) {
            print("The maximum number of iterations has been reached. The simulation has been aborted.")
            break
        }

        
        # Generate a new candidate parameter set
        proposed_params <- proposal_func(current_params)

        # Evaluate the likelihood of the new parameter set
        proposed_log_lh <- log_lh_func(strategy, proposed_params, count_iterations+1)
        proposed_log_prior <- log_priors_func(proposed_params)

        # Acceptance or rejection?
        accepted = 0
        log_proba_of_acceptance = proposed_log_prior + proposed_log_lh - (current_log_prior + current_log_lh) # we could have stored (current_log_prior + current_log_lh) in a variable
        proba_of_acceptance = exp(log_proba_of_acceptance) # transform to actual proba

        
        if (proba_of_acceptance >= 1) {
            # the proposed parameter set is "better" than the current one
            accepted = 1
        } else {
            accepted = rbinom(n = 1, size = 1, prob = proba_of_acceptance)
        }

        # storage
        new_row = list(log_lh = proposed_log_lh + proposed_log_prior, accepted = accepted)
        for (param_name in names(proposed_params)) {
            new_row[[param_name]] = proposed_params[[param_name]]
        }
        results = rbind(results, new_row)

        # If the run is accepted, we update the relevant variables
        if (accepted == 1) {
            current_params = proposed_params
            current_log_prior = proposed_log_prior
            current_log_lh = proposed_log_lh
            count_accepted = count_accepted + 1
        }

        count_iterations = count_iterations + 1
    }

}


my_proposal_func <- function(params) {
    # params is a list of parameters and associated values
    # this function returns another list of parameters

    
    names(params) <- paste('cur_', names(params), sep = '')

    for (each in names(params)) {
        assign(each, params[[each]])
    }

    new_params = list()
    
    for (each in names(parameters)) {
        
        parameters[[each]]$env <- environment() # move evaluation of parameters to this environment
        assign(each, lazy_eval(parameters[[each]])) # generate a pair lists for each parameter
        new_params[[each]] <- eval(as.symbol(each)) # put the parameter value into a list
    }
    
    new_params
}



my_log_lh_func <- function(strategy, params, count_iterations) {
    # params is a list of parameters and associated values.
    # return the likelihood value associated with the parameter set.
    # this is p(D|theta)

    
    # run winter model with the input parameters
    winter_results <- run_model(strategy, params, cycles = strategy$cycles, mcmc_run_id = count_iterations)
    winter_output <- winter_results[[1]][['output']]
    winter_index <- winter_results[[2]][['index']]

    # Our likelihood is obtained by multiplying Gaussian elements centered on the model estimate for each datapoint.
    # We assume that the standard deviation is 0.05. This means that 95% of the Gaussian density sits within an 
    # interval of width 0.1 (2*sd).
    sd <- 0.05
    overall_log_lh <- 0
    
    # for each date of the dataset
    winter_calibration_data <- find_calibration_data(strategy,winter_index, winter_output)
        
    #model_output = sir_results$I[sir_results$time == my_data$date[i]]
    empirical_data <- find_empirical_data(strategy,data_variables = c('cln.detected', 'scr.detected'))
    

    log_lh_dt <- merge(winter_calibration_data, empirical_data, by = c('year'), all = TRUE)
    #single_log_lh = dnorm(x = my_data$in_bed[i], mean = model_output, sd = sd, log = TRUE)

    log_lh_dt[, scr.detected.llh := dnorm(x = log_lh_dt[,scr.detected.y], mean = log_lh_dt[,scr.detected.x], sd = sd, log = TRUE)]
    log_lh_dt[, cln.detected.llh := dnorm(x = log_lh_dt[, cln.detected.y], mean = log_lh_dt[, cln.detected.x], sd = sd, log = TRUE)]


    overall_log_lh <- log_lh_dt[, sum(cln.detected.llh,scr.detected.llh, na.rm = TRUE)]

    overall_log_lh
        
}


my_log_priors_func <- function(params) {
    # params is a list of parameters and associated values
    # this function returns the joint prior distribution (actually log version)

    
    joint_log_prior = 0
    for (param_name in names(params)) {
        if (param_name %in% c("CB","CR","CT"))  {
            # beta distribution on interval [0,1]
            y = dbeta(x = params[[param_name]], shape1 = 2, shape2 = 2, log = TRUE)
            
        } else {
            # flat prior, following Romain's example here min=0 and max=10. Verify this logic!
            y = dunif(x = params[[param_name]], min = 0, max = 10, log = TRUE)
        }
        joint_log_prior = joint_log_prior + y
    }

    joint_log_prior
}

