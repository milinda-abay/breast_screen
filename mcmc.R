# This is the MCMC runner for winter

mcmc_runner <- function(n_accepted = 100, parameters) {

    init_params <- list()

    for (each in names(parameters)) {

        parameters[[each]]$env <- environment() # move evaluation of parameters to this environment
        assign(each, lazy_eval(parameters[[each]])) # generate a pair lists for each parameter
        init_params[[each]] <- eval(as.symbol(each)) # put the parameter value into a list

    }
       
    M = metropolis_hastings(log_lh_func = my_log_lh_func, log_priors_func = my_log_priors_func, proposal_func = my_proposal_func, init_params = init_params, n_accepted = n_accepted)
    init_params

}

metropolis_hastings <- function(log_lh_func,
                                log_priors_func,
                                proposal_func,
                                init_params,
                                n_accepted,
                                max_iterations = 1e4) {









}

?UseMethod

my_proposal_func <- function(params, parameters) {
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



my_log_lh_func <- function(params) {
    # params is a list of parameters and associated values.
    # return the likelihood value associated with the parameter set.

    # run winter model with the input parameters
    winter_results <- run_model(strategy, params)

    # Our likelihood is obtained by multiplying Gaussian elements centered on the model estimate for each datapoint.
    # We assume that the standard deviation is 0.05. This means that 95% of the Gaussian density sits within an 
    # interval of width 0.1 (2*sd).
    sd = 0.05
    overall_log_lh = 0
    for (i in 1:nrow(my_data)) {
        # for each date of the dataset
        model_output = sir_results$I[sir_results$time == my_data$date[i]]
        single_log_lh = dnorm(x = my_data$in_bed[i], mean = model_output, sd = sd, log = TRUE)
        overall_log_lh = overall_log_lh + single_log_lh
    }

    return(overall_log_lh)
}







