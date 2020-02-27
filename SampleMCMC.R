
library(deSolve) # You first need to install this package if not already done

## Build an SIR model
sir <- function(time, state, parameters) {
    with(as.list(c(state, parameters)), {
        dS <- -beta * S * I
        dI <- beta * S * I - gamma * I
        dR <- gamma * I
        return(list(c(dS, dI, dR)))
    })
}


## Just an example using the SIR model
run_a_simulation <- function(parameters) {
    ### Set parameters
    init <- c(S = 1 - 1e-5, I = 1e-5, R = 0.0)

    ## Time frame
    times <- seq(0, 50, by = 0.1)

    ## Solve using ode (General Solver for Ordinary Differential Equations)
    out <- ode(y = init, times = times, func = sir, parms = parameters)

    ## change to data frame
    out <- as.data.frame(out)


    return(out)

}


plot_results <- function(outputs) {
    x11() # open a window for the plot
    par(lwd = 3) # line-width option
    plot(outputs$time, outputs$I, col = 'red', type = 'l', xlim = c(0, 50), ylim = c(0, 1), xlab = 'time', ylab = 'compartment proportions')
    lines(outputs$time, outputs$S, col = 'green')
    lines(outputs$time, outputs$R, col = 'blue')
}


metropolis_hastings <- function(log_lh_func, log_priors_func, proposal_func, init_params, n_accepted, max_iterations = 1e4) {
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
    count_iterations = 0
    count_accepted = 0

    # calculate the likelihoods of the initial parameter set
    current_params = init_params # current_params is the last accepted set of paramaters 
    current_log_lh = log_lh_func(init_params)
    current_log_prior = log_priors_func(init_params)

    while (count_accepted < n_accepted) {

        if (count_iterations >= max_iterations) {
            print("The maximum number of iterations has been reached. The simulation has been aborted.")
            break
        }

        # Generate a new candidate parameter set
        proposed_params = proposal_func(current_params)

        # Evaluate the likelihood of the new parameter set
        proposed_log_lh = log_lh_func(proposed_params)
        proposed_log_prior = log_priors_func(proposed_params)

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

    # post-simulation calculation of acceptance ratio. 
    acceptance_ratio = count_accepted / count_iterations
    print(paste('Acceptance ratio:', acceptance_ratio, sep = ' '))

    return(results)

}

my_log_lh_func <- function(params) {
    # params is a list of parameters and associated values.
    # return the likelihood value associated with the parameter set.

    # run the SIR model with the input parameters
    sir_results = run_a_simulation((params))

    # Our likelihood is obtained by multiplying Gaussian elements centered on the model estimate for each datapoint.
    # We assume that the standard deviation is 0.05. This means that 95% of the Gaussian dentisty sits within an 
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





my_log_priors_func <- function(params) {
    # params is a list of parameters and associated values
    # this funciton returns the joint prior distribution (actually log version)
    browser()
    joint_log_prior = 0
    for (param_name in names(params)) {
        if (param_name == "beta") {
            # flat prior
            y = dunif(x = params[[param_name]], min = 0, max = 10, log = TRUE)
        } else if (param_name == "gamma") {
            # beta distribution on interval [0,1]
            y = dbeta(x = params[[param_name]], shape1 = 2, shape2 = 2, log = TRUE)
        }
        joint_log_prior = joint_log_prior + y
    }
    return(joint_log_prior)
}


my_proposal_func <- function(params) {
    # params is a list of parameters and associated values
    # this function returns another list of parameters
    # We use normal distributions to generate new parameter values

    # standard deviations
    sd = list(beta = 0.02, gamma = 0.02)

    new_params = list()
    for (param_name in names(params)) {
        new_value = -1
        while (new_value < 0) {
            # we want the parameter value to be positive
            new_value = params[[param_name]] + rnorm(n = 1, mean = 0, sd = sd[[param_name]])
        }
        new_params[[param_name]] = new_value
    }

    return(new_params)
}

master_mcmc_runner <- function(n_accepted = 100) {
    init_params = list(beta = 1.0, gamma = 0.5)
    M = metropolis_hastings(log_lh_func = my_log_lh_func, log_priors_func = my_log_priors_func, proposal_func = my_proposal_func, init_params = init_params, n_accepted = n_accepted)
    return(M)
}





## beta: infection parameter; gamma: recovery parameter
# Run the code below for an example simulation.

# parameters = list(beta = 1.0, gamma = 0.5)
# output = run_a_simulation((parameters))
# plot_results(output)


library(outbreaks) # load outbreak datasets
my_data = influenza_england_1978_school # influenza outbreak in a boarding school
total_population = 763 # population of the boarding school

# rearrange the data by turning dates into numbers
my_data$date = as.numeric(my_data$date)

# The index case (not included in the dataset) was infected on 10-01-1978. data starts on 22-01-1978
# Then the first date of the dataset will have index 12.
adjustment = my_data$date[1] - 12
my_data$date = my_data$date - adjustment

# normalise prevalence data
my_data$in_bed = my_data$in_bed / total_population

plot_data <- function() {
    x11() # open a new window
    plot(my_data$date, my_data$in_bed, xlim = c(0, 30))
}





# create an outputs directory
output_dir = "outputs/"
dir.create(output_dir)

# specify file format for figure outputs
file_format = 'pdf' #  accepted values: 'pdf', 'png', 'jpeg' and 'jpg'.

get_list_of_params <- function(mcmc_outputs) {
    # automatically reads the list of parameter names from the mcmc output file
    col_names = colnames(mcmc_outputs)
    param_list = col_names[3:length(col_names)]
    return(param_list)
}

open_figure_for_saving <- function(filename, format) {
    # This function opens a file and prepare it for the specified format.
    if (format == 'png') {
        png(filename)
    } else if (format == 'pdf') {
        pdf(filename)
    } else if (format == 'jpeg' || format == 'jpg') {
        jpeg(filename)
    }
}

plot_parameter_progressions <- function(mcmc_outputs, param_list, accepted_only = TRUE) {
    # Plot the timeseries associated with the different parameters
    for (param in param_list) {
        filename = paste(output_dir, 'param_progress_', param, '.', file_format, sep = '')
        open_figure_for_saving(filename, file_format)
        # every plotting after this point will appear in the output file
        par(pch = 20) # specify the point style for the figure
        color_list = c('red', 'black') # black for accepted / red for rejected
        cex_list = c(1, 1) # point size for rejected / accepted
        if (accepted_only == TRUE) {
            cex_list = c(0, 1) # point-size=0 will plot nothing
        }
        colors = color_list[mcmc_outputs$accepted + 1]
        cexs = cex_list[mcmc_outputs$accepted + 1]
        plot(mcmc_outputs[[param]], main = 'Pameter progression', ylab = param, ylim = range(mcmc_outputs[[param]]), col = colors, cex = cexs)

        dev.off() # close the file
    }
}

plot_log_likelihood_progression <- function(mcmc_outputs) {
    # Represents the log-likelihood progression for the different iterations
    filename = paste(output_dir, 'loglikelihood_progress.', file_format, sep = '')
    open_figure_for_saving(filename, file_format)
    par(pch = 20)
    plot(mcmc_outputs$log_lh, main = 'Log-likelihood progression', ylab = 'log-likelihood')
    dev.off() # close the file
}

plot_best_fit <- function(mcmc_outputs, param_list) {
    # plot the maximum-likelihood model run and the datapoints

    best_run_index = which.max(M$log_lh) # get best run
    params = list()
    for (param in param_list) {
        params[[param]] = mcmc_outputs[[param]][best_run_index] # read associated parameter values
    }
    sir_results = run_a_simulation(params) # run the simulation
    y_max = max(sir_results$I, my_data$in_bed) # get the highest value for plotting purpose 

    filename = paste(output_dir, 'best_fit.', file_format, sep = '')
    open_figure_for_saving(filename, file_format)
    plot(sir_results$time, sir_results$I, main = 'Best model fit', col = 'red', type = 'l', xlim = c(0, 50), ylim = c(0, y_max), xlab = 'time (days)', ylab = 'Infection prevalence')
    lines(my_data$date, my_data$in_bed, type = 'p')
    dev.off()
}

plot_posterior_distributions <- function(mcmc_outputs, param_list) {
    # plot histograms for the posterior distributions of the parameters
    for (param in param_list) {
        filename = paste(output_dir, 'posterior_', param, '.', file_format, sep = '')
        open_figure_for_saving(filename, file_format)
        hist(mcmc_outputs[[param]][mcmc_outputs$accepted == 1], xlab = param, main = 'Posterior distribution')
        dev.off() # close the file
    }
}

plot_param_collinearity <- function(mcmc_outputs, param_list) {
    # Produce scatter plots showing the relationships between every pair of parameters
    filename = paste(output_dir, 'parameter_collinearity.', file_format, sep = '')
    open_figure_for_saving(filename, file_format)
    pairs(mcmc_outputs[, 3:ncol(mcmc_outputs)], main = 'Scatter plot', pch = 20)
    dev.off()
}

master_plotting <- function(mcmc_outputs) {
    # Runs all plotting functions
    param_list = get_list_of_params(mcmc_outputs)
    plot_parameter_progressions(mcmc_outputs, param_list, accepted_only = FALSE)
    plot_log_likelihood_progression(mcmc_outputs)
    plot_best_fit(mcmc_outputs, param_list)
    plot_posterior_distributions(mcmc_outputs, param_list)
    plot_param_collinearity(mcmc_outputs, param_list)
}



M = master_mcmc_runner(100)
#master_plotting(M)