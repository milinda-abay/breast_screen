
initialise_hfd5 <- function(..., strategy) {

    UseMethod('initialise_hfd5', strategy)

}


# Initialise HDF5 default
initialise_hfd5.default <- function(strategy) {

    cycles <- strategy$cycles

    # Create the scenario groups in HFD5
    group_name <- lazy_eval(strategy$properties$my_name)
          
    for (each in strategy$properties[names(strategy$properties) != 'my_name']) {
               
        group_name <- paste(group_name, lazy_eval(each), sep = '/')

    }

    # Check to see if the index exist or not. Then make appropriate link
    if (!(strategy$properties$my_name$expr %in% names(winter_h5))) {
        index_grp <- winter_h5$create_group(paste(strategy$properties$my_name$expr))

    } else {

        
        index_grp <- winter_h5[[strategy$properties$my_name$expr]]
    }

    # Check to see if the dataset exist or not. Then make appropriate link
    if (!(group_name %in% list.groups(winter_h5))) {

        scenario_grp <- winter_h5$create_group(group_name)

    } else {

        scenario_grp <- winter_h5[[group_name]]

    }


    # Creating a 3D array for the index data set
    # TODO - change all hard coded values to refer winter_input_dt for attributes

    index_column_names <- names(winter_input_dt[, -(..state_names)])

    get_type <- function(col_name) {

        typeof(winter_input_dt[[col_name]])

    }

    list_of_types <- lapply(index_column_names, get_type)

    # Creating the index datatypes
    # TODO - figure out how to create compound datatype using list_of_types
    compound_index <- H5T_COMPOUND$new(index_column_names,
                                   dtypes = list(h5types$H5T_NATIVE_INT, H5T_STRING$new(size = 3), h5types$H5T_NATIVE_INT,
                                   h5types$H5T_IEEE_F32LE, h5types$H5T_NATIVE_INT))

    index_space <- H5S$new(dim = c(cycles + 1, 1, nrow(init)), maxdims = c(cycles + 1, 1, Inf))
    #index_space <- H5S$new(dim = c(cycles, 1, nrow(init)), maxdims = c(cycles, 1, Inf))
    
    # Above should be implemented like this.
    compound_output <- H5T_COMPOUND$new(names(strategy$states),
                                   dtypes = rep(list(eval(Quote(h5types$H5T_IEEE_F32LE))), strategy$state_number))
 
    #index_space <- H5S$new(dim = c(cycles + 1, 1, nrow(init)), maxdims = c(cycles + 1, 1, Inf))

    output_space <- H5S$new(dim = c(cycles + 1, 5, nrow(init)), maxdims = c(cycles + 1, 5, Inf))
    #output_space <- H5S$new(dim = c(cycles, 5, nrow(init)), maxdims = c(cycles, 5, Inf))
    
    if ('index' %in% names(winter_h5[[strategy$properties$my_name$expr]])) {

        index_grp$link_delete('index')
        index_grp$create_dataset(name = 'index', space = index_space,
                                                        dtype = compound_index)
    } else {

        index_grp$create_dataset(name = 'index', space = index_space,
                                                        dtype = compound_index)
    }


    if (!('output' %in% list.datasets(scenario_grp))) {

        scenario_grp$create_dataset(name = 'output', space = output_space, dtype = compound_output)

    }


    index_ds <- index_grp[['index']]



    x <- copy(init[, ..index_column_names])

    
    index_ds[0,1, ] <- x

    output_ds <- scenario_grp[['output']]

    output_ds[0, 1,] <- init[, ..state_names]

    list(scenario_grp,index_grp)

}

# Initialise HDF5 for MCMC
initialise_hfd5.mcmc <- function(mcmc_run_id, strategy) {

    cycles <- strategy$cycles

    
    # Check to see if the index exist or not. Then make appropriate link
    if (!(strategy$properties$my_name$expr %in% names(winter_h5))) {
        index_grp <- winter_h5$create_group(paste(strategy$properties$my_name$expr))

    } else {

        index_grp <- winter_h5[[strategy$properties$my_name$expr]]
    }

    # Create the scenario groups in HFD5
    group_name <- lazy_eval(strategy$properties$my_name)

    for (each in strategy$properties[names(strategy$properties) != 'my_name']) {

        group_name <- paste(group_name, lazy_eval(each), sep = '/')
        if (!(group_name %in% list.groups(winter_h5))) {
            winter_h5$create_group(group_name)
        }

    }
    
    # because this is MCMC
    group_name <- paste(group_name, 'MCMC', sep = '/')
    if (!(group_name %in% list.groups(winter_h5))) {
        parameter_grp <- winter_h5$create_group(group_name)
    }
    

    # Create the final group for MCMC run id
    group_name <- paste(group_name, mcmc_run_id, sep = '/')
    
    # Check to see if the dataset exist or not. Then make appropriate link
    if (!(group_name %in% list.groups(winter_h5))) {

        scenario_grp <- winter_h5$create_group(group_name)

    } else {

        scenario_grp <- winter_h5[[group_name]]

    }


    # Creating a 3D array for the index data set
    # TODO - change all hard coded values to refer winter_input_dt for attributes

    index_column_names <- names(winter_input_dt[, - (..state_names)])

    get_type <- function(col_name) {

        typeof(winter_input_dt[[col_name]])

    }

    list_of_types <- lapply(index_column_names, get_type)

    # Creating the index datatypes
    # TODO - figure out how to create compound datatype using list_of_types
    compound_index <- H5T_COMPOUND$new(index_column_names,
                                   dtypes = list(h5types$H5T_NATIVE_INT, H5T_STRING$new(size = 3), h5types$H5T_NATIVE_INT,
                                   h5types$H5T_IEEE_F32LE, h5types$H5T_NATIVE_INT))

    index_space <- H5S$new(dim = c(cycles + 1, 1, nrow(init)), maxdims = c(cycles + 1, 1, Inf))
    #index_space <- H5S$new(dim = c(cycles, 1, nrow(init)), maxdims = c(cycles, 1, Inf))

    # Above should be implemented like this.
    compound_output <- H5T_COMPOUND$new(names(strategy$states),
                                   dtypes = rep(list(eval(Quote(h5types$H5T_IEEE_F32LE))), strategy$state_number))

    #index_space <- H5S$new(dim = c(cycles + 1, 1, nrow(init)), maxdims = c(cycles + 1, 1, Inf))

    output_space <- H5S$new(dim = c(cycles + 1, 5, nrow(init)), maxdims = c(cycles + 1, 5, Inf))
    #output_space <- H5S$new(dim = c(cycles, 5, nrow(init)), maxdims = c(cycles, 5, Inf))

    # create a table to hold parameter values
    #cols <- length(parameters) + 2 # llh and accepted

    #col_names <- c(names(parameters), 'log likelihood', 'accepted')
    

    #parameters_type <- h5types$H5T
    #H5T_FLOAT$new()


    if ('index' %in% names(winter_h5[[strategy$properties$my_name$expr]])) {

        index_grp$link_delete('index')
        index_grp$create_dataset(name = 'index', space = index_space,
                                                        dtype = compound_index)
    } else {

        index_grp$create_dataset(name = 'index', space = index_space,
                                                        dtype = compound_index)
    }


    if (!('output' %in% list.datasets(scenario_grp))) {

        scenario_grp$create_dataset(name = 'output', space = output_space, dtype = compound_output)

    }

    # browser()
    

    # parameter_space <- H5S$new( dim = c(1,cols), maxdims = c(Inf, cols))


    #if (!('parameter' %in% list.datasets(parameter_grp))) {

        #parameter_grp$create_dataset(name = 'parameter', space = output_space, dtype = compound_output)

    #}


    index_ds <- index_grp[['index']]



    x <- copy(init[, ..index_column_names])


    index_ds[0, 1,] <- x

    output_ds <- scenario_grp[['output']]

    output_ds[0, 1,] <- init[, ..state_names]

    list(scenario_grp, index_grp)

}

# Initialise HDF5 for das
initialise_hfd5.dsa <- function(strategy.grp, strategy_p1, strategy_p2, my_name, a_run, strategy) {

    cycles <- strategy$cycles

    dsa <- paste('dsa', a_run, sep = '_')
    group_name <- paste(strategy$properties$my_name$expr, paste(strategy_p1, strategy_p2, sep = "_"), sep = '/')


    # Create the scenario groups in HFD5

    if (!(strategy$properties$my_name$expr %in% names(winter_h5))) {
        index_grp <- winter_h5$create_group(paste(strategy$properties$my_name$expr))

    } else {

        index_grp <- winter_h5[[strategy$properties$my_name$expr]]
    }

    if (a_run == '1') {

        scenario_grp <- winter_h5$create_group(group_name)

    } else {

        scenario_grp <- winter_h5[[group_name]]

    }

    # Creating a 3D array for the index data set
    # TODO - change all hard coded values to refer winter_input_dt for attributes
    compound_index <- H5T_COMPOUND$new(c('AGEP', 'ISO3', 'YARP', 'NUMP', 'LTBP', 'AGERP', 'SEXP'),
                                   dtypes = list(h5types$H5T_NATIVE_INT, H5T_STRING$new(size = 3), h5types$H5T_NATIVE_INT,
                                   h5types$H5T_IEEE_F32LE, h5types$H5T_IEEE_F32LE, h5types$H5T_NATIVE_INT, H5T_STRING$new(size = 3)))

    index_space <- H5S$new(dim = c(cycles + 1, 1, nrow(init)), maxdims = c(cycles + 1, 1, Inf))

    # Above should be implemented like this.
    compound_output <- H5T_COMPOUND$new(names(strategy$states),
                                   dtypes = rep(list(eval(Quote(h5types$H5T_IEEE_F32LE))), strategy$state_number))

    #index_space <- H5S$new(dim = c(cycles + 1, 1, nrow(init)), maxdims = c(cycles + 1, 1, Inf))

    output_space <- H5S$new(dim = c(cycles + 1, 5, nrow(init)), maxdims = c(cycles + 1, 5, Inf))

    if ('index' %in% names(winter_h5[[strategy$properties$my_name$expr]])) {

        index_grp$link_delete('index')
        index_grp$create_dataset(name = 'index', space = index_space,
                                                        dtype = compound_index)
    } else {

        index_grp$create_dataset(name = 'index', space = index_space,
                                                        dtype = compound_index)
    }

    scenario_grp$create_dataset(name = dsa, space = output_space, dtype = compound_output)

    index_ds <- index_grp[['index']]

    x <- copy(init[, .(AGEP, ISO3, YARP, NUMP, LTBP, AGERP, SEXP)])

    index_ds[1, 1,] <- x

    output_ds <- scenario_grp[[dsa]]

    output_ds[1, 1,] <- init[, ..state_names]

    list(scenario_grp, index_grp)

}

#browser()

#group_name <- "baseline/Mammogram/MCMC/0"
#group_elements <- strsplit(group_name, '/')[[1]]

#build_list <- function(item, res) {
#if (length(res) > 1) {
#res <- build_list(tail(item, -1), res[[item]])
#} else {
#res = c(res, list(item))
#}
#res
#}

#res <- list()

#for (each in group_elements) {
#res <- build_list(each, res)
#}

#browser()

#head(res, -1)
#while (!group_name %in% list.groups(winter_h5)) {

#for (each in group )


#}