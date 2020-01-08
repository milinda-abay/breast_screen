
# tow create a HDF5 file
winter_h5 <- H5File$new("Winter.h5", mode = "a")

# give all the constants and types
h5const$overview
h5types$overview

# close file
winter_h5$close_all()

# to delete a dataset
scenario_grp$link_delete("index2")

input.grp <- winter_h5$create_group("input")
output.grp <- winter_h5$create_group("output")

# useful for listing the groups, datasets and attributes
list.groups(winter_h5)
list.datasets(winter_h5)
list.attributes(winter_h5[['S2/QTFGIT_4R/dsa_2']])

# Creating datatypes
str_fixed_len <- H5T_STRING$new(size = 3)
str_var_length <- H5T_STRING$new(size = Inf)
logical_example <- H5T_LOGICAL$new(include_NA = TRUE)
array_example <- H5T_ARRAY$new(dims = c(3, 4), dtype_base = h5types$H5T_NATIVE_INT)



compound_index <- H5T_COMPOUND$new(c('AGEP', 'ISO3', 'YARP', 'NUMP', 'LTBP', 'AGERP', 'SEXP'),
                                   dtypes = list(h5types$H5T_NATIVE_INT, H5T_STRING$new(size = 3), h5types$H5T_NATIVE_INT,
                                   h5types$H5T_IEEE_F32LE, h5types$H5T_IEEE_F32LE, h5types$H5T_NATIVE_INT, H5T_STRING$new(size = 3)))

data_space <- H5S$new(dim = c(10, 1, 10), maxdims = c(Inf, 1, 10))

# Creates the properties of the dataset
data_set_properties <- H5P_DATASET_CREATE$new()
data_set_properties$set_fill_value(compound_index, value=list(0,NULL,0,0.0,0.0,0,NULL))


winter_h5[['S2/TST15_4R/']]$create_dataset(name = 'index2', space = data_space,
                                                        dtype = compound_index)

index2_ds <- winter_h5[['S2/TST15_4R/index2']]


index2_ds[, 1, 1] <- x


index2_ds[1:100,1,10]


x <- winter_input_dt[1:10, .(AGEP,ISO3,YARP,NUMP,LTBP,AGERP,SEXP)]

winter_h5[['S2']]

# To delete datasets
winter_h5[['S2/TST15_4R/']]$link_delete("index2")


winter_h5[["calculator_index"]] <- vic.mortality

# Hack job to create the N dimensional input dataset of array[cycle,state,outputs] e.g. 10yr , 20 state, 5 outputs
dim_age <- length(unique(winter_input_dt[,AGEP]))
dim_iso <- length(unique(winter_input_dt[,ISO3]))
dim_yarp <- length(unique(winter_input_dt[, YARP]))
dim_sexp <- length(unique(winter_input_dt[, SEXP]))

winter_h5[['S2']]
winter_h5[['S2/TST15_4R']]
ds <- winter_h5[['S2/calculator_state_count']]

ds[]





# grab input data
x <- winter_input_dt[, ..state_names]

x <- unlist(x)

x[1:195000,,]


dim(x) <- c(nrow(winter_input_dt), 20, 1)
x <- aperm(x, perm = c(3, 2, 1))

internal_array <- array(x, dim = c(1, 20), dimnames = list(NULL, state_names))


input_array <- H5T_ARRAY$new(dims = c(1,20), dtype_base = h5types$H5T_IEEE_F64LE)
space_ds <- H5S$new(dims = c(dim_age, dim_iso, dim_yarp, dim_sexp), maxdims = c(dim_age, dim_iso, dim_yarp, dim_sexp))


dataset_arrays <- H5P_DATASET_CREATE$new()
dataset_arrays$set_fill_value(input_array, rep(0, 20))


try1 <- input.grp$create_dataset(name = "winter_input", space = space_ds, dtype = input_array, dataset_create_pl = dataset_arrays)

x[,,][1:100]

x <- x[,, 1:190000]

try1[1:100, 1:95, 1:20, 1] <- x[1:190000,,]

try1[,,$write(args =NULL, x)


try1[100,95,20,1]





rm(try)



input.grp[["winter_input_dt"]] <- winter_input_dt
output.grp[["index"]] <- model_output$outputs$index
output.grp[["state_counts"]] <- model_output$outputs$state_count
output.grp[["state_costs"]] <- model_output$outputs$state_cost

input.grp$ls()
output.grp$ls()
uint2.grp$ls()




#creating a compound table
cpd_example <- H5T_COMPOUND$new(c("Double_col", "Int_col", "Logical_col", "array_col"), dtypes = list(h5types$H5T_NATIVE_DOUBLE,
    h5types$H5T_NATIVE_INT, logical_example, array_example))

cpd_example$get_class()

cpd_table <- create_empty(12, cpd_example)

cpd_table

output.grp[["cpd_table1"]] <- cpd_table

y <- output.grp[["cpd_table"]]

?H5T_ARRAY

x <- 1:144
dim(x) <- c(12, 3, 4)

output.grp[["3d_array"]] <- x

state_counts <- output.grp[["state_counts"]]
array3d <- output.grp[["3d_array"]]

array_type <- array3d$get_type()
array_type$get_class()

state_count_type <- state_counts$get_type()
state_count_type$get_class()

cat(state_count_type$to_text())

state_counts$dims
state_counts$maxdims
state_counts$chunk_dims


state_counts[1:5]

array3d[1,,] <- rep(-1, 12)
array3d[,,]

# create a dataset
uint2_dt <- h5types$H5T_NATIVE_UINT32$set_size(1)$set_precision(2)$set_sign(h5const$H5T_SGN_NONE)
space_ds <- H5S$new(dims = c(10, 10), maxdims = c(Inf, 10))

ds_create_pl_nbit <- H5P_DATASET_CREATE$new()
ds_create_pl_nbit$set_chunk(c(10, 10))$set_fill_value(uint2_dt, 1)$set_nbit()

uint2.grp <- file.h5$create_group("uint2")
uint2_ds_nbit <- uint2.grp$create_dataset(name = "nbit_filter", space = space_ds,
    dtype = uint2_dt, dataset_create_pl = ds_create_pl_nbit, chunk_dim = NULL,
    gzip_level = NULL)
uint2_ds_nbit[,] <- sample(0:3, size = 100, replace = TRUE)
uint2_ds_nbit$get_storage_size()

testtable <- output.grp$create_dataset(obj_empty,name = "mili", space=space_ds)

h5const$overview
h5types$overview

str_fixed_len <- H5T_STRING$new(size = 20)
cplx_example <- H5T_COMPLEX$new()
array_example <- H5T_ARRAY$new(dims = c(3, 4), dtype_base = h5types$H5T_NATIVE_INT)

cpd_several <- H5T_COMPOUND$new(c("STRING_fixed", "Double", "Complex", "Array"),
    dtypes = list(str_fixed_len, h5types$H5T_NATIVE_DOUBLE, cplx_example, array_example))
cat(cpd_several$to_text())

obj_empty <- create_empty(1, cpd_several)
obj_empty
class(obj_empty)
setDT(obj_empty)

output.grp[["compound_table"]] <- obj_empty

winter_h5$close_all()

file.h5$open("testing.h5")

saveRDS(pop_master, "pop_master.rds")


eval(x)

index_ds[1,1,]
output_ds[1, 1,]

class(NA)