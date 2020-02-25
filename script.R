
winter_output[,1,1]

winter_index[,,1]

x <- c(1:12)
dim(x) <- c(3,4)

x


strategy$parameters


DT[, list(age = seq(from = l_age, to = u_age)), by = .N]


ind4_cancer <- fread(paste(data_path, 'Cancer_4.csv', sep = ""))
ind4_cancer <- clean_up_indicator_2345(ind4_cancer)


x[Period == 2006 & NUM_WOMEN == 3,]

DT[, l_age, by = .N]

x <- DT

DT
DT[, length(l_age), by = seq_len(nrow(DT))]
DT[(NUM_WOMEN == 39 | NUM_WOMEN == 15) & STATE == 'NT',,]

winter_results[1, 1, 1]

winter_results[9, 1,][, 11,]

dim(winter_results[, 3,])
winter_results <- winter_results[[1]][['output']]

sum(winter_results[9, 1,][, 11:12])

winter_results[9, 1,][, 11:12]


sum(winter_results[, 1,][])

x <- winter_results[1, 1, 1:4376]


dim(x) <- c(9, 12)

plot((winter_results[, 1, 1][, 11:12])


sum(ind4_cancer[, 5])
apply(ind1_participation[, 4], 1, sum)


winter_input_dt[, inflow := calculated_mean,]


w_2008 <- winter_input_dt[l_period == 2008]


winter_input_dt[w_2008,, on = .(STATE, l_period, age)]


ind1_population[age == 40,]

ind1_population
ind1_participation

setnames(ind1_population, c('NUM_WOMEN', 'STATE', 'l_period', 'calculated_mean'),
         c('original_population', 'state', 'year', 'calculated_population'))

setnames(ind1_participation, c('NUM_WOMEN', 'STATE', 'l_period', 'calculated_mean'),
         c('orig_participation', 'state', 'year', 'calc_part'))



ind1_population[ind1_participation,, on = .(STATE, l_period, age)]

index_ds[1, 1,][1:10,]
output_ds[0, 1,][1:10,]

output_ds[1, 1,] <- init[, ..state_names]
index_ds <- NULL