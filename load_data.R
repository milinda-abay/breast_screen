

clean_up_indicator_1 <- function(DT) {

    DT[`AGE GROUP (YEARS)` == '85+', `AGE GROUP (YEARS)` := '85–100'] # all 85+ year old women considered 85-100
    DT[, c('l_age', 'u_age') := tstrsplit(`AGE GROUP (YEARS)`, '–', fixed = TRUE, type.convert = TRUE)]

    DT[Period %like% '-', Period := gsub('-', '–', Period)]
    DT[, c('l_period', 'u_period') := tstrsplit(Period, '–', fixed = TRUE, type.convert = TRUE)]

    DT[, c('Mean_1') := NUM_WOMEN / (u_period - l_period + 1)]
    DT[DT, Mean_2 := i.Mean_1, on = c(l_period = 'u_period', 'STATE', 'l_age', 'u_age')]
    DT[, calculated_mean := rowMeans(DT[, .(Mean_1, Mean_2)], na.rm = TRUE)]
             
    DT[, c('AGE GROUP (YEARS)', 'Period', 'Mean_1', 'Mean_2') := NULL]

    DT[, calculated_mean := calculated_mean / (u_age - l_age + 1)]

    DT[, list(age = seq(from = l_age, to = u_age)), by = .(NUM_WOMEN, STATE, l_period, calculated_mean)]

}

clean_up_indicator_2345 <- function(DT) {

    # all ages 70+  considered 70-85
    # all ages 75+ considered 75-85


    DT[`AGE GROUP (YEARS)` == '70+', `AGE GROUP (YEARS)` := '70–85']
    DT[`AGE GROUP (YEARS)` == '75+', `AGE GROUP (YEARS)` := '75–85']
    DT[, c('l_age', 'u_age') := tstrsplit(`AGE GROUP (YEARS)`, '–', fixed = TRUE, type.convert = TRUE)]
    DT[, c('AGE GROUP (YEARS)') := NULL]

}

clean_up_indicator_6 <- function(DT) {

    # all 70+ year old women considered 70-85
    DT[Detected %like% '–', Detected := gsub('–', '-', Detected)]
    DT[`AGE GROUP (YEARS)` == '70+', `AGE GROUP (YEARS)` := '70–85']
    DT[, c('l_age', 'u_age') := tstrsplit(`AGE GROUP (YEARS)`, '–', fixed = TRUE, type.convert = TRUE)]
    DT[, c('l_period', 'u_period') := tstrsplit(Period, '-', fixed = TRUE, type.convert = TRUE)]

    DT[, Mean_1 := NUM_WOMEN / (u_period - l_period + 1)]
    DT[, c('l_period_1', 'l_period_2') := .(l_period - 1, l_period - 2)]
    DT[DT, Mean_2 := i.Mean_1, on = c(l_period_1 = 'l_period', 'u_age', 'l_age', 'STATE', 'Screen', 'Detected', 'Cancer')]
    DT[DT, Mean_3 := i.Mean_1, on = c(l_period_2 = 'l_period', 'u_age', 'l_age', 'STATE', 'Screen', 'Detected', 'Cancer')]
    DT[, calculated_mean := rowMeans(DT[, .(Mean_1, Mean_2, Mean_3)], na.rm = TRUE)]
    DT[, c('AGE GROUP (YEARS)', 'Period', 'l_period_1', 'l_period_2', 'Mean_1', 'Mean_2', 'Mean_3') := NULL]

}

clean_up_indicator_7 <- function(DT) {

    # <20 year olds considered 0 - 20

    DT[`AGE GROUP (YEARS)` == '<20', `AGE GROUP (YEARS)` := '0–20']
    DT[`AGE GROUP (YEARS)` == '<30', `AGE GROUP (YEARS)` := '0–30']
    DT[`AGE GROUP (YEARS)` == '30-39', `AGE GROUP (YEARS)` := '30–39']
    
    DT[`AGE GROUP (YEARS)` == '85+', `AGE GROUP (YEARS)` := '85–85']

    if (any(DT$Period %like% '–')) {

        DT[, c('l_period', 'u_period') := tstrsplit(Period, '–', fixed = TRUE, type.convert = TRUE)]
        DT[, c('Period') := NULL]

    }

    DT[, c('l_age', 'u_age') := tstrsplit(`AGE GROUP (YEARS)`, '–', fixed = TRUE, type.convert = TRUE)]
    DT[, c('AGE GROUP (YEARS)') := NULL]

}


data_path <- "M:/Documents/@Projects/BCS/data/PBI Indicators/"

ind1_participation <- fread(paste(data_path, 'Participation_1.csv', sep = ''))
ind1_population <- fread(paste(data_path, 'Population_1.csv', sep = ''))
ind2_rescreen <- fread(paste(data_path, 'Rescreening_2.csv', sep = ''))
ind3_recall <- fread(paste(data_path, 'Recall_3.csv', sep = ''))
ind4_cancer <- fread(paste(data_path, 'Cancer_4.csv', sep = ""))
ind5_dcis <- fread(paste(data_path, 'DCIS_5.csv', sep = ''))
ind6_interval <- fread(paste(data_path, 'Interval_6.csv', sep = ''))
ind7_cancer <- fread(paste(data_path, 'Cancer_7.csv', sep = ""))
ind7_cancer_state <- fread(paste(data_path, 'Cancer_state_7.csv', sep = ""))
ind7_dcis <- fread(paste(data_path, 'DCIS_7.csv', sep = ''))


ind1_participation <- clean_up_indicator_1(ind1_participation)
ind1_population <- clean_up_indicator_1(ind1_population)
ind2_rescreen <- clean_up_indicator_2345(ind2_rescreen)
ind3_recall <- clean_up_indicator_2345(ind3_recall)
ind4_cancer <- clean_up_indicator_2345(ind4_cancer)
ind5_dcis <- clean_up_indicator_2345(ind5_dcis)
ind6_interval <- clean_up_indicator_6(ind6_interval)
ind7_cancer <- clean_up_indicator_7(ind7_cancer)
ind7_cancer_state <- clean_up_indicator_7(ind7_cancer_state)
ind7_dcis <- clean_up_indicator_7(ind7_dcis)








