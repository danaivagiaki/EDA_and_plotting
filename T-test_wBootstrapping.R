library(tidyverse)
library(ggpubr)
library(boot)

## T test for the blood measurements
labka_train <- read.table("./LABKA_train.tsv", sep="\t", fileEncoding = "ISO-8859-1", header = TRUE, na.strings = "NA")
labka_train_diabetes <- read.table("./LABKA_train_diabetes.tsv", sep="\t", fileEncoding = "ISO-8859-1", header = TRUE, na.strings = "NA")
labka_val <- read.table("./LABKA_val.tsv", sep="\t", fileEncoding = "ISO-8859-1", header = TRUE, na.strings = "NA")
labka_val_diabetes <- read.table("./LABKA_val_diabetes.tsv", sep="\t", fileEncoding = "ISO-8859-1", header = TRUE, na.strings = "NA")

iupac_measurement = tibble(`NPU_code`= c("NPU19763", "NPU02319", "NPU01961", "NPU02320", "NPU02321", "NPU01944", "NPU03562", "NPU03568",
                                         "NPU01960", "NPU18162", "NPU02593"),
                           `Name`= c("FERRITIN", "HGB", "HCT", "MCH", "MCHC", "MCV", "MPV", "PLT", "RBC", "RDW-CV", "WBC"))



labka_train <- mutate(labka_train, Diabetes = ifelse(PID %in% labka_train_diabetes$PID, 1, 0))
labka_val <- mutate(labka_val, Diabetes = ifelse(PID %in% labka_val_diabetes$PID, 1, 0))

labka_train_wide <- spread(labka_train, key = NPU_CODE, value = SHOWN_VALUE)
labka_val_wide <- spread(labka_val, key = NPU_CODE, value = SHOWN_VALUE)

normal_distr <- c("NPU01944", "NPU01960", "NPU01961", "NPU02321") # c("MCV", "RBC", "HCT", "MCHC")
not_normal_distr <- c("NPU02320", "NPU02593", "NPU03562", "NPU18162", "NPU19763", "NPU03568", "NPU02319") # c("MCH", "WBC", "MPV", "RDW-CV", "FERRITIN", "PLT", "HGB")


ttest_function <- function(dataset, indices){
  data <- dataset[indices,]
  data_normal <- filter(data, Diabetes == 0)
  data_diabetes <- filter(data, Diabetes == 1)
  t_statistic <- t.test(x=data_normal[,2], y=data_diabetes[,2], alternative = "two.sided", var.equal=FALSE)$statistic
  return(t_statistic)
}


print("Performing Welch t-test for the training set")
ttest_train <- data.frame(Measurement = normal_distr,
                          t_statistic = double(length(normal_distr)),
                          Lower_perc_boots_interv = double(length(normal_distr)),
                          Upper_perc_boots_interv = double(length(normal_distr)))

set.seed(32)
for(var in normal_distr){
  labka_measurement <- select(labka_train_wide, c(Diabetes, var))
  boots <- boot(data=labka_measurement, statistic=ttest_function, R=1000, parallel="multicore")
  boots_ci <- boot.ci(boots, type="perc")
  ttest_train[ttest_train$Measurement == var, 2:4] <- c(boots_ci$t0, boots_ci$percent[4], boots_ci$percent[5])
}

print(ttest_train)


print("Performing Welch t-test for the test set")
ttest_test <- data.frame(Measurement = normal_distr,
                          t_statistic = double(length(normal_distr)),
                          Lower_perc_boots_interv = double(length(normal_distr)),
                          Upper_perc_boots_interv = double(length(normal_distr)))

set.seed(32)
for(var in normal_distr){
  print(var)
  labka_measurement <- select(labka_val_wide, c(Diabetes, var))
  boots <- boot(data=labka_measurement, statistic=ttest_function, R=1000, parallel="multicore")
  boots_ci <- boot.ci(boots, type="perc")
  ttest_test[ttest_test$Measurement == var, 2:4] <- c(boots_ci$t0, boots_ci$percent[4], boots_ci$percent[5])
}

print(ttest_test)
