library(tidyverse)
library(ggpubr)
library(rcompanion)


var <- scan(file="stdin", what=character())

print(var)

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


not_normal_distr <- c("NPU02320", "NPU02593", "NPU03562", "NPU18162", "NPU19763", "NPU03568", "NPU02319") # c("MCH", "WBC", "MPV", "RDW-CV", "FERRITIN", "PLT", "HGB")
stopifnot(var %in% not_normal_distr)


set.seed(32)
labka_measurement_train <- select(labka_train_wide, c(Diabetes, var))
wt_train <- wilcoxonR(x=labka_measurement_train[,var], g=labka_measurement_train[,"Diabetes"], ci = TRUE, R=1000)
writeLines(paste("\n", var, "in the training set\nWilcocon r:", wt_train[1], "\tLower bootstrap perc CI:", wt_train[2], "\tUpper bootstrap perc CI:", wt_train[3]))


set.seed(32)
labka_measurement_val <- select(labka_val_wide, c(Diabetes, var))
wt_val <- wilcoxonR(x=labka_measurement_val[,var], g=labka_measurement_val[,"Diabetes"], ci = TRUE, R=1000)
writeLines(paste("\n", var, "in the test set\nWilcocon r:", wt_val[1], "\tLower bootstrap perc CI:", wt_val[2], "\tUpper bootstrap perc CI:", wt_val[3]))

