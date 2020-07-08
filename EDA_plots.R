library(tidyverse)
library(ggpubr)
library(data.table)


## Hematological measurements

labka_train <- read.table("./LABKA_train.tsv", sep="\t", fileEncoding = "ISO-8859-1", header = TRUE, na.strings = "NA")
labka_train_diabetes <- read.table("./LABKA_train_diabetes.tsv", sep="\t", fileEncoding = "ISO-8859-1", header = TRUE, na.strings = "NA")
labka_val <- read.table("./LABKA_val.tsv", sep="\t", fileEncoding = "ISO-8859-1", header = TRUE, na.strings = "NA")
labka_val_diabetes <- read.table("/./LABKA_val_diabetes.tsv", sep="\t", fileEncoding = "ISO-8859-1", header = TRUE, na.strings = "NA")

iupac_measurement = tibble(`NPU_code`= c("NPU19763", "NPU02319", "NPU01961", "NPU02320", "NPU02321", "NPU01944", "NPU03562", "NPU03568",
                                         "NPU01960", "NPU18162", "NPU02593"),
                           `Name`= c("FERRITIN", "HGB", "HCT", "MCH", "MCHC", "MCV", "MPV", "PLT", "RBC", "RDW-CV", "WBC"))


blood_measurements_eda <- function(original_data, column_names, plot_title) {
  stopifnot(all(column_names == c("NPU_CODE", "SHOWN_VALUE", "UNITS")))
  if (!"tbl" %in% class(original_data)) { original_data <- as_tibble(original_data) }
  wide_format <- spread(original_data, key = column_names[1], value=column_names[2])
  measurements_only <- select(wide_format, starts_with("NPU"))
  for (i in seq(1, length(levels(original_data$NPU_CODE)))) {
    print(i)
    levels(original_data$NPU_CODE)[i] <- iupac_measurement$Name[which(iupac_measurement$NPU_code == levels(original_data$NPU_CODE)[i])]
    colnames(measurements_only)[i] <- iupac_measurement$Name[which(iupac_measurement$NPU_code == colnames(measurements_only)[i])]
  }
  
  violins_wrapped <- ggplot(original_data) +
    geom_violin(aes(x="Value", y=SHOWN_VALUE, colour=as.factor(Diabetes), fill=as.factor(Diabetes)), size=0.9, alpha=0.1) +
    labs(title = plot_title, x=NULL, y="Value") +
    scale_color_manual(name = "Diabetes", breaks = c(1, 0), labels = c("Yes", "No"), values = c("#3399FF", "#CC0000")) +
    scale_fill_manual(name = "Diabetes", breaks = c(1, 0), labels = c("Yes", "No"), values = c( "#3399FF", "#CC0000")) +
    guides(fill = FALSE) +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.0, face="bold", size=12),
          legend.title = element_text(face="bold"),
          axis.title.y = element_text(face="bold"),
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank()) +
    facet_wrap(~NPU_CODE, scales = "free")
  
  
  return(list("Violin_plots" = violins_wrapped))
  
}


labka_train <- mutate(labka_train, Diabetes = ifelse(PID %in% labka_train_diabetes$PID, 1, 0))
labka_val <- mutate(labka_val, Diabetes = ifelse(PID %in% labka_val_diabetes$PID, 1, 0))


eda_train <- blood_measurements_eda(labka_train, c("NPU_CODE", "SHOWN_VALUE", "UNITS"), "A.")
pdf("./Violin_plots_LABKA_train.pdf")
print(eda_train$Violin_plots)
dev.off()

eda_val <- blood_measurements_eda(labka_val, c("NPU_CODE", "SHOWN_VALUE", "UNITS"), "B.")
pdf("./Violin_plots_LABKA_val.pdf")
print(eda_val$Violin_plots)
dev.off()


## Age

true_labels_train <- read_delim("./Analysis/True_Labels_Train.tsv", delim = "\t")
true_labels_val <- read_delim("./Analysis/True_Labels_Val.tsv", delim = "\t")


ggarrange(ggplot(true_labels_train) +
            geom_violin(aes(x="Age", y=AGE, colour=as.factor(T2D), fill=as.factor(T2D)), size=0.9, alpha=0.1) +
            labs(title = "A.", x=NULL, y="Age") +
            guides(fill=FALSE) +
            scale_fill_manual(name = "Diabetes", breaks = c(1, 0), labels = c("Yes", "No"), values = c("#3399FF", "#CC0000")) +
            scale_color_manual(name = "Diabetes", breaks = c(1, 0), labels = c("Yes", "No"), values = c("#3399FF", "#CC0000")) +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5, face="bold", size=18), axis.text.x = element_blank(), axis.text.y = element_text(size = 12),
                  axis.title.y = element_text(size = 16), legend.title = element_text(size=18, face="bold")),
          ggplot(true_labels_val) +
            geom_violin(aes(x="Age", y=AGE, colour=as.factor(T2D), fill=as.factor(T2D)), size=0.9, alpha=0.1) +
            labs(title = "B.", x=NULL, y="Age") +
            guides(fill=FALSE) +
            scale_fill_manual(name = "Diabetes", breaks = c(1, 0), labels = c("Yes", "No"), values = c("#3399FF", "#CC0000")) +
            scale_color_manual(name = "Diabetes", breaks = c(1, 0), labels = c("Yes", "No"), values = c("#3399FF", "#CC0000")) +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5, face="bold", size=18), axis.text.x = element_blank(), axis.text.y = element_text(size = 12),
                  axis.title.y = element_text(size = 16), legend.title = element_text(size=18, face="bold")),
          ncol=2, common.legend = TRUE, legend = "bottom")

