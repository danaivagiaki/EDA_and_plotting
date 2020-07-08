library(tidyverse)
library(ggpubr)


## Class weights 

# Precision - Recall
PR_simple_weights <- read_delim("./Analysis/Precision_Recall_simple_model_weighted.tsv", delim = "\t")
PR_wide_weights <- read_delim("./Analysis/Precision_Recall_wide_model_weighted.tsv", delim = "\t")
PR_bidir_weights <- read_delim("./Analysis/Precision_Recall_bidir_model_weighted.tsv", delim = "\t")
PR_deep_weights <- read_delim("./Analysis/Precision_Recall_deep_model_weighted.tsv", delim = "\t")
PR_deep_bidir_weights <- read_delim("./Analysis/Precision_Recall_deep_bidir_model_weighted.tsv", delim = "\t")

PR_simple_weights <- mutate(PR_simple_weights, Model = rep("Simple", nrow(PR_simple_weights)))
PR_wide_weights <- mutate(PR_wide_weights, Model = rep("Wide", nrow(PR_wide_weights)))
PR_bidir_weights <- mutate(PR_bidir_weights, Model = rep("Bidirectional", nrow(PR_bidir_weights)))
PR_deep_weights <- mutate(PR_deep_weights, Model = rep("Deep", nrow(PR_deep_weights)))
PR_deep_bidir_weights <- mutate(PR_deep_bidir_weights, Model = rep("Deep bidirectional", nrow(PR_deep_bidir_weights)))

PR_weights <- bind_rows(bind_rows(bind_rows(PR_bidir_weights, bind_rows(PR_simple_weights, PR_wide_weights)), PR_deep_weights), PR_deep_bidir_weights)

# ROC
ROC_simple_weights <- read_delim("./Analysis/FPR_TPR_simple_model_weighted.tsv", delim = "\t")
ROC_wide_weights <- read_delim("./Analysis/FPR_TPR_wide_model_weighted.tsv", delim = "\t")
ROC_bidir_weights <- read_delim("./Analysis/FPR_TPR_bidir_model_weighted.tsv", delim = "\t")
ROC_deep_weights <- read_delim("./Analysis/FPR_TPR_deep_model_weighted.tsv", delim = "\t")
ROC_deep_bidir_weights <- read_delim("./Analysis/FPR_TPR_deep_bidir_model_weighted.tsv", delim = "\t")

ROC_simple_weights <- mutate(ROC_simple_weights, Model = rep("Simple", nrow(ROC_simple_weights)))
ROC_wide_weights <- mutate(ROC_wide_weights, Model = rep("Wide", nrow(ROC_wide_weights)))
ROC_bidir_weights <- mutate(ROC_bidir_weights, Model = rep("Bidirectional", nrow(ROC_bidir_weights)))
ROC_deep_weights <- mutate(ROC_deep_weights, Model = rep("Deep", nrow(ROC_deep_weights)))
ROC_deep_bidir_weights <- mutate(ROC_deep_bidir_weights, Model = rep("Deep bidirectional", nrow(ROC_deep_bidir_weights)))

ROC_weights <- bind_rows(bind_rows(bind_rows(ROC_bidir_weights, bind_rows(ROC_simple_weights, ROC_wide_weights)), ROC_deep_weights), ROC_deep_bidir_weights)


##  Undersampling

# Precision - Recall
PR_simple_undersampling <- read_delim("./Analysis/Precision_Recall_simple_model_undersampling.tsv", delim = "\t")
PR_wide_undersampling <- read_delim("./Analysis/Precision_Recall_wide_model_undersampling.tsv", delim =  "\t")
PR_bidir_undersampling <- read_delim("./Analysis/Precision_Recall_bidir_model_undersampling.tsv", delim =  "\t")
PR_deep_undersampling <- read_delim("./Analysis/Precision_Recall_deep_model_undersampling.tsv", delim =  "\t")
PR_deep_bidir_undersampling <- read_delim("./Analysis/Precision_Recall_deep_bidir_model_undersampling.tsv", delim =  "\t")

PR_simple_undersampling <- mutate(PR_simple_undersampling, Model = rep("Simple", nrow(PR_simple_undersampling)))
PR_wide_undersampling <- mutate(PR_wide_undersampling, Model = rep("Wide", nrow(PR_wide_undersampling)))
PR_bidir_undersampling <- mutate(PR_bidir_undersampling, Model = rep("Bidirectional", nrow(PR_bidir_undersampling)))
PR_deep_undersampling <- mutate(PR_deep_undersampling, Model = rep("Deep", nrow(PR_deep_undersampling)))
PR_deep_bidir_undersampling <- mutate(PR_deep_bidir_undersampling, Model = rep("Deep bidirectional", nrow(PR_deep_bidir_undersampling)))

PR_undersampling <- bind_rows(bind_rows(bind_rows(PR_bidir_undersampling, bind_rows(PR_simple_undersampling, PR_wide_undersampling)), PR_deep_undersampling), PR_deep_bidir_undersampling)

# ROC
ROC_simple_undersampling <- read_delim("./Analysis/FPR_TPR_simple_model_undersampling.tsv", delim = "\t")
ROC_wide_undersampling <- read_delim("./Analysis/FPR_TPR_wide_model_undersampling.tsv", delim = "\t")
ROC_bidir_undersampling <- read_delim("./Analysis/FPR_TPR_bidir_model_undersampling.tsv", delim = "\t")
ROC_deep_undersampling <- read_delim("./Analysis/FPR_TPR_deep_model_undersampling.tsv", delim = "\t")
ROC_deep_bidir_undersampling <- read_delim("./Analysis/FPR_TPR_deep_bidir_model_undersampling.tsv", delim = "\t")

ROC_simple_undersampling <- mutate(ROC_simple_undersampling, Model = rep("Simple", nrow(ROC_simple_undersampling)))
ROC_wide_undersampling <- mutate(ROC_wide_undersampling, Model = rep("Wide", nrow(ROC_wide_undersampling)))
ROC_bidir_undersampling <- mutate(ROC_bidir_undersampling, Model = rep("Bidirectional", nrow(ROC_bidir_undersampling)))
ROC_deep_undersampling <- mutate(ROC_deep_undersampling, Model = rep("Deep", nrow(ROC_deep_undersampling)))
ROC_deep_bidir_undersampling <- mutate(ROC_deep_bidir_undersampling, Model = rep("Deep bidirectional", nrow(ROC_deep_bidir_undersampling)))

ROC_undersampling <- bind_rows(bind_rows(bind_rows(ROC_bidir_undersampling, bind_rows(ROC_simple_undersampling, ROC_wide_undersampling)), ROC_deep_undersampling), ROC_deep_bidir_undersampling)


## Oversampling

# Precision - Recall
PR_simple_oversampling <- read_delim("./Analysis/Precision_Recall_simple_model_oversampling.tsv", delim = "\t")
PR_wide_oversampling <- read_delim("./Analysis/Precision_Recall_wide_model_oversampling.tsv", delim = "\t")
PR_bidir_oversampling <- read_delim("./Analysis/Precision_Recall_bidir_model_oversampling.tsv", delim = "\t")
PR_deep_oversampling <- read_delim("./Analysis/Precision_Recall_deep_model_oversampling.tsv", delim = "\t")
PR_deep_bidir_oversampling <- read_delim("./Analysis/Precision_Recall_deep_bidir_model_oversampling.tsv", delim = "\t")

PR_simple_oversampling <- mutate(PR_simple_oversampling, Model = rep("Simple", nrow(PR_simple_oversampling)))
PR_wide_oversampling <- mutate(PR_wide_oversampling, Model = rep("Wide", nrow(PR_wide_oversampling)))
PR_bidir_oversampling <- mutate(PR_bidir_oversampling, Model = rep("Bidirectional", nrow(PR_bidir_oversampling)))
PR_deep_oversampling <- mutate(PR_deep_oversampling, Model = rep("Deep", nrow(PR_deep_oversampling)))
PR_deep_bidir_oversampling <- mutate(PR_deep_bidir_oversampling, Model = rep("Deep bidirectional", nrow(PR_deep_bidir_oversampling)))

PR_oversampling <- bind_rows(bind_rows(bind_rows(PR_bidir_oversampling, bind_rows(PR_simple_oversampling, PR_wide_oversampling)), PR_deep_oversampling), PR_deep_bidir_oversampling)

# ROC
ROC_simple_oversampling <- read_delim("./Analysis/FPR_TPR_simple_model_oversampling.tsv", delim = "\t")
ROC_wide_oversampling <- read_delim("./Analysis/FPR_TPR_wide_model_oversampling.tsv", delim = "\t")
ROC_bidir_oversampling <- read_delim("./Analysis/FPR_TPR_bidir_model_oversampling.tsv", delim = "\t")
ROC_deep_oversampling <- read_delim("./Analysis/FPR_TPR_deep_model_oversampling.tsv", delim = "\t")
ROC_deep_bidir_oversampling <- read_delim("./Analysis/FPR_TPR_deep_bidir_model_oversampling.tsv", delim = "\t")

ROC_simple_oversampling <- mutate(ROC_simple_oversampling, Model = rep("Simple", nrow(ROC_simple_oversampling)))
ROC_wide_oversampling <- mutate(ROC_wide_oversampling, Model = rep("Wide", nrow(ROC_wide_oversampling)))
ROC_bidir_oversampling <- mutate(ROC_bidir_oversampling, Model = rep("Bidirectional", nrow(ROC_bidir_oversampling)))
ROC_deep_oversampling <- mutate(ROC_deep_oversampling, Model = rep("Deep", nrow(ROC_deep_oversampling)))
ROC_deep_bidir_oversampling <- mutate(ROC_deep_bidir_oversampling, Model = rep("Deep bidirectional", nrow(ROC_deep_bidir_oversampling)))

ROC_oversampling <- bind_rows(bind_rows(bind_rows(ROC_bidir_oversampling, bind_rows(ROC_simple_oversampling, ROC_wide_oversampling)), ROC_deep_oversampling), ROC_deep_bidir_oversampling)



weights_plot <- ggarrange(ggplot(ROC_weights) +
                            geom_abline(slope=1, intercept = 0, linetype = "dashed") + 
                            geom_line(aes(x=FPR, y=TPR, color=Model, linetype=Dataset)) +
                            ylim(0, 1) +
                            guides(color=FALSE, linetype=FALSE) +
                            labs(x="FPR", y="TPR") +
                            scale_linetype_manual(breaks = c("Test", "Training"), labels = c("HC test", "HC training"), values = c("dotted", "solid")) +
                            theme_bw() +
                            theme(title = element_text(face="bold", size=15), axis.text = element_text(size = 12)),
                          ggplot(PR_weights) +
                            geom_abline(slope=1, intercept = 0, linetype = "dashed") + 
                            geom_line(aes(x=Recall, y=Precision, color=Model, linetype=Dataset)) +
                            ylim(0, 1) +
                            guides(color=FALSE, linetype=FALSE) +
                            labs(x="Recall", y="Precision") +
                            scale_linetype_manual(breaks = c("Test", "Training"), labels = c("HC test", "HC training"), values = c("dotted", "solid")) +
                            theme_bw() +
                            theme(title = element_text(face="bold", size=15), axis.text = element_text(size = 12)),
                          ncol = 2)

weights_plot <- annotate_figure(weights_plot, fig.lab = "A.", fig.lab.face = "bold", fig.lab.size = 18)



undersampling_plot <- ggarrange(ggplot(ROC_undersampling) +
                                  geom_abline(slope=1, intercept = 0, linetype = "dashed") + 
                                  geom_line(aes(x=FPR, y=TPR, color=Model, linetype=Dataset)) +
                                  ylim(0, 1) +
                                  guides(color=FALSE, linetype=FALSE) +
                                  labs(x="FPR", y="TPR") +
                                  scale_linetype_manual(breaks = c("Test", "Training"), labels = c("HC test", "HC training"), values = c("dotted", "solid")) +
                                  theme_bw() +
                                  theme(title = element_text(face="bold", size=15), axis.text = element_text(size = 12)),
                                ggplot(PR_undersampling) +
                                  geom_abline(slope=1, intercept = 0, linetype = "dashed") + 
                                  geom_line(aes(x=Recall, y=Precision, color=Model, linetype=Dataset)) +
                                  ylim(0, 1) +
                                  guides(color=FALSE, linetype=FALSE) +
                                  labs(x="Recall", y="Precision") +
                                  scale_linetype_manual(breaks = c("Test", "Training"), labels = c("HC test", "HC training"), values = c("dotted", "solid")) +
                                  theme_bw() +
                                  theme(title = element_text(face="bold", size=15), axis.text = element_text(size = 12)),
                                ncol = 2)

undersampling_plot <- annotate_figure(undersampling_plot, fig.lab = "??.", fig.lab.face = "bold", fig.lab.size = 18)


oversampling_plot <- ggarrange(ggplot(ROC_oversampling) +
                                 geom_abline(slope=1, intercept = 0, linetype = "dashed") + 
                                 geom_line(aes(x=FPR, y=TPR, color=Model, linetype=Dataset)) +
                                 ylim(0, 1) +
                                 labs(x="FPR", y="TPR") +
                                 scale_linetype_manual(breaks = c("Test", "Training"), labels = c("HC test", "HC training"), values = c("dotted", "solid")) +
                                 theme_bw() +
                                 theme(title = element_text(face="bold", size=15), axis.text = element_text(size = 12)),
                               ggplot(PR_oversampling) +
                                 geom_abline(slope=1, intercept = 0, linetype = "dashed") + 
                                 geom_line(aes(x=Recall, y=Precision, color=Model, linetype=Dataset)) +
                                 ylim(0, 1) +
                                 labs(x="Recall", y="Precision") +
                                 scale_linetype_manual(breaks = c("Test", "Training"), labels = c("HC test", "HC training"), values = c("dotted", "solid")) +
                                 theme_bw() +
                                 theme(title = element_text(face="bold", size=15), axis.text = element_text(size = 12), legend.title = element_text(size=18), legend.text = element_text(size = 18, face="bold"), legend.position = "bottom"),
                               ncol = 2, common.legend = TRUE, legend = "bottom")

oversampling_plot <- annotate_figure(oversampling_plot, fig.lab = "C.", fig.lab.face = "bold", fig.lab.size = 18)


class_imbalance_plot <- ggarrange(weights_plot, undersampling_plot, oversampling_plot, nrow = 3)
class_imbalance_plot
