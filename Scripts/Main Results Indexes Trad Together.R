# Main results 

# read data
survey <- read.csv('Datasets/survey_final_jointV3_ICW_Both.csv')

# Aux for rewight analysis
weight1 <- FALSE
weight2 <- FALSE

# ------------------------------------------------------------------------------
# Auxiliary variables for stargazer
covariates <- c("Course and verification", "Course", "Verification")
interaction_name <- c("WhatsApp Delivery")

# Aux for regressions
treatments_separate <- c("curso_verificacion", "curso", "verificacion")
interacted <- c("WhatsApp")
covariates_interacted <- c(covariates, paste(covariates, interaction_name, sep = " x "))

additional_note <- ""

# Summary of main indexes ------------------------------------------------------
aux <- c("index_mec_importance_end", "false_traditional_end", "false_social_end", 
         "trust_traditional_end", "trust_social_end", "index_mec_attention",
         "index_know_identify", "index_know_verify", 
         "consumption_traditional_end", "consumption_social_end",
         "index_sharing", "index_verifying")

aux_data <- survey[aux]

dep_var <- c("\\shortstack{Mis-\\\\information \\\\ importance \\\\ index}",
             "\\shortstack{Decrease \\\\ likelihood of \\\\ false of \\\\ traditional \\\\sources \\\\index}", 
             "\\shortstack{Increase \\\\ likelihood of \\\\ false of \\\\ social media \\\\sources \\\\index}", 
             "\\shortstack{Trust \\\\ traditional \\\\ sources \\\\ index}",
             "\\shortstack{Disrust \\\\ social media \\\\ sources \\\\ index}",
             "\\shortstack{Attention to \\\\ mis-\\\\information \\\\ index}",
             "\\shortstack{Increase \\\\ knowledge to \\\\ identify \\\\  information\\\\  index}",
             "\\shortstack{Increase \\\\ knowledge to \\\\ verify \\\\  information \\\\ index}",
             "\\shortstack{Increase \\\\ consumption \\\\ behavior \\\\ of traditional \\\\sources \\\\ index}",
             "\\shortstack{Decrease \\\\ consumption \\\\ behavior \\\\ of social media \\\\sources \\\\ index}",
             "\\shortstack{Decrease \\\\ sharing \\\\ behavior \\\\  index}",
             "\\shortstack{Increase \\\\ verifying  \\\\ behavior \\\\  index}")

control1 <- c("education", "gender", "edad", "index_mec_importance_baseline", 
              "false_baseline", "trust_traditional_baseline", "trust_social_baseline", "freq_analyze") # mechanisms
control2 <- c("education", "gender", "edad", "knowledge_identify_baseline") # knowledge to identify
control3 <- c("education", "gender", "edad", "index_know_verify_baseline") # Knowledge to verify
control4 <- c("education", "gender", "edad", 
              "consumption_traditional_baseline", "consumption_social_baseline", "freq_use_social", "freq_use_whats") # Consumption - Nonmissing observations 
control5 <- c("education", "gender", "edad", "index_sharing_baseline") # Sharing
control6 <- c("education", "gender", "edad", "ver_beh_baseline") # Verifying 

control_list <- list(control1, control1, control1, control1, control1, control1,
                     control2, control3, control4, control4, control5, control6)

omit_var <- c("Constant", "blockid1", control1, control2, control3, control4, 
              control5, control6, interacted)

title <- "Treatment effects on indexes by course delivered online and via WhatsApp"

# Create tables and save results
size = length(aux) + 1
cm = 21 
interacted_bool <- c(T,T,T,T,T,T,T,T,T,T,T,T,T)
label = "MainIndexes"
source('Scripts/General Scripts/ATEsGeneralSetUp.R')
panelA <- final_table

# Auxiliary regressions with robust s.e. for ggplot
lm_ate <- list()
count <- 1
for (x in aux) {
  
  fmla1 <- as.formula(paste0(x, "~ ", paste(treatments_separate, collapse = " + "), " + ",
                             paste(paste0(treatments_separate, ":", interacted), collapse = " + "), " + ",
                             paste(control_list[[count]], collapse = " + ")))
  
  nam1 <- paste("lm_", count, "_ols", sep = "")
  assign(nam1, lm_robust(fmla1, se_type = "HC0", data = survey, fixed_effects = blockid1, alpha = 0.1))
  
  lm_ate[[count]] <- get(nam1, envir = globalenv())
  
  count <- count + 1
}

# Run IPSW ----------------------------------------------------------------------
survey_attrition <- read.csv('Datasets/survey_final_jointV3_ICW_Attrition_Both.csv')

source('Scripts/General Scripts/PSMGeneralSetUpV4.R')
panelB <- final_table

# Auxiliary regressions with robust s.e. for ggplot
lm_psm <- list()
count <- 1
for (x in aux) {
  
  fmla1 <- as.formula(paste0(x, "~ ", paste(treatments_separate, collapse = " + "), " + ",
                             paste(paste0(treatments_separate, ":", interacted), collapse = " + "), " + ",
                             paste(control_list[[count]], collapse = " + ")))
  
  nam1 <- paste("lm_", count, "_ols", sep = "")
  assign(nam1, lm_robust(fmla1, data = df_aux, se_type = "HC0", weights = pr_score,  fixed_effects = blockid1, alpha = 0.1))
  
  lm_psm[[count]] <- get(nam1, envir = globalenv())
  
  count <- count + 1
}

# Run LASSO --------------------------------------------------------------------
controls <- c("edu7", "edu8", "edu9", "edu10", "gender", "edad25", "edad35", "edad45", "edad55", # Demographics
              paste0("freq_tipos_", 1:6), "freq_use_social", "freq_use_whats", # Consumption
              paste0("confia_", 1:6), # Trust
              paste0("case", c(1,3), "_hyp_2"), # False
              c(paste0("knowledge_plat_", 1:6), "knowledge_verify", paste0("characts_false_", c(1:10))), # Identify 
              c(paste0("problemas_", 1:6), "temas_2"), # Mech importance
              c("know_to_verify", paste0("forms_verify_", c(1:6)), "know_fact_checkers_sum"), # Know verify
              c("freq_desc_comp", paste0("case", c(1,3), "_hyp_1")), # Sharing
              c("freq_veri_compar"), # Verifying behavior
              "index_mec_importance_baseline", "false_baseline", "trust_traditional_baseline", "trust_social_baseline",  # Indexes
              "freq_analyze", "knowledge_identify_baseline", # Indexes
              "index_know_verify_baseline", "consumption_traditional_baseline", "consumption_social_baseline", # Indexes 
              "index_sharing_baseline", "ver_beh_baseline") # Indexes 

lagged <- c("index_mec_importance_baseline", "false_baseline", "false_baseline",
            "trust_traditional_baseline", "trust_social_baseline", "freq_analyze", "knowledge_identify_baseline",
            "index_know_verify_baseline", "consumption_traditional_baseline", "consumption_social_baseline", 
            "index_sharing_baseline", "ver_beh_baseline")

# Get LASSO covariates in the list "control_list" to be used in GeneralSetUp.R
source('Scripts/General Scripts/LASSO_Covariates.R')

omit_var <- c("Constant", "blockid1", controls, interacted)

# Create tables and save results
number_lasso <- length(controls)
source('Scripts/General Scripts/ATEsGeneralSetUp LASSO.R')
panelC <- final_table

# Auxiliary regressions with robust s.e. for ggplot
lm_lasso <- list()
count <- 1
for (x in aux) {
  
  fmla1 <- as.formula(paste0(x, "~ ", paste(treatments_separate, collapse = " + "), " + ",
                             paste(paste0(treatments_separate, ":", interacted), collapse = " + "), " + ",
                             paste(control_list[[count]], collapse = " + ")))
  
  nam1 <- paste("lm_", count, "_ols", sep = "")
  assign(nam1, lm_robust(fmla1, se_type = "HC0", data = survey, fixed_effects = blockid1,  alpha = 0.1))
  
  lm_lasso[[count]] <- get(nam1, envir = globalenv())
  
  count <- count + 1
}

# Append and save final table --------------------------------------------------
cat(main_panel(panelA, panelB, panelC, size, "\\\\shortstack\\{Mis-", 2.75, 2.5), file = "Tables/Appendix/Main_results.txt")

# Figures ----------------------------------------------------------------------

# ggplot setup 
text.11 <- element_text(color = "black", size = 11)
final.text.12 <- element_text(color = "black", size = 12, hjust = 0.5, family="Arial")
final.text.16 <- element_text(color = "black", size = 16, hjust = 0.5, family="Arial")
text.9 <- element_text(color = "black", size = 9)
text.16 <- element_text(color = "black", size = 16)

th <- theme(strip.text.x = final.text.11,
            strip.placement = "inside",
            strip.background = element_rect(colour = "black", fill = "white"),
            axis.text.x = element_blank(),
            axis.text.y = text.11,
            axis.title.x = text.16,
            axis.title.y = text.16,
            legend.text = final.text.16,
            legend.title = final.text.16,
            axis.text = final.text.12, 
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y =  element_line(color = "grey",
                                               size = 0.1,
                                               linetype = 1),
            axis.ticks.length.x = unit(.25, "cm"),
            panel.border = element_rect(colour = "black", fill = NA),
            legend.position = "bottom")

legend_guide <- guides(color = guide_legend(title.position = "top", title.hjust = 0.5))
legend_guide1 <- guides(color = guide_legend(title.position = "top", title.hjust = 0.5))
legend_guide2 <- guides(linetype = guide_legend(title.position = "top", title.hjust = 0.5))

colors <- c("#E7B800", "#0072B2","#D55E00")

y_text <-c("Treatment effect (90% C.I.)")

position_dodge = position_dodge(0.6)

test <- c("curso_verificacion + curso_verificacion:WhatsApp", 
          "curso + curso:WhatsApp",
          "verificacion + verificacion:WhatsApp")

# Main Figures -----------------------------------------------------------------

# Figure 3A: mechanisms
c1 <- as.numeric(c(lm_ate[[1]]$coefficients[1:3], lm_ate[[2]]$coefficients[1:3], lm_ate[[3]]$coefficients[1:3], lm_ate[[4]]$coefficients[1:3], lm_ate[[5]]$coefficients[1:3], lm_ate[[6]]$coefficients[1:3]))
c2 <- as.numeric(c(lm_ate[[1]]$conf.low[1:3], lm_ate[[2]]$conf.low[1:3], lm_ate[[3]]$conf.low[1:3], lm_ate[[4]]$conf.low[1:3], lm_ate[[5]]$conf.low[1:3], lm_ate[[6]]$conf.low[1:3]))
c3 <- as.numeric(c(lm_ate[[1]]$conf.high[1:3], lm_ate[[2]]$conf.high[1:3], lm_ate[[3]]$conf.high[1:3], lm_ate[[4]]$conf.high[1:3], lm_ate[[5]]$conf.high[1:3], lm_ate[[6]]$conf.high[1:3]))
c4 <- rep(c("Course and \nverification", "Course", "Verification"), 6)
c5 <- c(rep("Misinformation importance index",3),
        rep("Decrease likelihood of false of traditional sources index",3),
        rep("Increase likelihood of false of social media sources index",3),
        rep("Trust traditional sources index",3),
        rep("Distrust social media sources index",3),
        rep("Attention to misinformation index",3))

data_aux <- cbind.data.frame(c1, c2, c3, c4,c5)

colnames(data_aux) <- c("coefs", "left", "right", "treatment", "variables")
positions <- unique(data_aux$treatment)
data_aux$treatment <- factor(data_aux$treatment, levels = positions)
data_aux$variables <- factor(data_aux$variables, levels = unique(data_aux$variables))


Figure3A <- ggplot(data_aux, aes(treatment, coefs), color = factor(treatment)) + 
  geom_point(aes(color = factor(treatment)), size = 4, fill = "#d3d3d3") + 
  geom_errorbar(aes(ymin = left, ymax = right, color = factor(treatment)), width = 0.2,
                cex = 0.75) +
  facet_grid(. ~ variables, labeller = labeller(variables = label_wrap_gen(21))) +
  scale_x_discrete(limits = positions) +
  theme_light() +
  th + 
  scale_color_manual(name = "Treatment",  labels = c("Course and verification", "Course", "Verification"),
                     values= colors) +
  geom_hline(yintercept=0, size = 0.8, alpha = 0.4, linetype = "dashed") +
  ylab(y_text) +
  xlab("") + 
  ylim(-0.6, 0.75) + 
  legend_guide

ggsave(Figure3A, path = 'Figures', filename = "Figure3A.pdf", device = cairo_pdf, 
       width = 12, height = 8, dpi = 300)

# Figure 3B: mechanisms
c1 <- rbind.data.frame(lincom(lm_ate[[1]], test, level = 0.90)[,1:3],
                       lincom(lm_ate[[2]], test, level = 0.90)[,1:3],
                       lincom(lm_ate[[3]], test, level = 0.90)[,1:3],
                       lincom(lm_ate[[4]], test, level = 0.90)[,1:3],
                       lincom(lm_ate[[5]], test, level = 0.90)[,1:3],
                       lincom(lm_ate[[6]], test, level = 0.90)[,1:3])

data_aux <- cbind.data.frame(c1, c4, c5)

colnames(data_aux) <- c("coefs", "left", "right", "treatment", "variables")
positions <- unique(data_aux$treatment)
data_aux$treatment <- factor(data_aux$treatment, levels = positions)
data_aux$variables <- factor(data_aux$variables, levels = unique(data_aux$variables))

data_aux$coefs <- unlist(data_aux$coefs)
data_aux$left <- unlist(data_aux$left)
data_aux$right <- unlist(data_aux$right)

Figure3B <- ggplot(data_aux, aes(treatment, coefs), color = factor(treatment)) + 
  geom_point(aes(color = factor(treatment)), size = 4, fill = "#d3d3d3") + 
  geom_errorbar(aes(ymin = left, ymax = right, color = factor(treatment)), width = 0.2,
                cex = 0.75) +
  facet_grid(. ~ variables, labeller = labeller(variables = label_wrap_gen(21))) +
  scale_x_discrete(limits = positions) +
  theme_light() +
  th + 
  scale_color_manual(name = "Treatment",  labels = c("Course and verification", "Course", "Verification"),
                     values= colors) +
  geom_hline(yintercept=0, size = 0.8, alpha = 0.4, linetype = "dashed") +
  ylab(y_text) +
  xlab("") + 
  ylim(-0.6, 0.75) + 
  legend_guide

ggsave(Figure3B, path = 'Figures', filename = "Figure3B.pdf", device = cairo_pdf, 
       width = 12, height = 8, dpi = 300)

# Figure 4A: identify + verify
c1 <- as.numeric(c(lm_ate[[7]]$coefficients[1:3], lm_ate[[8]]$coefficients[1:3]))
c2 <- as.numeric(c(lm_ate[[7]]$conf.low[1:3], lm_ate[[8]]$conf.low[1:3]))
c3 <- as.numeric(c(lm_ate[[7]]$conf.high[1:3], lm_ate[[8]]$conf.high[1:3]))
c4 <- rep(c("Course and \nverification", "Course", "Verification"), 2)
c5 <- c(rep("Increase knowledge to identify information index",3),
        rep("Increase knowledge to verify information index",3))

data_aux <- cbind.data.frame(c1, c2, c3, c4,c5)

colnames(data_aux) <- c("coefs", "left", "right", "treatment", "variables")
positions <- unique(data_aux$treatment)
data_aux$treatment <- factor(data_aux$treatment, levels = positions)
data_aux$variables <- factor(data_aux$variables, levels = unique(data_aux$variables))


Figure4A <- ggplot(data_aux, aes(treatment, coefs), color = factor(treatment)) + 
  geom_point(aes(color = factor(treatment)), size = 4, fill = "#d3d3d3") + 
  geom_errorbar(aes(ymin = left, ymax = right, color = factor(treatment)), width = 0.2,
                cex = 0.75) +
  facet_grid(. ~ variables, labeller = labeller(variables = label_wrap_gen(21))) +
  scale_x_discrete(limits = positions) +
  theme_light() +
  th + 
  scale_color_manual(name = "Treatment",  labels = c("Course and verification", "Course", "Verification"),
                     values= colors) +
  geom_hline(yintercept=0, size = 0.8, alpha = 0.4, linetype = "dashed") +
  ylab(y_text) +
  xlab("") + 
  ylim(-.35,0.9) +
  legend_guide

ggsave(Figure4A, path = 'Figures', filename = "Figure4A.pdf", device = cairo_pdf, 
       width = 12, height = 8, dpi = 300)

# Figure 4B: mechanisms
c1 <- rbind.data.frame(lincom(lm_ate[[7]], test, level = 0.90)[,1:3],
                       lincom(lm_ate[[8]], test, level = 0.90)[,1:3])

data_aux <- cbind.data.frame(c1, c4,c5)

colnames(data_aux) <- c("coefs", "left", "right", "treatment", "variables")
positions <- unique(data_aux$treatment)
data_aux$treatment <- factor(data_aux$treatment, levels = positions)
data_aux$variables <- factor(data_aux$variables, levels = unique(data_aux$variables))

data_aux$coefs <- unlist(data_aux$coefs)
data_aux$left <- unlist(data_aux$left)
data_aux$right <- unlist(data_aux$right)

Figure4B <- ggplot(data_aux, aes(treatment, coefs), color = factor(treatment)) + 
  geom_point(aes(color = factor(treatment)), size = 4, fill = "#d3d3d3") + 
  geom_errorbar(aes(ymin = left, ymax = right, color = factor(treatment)), width = 0.2,
                cex = 0.75) +
  facet_grid(. ~ variables, labeller = labeller(variables = label_wrap_gen(21))) +
  scale_x_discrete(limits = positions) +
  theme_light() +
  th + 
  scale_color_manual(name = "Treatment",  labels = c("Course and verification", "Course", "Verification"),
                     values= colors) +
  geom_hline(yintercept=0, size = 0.8, alpha = 0.4, linetype = "dashed") +
  ylab(y_text) +
  xlab("") + 
  ylim(-.35,0.9) +
  legend_guide

ggsave(Figure4B, path = 'Figures', filename = "Figure4B.pdf", device = cairo_pdf, 
       width = 12, height = 8, dpi = 300)

# Figure 5A: mechanisms
c1 <- as.numeric(c(lm_ate[[9]]$coefficients[1:3], lm_ate[[10]]$coefficients[1:3], lm_ate[[11]]$coefficients[1:3], lm_ate[[12]]$coefficients[1:3]))
c2 <- as.numeric(c(lm_ate[[9]]$conf.low[1:3], lm_ate[[10]]$conf.low[1:3], lm_ate[[11]]$conf.low[1:3], lm_ate[[12]]$conf.low[1:3]))
c3 <- as.numeric(c(lm_ate[[9]]$conf.high[1:3], lm_ate[[10]]$conf.high[1:3], lm_ate[[11]]$conf.high[1:3], lm_ate[[12]]$conf.high[1:3]))
c4 <- rep(c("Course and \nverification", "Course", "Verification"), 4)
c5 <- c(rep("Increase consumption behavior of traditional sources index",3),
        rep("Decrease consumption behavior of social media sources index",3),
        rep("Decrease sharing behavior index",3),
        rep("Increase verifying behavior index",3))

data_aux <- cbind.data.frame(c1, c2, c3, c4,c5)

colnames(data_aux) <- c("coefs", "left", "right", "treatment", "variables")
positions <- unique(data_aux$treatment)
data_aux$treatment <- factor(data_aux$treatment, levels = positions)
data_aux$variables <- factor(data_aux$variables, levels = unique(data_aux$variables))

Figure5A <- ggplot(data_aux, aes(treatment, coefs), color = factor(treatment)) + 
  geom_point(aes(color = factor(treatment)), size = 4, fill = "#d3d3d3") + 
  geom_errorbar(aes(ymin = left, ymax = right, color = factor(treatment)), width = 0.2,
                cex = 0.75) +
  facet_grid(. ~ variables, labeller = labeller(variables = label_wrap_gen(21))) +
  scale_x_discrete(limits = positions) +
  theme_light() +
  th + 
  scale_color_manual(name = "Treatment",  labels = c("Course and verification", "Course", "Verification"),
                     values= colors) +
  geom_hline(yintercept=0, size = 0.8, alpha = 0.4, linetype = "dashed") +
  ylab(y_text) +
  xlab("") + 
  ylim(-.6, 0.7) +
  legend_guide

ggsave(Figure5A, path = 'Figures', filename = "Figure5A.pdf", device = cairo_pdf, 
       width = 12, height = 8, dpi = 300)

# Figure 5B: consumption, sharing, and verifying behavior
c1 <- rbind.data.frame(lincom(lm_ate[[9]], test, level = 0.90)[,1:3],
                       lincom(lm_ate[[10]], test, level = 0.90)[,1:3],
                       lincom(lm_ate[[11]], test, level = 0.90)[,1:3],
                       lincom(lm_ate[[12]], test, level = 0.90)[,1:3])

data_aux <- cbind.data.frame(c1, c4,c5)

colnames(data_aux) <- c("coefs", "left", "right", "treatment", "variables")
positions <- unique(data_aux$treatment)
data_aux$treatment <- factor(data_aux$treatment, levels = positions)
data_aux$variables <- factor(data_aux$variables, levels = unique(data_aux$variables))

data_aux$coefs <- unlist(data_aux$coefs)
data_aux$left <- unlist(data_aux$left)
data_aux$right <- unlist(data_aux$right)

Figure5B <- ggplot(data_aux, aes(treatment, coefs), color = factor(treatment)) + 
  geom_point(aes(color = factor(treatment)), size = 4, fill = "#d3d3d3") + 
  geom_errorbar(aes(ymin = left, ymax = right, color = factor(treatment)), width = 0.2,
                cex = 0.75) +
  facet_grid(. ~ variables, labeller = labeller(variables = label_wrap_gen(21))) +
  scale_x_discrete(limits = positions) +
  theme_light() +
  th + 
  scale_color_manual(name = "Treatment",  labels = c("Course and verification", "Course", "Verification"),
                     values= colors) +
  geom_hline(yintercept=0, size = 0.8, alpha = 0.4, linetype = "dashed") +
  ylab(y_text) +
  xlab("") + 
  ylim(-.6, 0.7) +
  legend_guide

ggsave(Figure5B, path = 'Figures', filename = "Figure5B.pdf", device = cairo_pdf, 
       width = 12, height = 8, dpi = 300)

# ------------------------------------------------------------------------------
# Appendix figures with IPSW and LASSO ------------------------------------------

# Figure A1: columns 1 and 6----------------------------------------------------
# OLS
c1 <- as.numeric(c(lm_ate[[1]]$coefficients[1:3], lm_ate[[6]]$coefficients[1:3]))
c2 <- as.numeric(c(lm_ate[[1]]$conf.low[1:3], lm_ate[[6]]$conf.low[1:3]))
c3 <- as.numeric(c(lm_ate[[1]]$conf.high[1:3], lm_ate[[6]]$conf.high[1:3]))
c4 <- rep(c("Course and \nverification", "Course", "Verification"), 2)
c5 <- c(rep("Misinformation importance index",3),
        rep("Attention to misinformation index",3))
c6 <- rep("OLS", length(c1))

data_aux1 <- cbind.data.frame(c1, c2, c3, c4,c5, c6)

colnames(data_aux1) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux1$treatment)
data_aux1$treatment <- factor(data_aux1$treatment, levels = positions)
data_aux1$variables <- factor(data_aux1$variables, levels = unique(data_aux1$variables))
data_aux1$method <- factor(data_aux1$method, levels = unique(data_aux1$method))

# IPSW
c1 <- as.numeric(c(lm_psm[[1]]$coefficients[1:3], lm_psm[[6]]$coefficients[1:3]))
c2 <- as.numeric(c(lm_psm[[1]]$conf.low[1:3], lm_psm[[6]]$conf.low[1:3]))
c3 <- as.numeric(c(lm_psm[[1]]$conf.high[1:3], lm_psm[[6]]$conf.high[1:3]))
c6 <- rep("IPSW", length(c1))

data_aux2 <- cbind.data.frame(c1, c2, c3, c4,c5, c6)

colnames(data_aux2) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux2$treatment)
data_aux2$treatment <- factor(data_aux2$treatment, levels = positions)
data_aux2$variables <- factor(data_aux2$variables, levels = unique(data_aux2$variables))
data_aux2$method <- factor(data_aux2$method, levels = unique(data_aux2$method))

# LASSO
c1 <- as.numeric(c(lm_lasso[[1]]$coefficients[1:3], lm_lasso[[6]]$coefficients[1:3]))
c2 <- as.numeric(c(lm_lasso[[1]]$conf.low[1:3], lm_lasso[[6]]$conf.low[1:3]))
c3 <- as.numeric(c(lm_lasso[[1]]$conf.high[1:3], lm_lasso[[6]]$conf.high[1:3]))
c6 <- rep("LASSO", length(c1))

data_aux3 <- cbind.data.frame(c1, c2, c3, c4,c5, c6)

colnames(data_aux3) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux3$treatment)
data_aux3$treatment <- factor(data_aux3$treatment, levels = positions)
data_aux3$variables <- factor(data_aux3$variables, levels = unique(data_aux3$variables))
data_aux3$method <- factor(data_aux3$method, levels = unique(data_aux3$method))

# Append the 3 datasets
data_aux <- rbind.data.frame(data_aux1, data_aux2, data_aux3)

# Plot
figureA1_a <- ggplot(data_aux, aes(x = treatment, y = coefs, colour = method)) +
  geom_point(aes(shape=method, color = factor(method)), size = 3, fill = "#d3d3d3", position = position_dodge) +
  geom_errorbar(aes(ymin = left, ymax = right), width = 0.5,
                cex = 0.75, position = position_dodge) +
  facet_grid(. ~ variables,  labeller = labeller(variables = label_wrap_gen(21))) +
  theme_light() +
  th +
  scale_color_manual(name = "Method",  labels = c("OLS", "IPSW", "LASSO"),
                     values = colors) +
  scale_shape_manual(name="Method",values=c(15,19, 17)) +
  geom_hline(yintercept=0, size = 0.8, alpha = 0.4, linetype = "dashed") +
  ylab(y_text) +
  xlab("") +
  legend_guide1 +
  legend_guide2 +
  ylim(-.7, 0.8) +
  theme(axis.text.x = element_text(vjust = - 0.2)) 

ggsave(figureA1_a, path = 'Figures', filename = "FigureA1_a.pdf", device = cairo_pdf, 
       width = 12, height = 8,dpi = 300)

# WhatsApp survey plot
# OLS
c1 <- rbind.data.frame(lincom(lm_ate[[1]], test, level = 0.90)[,1:3],
                       lincom(lm_ate[[6]], test, level = 0.90)[,1:3])

c6 <- rep("OLS", length(c1))

data_aux1 <- cbind.data.frame(c1, c4,c5, c6)

colnames(data_aux1) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux1$treatment)
data_aux1$treatment <- factor(data_aux1$treatment, levels = positions)
data_aux1$variables <- factor(data_aux1$variables, levels = unique(data_aux1$variables))
data_aux1$method <- factor(data_aux1$method, levels = unique(data_aux1$method))

data_aux1$coefs <- unlist(data_aux1$coefs)
data_aux1$left <- unlist(data_aux1$left)
data_aux1$right <- unlist(data_aux1$right)

# IPSW
c1 <- rbind.data.frame(lincom(lm_psm[[1]], test, level = 0.90)[,1:3],
                       lincom(lm_psm[[6]], test, level = 0.90)[,1:3])

c6 <- rep("IPSW", length(c1))

data_aux2 <- cbind.data.frame(c1,c4,c5, c6)

colnames(data_aux2) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux2$treatment)
data_aux2$treatment <- factor(data_aux2$treatment, levels = positions)
data_aux2$variables <- factor(data_aux2$variables, levels = unique(data_aux2$variables))
data_aux2$method <- factor(data_aux2$method, levels = unique(data_aux2$method))

data_aux2$coefs <- unlist(data_aux2$coefs)
data_aux2$left <- unlist(data_aux2$left)
data_aux2$right <- unlist(data_aux2$right)

# LASSO
c1 <- rbind.data.frame(lincom(lm_lasso[[1]], test, level = 0.90)[,1:3],
                       lincom(lm_lasso[[6]], test, level = 0.90)[,1:3])

c6 <- rep("LASSO", length(c1))

data_aux3 <- cbind.data.frame(c1, c4,c5, c6)

colnames(data_aux3) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux3$treatment)
data_aux3$treatment <- factor(data_aux3$treatment, levels = positions)
data_aux3$variables <- factor(data_aux3$variables, levels = unique(data_aux3$variables))
data_aux3$method <- factor(data_aux3$method, levels = unique(data_aux3$method))

data_aux3$coefs <- unlist(data_aux3$coefs)
data_aux3$left <- unlist(data_aux3$left)
data_aux3$right <- unlist(data_aux3$right)

# Append the 3 datasets
data_aux <- rbind.data.frame(data_aux1, data_aux2, data_aux3)

figureA1_b <- ggplot(data_aux, aes(x = treatment, y = coefs, colour = method)) +
  geom_point(aes(shape=method, color = factor(method)), size = 3, fill = "#d3d3d3", position =position_dodge) +
  geom_errorbar(aes(ymin = left, ymax = right), width = 0.5,
                cex = 0.75, position =position_dodge) +
  facet_grid(. ~ variables,  labeller = labeller(variables = label_wrap_gen(21))) +
  theme_light() +
  th +
  scale_color_manual(name = "Method",  labels = c("OLS", "IPSW", "LASSO"),
                     values = colors) +
  scale_shape_manual(name="Method",values=c(15,19, 17)) +
  geom_hline(yintercept=0, size = 0.8, alpha = 0.4, linetype = "dashed") +
  ylab(y_text) +
  xlab("") +
  legend_guide1 +
  legend_guide2 +
  ylim(-.7, 0.8) +
  theme(axis.text.x = element_text(vjust = - 0.2)) 


ggsave(figureA1_b, path = 'Figures', filename = "FigureA1_b.pdf", device = cairo_pdf, 
       width = 12, height = 8,dpi = 300)

# Figure A2: columns 2-5 -------------------------------------------------------
# OLS
c1 <- as.numeric(c(lm_ate[[2]]$coefficients[1:3], lm_ate[[3]]$coefficients[1:3], lm_ate[[4]]$coefficients[1:3], lm_ate[[5]]$coefficients[1:3]))
c2 <- as.numeric(c(lm_ate[[2]]$conf.low[1:3], lm_ate[[3]]$conf.low[1:3], lm_ate[[4]]$conf.low[1:3], lm_ate[[5]]$conf.low[1:3]))
c3 <- as.numeric(c(lm_ate[[2]]$conf.high[1:3], lm_ate[[3]]$conf.high[1:3], lm_ate[[4]]$conf.high[1:3], lm_ate[[5]]$conf.high[1:3]))
c4 <- rep(c("Course and \nverification", "Course", "Verification"), 4)
c5 <- c(rep("Decrease likelihood of false of traditional sources index", 3),
        rep("Increase likelihood of false of social media sources index", 3),
        rep("Trust traditional sources index", 3),
        rep("Distrust social media sources index", 3))
c6 <- rep("OLS", length(c1))

data_aux1 <- cbind.data.frame(c1, c2, c3, c4,c5, c6)

colnames(data_aux1) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux1$treatment)
data_aux1$treatment <- factor(data_aux1$treatment, levels = positions)
data_aux1$variables <- factor(data_aux1$variables, levels = unique(data_aux1$variables))
data_aux1$method <- factor(data_aux1$method, levels = unique(data_aux1$method))

# IPSW
c1 <- as.numeric(c(lm_psm[[2]]$coefficients[1:3], lm_psm[[3]]$coefficients[1:3], lm_psm[[4]]$coefficients[1:3], lm_psm[[5]]$coefficients[1:3]))
c2 <- as.numeric(c(lm_psm[[2]]$conf.low[1:3], lm_psm[[3]]$conf.low[1:3], lm_psm[[4]]$conf.low[1:3], lm_psm[[5]]$conf.low[1:3]))
c3 <- as.numeric(c(lm_psm[[2]]$conf.high[1:3], lm_psm[[3]]$conf.high[1:3], lm_psm[[4]]$conf.high[1:3], lm_psm[[5]]$conf.high[1:3]))
c6 <- rep("IPSW", length(c1))

data_aux2 <- cbind.data.frame(c1, c2, c3, c4,c5, c6)

colnames(data_aux2) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux2$treatment)
data_aux2$treatment <- factor(data_aux2$treatment, levels = positions)
data_aux2$variables <- factor(data_aux2$variables, levels = unique(data_aux2$variables))
data_aux2$method <- factor(data_aux2$method, levels = unique(data_aux2$method))

# LASSO
c1 <- as.numeric(c(lm_lasso[[2]]$coefficients[1:3], lm_lasso[[3]]$coefficients[1:3], lm_lasso[[4]]$coefficients[1:3], lm_lasso[[5]]$coefficients[1:3]))
c2 <- as.numeric(c(lm_lasso[[2]]$conf.low[1:3], lm_lasso[[3]]$conf.low[1:3], lm_lasso[[4]]$conf.low[1:3], lm_lasso[[5]]$conf.low[1:3]))
c3 <- as.numeric(c(lm_lasso[[2]]$conf.high[1:3], lm_lasso[[3]]$conf.high[1:3], lm_lasso[[4]]$conf.high[1:3], lm_lasso[[5]]$conf.high[1:3]))
c6 <- rep("LASSO", length(c1))

data_aux3 <- cbind.data.frame(c1, c2, c3, c4,c5, c6)

colnames(data_aux3) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux3$treatment)
data_aux3$treatment <- factor(data_aux3$treatment, levels = positions)
data_aux3$variables <- factor(data_aux3$variables, levels = unique(data_aux3$variables))
data_aux3$method <- factor(data_aux3$method, levels = unique(data_aux3$method))

# Append the 3 datasets
data_aux <- rbind.data.frame(data_aux1,data_aux2,data_aux3)

# Plot
figureA2_a <- ggplot(data_aux, aes(x = treatment, y = coefs, colour = method)) +
  geom_point(aes(shape=method, color = factor(method)), size = 3, fill = "#d3d3d3", position =position_dodge) +
  geom_errorbar(aes(ymin = left, ymax = right), width = 0.5,
                cex = 0.75, position =position_dodge) +
  facet_grid(. ~ variables,  labeller = labeller(variables = label_wrap_gen(21))) +
  theme_light() +
  th +
  scale_color_manual(name = "Method",  labels = c("OLS", "IPSW", "LASSO"),
                     values = colors) +
  scale_shape_manual(name="Method",values=c(15,19, 17)) +
  geom_hline(yintercept=0, size = 0.8, alpha = 0.4, linetype = "dashed") +
  ylab(y_text) +
  xlab("") +
  legend_guide1 +
  legend_guide2 +
  ylim(-0.7, 0.8) +
  theme(axis.text.x = element_text(vjust = - 0.2)) 


ggsave(figureA2_a, path = 'Figures', filename = "FigureA2_a.pdf", device = cairo_pdf, 
       width = 12, height = 8,dpi = 300)

# WhatsApp survey plot
# OLS
c1 <- rbind.data.frame(lincom(lm_ate[[2]], test, level = 0.90)[,1:3],
                       lincom(lm_ate[[3]], test, level = 0.90)[,1:3],
                       lincom(lm_ate[[4]], test, level = 0.90)[,1:3],
                       lincom(lm_ate[[5]], test, level = 0.90)[,1:3])

c6 <- rep("OLS", length(c1))

data_aux1 <- cbind.data.frame(c1, c4,c5, c6)

colnames(data_aux1) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux1$treatment)
data_aux1$treatment <- factor(data_aux1$treatment, levels = positions)
data_aux1$variables <- factor(data_aux1$variables, levels = unique(data_aux1$variables))
data_aux1$method <- factor(data_aux1$method, levels = unique(data_aux1$method))

data_aux1$coefs <- unlist(data_aux1$coefs)
data_aux1$left <- unlist(data_aux1$left)
data_aux1$right <- unlist(data_aux1$right)

# IPSW
c1 <- rbind.data.frame(lincom(lm_psm[[2]], test, level = 0.90)[,1:3],
                       lincom(lm_psm[[3]], test, level = 0.90)[,1:3],
                       lincom(lm_psm[[4]], test, level = 0.90)[,1:3],
                       lincom(lm_psm[[5]], test, level = 0.90)[,1:3])

c6 <- rep("IPSW", length(c1))

data_aux2 <- cbind.data.frame(c1, c4,c5, c6)

colnames(data_aux2) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux2$treatment)
data_aux2$treatment <- factor(data_aux2$treatment, levels = positions)
data_aux2$variables <- factor(data_aux2$variables, levels = unique(data_aux2$variables))
data_aux2$method <- factor(data_aux2$method, levels = unique(data_aux2$method))

data_aux2$coefs <- unlist(data_aux2$coefs)
data_aux2$left <- unlist(data_aux2$left)
data_aux2$right <- unlist(data_aux2$right)

# LASSO
c1 <- rbind.data.frame(lincom(lm_lasso[[2]], test, level = 0.90)[,1:3],
                       lincom(lm_lasso[[3]], test, level = 0.90)[,1:3],
                       lincom(lm_lasso[[4]], test, level = 0.90)[,1:3],
                       lincom(lm_lasso[[5]], test, level = 0.90)[,1:3])

c6 <- rep("LASSO", length(c1))

data_aux3 <- cbind.data.frame(c1, c4,c5, c6)

colnames(data_aux3) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux3$treatment)
data_aux3$treatment <- factor(data_aux3$treatment, levels = positions)
data_aux3$variables <- factor(data_aux3$variables, levels = unique(data_aux3$variables))
data_aux3$method <- factor(data_aux3$method, levels = unique(data_aux3$method))

data_aux3$coefs <- unlist(data_aux3$coefs)
data_aux3$left <- unlist(data_aux3$left)
data_aux3$right <- unlist(data_aux3$right)

# Append the 3 datasets
data_aux <- rbind.data.frame(data_aux1,data_aux2,data_aux3)

figureA2_b <- ggplot(data_aux, aes(x = treatment, y = coefs, colour = method)) +
  geom_point(aes(shape=method, color = factor(method)), size = 3, fill = "#d3d3d3", position =position_dodge) +
  geom_errorbar(aes(ymin = left, ymax = right), width = 0.5,
                cex = 0.75, position =position_dodge) +
  facet_grid(. ~ variables,  labeller = labeller(variables = label_wrap_gen(21))) +
  theme_light() +
  th +
  scale_color_manual(name = "Method",  labels = c("OLS", "IPSW", "LASSO"),
                     values = colors) +
  scale_shape_manual(name="Method",values=c(15,19, 17)) +
  geom_hline(yintercept=0, size = 0.8, alpha = 0.4, linetype = "dashed") +
  ylab(y_text) +
  xlab("") +
  legend_guide1 +
  legend_guide2 +
  ylim(-0.7, 0.8) +
  theme(axis.text.x = element_text(vjust = - 0.2)) 

ggsave(figureA2_b, path = 'Figures', filename = "FigureA2_b.pdf", device = cairo_pdf, 
       width = 12, height = 8,dpi = 300)

# Figure 4: columns 7 and 8 ----------------------------------------------------

# OLS
c1 <- as.numeric(c(lm_ate[[7]]$coefficients[1:3], lm_ate[[8]]$coefficients[1:3]))
c2 <- as.numeric(c(lm_ate[[7]]$conf.low[1:3], lm_ate[[8]]$conf.low[1:3]))
c3 <- as.numeric(c(lm_ate[[7]]$conf.high[1:3], lm_ate[[8]]$conf.high[1:3]))
c4 <- rep(c("Course and \nverification", "Course", "Verification"), 2)
c5 <- c(rep("Increase knowledge to identify information index", 3),
        rep("Increase knowledge to verify information index", 3))
c6 <- rep("OLS", length(c1))

data_aux1 <- cbind.data.frame(c1, c2, c3, c4,c5, c6)

colnames(data_aux1) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux1$treatment)
data_aux1$treatment <- factor(data_aux1$treatment, levels = positions)
data_aux1$variables <- factor(data_aux1$variables, levels = unique(data_aux1$variables))
data_aux1$method <- factor(data_aux1$method, levels = unique(data_aux1$method))

# IPSW
c1 <- as.numeric(c(lm_psm[[7]]$coefficients[1:3], lm_psm[[8]]$coefficients[1:3]))
c2 <- as.numeric(c(lm_psm[[7]]$conf.low[1:3], lm_psm[[8]]$conf.low[1:3]))
c3 <- as.numeric(c(lm_psm[[7]]$conf.high[1:3], lm_psm[[8]]$conf.high[1:3]))
c6 <- rep("IPSW", length(c1))

data_aux2 <- cbind.data.frame(c1, c2, c3, c4,c5, c6)

colnames(data_aux2) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux2$treatment)
data_aux2$treatment <- factor(data_aux2$treatment, levels = positions)
data_aux2$variables <- factor(data_aux2$variables, levels = unique(data_aux2$variables))
data_aux2$method <- factor(data_aux2$method, levels = unique(data_aux2$method))

# LASSO
c1 <- as.numeric(c(lm_lasso[[7]]$coefficients[1:3], lm_lasso[[8]]$coefficients[1:3]))
c2 <- as.numeric(c(lm_lasso[[7]]$conf.low[1:3], lm_lasso[[8]]$conf.low[1:3]))
c3 <- as.numeric(c(lm_lasso[[7]]$conf.high[1:3], lm_lasso[[8]]$conf.high[1:3]))
c6 <- rep("LASSO", length(c1))

data_aux3 <- cbind.data.frame(c1, c2, c3, c4,c5, c6)

colnames(data_aux3) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux3$treatment)
data_aux3$treatment <- factor(data_aux3$treatment, levels = positions)
data_aux3$variables <- factor(data_aux3$variables, levels = unique(data_aux3$variables))
data_aux3$method <- factor(data_aux3$method, levels = unique(data_aux3$method))

# Append the 3 datasets
data_aux <- rbind.data.frame(data_aux1,data_aux2,data_aux3)

# Plot
figureA3_a <- ggplot(data_aux, aes(x = treatment, y = coefs, colour = method)) +
  geom_point(aes(shape=method, color = factor(method)), size = 3, fill = "#d3d3d3", position =position_dodge) +
  geom_errorbar(aes(ymin = left, ymax = right), width = 0.5,
                cex = 0.75, position =position_dodge) +
  facet_grid(. ~ variables,  labeller = labeller(variables = label_wrap_gen(21))) +
  theme_light() +
  th +
  scale_color_manual(name = "Method",  labels = c("OLS", "IPSW", "LASSO"),
                     values = colors) +
  scale_shape_manual(name="Method",values=c(15,19, 17)) +
  geom_hline(yintercept=0, size = 0.8, alpha = 0.4, linetype = "dashed") +
  ylab(y_text) +
  xlab("") +
  legend_guide1 +
  legend_guide2 +
  ylim(-0.5, 1) +
  theme(axis.text.x = element_text(vjust = - 0.2)) 


ggsave(figureA3_a, path = 'Figures', filename = "FigureA3_a.pdf", device = cairo_pdf, 
       width = 12, height = 8,dpi = 300)

# WhatsApp survey plot
# OLS
c1 <- rbind.data.frame(lincom(lm_ate[[7]], test, level = 0.90)[,1:3],
                       lincom(lm_ate[[8]], test, level = 0.90)[,1:3])

c6 <- rep("OLS", length(c1))

data_aux1 <- cbind.data.frame(c1, c4,c5, c6)

colnames(data_aux1) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux1$treatment)
data_aux1$treatment <- factor(data_aux1$treatment, levels = positions)
data_aux1$variables <- factor(data_aux1$variables, levels = unique(data_aux1$variables))
data_aux1$method <- factor(data_aux1$method, levels = unique(data_aux1$method))

data_aux1$coefs <- unlist(data_aux1$coefs)
data_aux1$left <- unlist(data_aux1$left)
data_aux1$right <- unlist(data_aux1$right)

# IPSW
c1 <- rbind.data.frame(lincom(lm_psm[[7]], test, level = 0.90)[,1:3],
                       lincom(lm_psm[[8]], test, level = 0.90)[,1:3])

c6 <- rep("IPSW", length(c1))

data_aux2 <- cbind.data.frame(c1, c4,c5, c6)

colnames(data_aux2) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux2$treatment)
data_aux2$treatment <- factor(data_aux2$treatment, levels = positions)
data_aux2$variables <- factor(data_aux2$variables, levels = unique(data_aux2$variables))
data_aux2$method <- factor(data_aux2$method, levels = unique(data_aux2$method))

data_aux2$coefs <- unlist(data_aux2$coefs)
data_aux2$left <- unlist(data_aux2$left)
data_aux2$right <- unlist(data_aux2$right)
                         
# LASSO
c1 <- rbind.data.frame(lincom(lm_lasso[[7]], test, level = 0.90)[,1:3],
                       lincom(lm_lasso[[8]], test, level = 0.90)[,1:3])

c6 <- rep("LASSO", length(c1))

data_aux3 <- cbind.data.frame(c1, c4,c5, c6)

colnames(data_aux3) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux3$treatment)
data_aux3$treatment <- factor(data_aux3$treatment, levels = positions)
data_aux3$variables <- factor(data_aux3$variables, levels = unique(data_aux3$variables))
data_aux3$method <- factor(data_aux3$method, levels = unique(data_aux3$method))

data_aux3$coefs <- unlist(data_aux3$coefs)
data_aux3$left <- unlist(data_aux3$left)
data_aux3$right <- unlist(data_aux3$right)

# Append the 3 datasets
data_aux <- rbind.data.frame(data_aux1,data_aux2,data_aux3)

figureA3_b <- ggplot(data_aux, aes(x = treatment, y = coefs, colour = method)) +
  geom_point(aes(shape=method, color = factor(method)), size = 3, fill = "#d3d3d3", position =position_dodge) +
  geom_errorbar(aes(ymin = left, ymax = right), width = 0.5,
                cex = 0.75, position =position_dodge) +
  facet_grid(. ~ variables,  labeller = labeller(variables = label_wrap_gen(21))) +
  theme_light() +
  th +
  scale_color_manual(name = "Method",  labels = c("OLS", "IPSW", "LASSO"),
                     values = colors) +
  scale_shape_manual(name="Method",values=c(15,19, 17)) +
  geom_hline(yintercept=0, size = 0.8, alpha = 0.4, linetype = "dashed") +
  ylab(y_text) +
  xlab("") +
  legend_guide1 +
  legend_guide2 +
  ylim(-0.5, 1) +
  theme(axis.text.x = element_text(vjust = - 0.2)) 

ggsave(figureA3_b, path = 'Figures', filename = "FigureA3_b.pdf", device = cairo_pdf, 
       width = 12, height = 8,dpi = 300)

# Figure 5: columns 9 to 12 -----------------------------------------------------

# OLS
c1 <- as.numeric(c(lm_ate[[9]]$coefficients[1:3], lm_ate[[10]]$coefficients[1:3], lm_ate[[11]]$coefficients[1:3],  lm_ate[[12]]$coefficients[1:3]))
c2 <- as.numeric(c(lm_ate[[9]]$conf.low[1:3], lm_ate[[10]]$conf.low[1:3], lm_ate[[11]]$conf.low[1:3], lm_ate[[12]]$conf.low[1:3]))
c3 <- as.numeric(c(lm_ate[[9]]$conf.high[1:3], lm_ate[[10]]$conf.high[1:3], lm_ate[[11]]$conf.high[1:3], lm_ate[[12]]$conf.high[1:3]))
c4 <- rep(c("Course and \nverification", "Course", "Verification"), 4)
c5 <- c(rep("Increase consumption behavior of traditional sources index",3),
        rep("Decrease consumption behavior of social media sources index",3),
        rep("Decrease sharing behavior index",3),
        rep("increase verifying behavior index",3))

c6 <- rep("OLS", length(c1))

data_aux1 <- cbind.data.frame(c1, c2, c3, c4,c5, c6)

colnames(data_aux1) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux1$treatment)
data_aux1$treatment <- factor(data_aux1$treatment, levels = positions)
data_aux1$variables <- factor(data_aux1$variables, levels = unique(data_aux1$variables))
data_aux1$method <- factor(data_aux1$method, levels = unique(data_aux1$method))

# IPSW
c1 <- as.numeric(c(lm_psm[[9]]$coefficients[1:3], lm_psm[[10]]$coefficients[1:3], lm_psm[[11]]$coefficients[1:3],  lm_psm[[12]]$coefficients[1:3]))
c2 <- as.numeric(c(lm_psm[[9]]$conf.low[1:3], lm_psm[[10]]$conf.low[1:3], lm_psm[[11]]$conf.low[1:3], lm_psm[[12]]$conf.low[1:3]))
c3 <- as.numeric(c(lm_psm[[9]]$conf.high[1:3], lm_psm[[10]]$conf.high[1:3], lm_psm[[11]]$conf.high[1:3], lm_psm[[12]]$conf.high[1:3]))

c6 <- rep("IPSW", length(c1))

data_aux2 <- cbind.data.frame(c1, c2, c3, c4,c5, c6)

colnames(data_aux2) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux2$treatment)
data_aux2$treatment <- factor(data_aux2$treatment, levels = positions)
data_aux2$variables <- factor(data_aux2$variables, levels = unique(data_aux2$variables))
data_aux2$method <- factor(data_aux2$method, levels = unique(data_aux2$method))

# LASSO
c1 <- as.numeric(c(lm_lasso[[9]]$coefficients[1:3], lm_lasso[[10]]$coefficients[1:3], lm_lasso[[11]]$coefficients[1:3],  lm_lasso[[12]]$coefficients[1:3]))
c2 <- as.numeric(c(lm_lasso[[9]]$conf.low[1:3], lm_lasso[[10]]$conf.low[1:3], lm_lasso[[11]]$conf.low[1:3], lm_lasso[[12]]$conf.low[1:3]))
c3 <- as.numeric(c(lm_lasso[[9]]$conf.high[1:3], lm_lasso[[10]]$conf.high[1:3], lm_lasso[[11]]$conf.high[1:3], lm_lasso[[12]]$conf.high[1:3]))
c6 <- rep("LASSO", length(c1))

data_aux3 <- cbind.data.frame(c1, c2, c3, c4,c5, c6)

colnames(data_aux3) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux3$treatment)
data_aux3$treatment <- factor(data_aux3$treatment, levels = positions)
data_aux3$variables <- factor(data_aux3$variables, levels = unique(data_aux3$variables))
data_aux3$method <- factor(data_aux3$method, levels = unique(data_aux3$method))

# Append the 3 datasets
data_aux <- rbind.data.frame(data_aux1,data_aux2,data_aux3)

# Plot
figureA4_a <- ggplot(data_aux, aes(x = treatment, y = coefs, colour = method)) +
  geom_point(aes(shape=method, color = factor(method)), size = 3, fill = "#d3d3d3", position =position_dodge) +
  geom_errorbar(aes(ymin = left, ymax = right), width = 0.5,
                cex = 0.75, position =position_dodge) +
  facet_grid(. ~ variables,  labeller = labeller(variables = label_wrap_gen(21))) +
  theme_light() +
  th +
  scale_color_manual(name = "Method",  labels = c("OLS", "IPSW", "LASSO"),
                     values = colors) +
  scale_shape_manual(name="Method",values=c(15,19, 17)) +
  geom_hline(yintercept=0, size = 0.8, alpha = 0.4, linetype = "dashed") +
  ylab(y_text) +
  xlab("") +
  legend_guide1 +
  legend_guide2 +
  ylim(-0.7, 1) +
  theme(axis.text.x = element_text(vjust = - 0.2)) 

ggsave(figureA4_a, path = 'Figures', filename = "FigureA4_a.pdf", device = cairo_pdf, 
       width = 12, height = 8,dpi = 300)

# WhatsApp survey plot
# OLS
c1 <- rbind.data.frame(lincom(lm_ate[[9]], test, level = 0.90)[,1:3],
                       lincom(lm_ate[[10]], test, level = 0.90)[,1:3],
                       lincom(lm_ate[[11]], test, level = 0.90)[,1:3],
                       lincom(lm_ate[[12]], test, level = 0.90)[,1:3])
c6 <- rep("OLS", length(c1))

data_aux1 <- cbind.data.frame(c1, c4,c5, c6)

colnames(data_aux1) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux1$treatment)
data_aux1$treatment <- factor(data_aux1$treatment, levels = positions)
data_aux1$variables <- factor(data_aux1$variables, levels = unique(data_aux1$variables))
data_aux1$method <- factor(data_aux1$method, levels = unique(data_aux1$method))

data_aux1$coefs <- unlist(data_aux1$coefs)
data_aux1$left <- unlist(data_aux1$left)
data_aux1$right <- unlist(data_aux1$right)

# IPSW
c2 <- rbind.data.frame(lincom(lm_psm[[9]], test, level = 0.90)[,1:3],
                       lincom(lm_psm[[10]], test, level = 0.90)[,1:3],
                       lincom(lm_psm[[11]], test, level = 0.90)[,1:3],
                       lincom(lm_psm[[12]], test, level = 0.90)[,1:3])

c6 <- rep("IPSW", length(c1))

data_aux2 <- cbind.data.frame(c1, c4,c5, c6)

colnames(data_aux2) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux2$treatment)
data_aux2$treatment <- factor(data_aux2$treatment, levels = positions)
data_aux2$variables <- factor(data_aux2$variables, levels = unique(data_aux2$variables))
data_aux2$method <- factor(data_aux2$method, levels = unique(data_aux2$method))

data_aux2$coefs <- unlist(data_aux2$coefs)
data_aux2$left <- unlist(data_aux2$left)
data_aux2$right <- unlist(data_aux2$right)

# LASSO
c1 <- rbind.data.frame(lincom(lm_lasso[[9]], test, level = 0.90)[,1:3],
                       lincom(lm_lasso[[10]], test, level = 0.90)[,1:3],
                       lincom(lm_lasso[[11]], test, level = 0.90)[,1:3],
                       lincom(lm_lasso[[12]], test, level = 0.90)[,1:3])

c6 <- rep("LASSO", length(c1))

data_aux3 <- cbind.data.frame(c1, c4,c5, c6)

colnames(data_aux3) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux3$treatment)
data_aux3$treatment <- factor(data_aux3$treatment, levels = positions)
data_aux3$variables <- factor(data_aux3$variables, levels = unique(data_aux3$variables))
data_aux3$method <- factor(data_aux3$method, levels = unique(data_aux3$method))

data_aux3$coefs <- unlist(data_aux3$coefs)
data_aux3$left <- unlist(data_aux3$left)
data_aux3$right <- unlist(data_aux3$right)

# Append the 3 datasets
data_aux <- rbind.data.frame(data_aux1,data_aux2,data_aux3)

figureA4_b <- ggplot(data_aux, aes(x = treatment, y = coefs, colour = method)) +
  geom_point(aes(shape=method, color = factor(method)), size = 3, fill = "#d3d3d3", position =position_dodge) +
  geom_errorbar(aes(ymin = left, ymax = right), width = 0.5,
                cex = 0.75, position =position_dodge) +
  facet_grid(. ~ variables,  labeller = labeller(variables = label_wrap_gen(21))) +
  theme_light() +
  th +
  scale_color_manual(name = "Method",  labels = c("OLS", "IPSW", "LASSO"),
                     values = colors) +
  scale_shape_manual(name="Method",values=c(15,19, 17)) +
  geom_hline(yintercept=0, size = 0.8, alpha = 0.4, linetype = "dashed") +
  ylab(y_text) +
  xlab("") +
  legend_guide1 +
  legend_guide2 +
  ylim(-0.7, 1) +
  theme(axis.text.x = element_text(vjust = - 0.2)) 

ggsave(figureA4_b, path = 'Figures', filename = "FigureA4_b.pdf", device = cairo_pdf, 
       width = 12, height = 8,dpi = 300)

