# Main results reweigh to mimic national samples

# ------------------------------------------------------------------------------
# read data
survey <- read.csv('Datasets/Reweight/survey_final_reweight.csv')

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

title <- "Treatment effects on indexes by course delivered online and via WhatsApp with post-stratifications weights to mimic the Americas Barometer sample restricted to internet users"

# Create tables and save results
size = length(aux) + 1
cm = 20.5 
interacted_bool <- c(T,T,T,T,T,T,T,T,T,T,T,T,T)
# Aux for rewight analysis
weight1 <- TRUE
weight2 <- FALSE
label = "Reweight1"
source('Scripts/General Scripts/ATEsGeneralSetUp.R')
panelA <- final_table

# Auxiliary regressions with robust s.e. for ggplot (no weights)
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

# Auxiliary regressions with robust s.e. for ggplot (wieghts1)
lm_ate1 <- list()
count <- 1
for (x in aux) {
  
  fmla1 <- as.formula(paste0(x, "~ ", paste(treatments_separate, collapse = " + "), " + ",
                             paste(paste0(treatments_separate, ":", interacted), collapse = " + "), " + ",
                             paste(control_list[[count]], collapse = " + ")))
  
  nam1 <- paste("lm_", count, "_ols", sep = "")
  assign(nam1, lm_robust(fmla1, se_type = "HC0", weights = weight1, data = survey, fixed_effects = blockid1, alpha = 0.1))
  
  lm_ate1[[count]] <- get(nam1, envir = globalenv())
  
  count <- count + 1
}

# Run IPSW ----------------------------------------------------------------------
survey_attrition <- read.csv('Datasets/Reweight/survey_final_reweight_attrition.csv')

source('Scripts/General Scripts/PSMGeneralSetUp.R')
panelB <- final_table

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

# Append and save final table --------------------------------------------------
cat(main_panel(panelA, panelB, panelC, size, "\\\\shortstack\\{Mis-", 2.5, 2.25), file = "Tables/Appendix/Main_results_reweight1.txt")

# Weights 2: Latin Barometer ---------------------------------------------------
# Aux for rewight analysis
weight1 <- FALSE
weight2 <- TRUE

# Summary of main indexes ------------------------------------------------------
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

title <- "Treatment effects on indexes by course delivered online and via WhatsApp with post-stratifications weights to mimic the Latin Barometer sample restricted to internet users"

# Create tables and save results
size = length(aux) + 1
cm = 20.5 
interacted_bool <- c(T,T,T,T,T,T,T,T,T,T,T,T,T)
label = "Reweight2"
source('Scripts/General Scripts/ATEsGeneralSetUp.R')
panelA <- final_table

# Auxiliary regressions with robust s.e. for ggplot (wieghts2)
lm_ate2 <- list()
count <- 1
for (x in aux) {
  
  fmla1 <- as.formula(paste0(x, "~ ", paste(treatments_separate, collapse = " + "), " + ",
                             paste(paste0(treatments_separate, ":", interacted), collapse = " + "), " + ",
                             paste(control_list[[count]], collapse = " + ")))
  
  nam1 <- paste("lm_", count, "_ols", sep = "")
  assign(nam1, lm_robust(fmla1, se_type = "HC0", weights = weight2, data = survey, fixed_effects = blockid1, alpha = 0.1))
  
  lm_ate2[[count]] <- get(nam1, envir = globalenv())
  
  count <- count + 1
}

# Run IPSW ----------------------------------------------------------------------
survey_attrition <- read.csv('Datasets/Final Sets/survey_final_ICW_Attrition.csv')

source('Scripts/General Scripts/PSMGeneralSetUp.R')
panelB <- final_table

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

# Append and save final table --------------------------------------------------
cat(main_panel(panelA, panelB, panelC, size, "\\\\shortstack\\{Mis-", 2.5,2.25), file = "Tables/Appendix/Main_results_reweight2.txt")


# Figures ----------------------------------------------------------------------

# ggplot setup 
text.11 <- element_text(color = "black", size = 11)
final.text.12 <- element_text(color = "black", size = 12, hjust = 0.5, family="Arial")
final.text.16 <- element_text(color = "black", size = 16, hjust = 0.5, family="Arial")
text.9 <- element_text(color = "black", size = 9)
text.16 <- element_text(color = "black", size = 16)

th <- theme(strip.text.x = final.text.16,
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

# Figures Comparing Main indexes with reweighing -------------------------------

# Figure A6: columns 1 and 6----------------------------------------------------
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
c1 <- as.numeric(c(lm_ate1[[1]]$coefficients[1:3], lm_ate1[[6]]$coefficients[1:3]))
c2 <- as.numeric(c(lm_ate1[[1]]$conf.low[1:3], lm_ate1[[6]]$conf.low[1:3]))
c3 <- as.numeric(c(lm_ate1[[1]]$conf.high[1:3], lm_ate1[[6]]$conf.high[1:3]))
c6 <- rep("WLS Americas", length(c1))

data_aux2 <- cbind.data.frame(c1, c2, c3, c4,c5, c6)

colnames(data_aux2) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux2$treatment)
data_aux2$treatment <- factor(data_aux2$treatment, levels = positions)
data_aux2$variables <- factor(data_aux2$variables, levels = unique(data_aux2$variables))
data_aux2$method <- factor(data_aux2$method, levels = unique(data_aux2$method))

# LASSO
c1 <- as.numeric(c(lm_ate2[[1]]$coefficients[1:3], lm_ate2[[6]]$coefficients[1:3]))
c2 <- as.numeric(c(lm_ate2[[1]]$conf.low[1:3], lm_ate2[[6]]$conf.low[1:3]))
c3 <- as.numeric(c(lm_ate2[[1]]$conf.high[1:3], lm_ate2[[6]]$conf.high[1:3]))
c6 <- rep("WLS Latin", length(c1))

data_aux3 <- cbind.data.frame(c1, c2, c3, c4,c5, c6)

colnames(data_aux3) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux3$treatment)
data_aux3$treatment <- factor(data_aux3$treatment, levels = positions)
data_aux3$variables <- factor(data_aux3$variables, levels = unique(data_aux3$variables))
data_aux3$method <- factor(data_aux3$method, levels = unique(data_aux3$method))

# Append the 3 datasets
data_aux <- rbind.data.frame(data_aux1, data_aux2, data_aux3)

# Plot
figureA6_a <- ggplot(data_aux, aes(x = treatment, y = coefs, colour = method)) +
  geom_point(aes(shape=method, color = factor(method)), size = 3, fill = "#d3d3d3", position = position_dodge) +
  geom_errorbar(aes(ymin = left, ymax = right), width = 0.5,
                cex = 0.75, position = position_dodge) +
  facet_grid(. ~ variables,  labeller = labeller(variables = label_wrap_gen(35))) +
  theme_light() +
  th +
  scale_color_manual(name = "Method",  labels = c("OLS", "WLS Americas", "WLS Latin"),
                     values = colors) +
  scale_shape_manual(name="Method",values=c(15,19, 17)) +
  geom_hline(yintercept=0, size = 0.8, alpha = 0.4, linetype = "dashed") +
  ylab(y_text) +
  xlab("") +
  legend_guide1 +
  legend_guide2 +
  ylim(-.6, 0.7) +
  theme(axis.text.x = element_text(vjust = - 0.2)) 


ggsave(figureA6_a, path = 'Figures', filename = "FigureA6_a.pdf", device = cairo_pdf, 
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

# WLS Americas
c1 <- rbind.data.frame(lincom(lm_ate1[[1]], test, level = 0.90)[,1:3],
                       lincom(lm_ate1[[6]], test, level = 0.90)[,1:3])

c6 <- rep("WLS Americas", length(c1))

data_aux2 <- cbind.data.frame(c1,c4,c5, c6)

colnames(data_aux2) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux2$treatment)
data_aux2$treatment <- factor(data_aux2$treatment, levels = positions)
data_aux2$variables <- factor(data_aux2$variables, levels = unique(data_aux2$variables))
data_aux2$method <- factor(data_aux2$method, levels = unique(data_aux2$method))

data_aux2$coefs <- unlist(data_aux2$coefs)
data_aux2$left <- unlist(data_aux2$left)
data_aux2$right <- unlist(data_aux2$right)

# WLS Latin
c1 <- rbind.data.frame(lincom(lm_ate2[[1]], test, level = 0.90)[,1:3],
                       lincom(lm_ate2[[6]], test, level = 0.90)[,1:3])

c6 <- rep("WLS Latin", length(c1))

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

figureA6_b <- ggplot(data_aux, aes(x = treatment, y = coefs, colour = method)) +
  geom_point(aes(shape=method, color = factor(method)), size = 3, fill = "#d3d3d3", position =position_dodge) +
  geom_errorbar(aes(ymin = left, ymax = right), width = 0.5,
                cex = 0.75, position =position_dodge) +
  facet_grid(. ~ variables,  labeller = labeller(variables = label_wrap_gen(35))) +
  theme_light() +
  th +
  scale_color_manual(name = "Method", labels = c("OLS", "WLS Americas", "WLS Latin"),
                     values = colors) +
  scale_shape_manual(name="Method",values=c(15,19, 17)) +
  geom_hline(yintercept=0, size = 0.8, alpha = 0.4, linetype = "dashed") +
  ylab(y_text) +
  xlab("") +
  legend_guide1 +
  legend_guide2 +
  ylim(-.6, 0.7) +
  theme(axis.text.x = element_text(vjust = - 0.2)) 


ggsave(figureA6_b, path = 'Figures', filename = "FigureA6_b.pdf", device = cairo_pdf, 
       width = 12, height = 8,dpi = 300)

# Figure A7: columns 2-5 -------------------------------------------------------
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

# WLS Americas
c1 <- as.numeric(c(lm_ate1[[2]]$coefficients[1:3], lm_ate1[[3]]$coefficients[1:3], lm_ate1[[4]]$coefficients[1:3], lm_ate1[[5]]$coefficients[1:3]))
c2 <- as.numeric(c(lm_ate1[[2]]$conf.low[1:3], lm_ate1[[3]]$conf.low[1:3], lm_ate1[[4]]$conf.low[1:3], lm_ate1[[5]]$conf.low[1:3]))
c3 <- as.numeric(c(lm_ate1[[2]]$conf.high[1:3], lm_ate1[[3]]$conf.high[1:3], lm_ate1[[4]]$conf.high[1:3], lm_ate1[[5]]$conf.high[1:3]))
c6 <- rep("WLS Americas", length(c1))

data_aux2 <- cbind.data.frame(c1, c2, c3, c4,c5, c6)

colnames(data_aux2) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux2$treatment)
data_aux2$treatment <- factor(data_aux2$treatment, levels = positions)
data_aux2$variables <- factor(data_aux2$variables, levels = unique(data_aux2$variables))
data_aux2$method <- factor(data_aux2$method, levels = unique(data_aux2$method))

# WLS Latin
c1 <- as.numeric(c(lm_ate2[[2]]$coefficients[1:3], lm_ate2[[3]]$coefficients[1:3], lm_ate2[[4]]$coefficients[1:3], lm_ate2[[5]]$coefficients[1:3]))
c2 <- as.numeric(c(lm_ate2[[2]]$conf.low[1:3], lm_ate2[[3]]$conf.low[1:3], lm_ate2[[4]]$conf.low[1:3], lm_ate2[[5]]$conf.low[1:3]))
c3 <- as.numeric(c(lm_ate2[[2]]$conf.high[1:3], lm_ate2[[3]]$conf.high[1:3], lm_ate2[[4]]$conf.high[1:3], lm_ate2[[5]]$conf.high[1:3]))
c6 <- rep("WLS Latin", length(c1))

data_aux3 <- cbind.data.frame(c1, c2, c3, c4,c5, c6)

colnames(data_aux3) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux3$treatment)
data_aux3$treatment <- factor(data_aux3$treatment, levels = positions)
data_aux3$variables <- factor(data_aux3$variables, levels = unique(data_aux3$variables))
data_aux3$method <- factor(data_aux3$method, levels = unique(data_aux3$method))

# Append the 3 datasets
data_aux <- rbind.data.frame(data_aux1,data_aux2,data_aux3)

# Plot
figureA7_a <- ggplot(data_aux, aes(x = treatment, y = coefs, colour = method)) +
  geom_point(aes(shape=method, color = factor(method)), size = 3, fill = "#d3d3d3", position =position_dodge) +
  geom_errorbar(aes(ymin = left, ymax = right), width = 0.5,
                cex = 0.75, position =position_dodge) +
  facet_grid(. ~ variables,  labeller = labeller(variables = label_wrap_gen(21))) +
  theme_light() +
  th +
  scale_color_manual(name = "Method",  labels = c("OLS", "WLS Americas", "WLS Latin"),
                     values = colors) +
  scale_shape_manual(name="Method",values=c(15,19, 17)) +
  geom_hline(yintercept=0, size = 0.8, alpha = 0.4, linetype = "dashed") +
  ylab(y_text) +
  xlab("") +
  legend_guide1 +
  legend_guide2 +
  ylim(-0.6, 0.85) +
  theme(axis.text.x = element_text(vjust = - 0.2)) 


ggsave(figureA7_a, path = 'Figures', filename = "FigureA7_a.pdf", device = cairo_pdf, 
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

# WLS Americas
c1 <- rbind.data.frame(lincom(lm_ate1[[2]], test, level = 0.90)[,1:3],
                       lincom(lm_ate1[[3]], test, level = 0.90)[,1:3],
                       lincom(lm_ate1[[4]], test, level = 0.90)[,1:3],
                       lincom(lm_ate1[[5]], test, level = 0.90)[,1:3])

c6 <- rep("WLS Americas", length(c1))

data_aux2 <- cbind.data.frame(c1, c4,c5, c6)

colnames(data_aux2) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux2$treatment)
data_aux2$treatment <- factor(data_aux2$treatment, levels = positions)
data_aux2$variables <- factor(data_aux2$variables, levels = unique(data_aux2$variables))
data_aux2$method <- factor(data_aux2$method, levels = unique(data_aux2$method))

data_aux2$coefs <- unlist(data_aux2$coefs)
data_aux2$left <- unlist(data_aux2$left)
data_aux2$right <- unlist(data_aux2$right)

# WLS Latin
c1 <- rbind.data.frame(lincom(lm_ate2[[2]], test, level = 0.90)[,1:3],
                       lincom(lm_ate2[[3]], test, level = 0.90)[,1:3],
                       lincom(lm_ate2[[4]], test, level = 0.90)[,1:3],
                       lincom(lm_ate2[[5]], test, level = 0.90)[,1:3])

c6 <- rep("WLS Latin", length(c1))

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

figureA7_b <- ggplot(data_aux, aes(x = treatment, y = coefs, colour = method)) +
  geom_point(aes(shape=method, color = factor(method)), size = 3, fill = "#d3d3d3", position =position_dodge) +
  geom_errorbar(aes(ymin = left, ymax = right), width = 0.5,
                cex = 0.75, position =position_dodge) +
  facet_grid(. ~ variables,  labeller = labeller(variables = label_wrap_gen(21))) +
  theme_light() +
  th +
  scale_color_manual(name = "Method",  labels = c("OLS", "WLS Americas", "WLS Latin"),
                     values = colors) +
  scale_shape_manual(name="Method",values=c(15,19, 17)) +
  geom_hline(yintercept=0, size = 0.8, alpha = 0.4, linetype = "dashed") +
  ylab(y_text) +
  xlab("") +
  legend_guide1 +
  legend_guide2 +
  ylim(-0.6, 0.85) +
  theme(axis.text.x = element_text(vjust = - 0.2)) 

ggsave(figureA7_b, path = 'Figures', filename = "FigureA7_b.pdf", device = cairo_pdf, 
       width = 12, height = 8,dpi = 300)


# Figure A8: columns 7 and 8 ----------------------------------------------------
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

# WLS Americas
c1 <- as.numeric(c(lm_ate1[[7]]$coefficients[1:3], lm_ate1[[8]]$coefficients[1:3]))
c2 <- as.numeric(c(lm_ate1[[7]]$conf.low[1:3], lm_ate1[[8]]$conf.low[1:3]))
c3 <- as.numeric(c(lm_ate1[[7]]$conf.high[1:3], lm_ate1[[8]]$conf.high[1:3]))
c6 <- rep("WLS Americas", length(c1))

data_aux2 <- cbind.data.frame(c1, c2, c3, c4,c5, c6)

colnames(data_aux2) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux2$treatment)
data_aux2$treatment <- factor(data_aux2$treatment, levels = positions)
data_aux2$variables <- factor(data_aux2$variables, levels = unique(data_aux2$variables))
data_aux2$method <- factor(data_aux2$method, levels = unique(data_aux2$method))

# WLS Latin
c1 <- as.numeric(c(lm_ate2[[7]]$coefficients[1:3], lm_ate2[[8]]$coefficients[1:3]))
c2 <- as.numeric(c(lm_ate2[[7]]$conf.low[1:3], lm_ate2[[8]]$conf.low[1:3]))
c3 <- as.numeric(c(lm_ate2[[7]]$conf.high[1:3], lm_ate2[[8]]$conf.high[1:3]))
c6 <- rep("WLS Latin", length(c1))

data_aux3 <- cbind.data.frame(c1, c2, c3, c4,c5, c6)

colnames(data_aux3) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux3$treatment)
data_aux3$treatment <- factor(data_aux3$treatment, levels = positions)
data_aux3$variables <- factor(data_aux3$variables, levels = unique(data_aux3$variables))
data_aux3$method <- factor(data_aux3$method, levels = unique(data_aux3$method))

# Append the 3 datasets
data_aux <- rbind.data.frame(data_aux1,data_aux2,data_aux3)

# Plot
figureA8_a <- ggplot(data_aux, aes(x = treatment, y = coefs, colour = method)) +
  geom_point(aes(shape=method, color = factor(method)), size = 3, fill = "#d3d3d3", position =position_dodge) +
  geom_errorbar(aes(ymin = left, ymax = right), width = 0.5,
                cex = 0.75, position =position_dodge) +
  facet_grid(. ~ variables,  labeller = labeller(variables = label_wrap_gen(35))) +
  theme_light() +
  th +
  scale_color_manual(name = "Method",  c("OLS", "WLS Americas", "WLS Latin"),
                     values = colors) +
  scale_shape_manual(name="Method",values=c(15,19, 17)) +
  geom_hline(yintercept=0, size = 0.8, alpha = 0.4, linetype = "dashed") +
  ylab(y_text) +
  xlab("") +
  legend_guide1 +
  legend_guide2 +
  ylim(-0.5, 0.9) +
  theme(axis.text.x = element_text(vjust = - 0.2)) 


ggsave(figureA8_a, path = 'Figures', filename = "FigureA8_a.pdf", device = cairo_pdf, 
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

# WLS Americas
c1 <- rbind.data.frame(lincom(lm_ate1[[7]], test, level = 0.90)[,1:3],
                       lincom(lm_ate1[[8]], test, level = 0.90)[,1:3])

c6 <- rep("WLS Americas", length(c1))

data_aux2 <- cbind.data.frame(c1, c4,c5, c6)

colnames(data_aux2) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux2$treatment)
data_aux2$treatment <- factor(data_aux2$treatment, levels = positions)
data_aux2$variables <- factor(data_aux2$variables, levels = unique(data_aux2$variables))
data_aux2$method <- factor(data_aux2$method, levels = unique(data_aux2$method))

data_aux2$coefs <- unlist(data_aux2$coefs)
data_aux2$left <- unlist(data_aux2$left)
data_aux2$right <- unlist(data_aux2$right)

# WLS Latin
c1 <- rbind.data.frame(lincom(lm_ate2[[7]], test, level = 0.90)[,1:3],
                       lincom(lm_ate2[[8]], test, level = 0.90)[,1:3])

c6 <- rep("WLS Latin", length(c1))

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

figureA8_b <- ggplot(data_aux, aes(x = treatment, y = coefs, colour = method)) +
  geom_point(aes(shape=method, color = factor(method)), size = 3, fill = "#d3d3d3", position =position_dodge) +
  geom_errorbar(aes(ymin = left, ymax = right), width = 0.5,
                cex = 0.75, position =position_dodge) +
  facet_grid(. ~ variables,  labeller = labeller(variables = label_wrap_gen(35))) +
  theme_light() +
  th +
  scale_color_manual(name = "Method",  labels = c("OLS", "WLS Americas", "WLS Latin"),
                     values = colors) +
  scale_shape_manual(name="Method",values=c(15,19, 17)) +
  geom_hline(yintercept=0, size = 0.8, alpha = 0.4, linetype = "dashed") +
  ylab(y_text) +
  xlab("") +
  legend_guide1 +
  legend_guide2 +
  ylim(-0.5, 0.9) +
  theme(axis.text.x = element_text(vjust = - 0.2)) 

ggsave(figureA8_b, path = 'Figures', filename = "FigureA8_b.pdf", device = cairo_pdf, 
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
        rep("Increase verifying behavior index",3))

c6 <- rep("OLS", length(c1))

data_aux1 <- cbind.data.frame(c1, c2, c3, c4,c5, c6)

colnames(data_aux1) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux1$treatment)
data_aux1$treatment <- factor(data_aux1$treatment, levels = positions)
data_aux1$variables <- factor(data_aux1$variables, levels = unique(data_aux1$variables))
data_aux1$method <- factor(data_aux1$method, levels = unique(data_aux1$method))

# WLS Americas
c1 <- as.numeric(c(lm_ate1[[9]]$coefficients[1:3], lm_ate1[[10]]$coefficients[1:3], lm_ate1[[11]]$coefficients[1:3],  lm_ate1[[12]]$coefficients[1:3]))
c2 <- as.numeric(c(lm_ate1[[9]]$conf.low[1:3], lm_ate1[[10]]$conf.low[1:3], lm_ate1[[11]]$conf.low[1:3], lm_ate1[[12]]$conf.low[1:3]))
c3 <- as.numeric(c(lm_ate1[[9]]$conf.high[1:3], lm_ate1[[10]]$conf.high[1:3], lm_ate1[[11]]$conf.high[1:3], lm_ate1[[12]]$conf.high[1:3]))

c6 <- rep("WLS Americas", length(c1))

data_aux2 <- cbind.data.frame(c1, c2, c3, c4,c5, c6)

colnames(data_aux2) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux2$treatment)
data_aux2$treatment <- factor(data_aux2$treatment, levels = positions)
data_aux2$variables <- factor(data_aux2$variables, levels = unique(data_aux2$variables))
data_aux2$method <- factor(data_aux2$method, levels = unique(data_aux2$method))

# WLS Latin
c1 <- as.numeric(c(lm_ate2[[9]]$coefficients[1:3], lm_ate2[[10]]$coefficients[1:3], lm_ate2[[11]]$coefficients[1:3],  lm_ate2[[12]]$coefficients[1:3]))
c2 <- as.numeric(c(lm_ate2[[9]]$conf.low[1:3], lm_ate2[[10]]$conf.low[1:3], lm_ate2[[11]]$conf.low[1:3], lm_ate2[[12]]$conf.low[1:3]))
c3 <- as.numeric(c(lm_ate2[[9]]$conf.high[1:3], lm_ate2[[10]]$conf.high[1:3], lm_ate2[[11]]$conf.high[1:3], lm_ate2[[12]]$conf.high[1:3]))
c6 <- rep("WLS Latin", length(c1))

data_aux3 <- cbind.data.frame(c1, c2, c3, c4,c5, c6)

colnames(data_aux3) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux3$treatment)
data_aux3$treatment <- factor(data_aux3$treatment, levels = positions)
data_aux3$variables <- factor(data_aux3$variables, levels = unique(data_aux3$variables))
data_aux3$method <- factor(data_aux3$method, levels = unique(data_aux3$method))

# Append the 3 datasets
data_aux <- rbind.data.frame(data_aux1,data_aux2,data_aux3)

# Plot
figureA9_a <- ggplot(data_aux, aes(x = treatment, y = coefs, colour = method)) +
  geom_point(aes(shape=method, color = factor(method)), size = 3, fill = "#d3d3d3", position =position_dodge) +
  geom_errorbar(aes(ymin = left, ymax = right), width = 0.5,
                cex = 0.75, position =position_dodge) +
  facet_grid(. ~ variables,  labeller = labeller(variables = label_wrap_gen(21))) +
  theme_light() +
  th +
  scale_color_manual(name = "Method",  c("OLS", "WLS Americas", "WLS Latin"),
                     values = colors) +
  scale_shape_manual(name="Method",values=c(15,19, 17)) +
  geom_hline(yintercept=0, size = 0.8, alpha = 0.4, linetype = "dashed") +
  ylab(y_text) +
  xlab("") +
  legend_guide1 +
  legend_guide2 +
  ylim(-0.6, 0.8) +
  theme(axis.text.x = element_text(vjust = - 0.2)) 

ggsave(figureA9_a, path = 'Figures', filename = "FigureA9_a.pdf", device = cairo_pdf, 
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

# WLS Americas
c2 <- rbind.data.frame(lincom(lm_ate1[[9]], test, level = 0.90)[,1:3],
                       lincom(lm_ate1[[10]], test, level = 0.90)[,1:3],
                       lincom(lm_ate1[[11]], test, level = 0.90)[,1:3],
                       lincom(lm_ate1[[12]], test, level = 0.90)[,1:3])

c6 <- rep("WLS Americas", length(c1))

data_aux2 <- cbind.data.frame(c1, c4,c5, c6)

colnames(data_aux2) <- c("coefs", "left", "right", "treatment", "variables", "method")
positions <- unique(data_aux2$treatment)
data_aux2$treatment <- factor(data_aux2$treatment, levels = positions)
data_aux2$variables <- factor(data_aux2$variables, levels = unique(data_aux2$variables))
data_aux2$method <- factor(data_aux2$method, levels = unique(data_aux2$method))

data_aux2$coefs <- unlist(data_aux2$coefs)
data_aux2$left <- unlist(data_aux2$left)
data_aux2$right <- unlist(data_aux2$right)

# WLS Latin
c1 <- rbind.data.frame(lincom(lm_ate2[[9]], test, level = 0.90)[,1:3],
                       lincom(lm_ate2[[10]], test, level = 0.90)[,1:3],
                       lincom(lm_ate2[[11]], test, level = 0.90)[,1:3],
                       lincom(lm_ate2[[12]], test, level = 0.90)[,1:3])

c6 <- rep("WLS Latin", length(c1))

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

figureA9_b <- ggplot(data_aux, aes(x = treatment, y = coefs, colour = method)) +
  geom_point(aes(shape=method, color = factor(method)), size = 3, fill = "#d3d3d3", position =position_dodge) +
  geom_errorbar(aes(ymin = left, ymax = right), width = 0.5,
                cex = 0.75, position =position_dodge) +
  facet_grid(. ~ variables,  labeller = labeller(variables = label_wrap_gen(21))) +
  theme_light() +
  th +
  scale_color_manual(name = "Method",  labels = c("OLS", "WLS Americas", "WLS Latin"),
                     values = colors) +
  scale_shape_manual(name="Method",values=c(15,19, 17)) +
  geom_hline(yintercept=0, size = 0.8, alpha = 0.4, linetype = "dashed") +
  ylab(y_text) +
  xlab("") +
  legend_guide1 +
  legend_guide2 +
  ylim(-0.6, 0.8) +
  theme(axis.text.x = element_text(vjust = - 0.2)) 

ggsave(figureA9_b, path = 'Figures', filename = "FigureA9_b.pdf", device = cairo_pdf, 
       width = 12, height = 8,dpi = 300)

