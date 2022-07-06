# Main results 

# read data
survey <- read.csv('Datasets/survey_final_jointV3_ICW.csv')

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
             "\\shortstack{Increase \\\\ likelihood of \\\\ false of \\\\ traditional \\\\sources \\\\index}", 
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
cm = 19.5 
interacted_bool <- c(T,T,T,T,T,T,T,T,T,T,T,T,T)
label = "MainIndexesUnion"

source('Scripts/General Scripts/ATEsGeneralSetUp.R')
panelA <- final_table

# Run PSM ----------------------------------------------------------------------
survey_attrition <- read.csv('Datasets/survey_final_jointV3_ICW_Attrition_Both.csv')

source('Scripts/General Scripts/PSMGeneralSetUpV4.R')
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
cat(main_panel(panelA, panelB, panelC, size, "\\\\shortstack\\{Mis-", 2, 2.25), file = "Tables/Appendix/Main_results_union.txt")
