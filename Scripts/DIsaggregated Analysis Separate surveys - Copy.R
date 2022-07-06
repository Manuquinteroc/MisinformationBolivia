# Appendix tables for version of disaggregated tables for the ICW indexes

# read data
survey <- read.csv('Datasets/survey_final_jointV3_ICW.csv')

# ------------------------------------------------------------------------------
# Auxiliary variables for stargazer
covariates <- c("Course and verification", "Course", "Verification")
interaction_name <- c("WhatsApp Delivery")

# Aux for regressions
treatments_separate <- c("curso_verificacion", "curso", "verificacion")
interacted <- c("WhatsApp")
covariates_interacted <- c(covariates, paste(covariates, interaction_name, sep = " x "))


# Mechanisms: importance -------------------------------------------------------
aux <- c("index_mec_importance_end", c(paste0("problemas_", 1:6, "_end"), "temas_2_end"))

aux_data <- survey[aux]

dep_var <- c("\\shortstack{Misinformation \\\\ importance  \\\\ index \\\\ of (1,1,1,1 \\\\ 1,1,1)}",
             "\\shortstack{Problem 1}",
             "\\shortstack{Problem 2}", 
             "\\shortstack{Problem 3}", 
             "\\shortstack{Problem 4}",
             "\\shortstack{Problem 5}", 
             "\\shortstack{Problem 6}", 
             "\\shortstack{Interest in \\\\ misinformation}")

control <- c("education", "gender", "edad", "index_mec_importance_baseline", 
             "false_baseline", "trust_baseline", "freq_analyze") # mechanisms


omit_var <- c("Constant", "blockid1", control, interacted)

title <- "Treatment effects on mechanisms: Misinformation relevance index's components"

# Create tables and save results
size = length(aux) + 1
cm = 16 # length in cm of the note for the table
interacted_bool <- c(T,T,T,T,T,T,T,F)

source('Scripts/General Scripts/ATEsGeneralSetUp.R')

cat(final_table, file = "Tables/Appendix/Disaggregated/mechanism_importance.txt")

# Mechanisms: falsehood --------------------------------------------------------
aux <- c("false_end", paste0("freq_plat_false_", c(1:3, 9, 4:6), "_end"), paste0("case", c(1,3), "_hyp_2_end"))

aux_data <- survey[aux]

dep_var <- c("\\shortstack{Likelihood of \\\\ false \\\\ index \\\\ of (-1,-1,-1,-1 \\\\ 1,1,1,1,1)}",
             "\\shortstack{false 1}",
             "\\shortstack{false 2}", 
             "\\shortstack{false 3}", 
             "\\shortstack{false 4}",
             "\\shortstack{false 5}", 
             "\\shortstack{false 6}", 
             "\\shortstack{false 7}", 
             "\\shortstack{hyp 1}",
             "\\shortstack{hyp 2}")

control <- c("education", "gender", "edad", "index_mec_importance_baseline", 
             "false_baseline", "trust_baseline", "freq_analyze") # mechanisms

omit_var <- c("Constant", "blockid1", control, interacted)

title <- "Treatment effects on mechanisms: Likelihood of false index's components"

# Create tables and save results
size = length(aux) + 1
cm = 20 # length in cm of the note for the table
interacted_bool <- c(T,T,T,T,F,T,T,T,T,T)

source('Scripts/General Scripts/ATEsGeneralSetUp.R')

cat(final_table, file = "Tables/Appendix/Disaggregated/mechanism_falsehood.txt")


# Mechanisms: Trust --------------------------------------------------------
aux <- c("trust_end", paste0("confia_", c(1:3, 9, 4:6), "_end"))

aux_data <- survey[aux]

dep_var <- c("\\shortstack{Trust \\\\ index \\\\ of (1,1,1,1 \\\\ -1,-1,-1)}",
             "\\shortstack{Trust 1}",
             "\\shortstack{Trust 2}", 
             "\\shortstack{Trust 3}", 
             "\\shortstack{Trust 4}",
             "\\shortstack{Trust 5}", 
             "\\shortstack{Trust 6}", 
             "\\shortstack{Trust 7}")

control <- c("education", "gender", "edad", "index_mec_importance_baseline", 
             "false_baseline", "trust_baseline", "freq_analyze") # mechanisms

omit_var <- c("Constant", "blockid1", control, interacted)

title <- "Treatment effects on mechanisms: Trust index's components"

# Create tables and save results
size = length(aux) + 1
cm = 16 # length in cm of the note for the table
interacted_bool <- c(T,T,T,T,F,T,T,T)

source('Scripts/General Scripts/ATEsGeneralSetUp.R')

cat(final_table, file = "Tables/Appendix/Disaggregated/mechanism_trust.txt")


# Mechanisms: Attention --------------------------------------------------------
aux <- c("index_mec_attention", "freq_analyze_end", "avoid_mis_1_end", "avoid_mis_2_end")

aux_data <- survey[aux]

dep_var <- c("\\shortstack{Attention to \\\\ misinformation \\\\ index \\\\ of (1,1,1,1 \\\\ -1,-1,-1)}",
             "\\shortstack{Attention 1}",
             "\\shortstack{Attention 2}", 
             "\\shortstack{Attention 3}")

control <- c("education", "gender", "edad", "index_mec_importance_baseline", 
             "false_baseline", "trust_baseline", "freq_analyze") # mechanisms

omit_var <- c("Constant", "blockid1", control, interacted)

title <- "Treatment effects on mechanisms: Attention to misinformation index's components"

# Create tables and save results
size = length(aux) + 1
cm = 13 # length in cm of the note for the table
interacted_bool <- c(T,T,F,F)

source('Scripts/General Scripts/ATEsGeneralSetUp.R')

cat(final_table, file = "Tables/Appendix/Disaggregated/mechanism_attention.txt")


# knowledge to identify information --------------------------------------------
aux <- c("index_know_identify", c("knowledge_verify_end", "characts_index_end", "recent_cases_sum_end",
                                  paste0("knowledge_plat_", c(1:3, 11, 4:6), "_end")))

aux_data <- survey[aux]

dep_var <- c("\\shortstack{Knowledge to \\\\ identify \\\\ information \\\\ index \\\\ of (1,1,1,1, 1 \\\\ 1,1, -1,-1,-1)}",
             "\\shortstack{Knowledge 1}",
             "\\shortstack{Knowledge 2}", 
             "\\shortstack{Knowledge 3}",
             "\\shortstack{Knowledge 4}",
             "\\shortstack{Knowledge 5}", 
             "\\shortstack{Knowledge 6}",
             "\\shortstack{Knowledge 7}",
             "\\shortstack{Knowledge 8}", 
             "\\shortstack{Knowledge 9}",
             "\\shortstack{Knowledge 10}")

control2 <- c("education", "gender", "edad", "knowledge_identify_baseline") # knowledge to identify

omit_var <- c("Constant", "blockid1", control, interacted)

title <- "Treatment effects on knowledge to identify information index's components"

# Create tables and save results
size = length(aux) + 1
cm = 20 # length in cm of the note for the table
interacted_bool <- c(T,T,T,T,T,T,T,F,T,T,T)

source('Scripts/General Scripts/ATEsGeneralSetUp.R')

cat(final_table, file = "Tables/Appendix/Disaggregated/knowledge_identify.txt")


# knowledge to verify information --------------------------------------------
aux <- c("index_know_verify", "know_to_verify_end", "forms_index_end", "know_fact_checkers_sum_end")

aux_data <- survey[aux]

dep_var <- c("\\shortstack{Knowledge to \\\\ identify \\\\ information \\\\ index \\\\ of (1,1,1)}",
             "\\shortstack{Knowledge 1}",
             "\\shortstack{Knowledge 2}", 
             "\\shortstack{Knowledge 3}")

control3 <- c("education", "gender", "edad", "index_know_verify_baseline") # Knowledge to verify

omit_var <- c("Constant", "blockid1", control, interacted)

title <- "Treatment effects on knowledge to verify information index's components"

# Create tables and save results
size = length(aux) + 1
cm = 12 # length in cm of the note for the table
interacted_bool <- c(T,T,T,T)

source('Scripts/General Scripts/ATEsGeneralSetUp.R')

cat(final_table, file = "Tables/Appendix/Disaggregated/knowledge_verify.txt")


# Consumption ------------------------------------------------------------------
aux <- c("index_consumption", paste0("freq_tipos_", c(1:3, 9, 4:6), "_end"))

aux_data <- survey[aux]

dep_var <- c("\\shortstack{Consumption \\\\  behavior index \\\\ of (1,1,1,1, \\\\-1,-1,-1)}",
             "\\shortstack{Consumption 1}",
             "\\shortstack{Consumption 2}", 
             "\\shortstack{Consumption 3}",
             "\\shortstack{Consumption 4}",
             "\\shortstack{Consumption 5}",
             "\\shortstack{Consumption 6}",
             "\\shortstack{Consumption 7}")

control4 <- c("education", "gender", "edad", "consumption_baseline", "freq_use_social", "freq_use_whats") # Consumption - Nonmissing observations 

omit_var <- c("Constant", "blockid1", control, interacted)

title <- "Treatment effects on consumption behavior index's components"

# Create tables and save results
size = length(aux) + 1
cm = 12 # length in cm of the note for the table
interacted_bool <- c(T,T,T,T,F,T,T,T)

source('Scripts/General Scripts/ATEsGeneralSetUp.R')

cat(final_table, file = "Tables/Appendix/Disaggregated/consumption.txt")


# Sharing Behavior -------------------------------------------------------------
aux <- c("index_sharing", "freq_desc_comp_end", "cree_falsa_do_index_end", paste0("case", c(1,3), "_hyp_1_end"))

aux_data <- survey[aux]

dep_var <- c("\\shortstack{Sharing \\\\ behavior index \\\\ of (1,1,\\\\-1,-1)}",
             "\\shortstack{Sharing 1}",
             "\\shortstack{Sharing 2}", 
             "\\shortstack{Sharing 3}",
             "\\shortstack{Sharing 3}")

control5 <- c("education", "gender", "edad", "index_sharing_baseline") # Sharing

omit_var <- c("Constant", "blockid1", control, interacted)

title <- "Treatment effects on sharing behavior index's components"

# Create tables and save results
size = length(aux) + 1
cm = 12 # length in cm of the note for the table
interacted_bool <- c(T,T,T,T,T)

source('Scripts/General Scripts/ATEsGeneralSetUp.R')

cat(final_table, file = "Tables/Appendix/Disaggregated/sharing.txt")


# Verifying behavior -----------------------------------------------------------
aux <- c("index_verifying",  "freq_veri_compar_end", "avoid_mis_3_end", paste0("case", c(1,3), "_hyp_3_end"))

aux_data <- survey[aux]

dep_var <- c("\\shortstack{Verifying  \\\\ behavior \\\\  index \\\\ of (1,1,1,1)}",
             "\\shortstack{Verifying 1}",
             "\\shortstack{Verifying 2}", 
             "\\shortstack{Verifying 3}",
             "\\shortstack{Verifying 3}")

control6 <- c("education", "gender", "edad", "ver_beh_baseline") # Verifying

omit_var <- c("Constant", "blockid1", control, interacted)

title <- "Treatment effects on verifying behavior index's components"

# Create tables and save results
size = length(aux) + 1
cm = 12 # length in cm of the note for the table
interacted_bool <- c(T,T,F,T,T)

source('Scripts/General Scripts/ATEsGeneralSetUp.R')

cat(final_table, file = "Tables/Appendix/Disaggregated/verifying.txt")



