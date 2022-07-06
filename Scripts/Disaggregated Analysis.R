# Appendix tables for version of disaggregated tables for the ICW indexes

# ------------------------------------------------------------------------------
# read data
survey <- read.csv('Datasets/Final Sets/survey_final_ICW.csv')
survey_attrition <- read.csv('Datasets/Final Sets/survey_final_ICW_Attrition.csv')

# Aux for rewight analysis
weight1 <- FALSE
weight2 <- FALSE

# Auxiliary variables for stargazer
covariates <- c("Course and verification", "Course", "Verification")
interaction_name <- c("WhatsApp Delivery")

# Aux for regressions
treatments_separate <- c("curso_verificacion", "curso", "verificacion")
interacted <- c("WhatsApp")
covariates_interacted <- c(covariates, paste(covariates, interaction_name, sep = " x "))

# LASSO baseline controls
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

number_lasso <- length(controls)

# Aux for stargazer notes
additional_note <- ""

# Mechanisms: importance -------------------------------------------------------
# ATE
aux <- c("index_mec_importance_end", paste0("problemas_", 1:6, "_end"))

aux_data <- survey[aux]

dep_var <- c("\\shortstack{Misinformation \\\\ importance  \\\\ index \\\\ of (1,1,1,1 \\\\ 1,1,1)}",
             "\\shortstack{Decisions that \\\\may affect \\\\health}",
             "\\shortstack{Election of \\\\candidates who \\\\do not represent \\\\the interests of \\\\citizens}", 
             "\\shortstack{To falsely \\\\discredit or \\\\glorify \\\\people}", 
             "\\shortstack{Ideological \\\\polarization}",
             "\\shortstack{Violence \\\\towards certain\\\\people \\\\or groups in \\\\society}", 
             "\\shortstack{Increase\\\\ hatred towards \\\\certain people \\\\or groups in \\\\society}")

control <- c("education", "gender", "edad", "index_mec_importance_baseline", 
             "false_baseline", "trust_traditional_baseline", "trust_social_baseline", "freq_analyze")

lagged <- paste0("problemas_", 1:6)

control_list <- list(control, c(control, "problemas_1"), c(control, "problemas_2"), c(control, "problemas_3"), c(control, "problemas_4"),
                     c(control, "problemas_5"),c(control, "problemas_6"))

omit_var <- c("Constant", "blockid1", control, lagged, interacted, controls)

title <- "Treatment effects on mechanisms: Misinformation relevance index's components"

# Create tables and save results
size = length(aux) + 1
cm = 15.5 # length in cm of the note for the table
label = "Dis1"

source('Scripts/General Scripts/ATEsGeneralSetUp.R')
panelA <- final_table

# IPSW
source('Scripts/General Scripts/PSMGeneralSetUp.R')
panelB <- final_table

# LASSO
lagged <- c("index_mec_importance_baseline", lagged)
source('Scripts/General Scripts/LASSO_Covariates.R') # Obtain control_list
source('Scripts/General Scripts/ATEsGeneralSetUp LASSO.R') # Run ATE with LASSO covariates
panelC <- final_table

cat(main_panel(panelA, panelB, panelC, size, "\\\\shortstack\\{Misinformation",2.75, NA), file = "Tables/Appendix/Disaggregated/mechanism_importance.txt")

# Mechanisms: falsehood --------------------------------------------------------
aux <- c("false_traditional_end", paste0("freq_plat_false_", c(1:3), "_end"), 
         "false_social_end", paste0("freq_plat_false_", c(4:6), "_end"), paste0("case", c(1,3), "_hyp_2_end"))

aux_data <- survey[aux]

dep_var <- c("\\shortstack{Decrease \\\\ likelihood of \\\\ false of \\\\ traditional \\\\sources \\\\index\\\\ of (-1,-1,-1)}", 
             "\\shortstack{Radio / TV}",
             "\\shortstack{Newspaper}", 
             "\\shortstack{Internet}", 
             "\\shortstack{Increase \\\\ likelihood of \\\\ false of \\\\ social media \\\\sources \\\\index\\\\ of (1,1,1,1,1)}", 
             "\\shortstack{WhatsApp}",
             "\\shortstack{Social media}", 
             "\\shortstack{Conversations}", 
             "\\shortstack{Believe \\\\scenario 1 \\\\is fake^{\\dagger}}",
             "\\shortstack{Believe \\\\scenario 3 \\\\is fake^{\\dagger\\dagger}}") # Need to add note about scenarios to each cases

control <- c("education", "gender", "edad", "index_mec_importance_baseline", 
             "false_baseline", "trust_traditional_baseline", "trust_social_baseline", "freq_analyze") # mechanisms

lagged <- paste0("case", c(1,3), "_hyp_2_end")

control_list <- list(control, control, control, control, control, control, control, control,
                     c(control, "case1_hyp_2"), c(control, "case3_hyp_2"))

omit_var <- c("Constant", "blockid1", control, lagged, interacted, controls)

title <- "Treatment effects on mechanisms: Likelihood of false index's components"

# Create tables and save results
size = length(aux) + 1
cm = 18 # length in cm of the note for the table
label = "Dis2"
additional_note <- "$\\dagger$: imagine you received an that came to you on WhatsApp with a screenshot of the Twitter account of a well-known person saying something very controversial. 
$\\dagger\\dagger$: imagine you received an audio that came to you by WhatsApp reporting evidence of corruption of a politician that you already suspected and you did not like. "


source('Scripts/General Scripts/ATEsGeneralSetUp.R')
panelA <- final_table

# IPSW
source('Scripts/General Scripts/PSMGeneralSetUp.R')
panelB <- final_table

# LASSO
lagged <- c("false_baseline", NA,NA,NA, "false_baseline", NA,NA,NA, lagged)
source('Scripts/General Scripts/LASSO_Covariates.R') # Obtain control_list

# Fix for control_list NA estimate values for dependent variable 9 and 10
est9 <- c("consumption_traditional_baseline",
          "trust_traditional_baseline",
          "index_sharing_baseline",
          "trust_social_baseline",
          "consumption_social_baseline",
          "index_mec_importance_baseline",
          "false_baseline",
          "edu10")

est10 <- c("edu10",
           "edad55",
            "false_baseline",
            "trust_traditional_baseline",
            "index_know_verify_baseline",
            "consumption_traditional_baseline",
            "index_sharing_baseline")

control_list[[9]] <- control_list[[9]][which(control_list[[9]] %!in% est9 )]
control_list[[10]] <- control_list[[10]][which(control_list[[10]] %!in% est10 )]

source('Scripts/General Scripts/ATEsGeneralSetUp LASSO.R') # Run ATE with LASSO covariates
panelC <- final_table

cat(main_panel(panelA, panelB, panelC, size, "\\\\shortstack\\{Decrease", 2.75, 0.5), file = "Tables/Appendix/Disaggregated/mechanism_falsehood.txt")

additional_note <- ""

# Mechanisms: Distrust --------------------------------------------------------
aux <- c("trust_traditional_end", paste0("confia_", c(1:3), "_end"), 
         "trust_social_end",  paste0("confia_", c(4:6), "_end"))

aux_data <- survey[aux]

dep_var <- c("\\shortstack{Trust \\\\ traditional \\\\sources index \\\\ of (1,1,1)}",
             "\\shortstack{Radio / TV}",
             "\\shortstack{Newspaper}", 
             "\\shortstack{Internet}", 
             "\\shortstack{Distrust \\\\ social media \\\\ sources index \\\\ of (-1,-1,-1)}",
             "\\shortstack{WhatsApp}",
             "\\shortstack{Social media}", 
             "\\shortstack{Conversations}")

control <- c("education", "gender", "edad", "index_mec_importance_baseline", 
             "false_baseline", "trust_traditional_baseline", "trust_social_baseline", "freq_analyze") # mechanisms

lagged <- paste0("confia_", c(1:6))

control_list <- list(control, c(control, "confia_1"), c(control, "confia_2"), c(control, "confia_3"), control,
                     c(control, "confia_4"), c(control, "confia_5"), c(control, "confia_6"))

omit_var <- c("Constant", "blockid1", control, lagged, interacted, controls)

title <- "Treatment effects on mechanisms: Distrust index's components"

# Create tables and save results
size = length(aux) + 1
cm = 15.5 # length in cm of the note for the table
label = "Dis3"

source('Scripts/General Scripts/ATEsGeneralSetUp.R')
panelA <- final_table

# IPSW
source('Scripts/General Scripts/PSMGeneralSetUp.R')
panelB <- final_table

# LASSO
lagged <- c("trust_traditional_baseline", paste0("confia_", c(1:3)), "trust_social_baseline", paste0("confia_", c(4:6)))
source('Scripts/General Scripts/LASSO_Covariates.R') # Obtain control_list
source('Scripts/General Scripts/ATEsGeneralSetUp LASSO.R') # Run ATE with LASSO covariates
panelC <- final_table

cat(main_panel(panelA, panelB, panelC, size, "\\\\shortstack\\{Trust",2.75, NA), file = "Tables/Appendix/Disaggregated/mechanism_trust.txt")

# Mechanisms: Attention --------------------------------------------------------
aux <- c("index_mec_attention", "freq_analyze_end")

aux_data <- survey[aux]

dep_var <- c("\\shortstack{Attention to \\\\ misinformation \\\\ index \\\\ of (1)}",
             "\\shortstack{How often \\\\do you question\\\\ wether a\\\\ news is false}")

control <- c("education", "gender", "edad", "index_mec_importance_baseline", 
              "false_baseline", "trust_traditional_baseline", "trust_social_baseline", "freq_analyze") # mechanisms

control_list <- list(control, control)

omit_var <- c("Constant", "blockid1", control, lagged, interacted, controls)

title <- "Treatment effects on mechanisms: Attention to misinformation index's components"

# Create tables and save results
size = length(aux) + 1
cm = 8.5 # length in cm of the note for the table
label = "Dis4"

source('Scripts/General Scripts/ATEsGeneralSetUp.R')
panelA <- final_table

# IPSW
source('Scripts/General Scripts/PSMGeneralSetUp.R')
panelB <- final_table

# LASSO
lagged <- c("freq_analyze", "freq_analyze")
source('Scripts/General Scripts/LASSO_Covariates.R') # Obtain control_list
source('Scripts/General Scripts/ATEsGeneralSetUp LASSO.R') # Run ATE with LASSO covariates
panelC <- final_table

cat(main_panel(panelA, panelB, panelC, size, "\\\\shortstack\\{Attention",2.75, NA), file = "Tables/Appendix/Disaggregated/mechanism_attention.txt")

# knowledge to identify information --------------------------------------------
aux <- c("index_know_identify", c("knowledge_verify_end", "characts_index_end", "recent_cases_sum_end",
                                  paste0("knowledge_plat_", c(1:3, 4:6), "_end")))

aux_data <- survey[aux]

dep_var <- c("\\shortstack{Increase \\\\ knowledge to \\\\ identify \\\\ information \\\\ index \\\\ of (1,1,1,-1, \\\\ -1,-1,1,1,1)}",
             "\\shortstack{How much \\\\knowledge do you \\\\have to identify \\\\whether a news is \\\\false or not?}",
             "\\shortstack{Knowing how\\\\ to identify \\\\a fake new \\\\index}", 
             "\\shortstack{How many \\\\recent \\\\misinformation \\\\cases in Bolivia \\\\do you know?}",
             "\\shortstack{Radio / TV}",
             "\\shortstack{Newspaper}", 
             "\\shortstack{Internet}", 
             "\\shortstack{WhatsApp}",
             "\\shortstack{Social media}", 
             "\\shortstack{Conversations}")

control <- c("education", "gender", "edad", "knowledge_identify_baseline") # knowledge to identify

lagged <- c("knowledge_verify", "characts_index", paste0("knowledge_plat_", c(1:3, 4:6)))

control_list <- list(control, c(control, "knowledge_verify"), c(control, "characts_index"), control, 
                     c(control, "knowledge_plat_1"), c(control, "knowledge_plat_2"), c(control, "knowledge_plat_3"),
                     c(control, "knowledge_plat_4"),c(control, "knowledge_plat_5"),c(control, "knowledge_plat_6"))

omit_var <- c("Constant", "blockid1", control, lagged, interacted, controls)

title <- "Treatment effects on knowledge to identify information index's components"

# Create tables and save results
size = length(aux) + 1
cm = 19 # length in cm of the note for the table
label = "Dis5"

source('Scripts/General Scripts/ATEsGeneralSetUp.R')
panelA <- final_table

# IPSW
source('Scripts/General Scripts/PSMGeneralSetUp.R')
panelB <- final_table

# LASSO
lagged <- c("knowledge_identify_baseline", c("knowledge_verify", "characts_index",NA,paste0("knowledge_plat_", c(1:3, 4:6))))
source('Scripts/General Scripts/LASSO_Covariates.R') # Obtain control_list
source('Scripts/General Scripts/ATEsGeneralSetUp LASSO.R') # Run ATE with LASSO covariates
panelC <- final_table

cat(main_panel(panelA, panelB, panelC, size, "\\\\shortstack\\{Increase",2.75, 1.5), file = "Tables/Appendix/Disaggregated/knowledge_identify.txt")

# knowledge to verify information ----------------------------------------------
aux <- c("index_know_verify", "know_to_verify_end", "forms_index_end", "know_fact_checkers_sum_end")

aux_data <- survey[aux]

dep_var <- c("\\shortstack{Increase \\\\ knowledge to \\\\ verify \\\\ information \\\\ index \\\\ of (1,1,1)}",
             "\\shortstack{How much \\\\knowledge do you \\\\have to verify \\\\if a doubtful news \\\\is false or not?}",
             "\\shortstack{Main ways \\\\to verify \\\\news \\\\index}", 
             "\\shortstack{How many\\\\ verifiers or\\\\ fact-checkers\\\\ do you know?}")

control <- c("education", "gender", "edad", "index_know_verify_baseline") # Knowledge to verify

lagged <- c("know_to_verify", "forms_index", "know_fact_checkers_sum")

control_list <- list(control, c(control, "know_to_verify"), c(control, "forms_index"), 
                     c(control, "know_fact_checkers_sum"))

omit_var <- c("Constant", "blockid1", control, lagged, interacted, controls)

title <- "Treatment effects on knowledge to verify information index's components"

# Create tables and save results
size = length(aux) + 1
cm = 10 # length in cm of the note for the table
label = "Dis6"

source('Scripts/General Scripts/ATEsGeneralSetUp.R')
panelA <- final_table

# IPSW
source('Scripts/General Scripts/PSMGeneralSetUp.R')
panelB <- final_table

# LASSO
lagged <- c("index_know_verify_baseline", lagged)
source('Scripts/General Scripts/LASSO_Covariates.R') # Obtain control_list
source('Scripts/General Scripts/ATEsGeneralSetUp LASSO.R') # Run ATE with LASSO covariates
panelC <- final_table

cat(main_panel(panelA, panelB, panelC, size, "\\\\shortstack\\{Increase", 2.75, NA), file = "Tables/Appendix/Disaggregated/knowledge_verify.txt")

# Consumption ------------------------------------------------------------------
aux <- c("consumption_traditional_end", paste0("freq_tipos_", c(1:3), "_end"),
         "consumption_social_end", paste0("freq_tipos_", c(4:6), "_end"))

aux_data <- survey[aux]

dep_var <- c("\\shortstack{Increase \\\\ consumption \\\\ behavior \\\\ of traditional \\\\ sources index \\\\ of (1,1,1)}",
             "\\shortstack{Radio / TV}",
             "\\shortstack{Newspaper}", 
             "\\shortstack{Internet}", 
             "\\shortstack{Decrease \\\\ consumption \\\\ behavior \\\\ of social media \\\\ sources index \\\\ of (-1,-1,-1)}",
             "\\shortstack{WhatsApp}",
             "\\shortstack{Social media}", 
             "\\shortstack{Conversations}")

control <- c("education", "gender", "edad", "consumption_traditional_baseline", "consumption_social_baseline",
             "freq_use_social", "freq_use_whats") 

lagged <- paste0("freq_tipos_", c(1:6))

control_list <- list(control, c(control, "freq_tipos_1"), c(control, "freq_tipos_2"), c(control, "freq_tipos_3"), control,
                     c(control, "freq_tipos_4"), c(control, "freq_tipos_5"), c(control, "freq_tipos_6"))

omit_var <- c("Constant", "blockid1", control, lagged, interacted, controls)

title <- "Treatment effects on consumption behavior index's components"

# Create tables and save results
size = length(aux) + 1
cm = 15 # length in cm of the note for the table
label = "Dis7"

source('Scripts/General Scripts/ATEsGeneralSetUp.R')
panelA <- final_table

# IPSW
source('Scripts/General Scripts/PSMGeneralSetUp.R')
panelB <- final_table

# LASSO
lagged <- c("consumption_traditional_baseline",  paste0("freq_tipos_", c(1:3)), 
            "consumption_social_baseline",  paste0("freq_tipos_", c(4:6)))

source('Scripts/General Scripts/LASSO_Covariates.R') # Obtain control_list
source('Scripts/General Scripts/ATEsGeneralSetUp LASSO.R') # Run ATE with LASSO covariates
panelC <- final_table

cat(main_panel(panelA, panelB, panelC, size, "\\\\shortstack\\{Increase",2.75, NA), file = "Tables/Appendix/Disaggregated/consumption.txt")

# Sharing Behavior -------------------------------------------------------------
aux <- c("index_sharing", "freq_desc_comp_end", "cree_falsa_do_index_end", paste0("case", c(1,3), "_hyp_1_end"))

aux_data <- survey[aux]

dep_var <- c("\\shortstack{Decrease \\\\ sharing \\\\ behavior index \\\\ of (1,-1,\\\\-1,-1)}",
             "\\shortstack{If you know a \\\\ story is false, \\\\ how often do you \\\\ share its falsehood \\\\ with others \\\\ on social media?}",
             "\\shortstack{How often \\\\ do you share \\\\ news you receive \\\\ on WhatsApp and \\\\ social media}", 
             "\\shortstack{Share \\\\scenario 1 \\\\withoout verification^{\\dagger}}",
             "\\shortstack{Share \\\\scenario 3 \\\\withoout verification^{\\dagger\\dagger}}")

control <- c("education", "gender", "edad", "index_sharing_baseline") # Sharing

lagged <- c("freq_desc_comp", paste0("case", c(1,3), "_hyp_1"))

control_list <- list(control, c(control, "freq_desc_comp"), control, c(control, "case1_hyp_1"), c(control, "case3_hyp_1"))

omit_var <- c("Constant", "blockid1", control, lagged, interacted, controls)

title <- "Treatment effects on sharing behavior index's components"

# Create tables and save results
size = length(aux) + 1
cm = 15 # length in cm of the note for the table
label = "Dis8"
additional_note <- "$\\dagger$: imagine you received an that came to you on WhatsApp with a screenshot of the Twitter account of a well-known person saying something very controversial. 
$\\dagger\\dagger$: imagine you received an audio that came to you by WhatsApp reporting evidence of corruption of a politician that you already suspected and you did not like. "

source('Scripts/General Scripts/ATEsGeneralSetUp.R')
panelA <- final_table

# IPSW
source('Scripts/General Scripts/PSMGeneralSetUp.R')
panelB <- final_table

# LASSO
lagged <- c("index_sharing_baseline", "freq_desc_comp", NA, paste0("case", c(1,3), "_hyp_1"))
source('Scripts/General Scripts/LASSO_Covariates.R') # Obtain control_list
source('Scripts/General Scripts/ATEsGeneralSetUp LASSO.R') # Run ATE with LASSO covariates
panelC <- final_table

cat(main_panel(panelA, panelB, panelC, size, "\\\\shortstack\\{Decrease",2.75, NA), file = "Tables/Appendix/Disaggregated/sharing.txt")

additional_note <- ""

# Verifying behavior -----------------------------------------------------------
aux <- c("index_verifying",  "freq_veri_compar_end")

aux_data <- survey[aux]

dep_var <- c("\\shortstack{Increase \\\\ verifying  \\\\ behavior \\\\  index \\\\ of (1)}",
             "\\shortstack{How often do \\\\ you verify news \\\\ that you doubt may \\\\ be false before \\\\sharing it?}")

control <- c("education", "gender", "edad", "ver_beh_baseline") # Verifying

lagged <- "freq_veri_compar"

control_list <- list(control, c(control))

omit_var <- c("Constant", "blockid1", control, lagged, interacted, controls)

title <- "Treatment effects on verifying behavior index's components"

# Create tables and save results
size = length(aux) + 1
cm = 8 # length in cm of the note for the table
label = "Dis9"

source('Scripts/General Scripts/ATEsGeneralSetUp.R')
panelA <- final_table

# IPSW
source('Scripts/General Scripts/PSMGeneralSetUp.R')
panelB <- final_table

# LASSO
lagged <- c("ver_beh_baseline", "freq_veri_compar")
source('Scripts/General Scripts/LASSO_Covariates.R') # Obtain control_list
source('Scripts/General Scripts/ATEsGeneralSetUp LASSO.R') # Run ATE with LASSO covariates
panelC <- final_table

cat(main_panel(panelA, panelB, panelC, size, "\\\\shortstack\\{Increase", 2.75, NA), file = "Tables/Appendix/Disaggregated/verifying.txt")

