# Balance Analysis Summary

# ------------------------------------------------------------------------------
# read data
survey <- read.csv('Datasets/Final Sets/survey_final_ICW.csv')

# Panel A ----------------------------------------------------------------------
# Auxiliary variables for stargazer
covariates <- c("Course and verification", "Course", "Verification")
interaction_name <- c("WhatsApp Delivery")

# Aux for regressions
treatments_separate <- c("curso_verificacion", "curso", "verificacion")
interacted <- c("WhatsApp")
covariates_interacted <- c(covariates, paste(covariates, interaction_name, sep = " x "))
# Additional note empty
additional_note <- ""

aux <- c("index_mec_importance_baseline",
         "false_baseline",
         "trust_traditional_baseline",
         "trust_social_baseline",
         "freq_analyze",
         "knowledge_identify_baseline",
         "index_know_verify_baseline",
         "consumption_traditional_baseline",
         "consumption_social_baseline",
         "index_sharing_baseline",
         "ver_beh_baseline")

aux_data <- survey[aux]

title <- "Balance summary on main indexes with and without IPSW"

omit_var <- c("Constant", "blockid1", interacted)

dep_var <- c("\\shortstack{Misinformation \\\\ importance  \\\\ index}",
             "\\shortstack{Increase \\\\ likelihood of \\\\ false of \\\\ social media \\\\sources \\\\index}", 
             "\\shortstack{Trust \\\\ traditional \\\\ sources \\\\ index}",
             "\\shortstack{Distrust \\\\ social media \\\\ sources \\\\ index}",
             "\\shortstack{Attention to \\\\ misinformation \\\\ index}",
             "\\shortstack{Increase \\\\ knowledge to \\\\ identify \\\\ information \\\\ index}",
             "\\shortstack{Increase \\\\ knowledge to \\\\ verify \\\\ information \\\\ index}",
             "\\shortstack{Increase \\\\ consumption \\\\ behavior \\\\ of traditional \\\\sources \\\\ index}",
             "\\shortstack{Decrease \\\\ consumption \\\\ behavior \\\\ of social media \\\\sources \\\\ index}",
             "\\shortstack{Decrease \\\\ sharing \\\\ behavior index}",
              "\\shortstack{Increase \\\\ verifying  \\\\ behavior \\\\  index}")

# Create tables and save results
size = length(aux) + 1
cm = 17 # length in cm of the note for the table
control <- NA
additional_note <- ""
label = "SummaryBalance"
source('Scripts/General Scripts/ATEsGeneralSetUpBalance.R')

panelA <- final_table

# Panel B ----------------------------------------------------------------------
# Read attrition data
survey_attrition <- read.csv('Datasets/Final Sets/survey_final_ICW_Attrition.csv')

# Demeaned variable
interaction_var <- survey_attrition$WhatsApp
interacted <- c("WhatsApp")

# Label for interacted variable
interaction_name <- c("WhatsApp Delivery")

# aux for regressions
treatments_separate <- c("curso_verificacion", "curso", "verificacion")
covariates_sep <- c("Course and verification", "Course", "Verification")

# Interacted labels 
covariates_interacted <- c(covariates_sep, paste(covariates_sep, interaction_name, sep = " x "))

aux_data <- survey_attrition[aux]


# Create tables and save results
size = length(aux) + 1
cm = 17 # length in cm of the note for the table
control <- NA
additional_note <- ""
source('Scripts/General Scripts/PSMGeneralSetUpAttrition.R')

panelB <- final_table

# Append tables ----------------------------------------------------------------

TableA <- paste(panelA, collapse = "")
TableB <- paste(panelB, collapse = "")

aux1 <- sub("\\\\\\[-1.8ex].*", 
            "\\hline \\\\hline \\\\\\\\[-0.5ex] \\\\\\\\[-0.5ex] \\\\\\\\[-0.5ex] \\\\multicolumn{%}{l}{\\\\textbf{Panel A: Balance (OLS) }} \\\\\\\\[-0.5ex] \\\\\\\\ \\\\hline \\\\\\\\[-1ex]", 
            TableA)

aux2 <- sub(paste0(".*\\]  & ",  "\\\\shortstack\\{"), paste0("& ", "\\\\shortstack\\{"), TableA) 

aux2 <- sub("\\\\hline \\\\\\\\\\[-1.8ex] \\\\multicolumn\\{.*", paste0("\\\\\\\\[-0.5ex] \\\\\\\\[-0.5ex] \\\\\\\\[-0.5ex] \\\\multicolumn{%}{l}{\\\\textbf{Panel B: Balance (IPSW)}} \\\\\\\\[-0.5ex] \\\\\\\\ \\\\hline ",
                                                                        "\\\\\\\\[-0.5ex] \\\\multicolumn{%}{l}{\\\\textbf{Panel B.1: Sample course Delivery Online}} \\\\\\\\[-1.8ex] \\\\\\\\ \\\\cline{1-2} \\\\\\\\[-0.5ex]"), aux2)

modified_tableA <- paste(aux1, aux2, collapse = "")

# data for panel B 
aux1_B <- sub(".*?Course ", "Course ", TableB) 

# Merge A and B
merged_AB <- paste(modified_tableA, aux1_B, collapse = "")

merged_AB <- gsub("%", 12, merged_AB)

# Save table
cat(merged_AB, file = "Tables/Appendix/Balance/SummaryBalance.txt")

