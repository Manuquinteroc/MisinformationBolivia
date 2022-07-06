# Differential Attrition

# ------------------------------------------------------------------------------
# read data
survey <- read.csv('Datasets/Final Sets/survey_final_ICW_Attrition.csv')

# Demeaned variable
interaction_var <- survey$WhatsApp
interacted <- c("WhatsApp")

# Label for interacted variable
interaction_name <- c("WhatsApp Delivery")

# aux for regressions
treatments_separate <- c("curso_verificacion", "curso", "verificacion")
covariates_sep <- c("Course and verification", "Course", "Verification")

covariates_pool <- c("All treatments")

# Interacted labels 
covariates_interacted <- c(covariates_sep, paste(covariates_sep, interaction_name, sep = " x "))

omit_var <- c("Constant", "blockid1", interacted)

# Demographics ----------------------------------------------------------------
aux <- c("complete_both")
aux_data <- survey[aux]
dep_var <- c("Differential Attrition")
title <- "Differential attrition"

# Create tables and save results
size = length(aux) + 1
cm = 8 # length in cm of the note for the table
interacted_bool <- c(T)
control <- NA
additional_note <- ""

source('Scripts/General Scripts/ATEsGeneralSetUpOnlyAttrition.R')
panelA <- final_table
 
# ------------------------------------------------------------------------------
# Attrition post-PSM
# read data
survey_attrition <- read.csv('Datasets/Final Sets/survey_final_ICW_Attrition.csv')

aux <- c("complete_both")
aux_data <- survey_attrition[aux]
dep_var <- c("Differential Attrition")
title <- "Difference in attrition"

# Create tables and save results
size = length(aux) + 1
cm = 8 # length in cm of the note for the table
interacted_bool <- c(T)
control <- NA
additional_note <- ""

source('Scripts/General Scripts/PSMGeneralSetUpAttrition.R')

panelB <- final_table

# ------------------------------------------------------------------------------
# Append tables 

TableA <- paste(panelA, collapse = "")
TableB <- paste(panelB, collapse = "")

aux1 <- sub("\\\\\\[-1.8ex].*", 
            "\\hline \\\\hline \\\\\\\\[-0.5ex] \\\\\\\\[-0.5ex] \\\\\\\\[-0.5ex] \\\\multicolumn{%}{l}{\\\\textbf{Panel A: Differential Attrition (OLS) }} \\\\\\\\[-0.5ex] \\\\\\\\ \\\\hline \\\\\\\\[-1ex]", 
            TableA)

aux2 <- sub(paste0(".*\\]  & ",  "Differential"), paste0("& ", "Differential"), TableA) 

aux2 <- sub("\\\\hline \\\\\\\\\\[-1.8ex] \\\\multicolumn\\{.*", paste0("\\\\\\\\[-0.5ex] \\\\\\\\[-0.5ex] \\\\\\\\[-0.5ex] \\\\multicolumn{%}{l}{\\\\textbf{Panel B: Differential Attrition (IPSW)}} \\\\\\\\[-0.5ex] \\\\\\\\ \\\\hline ",
                                                                "\\\\\\\\[-0.5ex] \\\\multicolumn{%}{l}{\\\\textbf{Panel B.1: Sample course Delivery Online}} \\\\\\\\[-1.8ex] \\\\\\\\ \\\\cline{1-2} \\\\\\\\[-0.5ex]"), aux2)

modified_tableA <- paste(aux1, aux2, collapse = "")

# data for panel B 
aux1_B <- sub(".*?Course ", "Course ", TableB) 

# Merge A and B
merged_AB <- paste(modified_tableA, aux1_B, collapse = "")

merged_AB <- gsub("%", 2, merged_AB)

# Add sub-caption to course panel
aux1 <- sub("\\\\\\\\  Course?.*", "\\\\\ \\\\hline \\\\\\\\", merged_AB)
subcaption <- paste0("\\\\[-0.5ex] \\multicolumn{", 2, "}{l}{\\textbf{A.1 Sample course Delivery Online }} \\\\[-1.8ex] \\\\ \\cline{1-2} \\\\[-0.5ex]")
aux2 <- sub(".*?Course", "Course", merged_AB)

final_table <- paste0( aux1, subcaption, aux2, sep = "")

# Save Attrition table
cat(final_table, file = "Tables/Appendix/Table_Attrition.txt")
