# Balance Analysis desegregated

# ------------------------------------------------------------------------------
# read data
survey <- read.csv('Datasets/Final Sets/survey_final_ICW.csv')
# ------------------------------------------------------------------------------
# Auxiliary variables for stargazer
covariates <- c("Course and verification", "Course", "Verification")
interaction_name <- c("WhatsApp Delivery")

# Aux for regressions
treatments_separate <- c("curso_verificacion", "curso", "verificacion")
interacted <- c("WhatsApp")
covariates_interacted <- c(covariates, paste(covariates, interaction_name, sep = " x "))
# Additional note empty
additional_note <- ""

# Demographics -----------------------------------------------------------------
aux <- c("edad", "gender", "education", "journalist")
aux_data <- survey[aux]
dep_var <- c("Age", "Gender (female)", "\\shortstack{Level of \\\\education}",
              "Journalist")

title <- "Balance on demographic variables"
omit_var <- c("Constant", "blockid1", interacted)

# Create tables and save results
size = length(aux) + 1
cm = 9 # length in cm of the note for the table
label = "B1"
source('Scripts/General Scripts/ATEsGeneralSetUpBalance.R')

cat(final_table, file = "Tables/Appendix/Balance/TableB1.txt")

# Mechanisms: importance and falsehood -----------------------------------------
aux <- c("index_mec_importance_baseline", paste0("problemas_", 1:6), "false_baseline", paste0("case", c(1,3), "_hyp_2"))

aux_data <- survey[aux]

dep_var <- c("\\shortstack{Misinformation \\\\ importance  \\\\ index \\\\ of (1,1,1,1 \\\\ 1,1,1)}",
             "\\shortstack{Decisions that \\\\may affect \\\\health}",
             "\\shortstack{Election of \\\\candidates who \\\\do not represent \\\\the interests of \\\\citizens}", 
             "\\shortstack{To falsely \\\\discredit or \\\\glorify \\\\people}", 
             "\\shortstack{Ideological \\\\polarization}",
             "\\shortstack{Violence \\\\towards certain\\\\people \\\\or groups in \\\\society}", 
             "\\shortstack{Increase\\\\ hatred towards \\\\certain people \\\\or groups in \\\\society}",
             "\\shortstack{Increase \\\\ likelihood of \\\\ false of \\\\ social media \\\\sources \\\\index \\\\ of (1,1)}",
             "\\shortstack{Believe \\\\scenario 1 \\\\is fake^{\\dagger}}",
             "\\shortstack{Believe \\\\scenario 3 \\\\is fake^{\\dagger\\dagger}}")

title <- "Balance on mechanisms: Misinformation relevance and falsehood"

# Create tables and save results
size = length(aux) + 1
cm = 19 # length in cm of the note for the table
label = "B2"
additional_note <- "$\\dagger$: imagine you received an that came to you on WhatsApp with a screenshot of the Twitter account of a well-known person saying something very controversial. 
$\\dagger\\dagger$: imagine you received an audio that came to you by WhatsApp reporting evidence of corruption of a politician that you already suspected and you did not like. "

source('Scripts/General Scripts/ATEsGeneralSetUpBalance.R')

cat(left_align(final_table, 1), file = "Tables/Appendix/Balance/TableB2.txt")

# Empty additional note
additional_note <- ""

# Mechanisms: Trust --------------------------------------------------------
aux <- c("trust_traditional_baseline", paste0("confia_", c(1:3)), 
         "trust_social_baseline",  paste0("confia_", c(4:6)), 
         "index_mec_attention", "freq_analyze_end")

aux_data <- survey[aux]

dep_var <- c("\\shortstack{Trust \\\\ traditional \\\\ sources \\\\ index \\\\ of (1,1,1)}",
             "\\shortstack{Radio / TV}",
             "\\shortstack{Newspaper}", 
             "\\shortstack{Internet}", 
             "\\shortstack{Distrust \\\\ social media \\\\ sources \\\\ index \\\\ of (-1,-1,-1)}",
             "\\shortstack{WhatsApp}",
             "\\shortstack{Social media}", 
             "\\shortstack{Conversations}",
             "\\shortstack{Attention to \\\\ misinformation \\\\ index \\\\ of (1)}",
             "\\shortstack{How often \\\\do you question\\\\ wether a\\\\ news is false}")

title <- "Balance on mechanisms: Trust and attention"

# Create tables and save results
size = length(aux) + 1
cm = 18 # length in cm of the note for the table
label = "B3"
source('Scripts/General Scripts/ATEsGeneralSetUpBalance.R')

cat(left_align(final_table, 1), file = "Tables/Appendix/Balance/TableB3.txt")

# knowledge to identify information --------------------------------------------
aux <- c("knowledge_identify_baseline", c("knowledge_verify", "characts_index",
                                  paste0("knowledge_plat_", c(1:3, 4:6))))

aux_data <- survey[aux]

dep_var <- c("\\shortstack{Increase \\\\ knowledge to \\\\ identify \\\\ information \\\\ index \\\\ of (1,1,1,1, \\\\ 1,1,-1,-1,-1)}",
             "\\shortstack{How much \\\\knowledge do you \\\\have to identify \\\\whether a news is \\\\false or not?}",
             "\\shortstack{Knowing how\\\\ to identify \\\\a fake new \\\\index}", 
             "\\shortstack{Radio / TV}",
             "\\shortstack{Newspaper}", 
             "\\shortstack{Internet}", 
             "\\shortstack{WhatsApp}",
             "\\shortstack{Social media}", 
             "\\shortstack{Conversations}")

title <- "Balance on knowledge to identify information"

# Create tables and save results
size = length(aux) + 1
cm = 16.5 # length in cm of the note for the table
label = "B4"
source('Scripts/General Scripts/ATEsGeneralSetUpBalance.R')

cat(final_table, file = "Tables/Appendix/Balance/TableB4.txt")

# knowledge to verify information and consumption behavior ---------------------
aux <- c("index_know_verify_baseline", "know_to_verify", "forms_index", "know_fact_checkers_sum",
         "consumption_baseline", paste0("freq_tipos_", c(1:3, 4:6)))

aux_data <- survey[aux]

dep_var <- c("\\shortstack{Increase \\\\ knowledge to \\\\ verify \\\\ information \\\\ index \\\\ of (1,1,1)}",
             "\\shortstack{How much \\\\knowledge do you \\\\have to verify \\\\if a doubtful news \\\\is false or not?}",
             "\\shortstack{Main ways \\\\to verify \\\\news \\\\index}", 
             "\\shortstack{How many\\\\ verifiers or\\\\ fact-checkers\\\\ do you know?}",
             "\\shortstack{Increase \\\\ consumption \\\\ behavior \\\\ of traditional \\\\sources \\\\ index \\\\ of (1,1,1)}",
             "\\shortstack{Radio / TV}",
             "\\shortstack{Newspaper}", 
             "\\shortstack{Internet}", 
             "\\shortstack{Decrease \\\\ consumption \\\\ behavior \\\\ of social media \\\\sources \\\\ index \\\\ of (-1,-1,-1)}",
             "\\shortstack{WhatsApp}",
             "\\shortstack{Social media}", 
             "\\shortstack{Conversations}")

title <- "Balance on knowledge to verify information and consumption behavior"

# Create tables and save results
size = length(aux) + 1
cm = 19 # length in cm of the note for the table
label = "B5"
source('Scripts/General Scripts/ATEsGeneralSetUpBalance.R')

cat(left_align(final_table, 1.75), file = "Tables/Appendix/Balance/TableB5.txt")

# Sharing Behavior -------------------------------------------------------------
aux <- c("index_sharing_baseline", "freq_desc_comp", paste0("case", c(1,3), "_hyp_1"),
         "index_verifying",  "freq_veri_compar_end")
aux_data <- survey[aux]

dep_var <- c("\\shortstack{Decrease \\\\ sharing \\\\ behavior index \\\\ of (1,-1,\\\\-1,-1)}",
             "\\shortstack{If you know a \\\\ story is false, \\\\ how often do you \\\\ share its falsehood \\\\ with others \\\\ on social media?}",
             "\\shortstack{Share \\\\scenario 1 \\\\withoout verification^{\\dagger}}",
             "\\shortstack{Share \\\\scenario 3 \\\\withoout verification^{\\dagger\\dagger}}",
             "\\shortstack{Increase \\\\ verifying  \\\\ behavior \\\\  index \\\\ of (1)}",
             "\\shortstack{How often do \\\\ you verify news \\\\ that you doubt may \\\\ be false before \\\\sharing it?}")

title <- "Balance on sharing and verifying behavior"

# Create tables and save results
size = length(aux) + 1
cm = 15.5 # length in cm of the note for the table
additional_note <- "$\\dagger$: imagine you received an that came to you on WhatsApp with a screenshot of the Twitter account of a well-known person saying something very controversial. 
$\\dagger\\dagger$: imagine you received an audio that came to you by WhatsApp reporting evidence of corruption of a politician that you already suspected and you did not like. "
label = "B6"
source('Scripts/General Scripts/ATEsGeneralSetUpBalance.R')

cat(final_table, file = "Tables/Appendix/Balance/TableB6.txt")

additional_note <- ""
