# Fixes for all datasets

# Recode the direction of confia variables for WhatsApp survey
mapping <- c("1" = 5, "2" = 4, "3" = 3,
             "4" = 2, "5" = 1)

survey_wa$cree_fuentes_1_end <- as.numeric(plyr::revalue(as.character(survey_wa$cree_fuentes_1_end), mapping))
survey_wa$cree_fuentes_2_end <- as.numeric(plyr::revalue(as.character(survey_wa$cree_fuentes_2_end), mapping))
survey_wa$cree_fuentes_3_end <- as.numeric(plyr::revalue(as.character(survey_wa$cree_fuentes_3_end), mapping))
survey_wa$cree_fuentes_4_end <- as.numeric(plyr::revalue(as.character(survey_wa$cree_fuentes_4_end), mapping))
survey_wa$cree_fuentes_5_end <- as.numeric(plyr::revalue(as.character(survey_wa$cree_fuentes_5_end), mapping))
survey_wa$cree_fuentes_6_end <- as.numeric(plyr::revalue(as.character(survey_wa$cree_fuentes_6_end), mapping))
survey_wa$cree_fuentes_7_end <- as.numeric(plyr::revalue(as.character(survey_wa$cree_fuentes_7_end), mapping))
survey_wa$cree_fuentes_8_end <- as.numeric(plyr::revalue(as.character(survey_wa$cree_fuentes_8_end), mapping))
survey_wa$cree_fuentes_9_end <- as.numeric(plyr::revalue(as.character(survey_wa$cree_fuentes_9_end), mapping))

#survey_wa[,paste0("cree_fuentes_", c(1:9), "_end")] <- -1*survey_wa[,paste0("cree_fuentes_", c(1:9), "_end")]

# Rename confia_ to freq_plat_false_
names(survey_wa)[which(names(survey_wa) %in% paste0("cree_fuentes_", 1:6))] <- paste0("freq_plat_false_", 1:6)
names(survey_wa)[which(names(survey_wa) %in% paste0("cree_fuentes_", 1:9, "_end"))] <- paste0("freq_plat_false_", 1:9, "_end")

# Fix traditional and Non-traditional variables --------------------------------
# Category 7 and 8 from WhatsApp go with category 4 from Alfabetizacion
survey_wa$confia_4_end <- rowMeans(survey_wa[c("confia_7_end","confia_8_end")])
survey_wa$freq_tipos_4_end <- rowMeans(survey_wa[c("freq_tipos_7_end","freq_tipos_8_end")])
survey_wa$freq_plat_false_4_end <- rowMeans(survey_wa[c("freq_plat_false_7_end","freq_plat_false_8_end")])

survey_wa$knowledge_plat_4_end <- rowMeans(survey_wa[c("knowledge_plat_9_end","knowledge_plat_10_end")])

# Fixing sharing variables -----------------------------------------------------
#survey_wa$freq_comp_social_end <- -1*survey_wa$freq_comp_social_end # change sign
mapping <- c("1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
survey_wa$freq_comp_social_end <- as.numeric(plyr::revalue(as.character(survey_wa$freq_comp_social_end), mapping))

# Standardize sharing variables from both surveys to append (alf: cree_falsa_do_index_end, already standardize)
survey_wa$freq_comp_social_end <- scale(survey_wa$freq_comp_social_end)

# Change names to append
survey_wa <- survey_wa %>% dplyr::select(-c("freq_desc_comp_end", "freq_veri_compar_end"))
names(survey_wa)[which(names(survey_wa) == "freq_alert_end_merged")] <- "freq_desc_comp_end"
names(survey_wa)[which(names(survey_wa) == "freq_comp_social_end")] <- "cree_falsa_do_index_end"

names(survey_wa)[which(names(survey_wa) == "freq_veri_compar")] <- "freq_veri_compar"
names(survey_wa)[which(names(survey_wa) == "freq_veri_end")] <- "freq_veri_compar_end"


# select variables for each survey 
# create WhatsApp indicator 
survey_alf$WhatsApp <- 0
survey_wa$WhatsApp <- 1

# variables of interest in WhatsApp survey
wa_vars <- c("WhatsApp", "control", "curso_verificacion", "curso", "verificacion", "curso_pooled", "verificacion_pooled", "blockid4",
             "edad", "gender", "education", "journalist",
             "temas_2", 
             "knowledge_verify", 
             "know_to_verify", "know_fact_checkers_sum",
             "freq_use_whats", "freq_use_social",
             "freq_desc_comp",  "freq_analyze", "freq_veri_compar",
             "temas_2_end", "freq_plat_false_9_end", 
             "confia_9_end", "avoid_mis_1_end", "avoid_mis_2_end",
             "knowledge_verify_end", "knowledge_plat_11_end", 
             "recent_cases_sum_end",
             "know_to_verify_end", "avoid_mis_4_end", "know_fact_checkers_sum_end",
             "freq_tipos_9_end",
             "cree_falsa_do_index_end", "freq_desc_comp_end",
             "avoid_mis_3_end", "freq_analyze_end", "freq_veri_compar_end")

# Vars for indexes
wa_indexes <- c(paste0("freq_tipos_", 1:6), paste0("confia_", 1:6), paste0("freq_plat_false_", 1:6), paste0("knowledge_plat_", 1:6),
                paste0("problemas_", 1:6), paste0("characts_false_", 1:10), paste0("forms_verify_", 1:6),
                paste0("case", 1:3, "_hyp_2"), paste0("case", 1:3, "_hyp_1"),
                paste0("freq_tipos_", 1:8, "_end"), paste0("confia_", 1:8, "_end"), paste0("freq_plat_false_", 1:8, "_end"), 
                paste0("knowledge_plat_", c(1:4, c(5:6,9:10)), "_end"), paste0("problemas_", 1:6, "_end"),
                paste0("characts_false_", 1:10, "_end"), paste0("forms_verify_", 1:6, "_end"), 
                paste0("case", 1:3, "_hyp_2_end"), paste0("case", 1:3, "_hyp_1_end"))

survey_wa <- survey_wa %>% dplyr::select(all_of(c(wa_vars, wa_indexes,"complete_both"))) %>% dplyr::rename(blockid1 = blockid4)

# Create separate blocks
survey_wa$blockid1 <- paste0(survey_wa$blockid1, "_w")

# variables of interest in Alfabetizacion survey
alf_vars <- c("WhatsApp", "control", "curso_verificacion", "curso", "verificacion", "curso_pooled", "verificacion_pooled", "blockid1",
              "edad", "gender", "education", "journalist",
              "knowledge_verify", 
              "know_to_verify", "know_fact_checkers_sum", 
              "freq_use_whats", "freq_use_social", "fuentes_principales_sum","cuentas_social_infor_sum_1", "cuentas_social_infor_sum_2", "cuentas_social_infor_sum_3",
              "freq_desc_comp", "freq_analyze", "freq_veri_compar",
              "knowledge_verify_end", "recent_cases_sum_end",
              "know_to_verify_end", "know_fact_checkers_sum_end",
              "fuentes_principales_sum_end", "cuentas_social_infor_Nontrad_end", "cuentas_social_infor_trad_end",
              "freq_desc_comp_end", "cree_falsa_do_1_end", "cree_falsa_do_3_end", "cree_falsa_do_4_end",
              "freq_analyze_end", "freq_veri_compar_end", "cree_falsa_do_index_end",
              "cuentas_social_infor_Nontrad", "cuentas_social_infor_trad")

alf_indexes <- c(paste0("freq_tipos_", 1:6), paste0("confia_", 1:6), paste0("knowledge_plat_", 1:6), paste0("problemas_", 1:6),
                 paste0("characts_false_", 1:10), paste0("forms_verify_", 1:6), paste0("case", 1:3, "_hyp_2"), paste0("case", 1:3, "_hyp_1"), 
                 paste0("freq_tipos_", 1:6, "_end"), paste0("confia_", 1:6, "_end"),
                 paste0("freq_plat_false_", 1:6, "_end"), paste0("knowledge_plat_", 1:6, "_end"), paste0("problemas_", 1:6, "_end"),
                 paste0("characts_false_", 1:10, "_end"), paste0("forms_verify_", 1:6, "_end"), 
                 paste0("case", 1:3, "_hyp_2_end"), paste0("case", 1:3, "_hyp_1_end"), paste0("case", 1:3, "_hyp_3_end"))

survey_alf <- survey_alf %>% dplyr::select(all_of(c(alf_vars, alf_indexes,"complete_both"))) 

# Create separate blocks
survey_alf$blockid1 <- paste0(survey_alf$blockid1, "_a")

# Append dataframe
names(survey_wa)[which((names(survey_wa) %in% names(survey_alf) == F))]
final <- bind_rows(survey_alf, survey_wa)