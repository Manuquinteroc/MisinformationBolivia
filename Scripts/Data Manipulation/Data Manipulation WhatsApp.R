# Data wrangling for the WhatsApp survey responses 

# ------------------------------------------------------------------------------
# read data frame
survey <- read_dta('Datasets/WhatsApp/final_data.dta') 

# Attrition --------------------------------------------------------------------
baseline_aux <- survey %>% filter(baseline_complete == 1)
endline_aux <- survey %>% filter(endline_complete == 1)
names(endline_aux) <- paste0(names(endline_aux), "_end")

survey_attrition <- left_join(baseline_aux, endline_aux, by = c("id" = "id_end"))

survey_attrition <- survey_attrition %>% filter(baseline_complete == 1)

survey_attrition$complete_both <- ifelse(!is.na(survey_attrition$treatment_end), 1, 0)

survey_attrition <- cbind(survey_attrition, fastDummies::dummy_cols(survey_attrition$treatment,  remove_selected_columns = T))
names(survey_attrition)[(length(survey_attrition)- 3):length(survey_attrition)] <- c("control", "curso_verificacion", "curso", "verificacion")

survey_attrition$curso_pooled <- survey_attrition$curso + survey_attrition$curso_verificacion
survey_attrition$verificacion_pooled <- survey_attrition$verificacion + survey_attrition$curso_verificacion

write.csv(survey_attrition, 'Datasets/WhatsApp/DataForAttrition.csv', row.names = F)

# ------------------------------------------------------------------------------
# Use only those observations that completed endline and have baseline
survey <- survey %>% filter(is.na(missing_baseline) & is.na(missing_endline))

# change format of dataframe from long to wide
baseline <- subset(survey, time == 1)

# names(baseline) <- paste0(names(baseline), "_base")
endline <- subset(survey, time == 2)
names(endline) <- paste0(names(endline), "_end")

survey <- inner_join(baseline, endline, by = c("id" = "id_end"))

# create treatment indicators
survey <- cbind(survey, fastDummies::dummy_cols(survey$treatment,  remove_selected_columns = T))
names(survey)[(length(survey)- 3):length(survey)] <- c("control", "curso_verificacion", "curso", "verificacion")

# Further treatments indicators
survey$curso_pooled <- survey$curso + survey$curso_verificacion
survey$verificacion_pooled <- survey$verificacion + survey$curso_verificacion

# Drop repeated variables and change names for base ----------------------------

# Auxiliary funciton for indexes -----------------------------------------------
indexes <- function(vars) {
  zscore <- scale(rowMeans(scale(survey[vars]) %*% diag(rep(1, length(vars)))))
  
  return (zscore)
}

# variables manipulation -------------------------------------------------------
# recode gender so that female = 1
survey <- survey %>% mutate(gender = ifelse(gender == 2, 1, 0))

# journalist NA as No 
survey <- survey %>% mutate(journalist  = ifelse(journalist  == 1, 1, 0))
survey$journalist[is.na(survey$journalist)] <- 0

# subjects variable (Temas)

# Export text variables
cbind.data.frame(which(survey$temas_9_text != ""), survey$temas_9_text[which(survey$temas_9_text != "")], survey$temas[which(survey$temas_9_text != "")])

# Create dummies for each response of "temas"
test <- strsplit(as.character(survey$temas), ",")
lev <- unique(unlist(test))
dummy_vars <- lapply(test, function(x) table(factor(x, levels=lev)))
dummies <- as.data.frame(do.call(rbind, dummy_vars))
names(dummies) <- paste0("temas_", names(dummies))
survey <- cbind.data.frame(survey, dummies)

# freq_analyze and knowledge_verify 
survey$freq_analyze[which(survey$freq_analyze == 6)] <- 3 # 1 obs
survey$knowledge_verify[which(survey$knowledge_verify == 6)] <- 3 # 2 obs

survey$freq_veri_compar[which(survey$freq_veri_compar == 6)] <- 3 
survey$know_to_verify[which(survey$know_to_verify == 6)] <- 3 

survey$freq_desc_comp[which(survey$freq_desc_comp == 6)] <- 3 

# hypothetical scenarios variables
survey$case1_hyp_1[which(survey$case1_hyp_1 == 6)] <- 3 # only 1 obs
survey$case1_hyp_2[which(survey$case1_hyp_2 == 6)] <- 3 # 0
survey$case2_hyp_1[which(survey$case2_hyp_1 == 6)] <- 3 # only 1 obs
survey$case2_hyp_2[which(survey$case2_hyp_2 == 6)] <- 3 # only 1 obs
survey$case3_hyp_1[which(survey$case3_hyp_1 == 6)] <- 3 # 0
survey$case3_hyp_2[which(survey$case3_hyp_2 == 6)] <- 3 # 0

# Create dummies for each response of "characts_false"
test <- strsplit(as.character(survey$characts_false), ",")
lev <- unique(unlist(test))
dummy_vars <- lapply(test, function(x) table(factor(x, levels=lev)))
dummies <- as.data.frame(do.call(rbind, dummy_vars))
names(dummies) <- paste0("characts_false_", names(dummies))
survey <- cbind.data.frame(survey, dummies)

# Create dummies for each response of "knowledge_plat"
test <- strsplit(as.character(survey$knowledge_plat), ",")
lev <- unique(unlist(test))
dummy_vars <- lapply(test, function(x) table(factor(x, levels=lev)))
dummies <- as.data.frame(do.call(rbind, dummy_vars))
names(dummies) <- paste0("knowledge_plat_", names(dummies))
survey <- cbind.data.frame(survey, dummies)

# Create dummies for each response of "forms_verify"
test <- strsplit(as.character(survey$forms_verify), ",")
lev <- unique(unlist(test))
dummy_vars <- lapply(test, function(x) table(factor(x, levels=lev)))
dummies <- as.data.frame(do.call(rbind, dummy_vars))
names(dummies) <- paste0("forms_verify_", names(dummies))
survey <- cbind.data.frame(survey, dummies)

# Code know_fact_checkers_1
survey$know_fact_checkers_1 <- tolower(stri_trans_general(survey$know_fact_checkers_1,"Latin-ASCII"))
survey$know_fact_checkers_2 <- tolower(stri_trans_general(survey$know_fact_checkers_2,"Latin-ASCII"))
survey$know_fact_checkers_3 <- tolower(stri_trans_general(survey$know_fact_checkers_3,"Latin-ASCII"))

survey$know_fact_checkers_1[which(survey$know_fact_checkers_1 == "")] <- "no se"
survey$know_fact_checkers_2[which(survey$know_fact_checkers_2 == "")] <- "no se"
survey$know_fact_checkers_3[which(survey$know_fact_checkers_3 == "")] <- "no se"

sum1 <- ifelse(grepl("ningu|no se|nose" , survey$know_fact_checkers_1),0,1)
sum2 <- ifelse(grepl("ningu|no se|nose" , survey$know_fact_checkers_2),0,1)
sum3 <- ifelse(grepl("ningu|no se|nose" , survey$know_fact_checkers_3),0,1)

survey$know_fact_checkers_sum <- sum1 + sum2 + sum3

# Code bolivian problems variable 
survey$problemas_1[which(survey$problemas_1 == 6)] <- 3 # 0 non responses
survey$problemas_2[which(survey$problemas_2 == 6)] <- 3 # 2 non responses
survey$problemas_3[which(survey$problemas_3 == 6)] <- 3# 2 non responses
survey$problemas_4[which(survey$problemas_4 == 6)] <- 3 # 3 non responses
survey$problemas_5[which(survey$problemas_5 == 6)] <- 3 # 3 non responses
survey$problemas_6[which(survey$problemas_6 == 6)] <- 3 # 3 non responses

# Indexes
# Consumption of news from different sources 
survey$freq_Ver_index <- indexes(paste0("freq_tipos_", 1:3))
survey$freq_NonVer_index  <- indexes(paste0("freq_tipos_", 4:6))

# Trust news from different sources 
survey$confia_Ver_index  <- indexes(paste0("confia_", 1:3))
survey$confia_NonVer_index  <- indexes(paste0("confia_", 4:6))

# Believe news from different sources 
survey$cree_Ver_index  <- indexes(paste0("cree_fuentes_", 1:3))
survey$cree_NonVer_index  <- indexes(paste0("cree_fuentes_", 4:6))

# Main means to spread fake news
survey$knowledge_Ver_index <- indexes(paste0("knowledge_plat_", 1:3))
survey$knowledge_NonVer_index <- indexes(paste0("knowledge_plat_", 4:6))

# Bolivian problems 
survey$problems_index <- indexes(paste0("problemas_", 1:6))

# Main characteristics to identify a fake news
right <- rowSums(survey[paste0("characts_false_", c(1,4,5,7,9))])/5
wrong <- rowSums(survey[paste0("characts_false_", c(2,3,6,8,10))])/5
survey$characts_index <- right - wrong

# Main ways to verify the veracity of news
right <- rowSums(survey[paste0("forms_verify_", c(1,4,5))])/3
wrong <- rowSums(survey[paste0("forms_verify_", c(2,3,6))])/3
survey$forms_index <- right - wrong

# index of likelihood of false for hypothetical scenarios
survey$hyp_false_index <- indexes(paste0("case", 1:3, "_hyp_2"))

# index of Sharing behavior for hypothetical scenarios
survey$hyp_sharing_index <- indexes(paste0("case", 1:3, "_hyp_1"))

#-------------------------------------------------------------------------------
# Endline manipulation ---------------------------------------------------------
# see other responses and recode them here (only 9 responses)
survey$tipo_info_chequea_5_text_end[which(grepl("5", survey$tipo_info_chequea_end))]
survey$tipo_info_chequea_end[which(grepl("5", survey$tipo_info_chequea_end))] <- 
  c("3", "2,4", "2", NA, "2,3", "2", NA, "3,4", "2,6")

# Create dummies for each response of "tipo_info_chequea_end"
test <- strsplit(as.character(survey$tipo_info_chequea_end), ",")
lev <- unique(unlist(test))
dummy_vars <- lapply(test, function(x) table(factor(x, levels=lev)))
dummies <- as.data.frame(do.call(rbind, dummy_vars))
names(dummies) <- paste0("tipo_info_chequea_", names(dummies), "_end")
survey <- cbind.data.frame(survey, dummies)

# Non-responses (only 2: category 6) are considered as 0; as if they did not receive any information

survey$curso_desinfo_freq_end[is.na(survey$curso_desinfo_freq_end)] <- 0 # NA values as 0

# Create dummies for each response of "curso_desinfo_cont_end"
test <- strsplit(as.character(survey$curso_desinfo_cont_end), ",")
lev <- unique(unlist(test))
dummy_vars <- lapply(test, function(x) table(factor(x, levels=lev)))
dummies <- as.data.frame(do.call(rbind, dummy_vars))
names(dummies) <- paste0("curso_desinfo_cont_", names(dummies), "_end")
survey <- cbind.data.frame(survey, dummies)

# Non responses treated as 0, they do not know the content of information received.
survey$ver_recib_freq_end[is.na(survey$ver_recib_freq_end)] <- 0 # NA values as 0
survey$ver_recib_freq_end[which(survey$ver_recib_freq_end == 7)] # 0 I dont know responses
survey$ver_freq_end[which(survey$ver_freq_end == 6)] # 0 I dont know responses

survey$ver_freq_end[is.na(survey$ver_freq_end)] <- 0 # NA values as 0


# Coding variable Temas (only one person said none (10), but selected other responses, no one selected only 9 = other)

# Create dummies for each response of "temas_end"
test <- strsplit(as.character(survey$temas_end), ",")
lev <- unique(unlist(test))
dummy_vars <- lapply(test, function(x) table(factor(x, levels=lev)))
dummies <- as.data.frame(do.call(rbind, dummy_vars))
names(dummies) <- paste0("temas_", names(dummies), "_end")
survey <- cbind.data.frame(survey, dummies)

# code bolivian problems variable 
survey$problemas_1_end[which(survey$problemas_1_end == 6)] <- 3 # 1 non responses
survey$problemas_2_end[which(survey$problemas_2_end == 6)] <- 3 # 0 non responses
survey$problemas_3_end[which(survey$problemas_3_end == 6)] <- 3# 0 non responses
survey$problemas_4_end[which(survey$problemas_4_end == 6)] <- 3 # 2 non responses
survey$problemas_5_end[which(survey$problemas_5_end == 6)] <- 3 # 1 non responses
survey$problemas_6_end[which(survey$problemas_6_end == 6)] <- 3 # 2 non responses

# WhatsApp and social media use
survey <- survey %>% mutate(freq_comp_social_end = dplyr::recode(freq_comp_social_end, 
                                                                 `1`= 1,
                                                                 `4`= 2,
                                                                 `5`= 3,
                                                                 `6`= 4,
                                                                 `7`= 5))
survey$freq_veri_end[which(survey$freq_veri_end == 6)] <- 3 # 1 non responses
survey$freq_alert_end[which(survey$freq_alert_end == 6)] <- 3 # 0 non responses
survey$freq_alert_comp_end[which(survey$freq_alert_comp_end == 6)] <- 3 # 1 non responses 

# coalesce freq_alert_comp_end and freq_alert_pot_end
survey <- survey %>% mutate(freq_alert_end_merged = coalesce(freq_alert_comp_end, freq_alert_pot_end))

# Create dummies for each response of "avoid_mis_end"
test <- strsplit(as.character(survey$avoid_mis_end), ",")
lev <- unique(unlist(test))
dummy_vars <- lapply(test, function(x) table(factor(x, levels=lev)))
dummies <- as.data.frame(do.call(rbind, dummy_vars))
names(dummies) <- paste0("avoid_mis_", names(dummies), "_end")
survey <- cbind.data.frame(survey, dummies)

# recode 8 other responses (All non responses have another answers besides 6)
survey$avoid_mis_end[grepl("6", survey$avoid_mis_end)] 
survey$avoid_mis_6_text_end[grepl("6", survey$avoid_mis_end)] 

# freq_analyze and knowledge_verify
survey$freq_analyze_end[which(survey$freq_analyze_end == 6)] <- 3 # 0 obs
survey$knowledge_verify_end[which(survey$knowledge_verify_end == 6)] <- 3 # 3 obs

survey$know_to_verify_end[which(survey$know_to_verify_end == 6)] <- 3 # 2 obs

# Create dummies for each response of "characts_false_end"
test <- strsplit(as.character(survey$characts_false_end), ",")
lev <- unique(unlist(test))
dummy_vars <- lapply(test, function(x) table(factor(x, levels=lev)))
dummies <- as.data.frame(do.call(rbind, dummy_vars))
names(dummies) <- paste0("characts_false_", names(dummies), "_end")
survey <- cbind.data.frame(survey, dummies)

# Create dummies for each response of "knowledge_plat_end"
test <- strsplit(as.character(survey$knowledge_plat_end), ",")
lev <- unique(unlist(test))
dummy_vars <- lapply(test, function(x) table(factor(x, levels=lev)))
dummies <- as.data.frame(do.call(rbind, dummy_vars))
names(dummies) <- paste0("knowledge_plat_", names(dummies), "_end")
survey <- cbind.data.frame(survey, dummies)


# Create dummies for each response of "forms_verify_end"
test <- strsplit(as.character(survey$forms_verify_end), ",")
lev <- unique(unlist(test))
dummy_vars <- lapply(test, function(x) table(factor(x, levels=lev)))
dummies <- as.data.frame(do.call(rbind, dummy_vars))
names(dummies) <- paste0("forms_verify_", names(dummies), "_end")
survey <- cbind.data.frame(survey, dummies)

# hypothetical scenarios variables
survey$case1_hyp_1_end[which(survey$case1_hyp_1_end == 6)] <- 3 # 0 obs
survey$case1_hyp_2_end[which(survey$case1_hyp_2_end == 6)] <- 3 # 1
survey$case2_hyp_1_end[which(survey$case2_hyp_1_end == 6)] <- 3 # 1
survey$case2_hyp_2_end[which(survey$case2_hyp_2_end == 6)] <- 3 # 0
survey$case3_hyp_1_end[which(survey$case3_hyp_1_end == 6)] <- 3 # 0
survey$case3_hyp_2_end[which(survey$case3_hyp_2_end == 6)] <- 3 # 0

# fact checkers MISSING
survey$know_fact_checkers_1_end <- tolower(stri_trans_general(survey$know_fact_checkers_1_end,"Latin-ASCII"))
survey$know_fact_checkers_2_end <- tolower(stri_trans_general(survey$know_fact_checkers_2_end,"Latin-ASCII"))
survey$know_fact_checkers_3_end <- tolower(stri_trans_general(survey$know_fact_checkers_3_end,"Latin-ASCII"))

survey$know_fact_checkers_1_end[which(survey$know_fact_checkers_1_end == "")] <- "no se"
survey$know_fact_checkers_2_end[which(survey$know_fact_checkers_2_end == "")] <- "no se"
survey$know_fact_checkers_3_end[which(survey$know_fact_checkers_3_end == "")] <- "no se"

sum1 <- ifelse(grepl("ningu|no se|nose|No seque|No sa(c)|no me acuerdo|no recuerdo|,,,,,,," , survey$know_fact_checkers_1_end),0,1)
sum2 <- ifelse(grepl("ningu|no se|nose|No seque|No sa(c)|no me acuerdo|no recuerdo|,,,,,,," , survey$know_fact_checkers_2_end),0,1)
sum3 <- ifelse(grepl("ningu|no se|nose|No seque|No sa(c)|no me acuerdo|no recuerdo|,,,,,,," , survey$know_fact_checkers_3_end),0,1)

survey$know_fact_checkers_sum_end <- sum1 + sum2 + sum3

# COVID-19
survey$affirm_covid_end[which(survey$affirm_covid_end == 6)] <- 3 # 1 obs
survey$affirm_lockdown_end[which(survey$affirm_lockdown_end == 6)] <- 3 # 0
survey$agree_vaccine_end[which(survey$agree_vaccine_end == 6)] <- 3 # 3 
survey$trust_vaccine_end[which(survey$trust_vaccine_end == 6)] <- 3 # 0

# recent cases
survey$recent_cases_1_end <- tolower(stri_trans_general(survey$recent_cases_1_end,"Latin-ASCII"))
survey$recent_cases_2_end <- tolower(stri_trans_general(survey$recent_cases_2_end,"Latin-ASCII"))
survey$recent_cases_3_end <- tolower(stri_trans_general(survey$recent_cases_3_end,"Latin-ASCII"))
survey$recent_cases_4_end <- tolower(stri_trans_general(survey$recent_cases_4_end,"Latin-ASCII"))
survey$recent_cases_5_end <- tolower(stri_trans_general(survey$recent_cases_5_end,"Latin-ASCII"))

survey$recent_cases_1_end[which(survey$recent_cases_1_end == "")] <- "no me acuerdo"
survey$recent_cases_2_end[which(survey$recent_cases_2_end == "")] <- "no me acuerdo"
survey$recent_cases_3_end[which(survey$recent_cases_3_end == "")] <- "no me acuerdo"
survey$recent_cases_4_end[which(survey$recent_cases_4_end == "")] <- "no me acuerdo"
survey$recent_cases_5_end[which(survey$recent_cases_5_end == "")] <- "no me acuerdo"

sum1 <- ifelse(grepl("ningu|no se|nose|No seque|No sa(c)|no me acuerdo|no recuerdo|,,,,,,,|no me" , survey$recent_cases_1_end),0,1)
sum2 <- ifelse(grepl("ningu|no se|nose|No seque|No sa(c)|no me acuerdo|no recuerdo|,,,,,,,|no me" , survey$recent_cases_2_end),0,1)
sum3 <- ifelse(grepl("ningu|no se|nose|No seque|No sa(c)|no me acuerdo|no recuerdo|,,,,,,,|no me" , survey$recent_cases_3_end),0,1)
sum4 <- ifelse(grepl("ningu|no se|nose|No seque|No sa(c)|no me acuerdo|no recuerdo|,,,,,,,|no me" , survey$recent_cases_4_end),0,1)
sum5 <- ifelse(grepl("ningu|no se|nose|No seque|No sa(c)|no me acuerdo|no recuerdo|,,,,,,,|no me" , survey$recent_cases_5_end),0,1)

survey$recent_cases_sum_end <- sum1 + sum2 + sum3 + sum4 + sum5


# Indexes
# Consumption of news from different sources 
survey$freq_Ver_index_end <- indexes(paste0("freq_tipos_", 1:3, "_end"))
survey$freq_NonVer_index_end <- indexes(paste0("freq_tipos_", 5:8, "_end"))

# Trust news from different sources 
survey$confia_Ver_index_end  <- indexes(paste0("confia_", 1:3, "_end"))
survey$confia_NonVer_index_end  <- indexes(paste0("confia_", 5:8, "_end"))

# Believe news from different sources 
survey$cree_Ver_index_end  <- indexes(paste0("cree_fuentes_", 1:3, "_end"))
survey$cree_NonVer_index_end  <- indexes(paste0("cree_fuentes_", 5:8, "_end"))

# Main means to spread fake news
survey$knowledge_Ver_index_end <- indexes(paste0("knowledge_plat_", 1:3, "_end"))
survey$knowledge_NonVer_index_end <- indexes(paste0("knowledge_plat_", c(5:6,9:10), "_end"))

# Bolivian problems 
survey$problems_index_end <- indexes(paste0("problemas_", 1:6, "_end"))

# Main characteristics to identify a fake news
right <- rowSums(survey[paste0("characts_false_", c(1,4,5,7,9), "_end")])/5
wrong <- rowSums(survey[paste0("characts_false_", c(2,3,6,8,10), "_end")])/5
survey$characts_index_end <- right - wrong

# Main ways to verify the veracity of news
right <- rowSums(survey[paste0("forms_verify_", c(1,4,5), "_end")])/3
wrong <- rowSums(survey[paste0("forms_verify_", c(2,3,6), "_end")])/3
survey$forms_index_end <- right - wrong

# index of likelihood of false for hypothetical scenarios
survey$hyp_false_index_end <- indexes(paste0("case", 1:3, "_hyp_2_end"))

# index of Sharing behavior for hypothetical scenarios
survey$hyp_sharing_index_end <- indexes(paste0("case", 1:3, "_hyp_1_end"))

# COVID-19 variables
which(survey$affirm_covid_end == 6)
survey$affirm_lockdown_end[which(survey$affirm_lockdown_end == 5)] <- 3 # 2 dont know responses
which(survey$agree_vaccine_end == 6)
which(survey$trust_vaccine_end == 6)

# Remain in the WhatsApp list from Chequea Bolivia
table(survey$suscribe_whats_end)

# Save data --------------------------------------------------------------------
write.csv(survey, 'Datasets/WhatsApp/survey_final.csv', row.names = F)
