# Database Manipulation alfabetizacion

# ------------------------------------------------------------------------------
# read data frame
survey <- read_dta('Datasets/Course/final_data_alfabetizacion.dta', encoding = "UTF-8") 

# drop variables
survey <- survey %>% select(-c(39,40,42,46:54)) # col 1,3,4, 11 are email, name, lastname, and celphone

# Attrition --------------------------------------------------------------------

# change format of dataframe from long to wide
baseline_attrition <- subset(survey, time == 1)
baseline_attrition <- baseline_attrition %>% filter(!is.na(works_journalist))

# names(baseline) <- paste0(names(baseline), "_base")
endline_attrition  <- subset(survey, time == 2)
endline_attrition <- endline_attrition %>%  filter(is.na(missing_baseline) & is.na(missing_endline))

names(endline_attrition) <- paste0(names(endline_attrition), "_end")

survey_attrition <- left_join(baseline_attrition, endline_attrition, by = c("id" = "id_end"))

survey_attrition$complete_both <- ifelse(!is.na(survey_attrition$treatment_end),1,0)

# create treatment indicators
survey_attrition <- cbind(survey_attrition, fastDummies::dummy_cols(survey_attrition$treatment,  remove_selected_columns = T))
names(survey_attrition)[(length(survey_attrition)- 3):length(survey_attrition)] <- c("curso_verificacion", "curso", "verificacion", "control")

# Further treatments indicators
survey_attrition$curso_pooled <- survey_attrition$curso + survey_attrition$curso_verificacion
survey_attrition$verificacion_pooled <- survey_attrition$verificacion + survey_attrition$curso_verificacion

write.csv(survey_attrition, 'Datasets/Course/DataForAttrition.csv', row.names = F)

# ------------------------------------------------------------------------------
# rename variables as in whatsapp survey for simplicity
survey <- survey %>% dplyr::rename(freq_use_whats = freq_whats,
                                   freq_use_social = freq_social,
                                   journalist = study_journalism,
                                   freq_veri_compar = verify_doub_false,
                                   know_to_verify = knowledge_verify2,
                                   freq_desc_comp = discover_share_false,
                                   confia_1 = confia_fuentes_1,
                                   confia_2 = confia_fuentes_2,
                                   confia_3 = confia_fuentes_3,
                                   confia_4 = confia_fuentes_4,
                                   confia_5 = confia_fuentes_5,
                                   confia_6 = confia_fuentes_6,
                                   temas = topics_work)

# Use only those observations that completed endline and have baseline
survey <- survey %>% filter(is.na(missing_baseline) & is.na(missing_endline))

# change format of dataframe from long to wide
baseline <- subset(survey, time == 1)

# names(baseline) <- paste0(names(baseline), "_base")
endline <- subset(survey, time == 2)
names(endline) <- paste0(names(endline), "_end")

# merge
survey <- inner_join(baseline, endline, by = c("id" = "id_end"))

# drop cplumns with only NA
survey <- survey[, colSums(is.na(survey)) != nrow(survey)]

# create treatment indicators
survey <- cbind(survey, fastDummies::dummy_cols(survey$treatment,  remove_selected_columns = T))
names(survey)[(length(survey)- 3):length(survey)] <- c("curso_verificacion", "curso", "verificacion", "control")

# Further treatments indicators
survey$curso_pooled <- survey$curso + survey$curso_verificacion
survey$verificacion_pooled <- survey$verificacion + survey$curso_verificacion

# Auxiliary funciton for indexes -----------------------------------------------
indexes <- function(vars) {
  zscore <- scale(rowMeans(scale(survey[vars]) %*% diag(rep(1, length(vars)))))
  
  return (zscore)
}


indexes_2 <- function(df, directions) {
  x <- scale(df)
  y <- diag(directions)
  x0 <- x
  x0[is.na(x)] <- 0
  z <- x0 %*% y
  z[is.na(x)] <- NA
  zscore <- scale(rowMeans(z, na.rm = T))
  
  return (zscore)
  
}

# variables manipulation -------------------------------------------------------
# recode gender so that female = 1
survey <- survey %>% mutate(gender = ifelse(gender == 2, 1, 0))                            

# journalist NA as No 
survey <- survey %>% mutate(journalist  = ifelse(journalist %in% c(1:2), 1, 0))
survey$journalist[is.na(survey$journalist)] <- 0

# Main sources of national and international news that you use to inform yourself
# MISSING 
survey$fuentes_principales_1

survey$fuentes_principales_1 <- tolower(stri_trans_general(survey$fuentes_principales_1,"Latin-ASCII"))
survey$fuentes_principales_2 <- tolower(stri_trans_general(survey$fuentes_principales_2,"Latin-ASCII"))
survey$fuentes_principales_3 <- tolower(stri_trans_general(survey$fuentes_principales_3,"Latin-ASCII"))
survey$fuentes_principales_4 <- tolower(stri_trans_general(survey$fuentes_principales_4,"Latin-ASCII"))
survey$fuentes_principales_5 <- tolower(stri_trans_general(survey$fuentes_principales_5,"Latin-ASCII"))

survey$fuentes_principales_1[which(survey$fuentes_principales_1 == "")] <- "ninguna"
survey$fuentes_principales_2[which(survey$fuentes_principales_2 == "")] <- "ninguna"
survey$fuentes_principales_3[which(survey$fuentes_principales_3 == "")] <- "ninguna"
survey$fuentes_principales_4[which(survey$fuentes_principales_4 == "")] <- "ninguna"
survey$fuentes_principales_5[which(survey$fuentes_principales_5 == "")] <- "ninguna"

sum1 <- ifelse(grepl("ningu|no se|nose|no sa(c)|ninga" , survey$fuentes_principales_1),0,1)
sum2 <- ifelse(grepl("ningu|no se|nose|no sa(c)|ninga" , survey$fuentes_principales_2),0,1)
sum3 <- ifelse(grepl("ningu|no se|nose|no sa(c)|ninga" , survey$fuentes_principales_3),0,1)
sum4 <- ifelse(grepl("ningu|no se|nose|no sa(c)|ninga" , survey$fuentes_principales_4),0,1)
sum5 <- ifelse(grepl("ningu|no se|nose|no sa(c)|ninga" , survey$fuentes_principales_5),0,1)

survey$fuentes_principales_sum <- sum1 + sum2 + sum3 + sum4 + sum5

# Social media accounts
survey$cuentas_social_infor_1 <- tolower(stri_trans_general(survey$cuentas_social_infor_1,"Latin-ASCII"))
survey$cuentas_social_infor_2 <- tolower(stri_trans_general(survey$cuentas_social_infor_2,"Latin-ASCII"))
survey$cuentas_social_infor_3 <- tolower(stri_trans_general(survey$cuentas_social_infor_3,"Latin-ASCII"))
survey$cuentas_social_infor_4 <- tolower(stri_trans_general(survey$cuentas_social_infor_4,"Latin-ASCII"))
survey$cuentas_social_infor_5 <- tolower(stri_trans_general(survey$cuentas_social_infor_5,"Latin-ASCII"))

survey$cuentas_social_infor_1[which(survey$cuentas_social_infor_1 == "")] <- "ninguna"
survey$cuentas_social_infor_2[which(survey$cuentas_social_infor_1 == "")] <- "ninguna"
survey$cuentas_social_infor_3[which(survey$cuentas_social_infor_1 == "")] <- "ninguna"
survey$cuentas_social_infor_4[which(survey$cuentas_social_infor_1 == "")] <- "ninguna"
survey$cuentas_social_infor_5[which(survey$cuentas_social_infor_1 == "")] <- "ninguna"

sum1_1 <- ifelse(grepl("facebook" , survey$cuentas_social_infor_1),1,0)
sum1_2 <- ifelse(grepl("twitter", survey$cuentas_social_infor_1),1,0)
sum1_3 <- ifelse(grepl("facebook|twitter|instagram" , survey$cuentas_social_infor_1),0,1)

sum2_1 <- ifelse(grepl("facebook" , survey$cuentas_social_infor_2),1,0)
sum2_2 <- ifelse(grepl("twitter", survey$cuentas_social_infor_2),1,0)
sum2_3 <- ifelse(grepl("facebook|twitter|instagram" , survey$cuentas_social_infor_2),0,1)

sum3_1 <- ifelse(grepl("facebook" , survey$cuentas_social_infor_3),1,0)
sum3_2 <- ifelse(grepl("twitter", survey$cuentas_social_infor_3),1,0)
sum3_3 <- ifelse(grepl("facebook|twitter|instagram" , survey$cuentas_social_infor_3),0,1)

sum4_1 <- ifelse(grepl("facebook" , survey$cuentas_social_infor_4),1,0)
sum4_2 <- ifelse(grepl("twitter", survey$cuentas_social_infor_4),1,0)
sum4_3 <- ifelse(grepl("facebook|twitter|instagram" , survey$cuentas_social_infor_4),0,1)

sum5_1 <- ifelse(grepl("facebook" , survey$cuentas_social_infor_5),1,0)
sum5_2 <- ifelse(grepl("twitter", survey$cuentas_social_infor_5),1,0)
sum5_3 <- ifelse(grepl("facebook|twitter|instagram" , survey$cuentas_social_infor_5),0,1)

survey$cuentas_social_infor_sum_1 <- sum1_1 + sum2_1 + sum3_1 + sum4_1 + sum5_1
survey$cuentas_social_infor_sum_2 <- sum1_2 + sum2_2 + sum3_2 + sum4_2 + sum5_2
survey$cuentas_social_infor_sum_3 <- sum1_3 + sum2_3 + sum3_3 + sum4_3 + sum5_3

# Replace non-responses 
survey$cuentas_social_infor_1[which(survey$cuentas_social_infor_1 == "")] <- "no se"
survey$cuentas_social_infor_2[which(survey$cuentas_social_infor_2 == "")] <- "no se"
survey$cuentas_social_infor_3[which(survey$cuentas_social_infor_3 == "")] <- "no se"
survey$cuentas_social_infor_4[which(survey$cuentas_social_infor_4 == "")] <- "no se"
survey$cuentas_social_infor_5[which(survey$cuentas_social_infor_5 == "")] <- "no se"

# Different coding for prior variables as traditional and social media indexes 
sum1_1 <- ifelse(grepl("facebook|twitter|instagram|whatsapp|social media|Linkedin|Watsap|youtube|sxsxdscv|IG", 
                       survey$cuentas_social_infor_1), 1, 0)
sum1_2 <- ifelse(grepl("facebook|twitter|instagram|whatsapp|social media|Linkedin|Watsap|youtube|sxsxdscv|IGningu|no se|nose|no sa(c)|ninga|No recuerdo|son muchas|rt|chrome google", 
                       survey$cuentas_social_infor_1), 0, 1)

sum2_1 <- ifelse(grepl("facebook|twitter|instagram|whatsapp|social media|Linkedin|Watsap|youtube|sxsxdscv|IG", 
                       survey$cuentas_social_infor_2), 1, 0)
sum2_2 <- ifelse(grepl("facebook|twitter|instagram|whatsapp|social media|Linkedin|Watsap|youtube|sxsxdscv|IGningu|no se|nose|no sa(c)|ninga|No recuerdo", 
                       survey$cuentas_social_infor_2), 0, 1)

sum3_1 <- ifelse(grepl("facebook|twitter|instagram|whatsapp|social media|Linkedin|Watsap|youtube|sxsxdscv|IG", 
                       survey$cuentas_social_infor_3), 1, 0)
sum3_2 <- ifelse(grepl("facebook|twitter|instagram|whatsapp|social media|Linkedin|Watsap|youtube|sxsxdscv|IGningu|no se|nose|no sa(c)|ninga|No recuerdo", 
                       survey$cuentas_social_infor_3), 0, 1)

sum4_1 <- ifelse(grepl("facebook|twitter|instagram|whatsapp|social media|Linkedin|Watsap|youtube|sxsxdscv|IG", 
                       survey$cuentas_social_infor_4), 1, 0)
sum4_2 <- ifelse(grepl("facebook|twitter|instagram|whatsapp|social media|Linkedin|Watsap|youtube|sxsxdscv|IGningu|no se|nose|no sa(c)|ninga|No recuerdo", 
                       survey$cuentas_social_infor_4), 0, 1)

sum5_1 <- ifelse(grepl("facebook|twitter|instagram|whatsapp|social media|Linkedin|Watsap|youtube|sxsxdscv|IG", 
                       survey$cuentas_social_infor_5), 1, 0)
sum5_2 <- ifelse(grepl("facebook|twitter|instagram|whatsapp|social media|Linkedin|Watsap|youtube|sxsxdscv|IGningu|no se|nose|no sa(c)|ninga|No recuerdo", 
                       survey$cuentas_social_infor_5), 0, 1)

survey$cuentas_social_infor_Nontrad <- sum1_1 + sum2_1 + sum3_1 + sum4_1 + sum5_1
survey$cuentas_social_infor_trad <- sum1_2 + sum2_2 + sum3_2 + sum4_2 + sum5_2

# freq_analyze and knowledge_verify 
survey$freq_analyze[which(survey$freq_analyze == 6)] <- 3 # 0 obs
survey$knowledge_verify[which(survey$knowledge_verify == 6)] <- 3 # 1 obs

survey$freq_veri_compar[which(survey$freq_veri_compar == 6)] <- 3 # 2 obs
survey$know_to_verify[which(survey$know_to_verify == 6)] <- 3 # 2 obs

survey$freq_desc_comp[which(survey$freq_desc_comp == 6)] <- 3 # 6 obs

# hypothetical scenarios variables
survey$case1_hyp_1[which(survey$case1_hyp_1 == 6)] <- 3 # 1 obs
survey$case1_hyp_2[which(survey$case1_hyp_2 == 6)] <- 3 # 2
survey$case2_hyp_1[which(survey$case2_hyp_1 == 6)] <- 3 # 1
survey$case2_hyp_2[which(survey$case2_hyp_2 == 6)] <- 3 # 2
survey$case3_hyp_1[which(survey$case3_hyp_1 == 6)] <- 3 # 2
survey$case3_hyp_2[which(survey$case3_hyp_2 == 6)] <- 3 # 2


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
survey$problemas_1[which(survey$problemas_1 == 6)] <- 3 # 1 obs
survey$problemas_2[which(survey$problemas_2 == 6)] <- 3 # 1
survey$problemas_3[which(survey$problemas_3 == 6)] <- 3 # 0
survey$problemas_4[which(survey$problemas_4 == 6)] <- 3 # 3
survey$problemas_5[which(survey$problemas_5 == 6)] <- 3 # 1
survey$problemas_6[which(survey$problemas_6 == 6)] <- 3 # 2

# recent cases
survey$recent_cases_1 <- tolower(stri_trans_general(survey$recent_cases_1,"Latin-ASCII"))
survey$recent_cases_2 <- tolower(stri_trans_general(survey$recent_cases_2_,"Latin-ASCII"))
survey$recent_cases_3 <- tolower(stri_trans_general(survey$recent_cases_3,"Latin-ASCII"))

survey$recent_cases_1[which(survey$recent_cases_1 == "")] <- "no me acuerdo"
survey$recent_cases_2[which(survey$recent_cases_2 == "")] <- "no me acuerdo"
survey$recent_cases_3[which(survey$recent_cases_3 == "")] <- "no me acuerdo"


sum1 <- ifelse(grepl("ningu|no se|nose|No seque|No sa(c)|no me acuerdo|no recuerdo|,,,,,,," , survey$know_fact_checkers_1_end),0,1)
sum2 <- ifelse(grepl("ningu|no se|nose|No seque|No sa(c)|no me acuerdo|no recuerdo|,,,,,,," , survey$know_fact_checkers_2_end),0,1)
sum3 <- ifelse(grepl("ningu|no se|nose|No seque|No sa(c)|no me acuerdo|no recuerdo|,,,,,,," , survey$know_fact_checkers_3_end),0,1)

# recent cases
survey$recent_cases_1 <- tolower(stri_trans_general(survey$recent_cases_1,"Latin-ASCII"))
survey$recent_cases_2 <- tolower(stri_trans_general(survey$recent_cases_2,"Latin-ASCII"))
survey$recent_cases_3 <- tolower(stri_trans_general(survey$recent_cases_3,"Latin-ASCII"))

survey$recent_cases_1[which(survey$recent_cases_1 == "")] <- "no me acuerdo"
survey$recent_cases_2[which(survey$recent_cases_2 == "")] <- "no me acuerdo"
survey$recent_cases_3[which(survey$recent_cases_3 == "")] <- "no me acuerdo"

sum1 <- ifelse(grepl("ningu|no se|nose|No seque|No sa(c)|no me acuerdo|no recuerdo|no me" , survey$know_fact_checkers_1_end),0,1)
sum2 <- ifelse(grepl("ningu|no se|nose|No seque|No sa(c)|no me acuerdo|no recuerdo|no me" , survey$know_fact_checkers_2_end),0,1)
sum3 <- ifelse(grepl("ningu|no se|nose|No seque|No sa(c)|no me acuerdo|no recuerdo|no me" , survey$know_fact_checkers_3_end),0,1)

survey$recent_cases_sum <- sum1 + sum2 + sum3

# Main activity questions MISSING!!!!
survey <- survey %>% mutate(works_journalist  = ifelse(works_journalist == 1, 1, 0))
sum(survey$works_journalist)

# SECTION ONLY FOR JOURNALISTS (If works_journalist == 1) ----------------------

survey$works_medios_1
survey$works_medios_2
survey$works_medios_3
survey$works_medios_4
survey$works_medios_5

# Create dummies for each response of "temas"
test <- strsplit(as.character(survey$temas), ",")
lev <- unique(unlist(test))
dummy_vars <- lapply(test, function(x) table(factor(x, levels=lev)))
dummies <- as.data.frame(do.call(rbind, dummy_vars))
names(dummies) <- paste0("temas_", names(dummies))
survey <- cbind.data.frame(survey, dummies)

# create opinion_news indicators
survey <- cbind(survey, fastDummies::dummy_cols(survey$opinion_news,  remove_selected_columns = T))
names(survey)[(length(survey)- 3):length(survey)] <- c("only_news", "only_opinion", "opinion_both")

survey <- survey %>% mutate(opinion_news = dplyr::recode(opinion_news, 
                                                         `1`= 1,
                                                         `2`= 3,
                                                         `3`= 2))

# publish_object 
survey <- survey %>% mutate(publish_object = dplyr::recode(publish_object, 
                                                         `1`= 1,
                                                         `2`= 0,
                                                         `3`= 0))

survey$publish_object # all journalist
# if publish_object == 1

survey$publish_combat  # all journalist
survey <- survey %>% mutate(publish_combat = dplyr::recode(publish_combat, 
                                                           `1`= 1,
                                                           `2`= 0))
# if publish_combat == 1
survey$freq_publish_combat

survey$difficulty_write_com  # all journalist

survey$difficulties_publish_1  # all journalist
survey$difficulties_publish_2  # all journalist
survey$difficulties_publish_3  # all journalist
survey$difficulties_publish_4  # all journalist
survey$difficulties_publish_5  # all journalist

# Indexes
# Consumption of news from different sources 
survey$freq_Ver_index <- indexes(paste0("freq_tipos_", 1:3))
survey$freq_NonVer_index  <- indexes(paste0("freq_tipos_", 4:6))

# Trust news from different sources 
survey$confia_Ver_index  <- indexes(paste0("confia_", 1:3))
survey$confia_NonVer_index  <- indexes(paste0("confia_", 4:6))

# Believe news from different sources 
#survey$cree_Ver_index  <- indexes(paste0("cree_fuentes_", 1:3, "_end"))
#survey$cree_NonVer_index  <- indexes(paste0("cree_fuentes_", 4:6, "_end"))


# Main means to spread fake news
survey$knowledge_Ver_index <- indexes(paste0("knowledge_plat_", 1:3))
survey$knowledge_NonVer_index <- indexes(paste0("knowledge_plat_", c(4:6)))

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

# Endline manipulation ---------------------------------------------------------
# Fuentes principales
survey$fuentes_principales_1_end <- tolower(stri_trans_general(survey$fuentes_principales_1_end,"Latin-ASCII"))
survey$fuentes_principales_2_end <- tolower(stri_trans_general(survey$fuentes_principales_2_end,"Latin-ASCII"))
survey$fuentes_principales_3_end <- tolower(stri_trans_general(survey$fuentes_principales_3_end,"Latin-ASCII"))
survey$fuentes_principales_4_end <- tolower(stri_trans_general(survey$fuentes_principales_4_end,"Latin-ASCII"))
survey$fuentes_principales_5_end <- tolower(stri_trans_general(survey$fuentes_principales_5_end,"Latin-ASCII"))

survey$fuentes_principales_1_end[which(survey$fuentes_principales_1_end == "")] <- "ninguna"
survey$fuentes_principales_2_end[which(survey$fuentes_principales_2_end == "")] <- "ninguna"
survey$fuentes_principales_3_end[which(survey$fuentes_principales_3_end == "")] <- "ninguna"
survey$fuentes_principales_4_end[which(survey$fuentes_principales_4_end == "")] <- "ninguna"
survey$fuentes_principales_5_end[which(survey$fuentes_principales_5_end == "")] <- "ninguna"

sum1 <- ifelse(grepl("ningu|no se|nose|no sa(c)|ninga|No recuerdo" , survey$fuentes_principales_1_end),0,1)
sum2 <- ifelse(grepl("ningu|no se|nose|no sa(c)|ninga|No recuerdo" , survey$fuentes_principales_2_end),0,1)
sum3 <- ifelse(grepl("ningu|no se|nose|no sa(c)|ninga|No recuerdo" , survey$fuentes_principales_3_end),0,1)
sum4 <- ifelse(grepl("ningu|no se|nose|no sa(c)|ninga|No recuerdo" , survey$fuentes_principales_4_end),0,1)
sum5 <- ifelse(grepl("ningu|no se|nose|no sa(c)|ninga|No recuerdo" , survey$fuentes_principales_5_end),0,1)

survey$fuentes_principales_sum_end <- sum1 + sum2 + sum3 + sum4 + sum5

# Social media accounts
survey$cuentas_social_infor_1_end <- tolower(stri_trans_general(survey$cuentas_social_infor_1_end,"Latin-ASCII"))
survey$cuentas_social_infor_2_end <- tolower(stri_trans_general(survey$cuentas_social_infor_2_end,"Latin-ASCII"))
survey$cuentas_social_infor_3_end <- tolower(stri_trans_general(survey$cuentas_social_infor_3_end,"Latin-ASCII"))
survey$cuentas_social_infor_4_end <- tolower(stri_trans_general(survey$cuentas_social_infor_4_end,"Latin-ASCII"))
survey$cuentas_social_infor_5_end <- tolower(stri_trans_general(survey$cuentas_social_infor_5_end,"Latin-ASCII"))

survey$cuentas_social_infor_1_end[which(survey$cuentas_social_infor_1_end == "")] <- "no se"
survey$cuentas_social_infor_2_end[which(survey$cuentas_social_infor_2_end == "")] <- "no se"
survey$cuentas_social_infor_3_end[which(survey$cuentas_social_infor_3_end == "")] <- "no se"
survey$cuentas_social_infor_4_end[which(survey$cuentas_social_infor_4_end == "")] <- "no se"
survey$cuentas_social_infor_5_end[which(survey$cuentas_social_infor_5_end == "")] <- "no se"

sum1_1 <- ifelse(grepl("facebook|twitter|instagram|whatsapp|social media|Linkedin|Watsap|youtube|sxsxdscv|IG", 
                       survey$cuentas_social_infor_1_end), 1, 0)
sum1_2 <- ifelse(grepl("facebook|twitter|instagram|whatsapp|social media|Linkedin|Watsap|youtube|sxsxdscv|IGningu|no se|nose|no sa(c)|ninga|No recuerdo", 
                       survey$cuentas_social_infor_1_end), 0, 1)

sum2_1 <- ifelse(grepl("facebook|twitter|instagram|whatsapp|social media|Linkedin|Watsap|youtube|sxsxdscv|IG", 
                       survey$cuentas_social_infor_2_end), 1, 0)
sum2_2 <- ifelse(grepl("facebook|twitter|instagram|whatsapp|social media|Linkedin|Watsap|youtube|sxsxdscv|IGningu|no se|nose|no sa(c)|ninga|No recuerdo", 
                       survey$cuentas_social_infor_2_end), 0, 1)

sum3_1 <- ifelse(grepl("facebook|twitter|instagram|whatsapp|social media|Linkedin|Watsap|youtube|sxsxdscv|IG", 
                       survey$cuentas_social_infor_3_end), 1, 0)
sum3_2 <- ifelse(grepl("facebook|twitter|instagram|whatsapp|social media|Linkedin|Watsap|youtube|sxsxdscv|IGningu|no se|nose|no sa(c)|ninga|No recuerdo", 
                       survey$cuentas_social_infor_3_end), 0, 1)

sum4_1 <- ifelse(grepl("facebook|twitter|instagram|whatsapp|social media|Linkedin|Watsap|youtube|sxsxdscv|IG", 
                       survey$cuentas_social_infor_4_end), 1, 0)
sum4_2 <- ifelse(grepl("facebook|twitter|instagram|whatsapp|social media|Linkedin|Watsap|youtube|sxsxdscv|IGningu|no se|nose|no sa(c)|ninga|No recuerdo", 
                       survey$cuentas_social_infor_4_end), 0, 1)

sum5_1 <- ifelse(grepl("facebook|twitter|instagram|whatsapp|social media|Linkedin|Watsap|youtube|sxsxdscv|IG", 
                       survey$cuentas_social_infor_5_end), 1, 0)
sum5_2 <- ifelse(grepl("facebook|twitter|instagram|whatsapp|social media|Linkedin|Watsap|youtube|sxsxdscv|IGningu|no se|nose|no sa(c)|ninga|No recuerdo", 
                       survey$cuentas_social_infor_5_end), 0, 1)

survey$cuentas_social_infor_Nontrad_end <- sum1_1 + sum2_1 + sum3_1 + sum4_1 + sum5_1
survey$cuentas_social_infor_trad_end <- sum1_2 + sum2_2 + sum3_2 + sum4_2 + sum5_2


# cree_falsa_do 
# Only 4 respondents selected 2 so we assigned them to NA, otherwise regressions could not run...
survey$cree_falsa_do_end[which(survey$cree_falsa_do_end == 5)] <- NA # NA
survey$cree_falsa_do_end_NA <- ifelse(is.na(survey$cree_falsa_do_end), 1, 0)

survey$cree_falsa_do_end[is.na(survey$cree_falsa_do_end)] <- 0

# create treatment indicators
survey$cree_falsa_do_1_end <- ifelse(survey$cree_falsa_do_end == 1, 1, 0)
survey$cree_falsa_do_2_end <- ifelse(survey$cree_falsa_do_end == 2, 1, 0)
survey$cree_falsa_do_3_end <- ifelse(survey$cree_falsa_do_end == 3, 1, 0)
survey$cree_falsa_do_4_end <- ifelse(survey$cree_falsa_do_end == 4, 1, 0)

# Create an index varying directions
cree_false_df <- survey[paste0("cree_falsa_do_", 1:4, "_end")]
survey$cree_falsa_do_index_end <- indexes_2(cree_false_df, c(1,-1,1,1))

# Hypothetical scenarios variables
survey$case1_hyp_1_end[which(survey$case1_hyp_1_end == 6)] <- 3 # 0 obs
survey$case1_hyp_2_end[which(survey$case1_hyp_2_end == 6)] <- 3 # 0
survey$case1_hyp_3_end[which(survey$case1_hyp_3_end == 6)] <- 3 # 0
survey$case2_hyp_1_end[which(survey$case2_hyp_1_end == 6)] <- 3 # 0
survey$case2_hyp_2_end[which(survey$case2_hyp_2_end == 6)] <- 3 # 0
survey$case2_hyp_3_end[which(survey$case2_hyp_3_end == 6)] <- 3 # 2
survey$case3_hyp_1_end[which(survey$case3_hyp_1_end == 6)] <- 3 # 0
survey$case3_hyp_2_end[which(survey$case3_hyp_2_end == 6)] <- 3 # 0
survey$case3_hyp_3_end[which(survey$case3_hyp_3_end == 6)] <- 3 # 0

# freq_analyze and knowledge_verify 
survey$freq_analyze_end[which(survey$freq_analyze_end == 6)] <- 3 # 0 obs
survey$knowledge_verify_end[which(survey$knowledge_verify_end == 6)] <- 3 # 0 obs

survey$freq_veri_compar_end[which(survey$freq_veri_compar_end == 6)] <- 3 # 2 obs
survey$know_to_verify_end[which(survey$know_to_verify_end == 6)] <- 3 # 2 obs

survey$freq_desc_comp[which(survey$freq_desc_comp == 6)] <- 3 # 6 obs

# Code know_fact_checkers_1
survey$know_fact_checkers_1_end <- tolower(stri_trans_general(survey$know_fact_checkers_1_end,"Latin-ASCII"))
survey$know_fact_checkers_2_end <- tolower(stri_trans_general(survey$know_fact_checkers_2_end,"Latin-ASCII"))
survey$know_fact_checkers_3_end <- tolower(stri_trans_general(survey$know_fact_checkers_3_end,"Latin-ASCII"))

survey$know_fact_checkers_1_end[which(survey$know_fact_checkers_1_end == "")] <- "no se"
survey$know_fact_checkers_2_end[which(survey$know_fact_checkers_2_end == "")] <- "no se"
survey$know_fact_checkers_3_end[which(survey$know_fact_checkers_3_end == "")] <- "no se"

sum1 <- ifelse(grepl("ningu|no se|nose|no sa(c)|ninga" , survey$know_fact_checkers_1_end),0,1)
sum2 <- ifelse(grepl("ningu|no se|nose|no sa(c)|ninga" , survey$know_fact_checkers_2_end),0,1)
sum3 <- ifelse(grepl("ningu|no se|nose|no sa(c)|ninga" , survey$know_fact_checkers_3_end),0,1)

survey$know_fact_checkers_sum_end <- sum1 + sum2 + sum3

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

# Code bolivian problems variable 
survey$problemas_1_end[which(survey$problemas_1_end == 6)] <- 3 # 1 non responses
survey$problemas_2_end[which(survey$problemas_2_end == 6)] <- 3 # 0 non responses
survey$problemas_3_end[which(survey$problemas_3_end == 6)] <- 3# 0 non responses
survey$problemas_4_end[which(survey$problemas_4_end == 6)] <- 3 # 2 non responses
survey$problemas_5_end[which(survey$problemas_5_end == 6)] <- 3 # 1 non responses
survey$problemas_6_end[which(survey$problemas_6_end == 6)] <- 3 # 2 non responses

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

sum1 <- ifelse(grepl("ningu|no se|nose|No seque|No sa(c)|no me acuerdo|no recuerdo|,,,,,,," , survey$recent_cases_1_end),0,1)
sum2 <- ifelse(grepl("ningu|no se|nose|No seque|No sa(c)|no me acuerdo|no recuerdo|,,,,,,," , survey$recent_cases_2_end),0,1)
sum3 <- ifelse(grepl("ningu|no se|nose|No seque|No sa(c)|no me acuerdo|no recuerdo|,,,,,,," , survey$recent_cases_3_end),0,1)
sum4 <- ifelse(grepl("ningu|no se|nose|No seque|No sa(c)|no me acuerdo|no recuerdo|,,,,,,," , survey$recent_cases_4_end),0,1)
sum5 <- ifelse(grepl("ningu|no se|nose|No seque|No sa(c)|no me acuerdo|no recuerdo|,,,,,,," , survey$recent_cases_5_end),0,1)

survey$recent_cases_sum_end <- sum1 + sum2 + sum3 + sum4 + sum5

# Indexes
# Consumption of news from different sources 
survey$freq_Ver_index_end <- indexes(paste0("freq_tipos_", 1:3, "_end"))
survey$freq_NonVer_index_end  <- indexes(paste0("freq_tipos_", 4:6, "_end"))

# Trust news from different sources 
survey$confia_Ver_index_end  <- indexes(paste0("confia_", 1:3, "_end"))
survey$confia_NonVer_index_end  <- indexes(paste0("confia_", 4:6, "_end"))

# Believe news from different sources 
#survey$cree_Ver_index  <- indexes(paste0("cree_fuentes_", 1:3, "_end"))
#survey$cree_NonVer_index  <- indexes(paste0("cree_fuentes_", 4:6, "_end"))

# Frequency of false news from sources
survey$freq_plat_false_Ver_index_end <- indexes(paste0("freq_plat_false_", 1:3, "_end"))
survey$freq_plat_false_NonVer_index_end <- indexes(paste0("freq_plat_false_", c(4:6), "_end"))

# Main means to spread fake news
survey$knowledge_Ver_index_end <- indexes(paste0("knowledge_plat_", 1:3, "_end"))
survey$knowledge_NonVer_index_end <- indexes(paste0("knowledge_plat_", c(4:6), "_end"))

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

# index of verifying behavior for hypothetical scenarios
survey$hyp_verifying_index_end <- indexes(paste0("case", 1:3, "_hyp_3_end"))

# Save data --------------------------------------------------------------------
write.csv(survey, 'Datasets/Course/survey course final.csv', row.names = F)


