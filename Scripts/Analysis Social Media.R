# Analyze social media data and attrition

# ------------------------------------------------------------------------------
# Read data with all variables
whatsApp <- read_parquet("Datasets/Social Media/content_citizen_survey.parquet", as_tibble = TRUE) # change name to match identifiers below
online <- read_parquet("Datasets/Social Media/content_whatsapp_survey.parquet", as_tibble = TRUE) # change name to match identifiers below

# Read id data 
id_o <- read_excel("Datasets/Social Media/ids_whatsapp_survey.xlsx")
id_w <- read_excel("Datasets/Social Media/ids_citizen_survey.xlsx") # two emails is WhatsApp

# Fix non-matching identifier between 
online$identifier[which(online$identifier == "condori_saca_carlo")] <- "condori_saca_carlos"

# Read baseline WhatasApp data
what_base <- read_dta('Datasets//WhatsApp/baseline_ready.dta') %>% filter(baseline_complete == 1)

# Create treatment indicators
what_base <- cbind(what_base, fastDummies::dummy_cols(what_base$treatment,  remove_selected_columns = T))
names(what_base)[(length(what_base)- 3):length(what_base)] <- c("control", "curso_verificacion", "curso", "verificacion")

# Read baseline Online data with treatments
alf_base <- read_dta('Datasets/Alfabetizacion/final_data_alfabetizacion.dta')
alf_base <- subset(alf_base, time == 1) # baseline
ind <- which(alf_base$baseline == 1 & is.na(alf_base$missing_baseline)) # completed baseline responses
alf_base <- alf_base[ind,] 

# Delte extra 5 baseline responses 
ind2 <- which(!is.na(alf_base$gender)) # notice that these 5 responses have all missing survey responses.
alf_base <- alf_base[ind2,] 

# Read dataframe with Matching emails
alf_base2 <- read_dta('Datasets/alfabetizacion/alfabetizacion_baseline.dta') # alf_base doesn't contain emails
alf_base2 <- alf_base2[which(!is.na(alf_base2$gender)),] # Delete 5 extra baseline responses

# merge treatment with df with matching emails
alf_base2 <- left_join(alf_base2, alf_base[c("responseid", "treatment", "blockid1")], by = "responseid") # append treatments and blocks to df with emails

# Create treatment indicators
alf_base2 <- cbind(alf_base2, fastDummies::dummy_cols(alf_base2$treatment,  remove_selected_columns = T))
names(alf_base2)[(length(alf_base2)- 3):length(alf_base2)] <- c("curso_verificacion", "curso", "verificacion", "control")

# Save for future references
alf_base <- alf_base2 

# Merge data -------------------------------------------------------------------
# All baseline observations have an email match
sum(alf_base$recipientemail %in% id_o$address) # all 310 baseline responses

unique(online$identifier) %in% id_o$identifier

# Whatapp merge
ind1 <- which(what_base$email1 %in% id_w$email) # 475
ind2 <- which(what_base$email2 %in% id_w$email) # 24
ind3 <- which(what_base$Email1 %in% id_w$email) # 433
ind4 <- which(what_base$Email2 %in% id_w$email) # 23

ind3[which(ind3 %!in% ind1)] # 4 of ind3 are not in ind 1, 0 for the rest 

# Replace emails of ind1 with ind3 when non matching # 479
what_base$email1[ind3[which(ind3 %!in% ind1)]] <- what_base$Email1[ind3[which(ind3 %!in% ind1)]]

# Merge treatment indicators with identifiers df
id_o <- left_join(id_o, alf_base[c("recipientemail", "curso_verificacion", "curso", "verificacion", "control", "blockid1")], 
                  by = c( "address" = "recipientemail"))

id_o2 <- id_o %>% filter(!is.na(control)) # Filter to only assigned respondents

id_w <- left_join(id_w, what_base[c("email1", "curso_verificacion", "curso", "verificacion", "control", "blockid1")], 
                  by = c("email" = "email1"))

id_w2 <- id_w %>% filter(!is.na(control)) # Filter to only assigned respondents

# Change names and typeof variables to rbind samples
which(names(online) %!in% names(whatsApp)) # All whatsApp variables are in the online sample

whatsApp$total_reactions <- as.numeric(whatsApp$total_reactions)
whatsApp$has_url <- as.numeric(whatsApp$has_url)
whatsApp$has_image <- as.numeric(whatsApp$has_image)
whatsApp$has_text <- as.numeric(whatsApp$has_text)

# Append treatments to original df
whatsApp_aux <- left_join(whatsApp, id_w2[c("identifier", "email", "curso_verificacion", "curso", "verificacion", "control", "blockid1", "n_social_content")], 
                          by = "identifier") 

whatsApp_aux <- whatsApp_aux %>% filter(!is.na(control)) # Filter to only assigned respondents

online_aux <- left_join(online, id_o2[c("identifier", "address", "curso_verificacion", "curso", "verificacion", "control", "blockid1", "n_social_content")], 
                        by = "identifier") 

online_aux <- online_aux %>% filter(!is.na(control)) # Filter to only assigned respondents

# Indicator fot whatsApp survey obs
whatsApp_aux$WhatsApp <- 1
online_aux$WhatsApp <- 0

whatsApp_aux$blockid1 <- paste0(whatsApp_aux$blockid1, "_w")
online_aux$blockid1 <- paste0(online_aux$blockid1, "_o")

# Create a pre-treatment and after-treatment variable (baseline - endline)
online_aux$baseline <- ifelse(online_aux$created_at <= "2021-02-21", 1, 0) # Online baseline data = 2021-20-21 (endline goes until the end of april)
whatsApp_aux$baseline <- ifelse(whatsApp_aux$created_at <= "2021-02-17", 1, 0) # WhatsApp started in 2021-02-17

# Create an outcome variable for those that have a post during the time of intervention
online_aux$has_post <- ifelse(online_aux$created_at >= "2021-02-21" & online_aux$created_at <= "2021-03-21", 1, 0) # Online baseline data = 2021-20-21 (endline goes until the end of april)
whatsApp_aux$has_post <- ifelse(whatsApp_aux$created_at >= "2021-02-17" & whatsApp_aux$created_at <= "2021-03-17", 1, 0) # WhatsApp started in 2021-02-17

# Obtain an auxiliary dataframe to evaluate the number or participants that posted within a month of the intervention
online_aux_post <- online_aux
whatsApp_aux_post <- whatsApp_aux

# Identify individuals with posts during the time period

id_online_haspost <- unique(online_aux_post$identifier[which(online_aux_post$has_post == 1)])
id_whatsapp_haspost <- unique(whatsApp_aux_post$identifier[which(whatsApp_aux_post$has_post == 1)])

online_aux_post <- online_aux_post %>% group_by(identifier, curso_verificacion, curso, verificacion, control, blockid1, WhatsApp) %>% summarise()
whatsApp_aux_post <- whatsApp_aux_post %>% group_by(identifier, curso_verificacion, curso, verificacion, control, blockid1, WhatsApp) %>% summarise()

online_aux_post$has_post <- ifelse(online_aux_post$identifier %in% id_online_haspost, 1,0)
whatsApp_aux_post$has_post <- ifelse(whatsApp_aux_post$identifier %in% id_whatsapp_haspost, 1,0)

online_aux_post$n_post <- id_o$n_social_content[which(online_aux_post$identifier %in% id_o$identifier)]
whatsApp_aux_post$n_post <- id_w$n_social_content[which(whatsApp_aux_post$identifier %in% id_w$identifier)]

aux_post_data <- rbind(online_aux_post, whatsApp_aux_post)

# Filter to a month prior and after the intervention
online_aux <- online_aux %>% filter(created_at >= "2021-01-21" & created_at <= "2021-03-21")
whatsApp_aux <- whatsApp_aux %>% filter(created_at >= "2021-01-17" & created_at <= "2021-03-17")

# Apepnd both dataframes for each individual en each sample
final_data <- bind_rows(online_aux, whatsApp_aux)

# Delete cells with sub dataframes
#final_data <- final_data %>% dplyr::select(-c(7:12))

# Create numeric variables where needed ----------------------------------------
# Final data with 133 identifiers
final_data$preds_model_beto_sm_num <- as.numeric(ifelse(final_data$preds_model_beto_sm == "true", 0, 1))
final_data$preds_model_numeric_sm_num <- as.numeric(ifelse(final_data$preds_model_numeric_sm == "true", 0, 1))
final_data$preds_model_wobeto_sm_num <- as.numeric(ifelse(final_data$preds_model_wobeto_sm == "true", 0, 1))
final_data$preds_model_beto_sm_num[is.na(final_data$preds_model_beto_sm_num)] <- 0
final_data$preds_model_numeric_sm_num[is.na(final_data$preds_model_numeric_sm_num)] <- 0
final_data$preds_model_wobeto_sm_num[is.na(final_data$preds_model_wobeto_sm_num)] <- 0

# subset to baseline and endline df  ------------------------------------------
final_data_baseline <- final_data %>% subset(baseline == 1)
final_data_endline <- final_data %>% subset(baseline == 0)

names(final_data_endline) <- paste0(names(final_data_endline), "_end")

# Fill with NA values if an endline identifier is missing at baseline
unique(final_data_endline$identifier_end[unique(final_data_endline$identifier_end) %!in% unique(final_data_baseline$identifier)])

# ------------------------------------------------------------------------------
# Make 12 observations 0 because of missing responses. From 155 ones to 133. 
# Get 133 identifiers
identifiers_133 <- unique(final_data_endline$identifier_end)[unique(final_data_endline$identifier_end) %in% unique(final_data_baseline$identifier)]

# Make 12 obs 0 and number of obse
aux_post_data$has_post[aux_post_data$identifier %!in% identifiers_133] <- 0
aux_post_data$n_post[aux_post_data$identifier %!in% identifiers_133] <- 0

aux_post_data$n_post <- log(aux_post_data$n_post + 1)

# Save both baseline adn endline dataframes for further analysis
write.csv(final_data_baseline, "Datasets/Social media/SocialMediaData_baseline.csv", row.names = F)
write.csv(final_data_endline, "Datasets/Social media/SocialMediaData_endline.csv", row.names = F)

# Attrition data ---------------------------------------------------------------
# Append email to datasets
alf_base$sm_info <- 0
what_base$sm_info <- 0

alf_base$sm_info[alf_base$recipientemail %in% id_o$address[id_o$identifier %in% unique(online$identifier)]] <- 1
what_base$sm_info[what_base$email1 %in% id_w$email[id_w$identifier %in% unique(whatsApp$identifier)]] <- 1

alf_base <- alf_base %>% dplyr::select(curso_verificacion, curso, verificacion, control, sm_info, blockid1)
what_base <- what_base %>% dplyr::select(curso_verificacion, curso, verificacion, control, sm_info, blockid1)

alf_base$WhatsApp <- 0
what_base$WhatsApp <- 1

alf_base$blockid1 <- paste0(alf_base$blockid1 , "_o")
what_base$blockid1 <- paste0(what_base$blockid1 , "_w")

# Merge attrition data
final_data_attrition <- bind_rows(alf_base, what_base)

# NA values to 0 for giving sm_info
final_data_attrition$sm_info[is.na(final_data_attrition$sm_info)] <- 0

# Run differential attrition model ---------------------------------------------
# Run first regression with attrition dataset and second with post dataset to see has_post variable
# Demeaned variable
interaction_var <- final_data_attrition$WhatsApp
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

aux <- c("sm_info", "has_post", "n_post")
means <- c(mean(final_data_attrition$sm_info), mean(aux_post_data$has_post), mean(aux_post_data$n_post))
means_control <- c(mean(final_data_attrition$sm_info[which(final_data_attrition$control == 1)]),
                   mean(aux_post_data$has_post[which(aux_post_data$control == 1)]),
                   mean(aux_post_data$n_post[which(aux_post_data$control == 1)]))

dep_var <- c("\\shortstack{Differential \\\\Attrition reporting \\\\social media \\\\ account}",
             "\\shortstack{Posted at least \\\\ once within \\\\1 month after the\\\\intervention began}",
             "\\shortstack{Number \\\\of posts \\\\ (log)}")

title <- "Difference in attrition reporting social media at baseline, posting at least once within one-month after the intervention, and number of posts"

# Create tables and save results
size = length(aux) + 1
cm = 9 # length in cm of the note for the table
interacted_bool <- c(T,T,T)
control <- NA
additional_note <- ""

source('Scripts/General Scripts/AttritionSocialMedia.R')

cat(final_table, file = "Tables/Appendix/Social Media/sm_att.txt")
