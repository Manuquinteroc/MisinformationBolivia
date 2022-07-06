# ATE Social Media

# ------------------------------------------------------------------------------
# Read df
sm_data_base <- read.csv("Datasets/Social media/SocialMediaData_baseline.csv")
sm_data_base <- sm_data_base[which(!is.na(sm_data_base$control)),] # deleting NA rows

sm_data_end <- read.csv("Datasets/Social media/SocialMediaData_endline.csv")
sm_data_end <- sm_data_end[which(!is.na(sm_data_end$control)),] # deleting NA rows

# Final data with 133 identifiers
sm_data_base$preds_model_beto_sm_num <- as.numeric(ifelse(sm_data_base$preds_model_beto_sm == "true", 0, 1))
sm_data_base$preds_model_numeric_sm_num <- as.numeric(ifelse(sm_data_base$preds_model_numeric_sm == "true", 0, 1))
sm_data_base$preds_model_wobeto_sm_num <- as.numeric(ifelse(sm_data_base$preds_model_wobeto_sm == "true", 0, 1))
sm_data_base$preds_model_beto_sm_num[is.na(sm_data_base$preds_model_beto_sm_num)] <- 0
sm_data_base$preds_model_numeric_sm_num[is.na(sm_data_base$preds_model_numeric_sm_num)] <- 0
sm_data_base$preds_model_wobeto_sm_num[is.na(sm_data_base$preds_model_wobeto_sm_num)] <- 0

sm_data_end$preds_model_beto_sm_num_end <- as.numeric(ifelse(sm_data_end$preds_model_beto_sm_end == "true", 0, 1))
sm_data_end$preds_model_numeric_sm_num_end <- as.numeric(ifelse(sm_data_end$preds_model_numeric_sm_end == "true", 0, 1))
sm_data_end$preds_model_wobeto_sm_num_end <- as.numeric(ifelse(sm_data_end$preds_model_wobeto_sm_end == "true", 0, 1))
sm_data_end$preds_model_beto_sm_num_end[is.na(sm_data_end$preds_model_beto_sm_num_end)] <- 0
sm_data_end$preds_model_numeric_sm_num_end[is.na(sm_data_end$preds_model_numeric_sm_num_end)] <- 0
sm_data_end$preds_model_wobeto_sm_num_end[is.na(sm_data_end$preds_model_wobeto_sm_num_end)] <- 0

sm_data_base$fact_check[is.na(sm_data_base$fact_check)] <- 0
sm_data_end$fact_check_end[is.na(sm_data_end$fact_check_end)] <- 0

# Number of posts per user
base_n_socialmedia <- sm_data_base %>% group_by(identifier) %>% dplyr::summarise(n = n())
end_n_socialmedia <- sm_data_end %>% group_by(identifier_end) %>% dplyr::summarise(n = n())

# Clean baseline to only individuals that appear at endline
sm_data_base <- sm_data_base %>% dplyr::select(identifier, curso, verificacion, curso_verificacion, control, blockid1, WhatsApp, 
                                               hapaxlegomenon_R, hapaxlegomenon_hapax, hapax_dislegomenon_h, hapax_dislegomenon_S, # Readability
                                               word_density, readability_index, simpsons_index, brunets_measure_w, szigriszt_pazos, # Readability
                                               type_token_ratio, shannon_entropy, yules_characteristic_K, # Readability
                                               prop_persons, prop_locations, prop_organizations, functional_words_count, # Lexical index factual
                                               word_count, sentence_count, find_url, has_url, count_persons, count_locations, count_organizations, syllable_count, 
                                               syllable_count,  # Lexical index length
                                               flesch_reading, fernandez_huerta, reading_time, crawford, gutierrez_polini, # Richness index
                                               total_reactions, total_comments, total_shares, # Interaction index
                                               sentiment_score, has_image, has_text, # quality
                                               preds_model_beto_sm_num, preds_model_numeric_sm_num, preds_model_wobeto_sm_num, feat_25common_unigrams, feat_50common_unigrams, # share misinformation
                                               fact_check) %>%
  group_by(identifier, curso, verificacion, curso_verificacion, control, blockid1, WhatsApp) %>% 
  summarise_all(funs(mean, sum), na.rm = T)

sm_data_end <- sm_data_end %>% dplyr::select(identifier_end, curso_end, verificacion_end, curso_verificacion_end, control_end, blockid1_end, WhatsApp_end,
                                             hapaxlegomenon_R_end, hapaxlegomenon_hapax_end, hapax_dislegomenon_h_end, hapax_dislegomenon_S_end, # Readability
                                             word_density_end, readability_index_end, simpsons_index_end, brunets_measure_w_end, szigriszt_pazos_end, # Readability
                                             type_token_ratio_end, shannon_entropy_end, yules_characteristic_K_end, # Readability
                                             prop_persons_end, prop_locations_end, prop_organizations_end, functional_words_count_end, # Lexical index factual
                                             word_count_end, sentence_count_end, find_url_end, has_url_end, count_persons_end, count_locations_end, count_organizations_end, syllable_count_end, # Lexical index length
                                             syllable_count_end, # Lexical index length
                                             flesch_reading_end, fernandez_huerta_end, reading_time_end, crawford_end, gutierrez_polini_end, # Richness index
                                             total_reactions_end, total_comments_end, total_shares_end, # Interaction index
                                             sentiment_score_end, has_image_end, has_text_end, # quality
                                             preds_model_beto_sm_num_end, preds_model_numeric_sm_num_end, preds_model_wobeto_sm_num_end, feat_25common_unigrams_end, feat_50common_unigrams_end, # share misinformation
                                             fact_check_end) %>%
  group_by(identifier_end, curso_end, verificacion_end, curso_verificacion_end, control_end, blockid1_end, WhatsApp_end) %>% 
  summarise_all(funs(mean, sum), na.rm = T)

# Append number of posts
sm_data_base$n_posts <- base_n_socialmedia$n
sm_data_end$n_posts_end <- end_n_socialmedia$n

# filter to only responses with both baseline and endline obs
which(sm_data_base$identifier %!in% sm_data_end$identifier_end)

sm_data_base <- sm_data_base[sm_data_base$identifier %in% sm_data_end$identifier_end, ]
sm_data_end <- sm_data_end[sm_data_end$identifier_end %in% sm_data_base$identifier, ]

# Append dataframes 
final_data <- cbind.data.frame(sm_data_base, sm_data_end)

# change NA or NaNs to actual values (if applicable) ---------------------------
final_data$reading_time_mean[is.nan(final_data$reading_time_mean)] <- 0
final_data$reading_time_end_mean[is.nan(final_data$reading_time_end_mean)] <- 0

final_data$word_density_mean[is.nan(final_data$word_density_mean)] <- 1
final_data$word_density_end_mean[is.nan(final_data$word_density_end_mean)] <- 1


final_data <- final_data %>% mutate_all(~ifelse(is.nan(.), NA, .))

# Richness index ------------------------------------------------------------
rich_base <- final_data[c("hapaxlegomenon_R_mean", "hapaxlegomenon_hapax_mean", "hapax_dislegomenon_h_mean", "hapax_dislegomenon_S_mean",
                          "type_token_ratio_mean", "shannon_entropy_mean", "yules_characteristic_K_mean",
                          "word_density_mean", "simpsons_index_mean", "brunets_measure_w_mean")]
final_data$index_rich_base <- scale(icwIndex(rich_base, revcols = c(2,3,4,6,7,10))[[2]])

rich_end <- final_data[c("hapaxlegomenon_R_end_mean", "hapaxlegomenon_hapax_end_mean", "hapax_dislegomenon_h_end_mean", "hapax_dislegomenon_S_end_mean",
                         "type_token_ratio_end_mean", "shannon_entropy_end_mean", "yules_characteristic_K_end_mean",
                         "word_density_end_mean", "simpsons_index_end_mean", "brunets_measure_w_end_mean")]
final_data$index_rich_end <- scale(icwIndex(rich_end, revcols = c(2,3,4,6,7,10))[[2]])

# Lexical index factual index --------------------------------------------------
lex_factual_base <- final_data[c("prop_persons_mean", "prop_locations_mean", "prop_organizations_mean", "functional_words_count_mean")]
final_data$index_lex_factual_base <- scale(icwIndex(lex_factual_base, revcols = c(4))[[2]])

lex_factual_end <- final_data[c("prop_persons_end_mean", "prop_locations_end_mean", "prop_organizations_end_mean", "functional_words_count_end_mean")]
final_data$index_lex_factual_end <- scale(icwIndex(lex_factual_end, revcols = c(4))[[2]])

# Lexical index length index ---------------------------------------------------
lex_length_base <- final_data[c("word_count_mean", "sentence_count_mean", "find_url_sum", "has_url_sum", "count_persons_sum", "count_locations_sum", "count_organizations_sum",
                                "syllable_count_mean")]
final_data$index_lex_length_base <- scale(icwIndex(lex_length_base)[[2]])

lex_length_end <- final_data[c("word_count_end_mean", "sentence_count_end_mean", "find_url_end_sum", "has_url_end_sum", "count_persons_end_sum", "count_locations_end_sum", "count_organizations_end_sum",
                               "syllable_count_end_mean")]
final_data$index_lex_length_end <- scale(icwIndex(lex_length_end)[[2]])

# Single lexical index ---------------------------------------------------------
lex_base <- final_data[c("prop_persons_mean", "prop_locations_mean", "prop_organizations_mean", "functional_words_count_mean", # factual
                         "word_count_mean", "sentence_count_mean", "find_url_sum", "has_url_sum", "count_persons_sum", "count_locations_sum", "count_organizations_sum", "syllable_count_mean")] # length
final_data$index_lex_base <- scale(icwIndex(lex_base, revcols = c(4))[[2]])

lex_end <- final_data[c("prop_persons_end_mean", "prop_locations_end_mean", "prop_organizations_end_mean", "functional_words_count_end_mean", # factual
                        "word_count_end_mean", "sentence_count_end_mean", "find_url_end_sum", "has_url_end_sum", "count_persons_end_sum", "count_locations_end_sum", "count_organizations_end_sum", "syllable_count_end_mean")] # length
final_data$index_lex_end <- scale(icwIndex(lex_end, revcols = c(4))[[2]])

# Readability index ---------------------------------------------------------------
read_base <- final_data[c("flesch_reading_mean", "fernandez_huerta_mean", "reading_time_mean", "crawford_mean", "gutierrez_polini_mean",
                          "szigriszt_pazos_mean")]
final_data$index_read_base <- scale(icwIndex(read_base, revcols = c(1,2,3,4))[[2]])

read_end <- final_data[c("flesch_reading_end_mean", "fernandez_huerta_end_mean", "reading_time_end_mean", "crawford_end_mean", "gutierrez_polini_end_mean",
                         "szigriszt_pazos_end_mean")]
final_data$index_read_end <- scale(icwIndex(read_end, revcols = c(1,2,3,4))[[2]])

# Interactions index -----------------------------------------------------------
int_base <- final_data[c("total_reactions_sum", "total_comments_sum", "total_shares_sum")]
final_data$index_int_base <- scale(icwIndex(int_base)[[2]])

int_end <- final_data[c("total_reactions_end_sum", "total_comments_end_sum", "total_shares_end_sum")]
final_data$index_int_end <- scale(icwIndex(int_end)[[2]])

# Quality index ----------------------------------------------------------------
quality_base <- final_data[c("find_url_sum", "has_url_sum", "sentiment_score_mean", "has_text_sum", "has_image_sum")]
final_data$index_quality_base <- scale(icwIndex(quality_base, revcols = c(5))[[2]])

quality_end <- final_data[c("find_url_end_sum", "has_url_end_sum", "sentiment_score_end_mean", "has_text_end_sum", "has_image_end_sum")]
final_data$index_quality_end <- scale(icwIndex(quality_end, revcols = c(5))[[2]])

# Share misinformation index
share_mis_base <- final_data[c("preds_model_beto_sm_num_mean", "preds_model_numeric_sm_num_mean", "preds_model_wobeto_sm_num_mean",
                               "feat_25common_unigrams_sum")]
final_data$index_share_mis_base <- scale(icwIndex(share_mis_base)[[2]])

share_mis_end <- final_data[c("preds_model_beto_sm_num_end_mean", "preds_model_numeric_sm_num_end_mean", "preds_model_wobeto_sm_num_end_mean",
                              "feat_25common_unigrams_end_sum")]
final_data$index_share_mis_end <- scale(icwIndex(share_mis_end)[[2]])

# Treatment Effects Setup ------------------------------------------------------
# Demeaned variable
interaction_var <- final_data$WhatsApp
interacted <- c("WhatsApp")

# Label for interacted variable
interaction_name <- c("WhatsApp Delivery")

# Aux for regressions
treatments_separate <- c("curso_verificacion", "curso", "verificacion")
covariates_sep <- c("Course and verification", "Course", "Verification")

covariates_pool <- c("All treatments")

# Interacted labels 
covariates_interacted <- c(covariates_sep, paste(covariates_sep, interaction_name, sep = " x "))

# Regressions ------------------------------------------------------------------
final_data$n_posts_end <- log(final_data$n_posts_end + 1)
final_data$n_posts <- log(final_data$n_posts + 1)
aux <- c("index_share_mis_end",  "fact_check_end_sum")

survey <- final_data

aux_data <- survey[aux]

dep_var <- c("\\shortstack{Share \\\\ misinformation \\\\ index}", 
             "\\shortstack{Share \\\\ fact-checks}")

title <- "Treatment Effects on information shared on social media"

control_list <- list("index_share_mis_base", "fact_check_sum")

omit_var <- c("Constant", "blockid1", interacted, unlist(control_list))

# Create tables and save results
interacted_bool <- c(T,T)
label <- "sm_ate"
size = length(aux) + 1
cm = 7 # length in cm of the note for the table

source('Scripts/General Scripts/ATESocialMedia.R')

cat(final_table, file = "Tables/Appendix/Social Media/sm_ate.txt")

