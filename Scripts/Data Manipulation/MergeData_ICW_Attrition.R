# Script to merge the attrition WhatsApp and Alfabetizacion dataset

# ------------------------------------------------------------------------------
# Read data
survey_wa <- read.csv('Datasets/WhatsApp/survey_attrition_final.csv')
survey_alf <- read.csv('Datasets/Alfabetizacion/survey_attrition_final.csv')

# Fix data and merge
source('Scripts/General Scripts/CommonFixes_Merge_Attrition.R')
  
# Discretizacion of demographics variables -------------------------------------
# Create indicators for Age intervals
aux <- fastDummies::dummy_cols(cut(final$edad, breaks = c(0, 25.1,35.1,45.15,100)), remove_selected_columns = T)
names(aux) <- c("edad25", "edad35", "edad45", "edad55")
final <- cbind.data.frame(final, aux)

# For Education
aux <- fastDummies::dummy_cols(cut(final$education, breaks = c(0,7,8,9,12)), remove_selected_columns = T)
names(aux) <- c("edu7", "edu8", "edu9", "edu10")
final <- cbind.data.frame(final, aux)

# ------------------------------------------------------------------------------
# Consumption of news from different sources 
consumption_baseline <- final[paste0("freq_tipos_", 1:6)]
final$consumption_baseline <- scale(icwIndex(consumption_baseline, revcols = c(4:6))[[2]])

# Trust news from different sources 
trust_baseline <- final[paste0("confia_", 1:6)]
final$trust_baseline <-  scale(icwIndex(trust_baseline,  revcols = c(4:6))[[2]])

# Believe news from different sources 
false_baseline <- final[paste0("case", c(1,3), "_hyp_2")]
final$false_baseline <-  scale(icwIndex(false_baseline)[[2]])

# Main characteristics to identify a fake news
right <- rowSums(final[paste0("characts_false_", c(1,4,5,7,9))])/5
wrong <- rowSums(final[paste0("characts_false_", c(2,3,6,8,10))])/5
final$characts_index <- right - wrong

# Main means to spread fake news
knowledge_identify_baseline <- final[c(paste0("knowledge_plat_", 1:6), "knowledge_verify", "characts_index")]
final$knowledge_identify_baseline <-  scale(icwIndex(knowledge_identify_baseline,  revcols = c(1:3))[[2]])

# Bolivian problems 
index_mec_importance_baseline <- final[c(paste0("problemas_", 1:6))]
final$index_mec_importance_baseline <- scale(icwIndex(index_mec_importance_baseline)[[2]])

# Main ways to verify the veracity of news
right <- rowSums(final[paste0("forms_verify_", c(1,4,5))])/3
wrong <- rowSums(final[paste0("forms_verify_", c(2,3,6))])/3
final$forms_index <- right - wrong

# Knowledge to verify
know_verify <- final[c("know_to_verify", "forms_index", "know_fact_checkers_sum")]
final$index_know_verify_baseline <- scale(icwIndex(know_verify)[[2]])

# Index of Sharing behavior for hypothetical scenarios
hyp_sharing <- final[paste0("case", c(1,3), "_hyp_1")]
final$hyp_sharing_index <- scale(icwIndex(hyp_sharing)[[2]])

# Sharing index 
sha_beh_baseline <- final[c("freq_desc_comp", paste0("case", c(1,3), "_hyp_1"))]
final$index_sharing_baseline <- scale(icwIndex(sha_beh_baseline, revcols = c(2,3))[[2]])

# Verifying behavior index
ver_beh_baseline <- final[c("freq_veri_compar")]
final$ver_beh_baseline <- scale(icwIndex(ver_beh_baseline)[[2]])

# Trust + False index at baseline (2nd option)
false_trust_baseline <- final[c(paste0("case", c(1,3), "_hyp_2"), # false
                                paste0("confia_", 1:6))] #trust

final$index_false_trust_baseline <- scale(icwIndex(false_trust_baseline, revcols = c(6:8))[[2]]) # change false signs

# Endline Indexes --------------------------------------------------------------
# Consumption of news from different sources (at the end with other variables)
# Trust news from different sources 
trust_end <- final[paste0("confia_", c(1:3,4:6), "_end")] # 9 is WhatsApp org, alfabetizacion is 4:6
final$trust_end <- scale(icwIndex(trust_end, revcols = c(4:6))[[2]])

# Believe news from different sources 
false_end <- final[c(paste0("freq_plat_false_", c(1:3, 4:6), "_end"), paste0("case", c(1,3), "_hyp_2_end"))] # 9 is WhatsApp org, alfabetizacion is 4:6
final$false_end <- scale(icwIndex(false_end, revcols = c(1,2,3))[[2]])

# Merge false and trust indexes (as a second option)
false_trust <- final[c(paste0("freq_plat_false_", c(1:3, 4:6), "_end"), paste0("case", c(1,3), "_hyp_2_end"),
                       paste0("confia_", c(1:3, 4:6), "_end"))]
final$index_false_trust <- scale(icwIndex(false_trust, revcols = c(4:6,12:14))[[2]])

# Main means to spread fake news (this one is below with the rest of knowledge to identify variables)

# Bolivian problems 
index_mec_importance_end <- final[c(paste0("problemas_", 1:6, "_end"))] 
final$index_mec_importance_end <- scale(icwIndex(index_mec_importance_end)[[2]])

# Main characteristics to identify a fake news
right <- rowSums(final[paste0("characts_false_", c(1,4,5,7,9), "_end")])/5
wrong <- rowSums(final[paste0("characts_false_", c(2,3,6,8,10), "_end")])/5
final$characts_index_end <- right - wrong

# Main ways to verify the veracity of news
right <- rowSums(final[paste0("forms_verify_", c(1,4,5), "_end")])/3
wrong <- rowSums(final[paste0("forms_verify_", c(2,3,6), "_end")])/3
final$forms_index_end <- right - wrong

# Index of verifying behavior for hypothetical scenarios

# ------------------------------------------------------------------------------
# Mechanism 
mech_attention <- final[c("freq_analyze_end")]
final$index_mec_attention <- scale(icwIndex(mech_attention)[[2]])

# Knowledge to identify
#knowledge_identify_end <- final[paste0("knowledge_plat_", c(1:3, 11, 4:6), "_end")]  # 11 is WhatsApp org, alfabetizacion is 4:6
#final$knowledge_identify_end <- indexes_2(knowledge_identify_end, c(1,1,1,1,-1,-1,-1))
know_identify <- final[c("knowledge_verify_end", "characts_index_end", "recent_cases_sum_end",
                         paste0("knowledge_plat_", c(1:3, 4:6), "_end"))]


final$index_know_identify <- scale(icwIndex(know_identify,  revcols = c(4:6))[[2]])

# Knowledge to verify
know_verify <- final[c("know_to_verify_end", "forms_index_end", "know_fact_checkers_sum_end")]
final$index_know_verify <- scale(icwIndex(know_verify)[[2]])

# Consumption
#consumption_end <- final[paste0("freq_tipos_", c(1:3, 9, 4:8), "_end")] # 9 is WhatsApp org, alfabetizacion is 4:6
#final$consumption_end <- indexes_2(consumption_end, c(-1,-1,-1,-1,1,1,1,1,1))
consumption <- final[c(paste0("freq_tipos_", c(1:3, 4:6), "_end"))]
final$index_consumption <- scale(icwIndex(consumption,  revcols = c(4:6))[[2]])

# Sharing behavior
sha_beh <- final[c("freq_desc_comp_end", "cree_falsa_do_index_end", paste0("case", c(1,3), "_hyp_1_end"))]
final$index_sharing <- scale(icwIndex(sha_beh, revcols = c(2,3,4))[[2]])

# Verifying behavior
ver_beh <- final[c("freq_veri_compar_end")]
final$index_verifying <- scale(icwIndex(ver_beh)[[2]])

# Additional sub-indexes: traditional and social media sources -----------------
# Likelihood of false
false_end1 <- final[c(paste0("freq_plat_false_", c(1:3), "_end"))]
false_end2 <- final[c(paste0("freq_plat_false_", c(4:6), "_end"), paste0("case", c(1,3), "_hyp_2_end"))]
final$false_traditional_end <- scale(icwIndex(false_end1, revcols = c(1,2,3))[[2]])
final$false_social_end <- scale(icwIndex(false_end2)[[2]])

# Trust baseline
trust_baseline1 <- final[paste0("confia_", 1:3)]
trust_baseline2 <- final[paste0("confia_", 4:6)]
final$trust_traditional_baseline <- scale(icwIndex(trust_baseline1)[[2]])
final$trust_social_baseline <- scale(icwIndex(trust_baseline2, revcols = c(1,2,3))[[2]])

# Trust endline
trust_end1 <- final[paste0("confia_", c(1:3), "_end")] 
trust_end2 <- final[paste0("confia_", c(4:6), "_end")]
final$trust_traditional_end <- scale(icwIndex(trust_end1)[[2]])
final$trust_social_end <- scale(icwIndex(trust_end2, revcols = c(1,2,3))[[2]])

# Consumption baseline
consumption_baseline1 <- final[paste0("freq_tipos_", 1:3)]
consumption_baseline2 <- final[paste0("freq_tipos_", 4:6)]
final$consumption_traditional_baseline <- scale(icwIndex(consumption_baseline1)[[2]])
final$consumption_social_baseline <- scale(icwIndex(consumption_baseline2, revcols = c(1,2,3))[[2]])

# Consumption endline
consumption_end1 <- final[paste0("freq_tipos_", 1:3, "_end")]
consumption_end2 <- final[paste0("freq_tipos_", 4:6, "_end")]
final$consumption_traditional_end <- scale(icwIndex(consumption_end1)[[2]])
final$consumption_social_end <- scale(icwIndex(consumption_end2, revcols = c(1,2,3))[[2]])

# Write final dataset for analysis
write.csv(final, 'Datasets/Final sets/survey_final_ICW_Attrition.csv', row.names = F)

