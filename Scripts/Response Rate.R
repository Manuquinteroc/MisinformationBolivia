# read data

# ------------------------------------------------------------------------------
survey_attrition <- read.csv('Datasets/Final Sets/survey_final_ICW_Attrition.csv')

treatment <- c("Control", "Course and Verification", "Course", "Verification")

survey_control1 <- survey_attrition %>% filter(control == 1  & WhatsApp == 0)
survey_treatment_cv1 <- survey_attrition %>% filter(curso_verificacion == 1 & WhatsApp == 0)
survey_treatment_c1 <- survey_attrition %>% filter(curso == 1 & WhatsApp == 0)
survey_treatment_v1 <- survey_attrition %>% filter(verificacion == 1  & WhatsApp == 0)

survey_control2 <- survey_attrition %>% filter(control == 1  & WhatsApp == 1)
survey_treatment_cv2 <- survey_attrition %>% filter(curso_verificacion == 1 & WhatsApp == 1)
survey_treatment_c2 <- survey_attrition %>% filter(curso == 1 & WhatsApp == 1)
survey_treatment_v2 <- survey_attrition %>% filter(verificacion == 1  & WhatsApp == 1)

# Online course
base_a <- c(dim(survey_control1)[1], dim(survey_treatment_cv1)[1], dim(survey_treatment_c1)[1], dim(survey_treatment_v1)[1])

end_a <- c(sum(survey_control1$complete_both), sum(survey_treatment_cv1$complete_both),
           sum(survey_treatment_c1$complete_both), sum(survey_treatment_v1$complete_both))

response_rate_a <- end_a / base_a

total_a <- c("Total", sum(base_a), sum(end_a), sum(end_a)/sum(base_a))

df_a <- cbind.data.frame(treatment, base_a, end_a, response_rate_a)

df_a <- rbind.data.frame(df_a, total_a)

# WhatsApp course
base_w <- c(dim(survey_control2)[1], dim(survey_treatment_cv2)[1], dim(survey_treatment_c2)[1], dim(survey_treatment_v2)[1])

end_w <- c(sum(survey_control2$complete_both), sum(survey_treatment_cv2$complete_both),
           sum(survey_treatment_c2$complete_both), sum(survey_treatment_v2$complete_both))

response_rate_w <- end_w / base_w

total_w <- c("Total", sum(base_w), sum(end_w), sum(end_w)/sum(base_w))

df_w <- cbind.data.frame(treatment, base_w, end_w, response_rate_w)

df_w <- rbind.data.frame(df_w, total_w)

# Legends
names_a <- c("Sample Course", "delivered", "online", "", "")
names_w <- c("Sample Course", "delivered via", "WhatsApp", "", "")

names(df_a) <- c("Treatment", "Baseline", "Endline", "Response rate")
names(df_w) <- c("Treatment", "Baseline", "Endline", "Response rate")

aux_data <- rbind.data.frame(df_a, df_w)

table_aux <- cbind.data.frame(c(names_a, names_w), aux_data)

names(final)[1] <- "Sample"

table_aux$`Response rate` <- as.numeric(table_aux$`Response rate`)

# Create TeX table 
M <- matrix(c(rep(0,10), rep(0,10), rep(0,10), rep(0,10), rep(0,10), rep(2, 10)), nrow = 10)

table_aux <- xtable(table_aux, 
                    digits = M, 
                    label = "table:ResponseRate",
                    align = "cclccc",
                    caption = "Response rates by treatment assignment and by sample")

names(table_aux)[1] <- "\\multicolumn{1}{>{\\centering}p{3cm}}{Sample}"
names(table_aux)[2] <- "\\multicolumn{1}{>{\\centering}p{3cm}}{Treatment}"
names(table_aux)[3] <- "\\multicolumn{1}{>{\\centering}p{2cm}}{Baseline}"
names(table_aux)[4] <- "\\multicolumn{1}{>{\\centering}p{2cm}}{Endline}"
names(table_aux)[5] <- "\\multicolumn{1}{>{\\centering}p{2cm}}{Response rate}"

table_aux <- print(table_aux, 
                   size = "footnotesize",
                   table.placement = "H",
                   caption.placement = "top",
                   include.rownames = FALSE,
                   comment = FALSE, 
                   sanitize.colnames.function = function(x){x},
                   hline.after = c(-1, 0, 4, 5, 9, 10))


cat(table_aux, file = "Tables/Appendix/Responses.txt")

