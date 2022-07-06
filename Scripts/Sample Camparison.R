# Make experimental sample comparison with Americas Barometer 2021 and LatinBarometer 2020

# ------------------------------------------------------------------------------
# Read Americas Barometer data 
americas <- read_dta('Datasets/Bolivia Sample/BOL_2021_LAPOP_AmericasBarometer_v1.2_w.dta') %>% 
  dplyr::rename(education = edr, edad = q2)

# Gender (1 = female, 0 = otherwise)
americas$q1tb[which(americas$q1tb == 1 | americas$q1tb == 3)] <- 0
americas$q1tb[which(americas$q1tb == 2)] <- 1

# Age 
americas$Age_strat <- cut(as.integer(americas$edad), breaks = c(17, 24, 34, 44, 54, 64, 150),
                          labels = c("18-24","25-34","35-44","45-54","55-64","65+"),
                          ordered_result = TRUE)

# Education (0 = none, 1 = some primary or complete, 2 = some secundary or complete, 3 = some tertiary or complete)
americas$education[is.na(americas$education)] <- 0

# Americas Barometer restricted to internet users
americas_restricted <- americas %>% filter(r18n == 1)

# Filter to variables of  interest
americas <- americas %>% dplyr::select(q1tb, edad, wt, education) 
americas_restricted <- americas_restricted %>% dplyr::select(q1tb, edad, wt, education) 

# Create Postratification table
postrat_americas <- americas_restricted
postrat_americas$Age <- ifelse(postrat_americas$edad <= 35, "18-35", "36+")
postrat_americas$Edu <- ifelse(postrat_americas$education <= 2, "Below Tertiary", "Above Tertiary")

postrat_americas <- postrat_americas %>% group_by(Age, Edu) %>% summarise(reach_a = n()) 
postrat_americas$freq_a <- postrat_americas$reach_a / sum(postrat_americas$reach_a)

# Read LatinBarometer ----------------------------------------------------------
latin <- read_dta('Datasets/Bolivia Sample/Latinobarometro_2020_Esp_Stata_v1_0.dta') %>% 
  filter(edad >= 18) %>% dplyr::rename(education = reeduc_1)

latin$wt <- as.numeric(latin$wt)

# Gender
latin$sexo[which(latin$sexo == 1)] <- 0
latin$sexo[which(latin$sexo == 2)] <- 1

# Age
latin$Age_strat <- cut(as.integer(latin$edad), breaks = c(17, 24, 34, 44, 54, 64, 150),
                          labels = c("18-24","25-34","35-44","45-54","55-64","65+"),
                          ordered_result = TRUE)

# Education
latin$education[which(latin$education <= 1)] <- 0
latin$education[which(latin$education == 2 | latin$education == 3)] <- 1
latin$education[which(latin$education == 4 | latin$education == 5)] <- 2
latin$education[which(latin$education > 5)] <- 3

# Latin Baromter restricted to internet users
latin_restricted <- latin %>% filter(m_nc == 1)

# Filter to variables of interest
latin <- latin %>% dplyr::select(edad, sexo, education, wt)
latin_restricted <- latin_restricted %>% dplyr::select(edad, sexo, education, wt)

# Create Postratification table
postrat_latin <- latin_restricted
postrat_latin$Age <- ifelse(postrat_latin$edad <= 35, "18-35", "36+")
postrat_latin$Edu <- ifelse(postrat_latin$education <= 2, "Below Tertiary", "Above Tertiary")

postrat_latin <- postrat_latin %>% group_by(Age, Edu) %>% summarise(reach_l = n()) 
postrat_latin$freq_l <- postrat_latin$reach_l / sum(postrat_latin$reach_l)

# Experimental Sample ----------------------------------------------------------
experimental <- read.csv('Datasets/Final Sets/survey_final_ICW.csv') %>% 
  dplyr::select(edad, gender, education)

# Age
experimental$Age_strat <- cut(as.integer(experimental$edad), breaks = c(17, 24, 34, 44, 54, 64, 150),
                       labels = c("18-24","25-34","35-44","45-54","55-64","65+"),
                       ordered_result = TRUE)
# Education
experimental$education[which(experimental$education <= 1)] <- 0
experimental$education[which(experimental$education >= 2 & experimental$education < 4)] <- 1
experimental$education[which(experimental$education >= 4 & experimental$education < 6)] <- 2
experimental$education[which(experimental$education >= 6)] <- 3

# Create Postratification table
postrat_survey <- experimental
postrat_survey$Age <- ifelse(postrat_survey$edad <= 35, "18-35", "36+")
postrat_survey$Edu <- ifelse(postrat_survey$education <= 2, "Below Tertiary", "Above Tertiary")

postrat_survey <- postrat_survey %>% group_by(Age, Edu) %>% summarise(reach_s = n()) 
postrat_survey$freq_s <- postrat_survey$reach_s / sum(postrat_survey$reach_s)

# Append surveys
aux <- left_join(postrat_americas, postrat_latin, by = c("Age", "Edu"))
postrat_merged <- left_join(aux, postrat_survey, by = c("Age", "Edu"))

# Create weights 
postrat_merged$weight1 <- postrat_merged$freq_a / postrat_merged$freq_s
postrat_merged$weight2 <- postrat_merged$freq_l / postrat_merged$freq_s

# Append weights to survey dataset
aux <- c("Age", "Edu", "weight1", "weight2")

# Read survey dataset and construct matching variables
survey <- read.csv('Datasets/Final Sets/survey_final_ICW.csv')
survey$Age <- ifelse(survey$edad <= 35, "18-35", "36+")
survey$Edu <- ifelse(survey$education <= 2, "Below Tertiary", "Above Tertiary")

# Merge
merged <- left_join(survey, postrat_merged[aux], by = c("Age", "Edu"))

# Read survey dataset for attrition and construct matching variables
survey_attrition <- read.csv('Datasets/Final Sets/survey_final_ICW_Attrition.csv')
survey_attrition$Age <- ifelse(survey_attrition$edad <= 35, "18-35", "36+")
survey_attrition$Edu <- ifelse(survey_attrition$education <= 2, "Below Tertiary", "Above Tertiary")

# Merge
merged_attrition <- left_join(survey_attrition, postrat_merged[aux], by = c("Age", "Edu"))

# Save dataset with new weights
write.csv(merged, "Datasets/Reweight/survey_final_reweight.csv")
write.csv(merged_attrition, "Datasets/Reweight/survey_final_reweight_attrition.csv")

# Figures ----------------------------------------------------------------------
# Setup
text.11 <- element_text(color = "black", size = 11)
final.text.11 <- element_text(color = "black", size = 12, hjust = 0.5, family="Arial")
text.9 <- element_text(color = "black", size = 9)
text.12 <- element_text(color = "black", size = 12)
final.text.14 <- element_text(color = "black", size = 14, hjust = 0.5, family="Arial")
legend.text.18 <- element_text(color = "black", size = 18, family="Arial")

th <- theme(strip.text.x = final.text.11,
            strip.placement = "inside",
            strip.background = element_rect(colour = "black", fill = "white"),
            axis.text.x = element_blank(),
            axis.text.y = text.11,
            legend.text = final.text.11,
            legend.title = final.text.11,
            axis.text = final.text.11, 
            panel.grid.minor.x = element_blank(),
            axis.ticks.length.x = unit(.25, "cm"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            axis.title.x=element_blank(), 
            axis.title.y=element_blank(),
            panel.border = element_blank())

legend_guide <- guides(color = guide_legend(title.position = "top", title.hjust = 0.5))

legend_guide1 <- guides(color = guide_legend(title.position = "top", title.hjust = 0.5))
legend_guide2 <- guides(linetype = guide_legend(title.position = "top", title.hjust = 0.5))

colors <- c("#E7B800", "#D55E00","#0072B2")

# set the background color for all graphs
folder <- c('Figures')

fill_all <- c("white")

# Age --------------------------------------------------------------------------
FigureA5_a <- ggplot() +
  geom_density(aes(edad, color = "Americas Barometer"), size = 1, alpha = .4, data = americas) +
  geom_density(aes(edad, color = "Latin Barometer"),  size = 1, alpha = .4, data = latin) +
  geom_density(aes(edad, color = "Experimental"), size = 1, alpha = .4, data = experimental) +
  th +
  ylab("Density") + 
  scale_x_continuous(limits = c(18, 80)) +
  theme(legend.position = c(0.75,0.8), legend.text = legend.text.18, legend.title = legend.text.18,
        axis.text.x = final.text.14, axis.text.y = final.text.14) + 
  scale_color_manual(name = "Dataset", 
                     breaks =  c("Americas Barometer","Latin Barometer", "Experimental"), # force order of labels and colors
                     labels = c("Americas Barometer","Latin Barometer", "Experimental"),
                     values = colors)  + 
  theme(legend.key.height = unit(0.5, "cm")) +
  geom_line() +
  guides(colour = guide_legend(override.aes = list(size= 10,linetype = 1))) +
  theme(legend.title = element_text(face = "bold"))


ggsave(FigureA5_a, path = folder, filename = "FigureA5_a.pdf", device = cairo_pdf,
       width=7, height=5,dpi=300)

# Gender -----------------------------------------------------------------------
data_aux <- as.data.frame(cbind(weighted.mean(americas$q1tb, w = americas$wt), 
                                weighted.mean(latin$sexo, w = latin$wt), 
                                mean(experimental$gender)))

colnames(data_aux) <- c("Americas", "Latin", "Experimental")
rownames(data_aux) <- c("Female")

FigureA5_c <- ggplot(data_aux, aes(rownames(data_aux), Americas)) +
  geom_col(aes(color = "Americas"),  size = 1, fill = fill_all, 
           width = 0.2,
           position = position_nudge(x = -0.21)) +
  geom_col(aes(y = Latin, color = "Latin"),  size = 1, fill = fill_all, 
           width = 0.2,
           position = position_nudge(x = 0.0)) +
  geom_col(aes(y = Experimental, color = "Experimental"),  size = 1, fill = fill_all, 
           width = 0.2,
           position = position_nudge(x = 0.21)) +
  th +
  scale_color_manual(name = "Dataset", 
                     breaks = c("Americas", "Latin", "Experimental"), 
                     labels = c("Americas Barometer", "Latin Barometer", "Experimental"),
                     values= colors) +
  theme(legend.position = "none", legend.text = legend.text.18, legend.title = legend.text.18,
        axis.text.x = final.text.14, axis.text.y = final.text.14)

ggsave(FigureA5_c, path = folder, filename = "FigureA5_c.pdf", device = cairo_pdf,
       width=7, height=5,dpi=300)

# Education --------------------------------------------------------------------
data_aux <- as.data.frame(cbind(
  fre(americas$education,  weight = americas$wt)[,3][1:4], 
  fre(latin$education, w = latin$wt)[,3][1:4],
  unlist(c(0,fre(experimental$education)[,3][1:3]))))

colnames(data_aux) <- c("Americas", "Latin", "Experimental")
rownames(data_aux) <- c("None", "Some \nPrimary", "Some \nSecundary", "Some \nTertiary")


FigureA5_e <- ggplot(data_aux, aes(rownames(data_aux), Americas)) +
  geom_col(aes(color = "Americas"),  size = 1, fill = fill_all, 
           width = 0.2,
           position = position_nudge(x = -0.21)) +
  geom_col(aes(y = Latin, color = "Latin"),  size = 1, fill = fill_all, 
           width = 0.2,
           position = position_nudge(x = 0.0)) +
  geom_col(aes(y = Experimental, color = "Experimental"),  size = 1, fill = fill_all, 
           width = 0.2,
           position = position_nudge(x = 0.21)) +
  th +
  scale_color_manual(name = "Dataset",  
                     breaks = c("Americas", "Latin", "Experimental"), 
                     labels = c("Americas Barometer","Experimental","Latin Barometer"),
                     values= colors) +
  theme(legend.position = "none", legend.text = legend.text.18, legend.title = legend.text.18,
        axis.text.x = final.text.14, axis.text.y = final.text.14)

ggsave(FigureA5_e, path = folder, filename = "FigureA5_e.pdf", device = cairo_pdf,
       width=7, height=5,dpi=300)


# -----------------------------------------------------------------------------
# Same analysis for internet users 

# Age --------------------------------------------------------------------------
FigureA5_b <- ggplot() +
  geom_density(aes(edad, color = "Americas Barometer"), size = 1, alpha = .4, data = americas_restricted) +
  geom_density(aes(edad, color = "Latin Barometer"),  size = 1, alpha = .4, data = latin_restricted) +
  geom_density(aes(edad, color = "Experimental"), size = 1, alpha = .4, data = experimental) +
  th +
  ylab("Density") + 
  scale_x_continuous(limits = c(18, 80)) +
  theme(legend.position = c(0.75,0.8), legend.text = legend.text.18, legend.title = legend.text.18,
        axis.text.x = final.text.14, axis.text.y = final.text.14) + 
  scale_color_manual(name = "Dataset",                      
                     breaks =  c("Americas Barometer","Latin Barometer", "Experimental"), # force order of labels and colors
                     labels = c("Americas Barometer \n internet users", "Latin Barometer \n internet users", "Experimental"),
                     values = colors)  + 
  theme(legend.key.height = unit(0.5, "cm")) +
  geom_line() +
  guides(colour = guide_legend(override.aes = list(size= 10,linetype = 1))) +
  theme(legend.title = element_text(face = "bold"))


ggsave(FigureA5_b, path = folder, filename = "FigureA5_b.pdf", device = cairo_pdf,
       width=7, height=5,dpi=300)

# Gender -----------------------------------------------------------------------
data_aux <- as.data.frame(cbind(weighted.mean(americas_restricted$q1tb, w = americas_restricted$wt), 
                                weighted.mean(latin_restricted$sexo, w = latin_restricted$wt), 
                                mean(experimental$gender)))

colnames(data_aux) <- c("Americas", "Latin", "Experimental")
rownames(data_aux) <- c("Female")

FigureA5_d <- ggplot(data_aux, aes(rownames(data_aux), Americas)) +
  geom_col(aes(color = "Americas"),  size = 1, fill = fill_all, 
           width = 0.2,
           position = position_nudge(x = -0.21)) +
  geom_col(aes(y = Latin, color = "Latin"),  size = 1, fill = fill_all, 
           width = 0.2,
           position = position_nudge(x = 0.0)) +
  geom_col(aes(y = Experimental, color = "Experimental"),  size = 1, fill = fill_all, 
           width = 0.2,
           position = position_nudge(x = 0.21)) +
  th +
  scale_color_manual(name = "Dataset",  
                     breaks = c("Americas", "Latin", "Experimental"), 
                     labels = c("Americas Barometer \n internet users", "Latin Barometer \n internet users", "Experimental"),
                     values= colors) +
  theme(legend.position = "none", legend.text = legend.text.18, legend.title = legend.text.18,
        axis.text.x = final.text.14, axis.text.y = final.text.14)

ggsave(FigureA5_d, path = folder, filename = "FigureA5_d.pdf", device = cairo_pdf,
       width=7, height=5,dpi=300)

# Education --------------------------------------------------------------------
data_aux <- as.data.frame(cbind(
  fre(americas_restricted$education,  weight = americas_restricted$wt)[,3][1:4], 
  fre(latin_restricted$education, w = latin_restricted$wt)[,3][1:4],
  unlist(c(0,fre(experimental$education)[,3][1:3]))))

colnames(data_aux) <- c("Americas", "Latin", "Experimental")
rownames(data_aux) <- c("None", "Some \nPrimary", "Some \nSecundary", "Some \nTertiary")


FigureA5_f <- ggplot(data_aux, aes(rownames(data_aux), Americas)) +
  geom_col(aes(color = "Americas"),  size = 1, fill = fill_all, 
           width = 0.2,
           position = position_nudge(x = -0.21)) +
  geom_col(aes(y = Latin, color = "Latin"),  size = 1, fill = fill_all, 
           width = 0.2,
           position = position_nudge(x = 0.0)) +
  geom_col(aes(y = Experimental, color = "Experimental"),  size = 1, fill = fill_all, 
           width = 0.2,
           position = position_nudge(x = 0.21)) +
  th +
  scale_color_manual(name = "Dataset",  
                     breaks = c("Americas", "Latin", "Experimental"), 
                     labels = c("Americas Barometer \n internet users", 
                                                   "Latin Barometer \n internet users",
                                                   "Experimental"),
                     values= colors) +
  theme(legend.position = "none", legend.text = legend.text.18, legend.title = legend.text.18,
        axis.text.x = final.text.14, axis.text.y = final.text.14)

ggsave(FigureA5_f, path = folder, filename = "FigureA5_f.pdf", device = cairo_pdf,
       width=7, height=5,dpi=300)




# Table Summary Statistics -----------------------------------------------------

# Generate Age dataframe
means <- c(weighted.mean(americas$edad, w = americas$wt),
           weighted.mean(latin$edad, w = latin$wt),
           weighted.mean(americas_restricted$edad, w = americas_restricted$wt),
           weighted.mean(latin_restricted$edad, w = latin_restricted$wt),
           mean(experimental$edad))

sd <- c(weightedSd(americas$edad, w = americas$wt),
        weightedSd(latin$edad, w = latin$wt),
        weightedSd(americas_restricted$edad, w = americas_restricted$wt),
        weightedSd(latin_restricted$edad, w = latin_restricted$wt),
        sd(experimental$edad))

obs <- c(length(americas$edad),
         length(latin$edad),
         length(americas_restricted$edad),
         length(latin_restricted$edad),
         length(experimental$edad))

summary_age <- round(as.data.frame(rbind(means, sd, obs)), 3)

colnames(summary_age) <- c("Americas Barometer", "Latin Barometer",
                           "Americas Barometer \n internet Users",  "Latin Barometer \n internet Users",
                           "Experimental")
rownames(summary_age) <- c( "Mean", "Standard Deviation", "Observations")


# Generate Gender dataframe
means <- c(weighted.mean(americas$q1tb, w = americas$wt),
           weighted.mean(latin$sexo, w = latin$wt),
           weighted.mean(americas_restricted$q1tb, w = americas_restricted$wt),
           weighted.mean(latin_restricted$sexo, w = latin_restricted$wt),
           mean(experimental$gender))

sd <- c(weightedSd(americas$q1tb, w = americas$wt),
        weightedSd(latin$sexo, w = latin$wt),
        weightedSd(americas_restricted$q1tb, w = americas_restricted$wt),
        weightedSd(latin_restricted$sexo, w = latin_restricted$wt),
        sd(experimental$gender))

obs <- c(length(americas$q1tb),
         length(latin$sexo),
         length(americas_restricted$q1tb),
         length(latin_restricted$sexo),
         length(experimental$gender))

summary_gender <- round(as.data.frame(rbind(means, sd, obs)), 3)

colnames(summary_gender) <- c("Americas Barometer", "Latin Barometer",
                           "Americas Barometer \n internet Users",  "Latin Barometer \n internet Users",
                           "Experimental")
rownames(summary_gender) <- c( "Mean", "Standard Deviation", "Observations")


# Generate Education dataframe
means <- c(weighted.mean(americas$education, w = americas$wt),
           weighted.mean(latin$education, w = latin$wt),
           weighted.mean(americas_restricted$education, w = americas_restricted$wt),
           weighted.mean(latin_restricted$education, w = latin_restricted$wt),
           mean(experimental$education))

sd <- c(weightedSd(americas$education, w = americas$wt),
        weightedSd(latin$education, w = latin$wt),
        weightedSd(americas_restricted$education, w = americas_restricted$wt),
        weightedSd(latin_restricted$education, w = latin_restricted$wt),
        sd(experimental$education))

obs <- c(length(americas$education),
         length(latin$education),
         length(americas_restricted$education),
         length(latin_restricted$education),
         length(experimental$education))

summary_education <- round(as.data.frame(rbind(means, sd, obs)), 3)

colnames(summary_education) <- c("Americas Barometer", "Latin Barometer",
                              "Americas Barometer \n internet Users",  "Latin Barometer \n internet Users",
                              "Experimental")
rownames(summary_education) <- c( "Mean", "Standard Deviation", "Observations")


# Create final table for appended summary statistics
table_aux <- rbind.data.frame(summary_age,
                          summary_gender, 
                          summary_education)

names <- c("Age", "", "",
           "Gender", "", "", 
           "Education", "", "")

table_aux <- cbind.data.frame(names, table_aux)

colnames(table_aux) <- c("", "Americas Barometer sample", "Latin Barometer sample",
                         "Americas Barometer internet users sample", "Latin Barometer internet users sample",
                         "Experimental sample")

M = matrix(c(rep(0,3),rep(0,3), rep(c(3,3,0), 5)), nrow = 3)
M <- matrix(rep(t(M), 3), ncol = ncol(M), byrow = TRUE)

table_aux <- xtable(table_aux, 
                    label = "table:SampleComparison",
                   digits = M, 
                   align = "clccccc",
                   caption = "Summary statistics of comparable demographics in the restricted and unrestricted Americas Barometer sample and the Latin Barometer sample to internet users, and the experimental sample")

names(table_aux)[2] <- "\\multicolumn{1}{>{\\centering}p{2.5cm}}{Americas Barometer sample}"
names(table_aux)[3] <- "\\multicolumn{1}{>{\\centering}p{2.5cm}}{Latin Barometer sample}"
names(table_aux)[4] <- "\\multicolumn{1}{>{\\centering}p{2.5cm}}{Americas Barometer internet users sample}"
names(table_aux)[5] <- "\\multicolumn{1}{>{\\centering}p{2.5cm}}{Latin Barometer internet users sample}"
names(table_aux)[6] <- "\\multicolumn{1}{>{\\centering}p{2.5cm}}{Experimental sample}"

table_aux <- print(table_aux, 
                   size = "scriptsize",
                  table.placement = "H",
                  caption.placement = "top",
                  include.rownames = FALSE,
                  comment = FALSE, 
                  sanitize.colnames.function=function(x){x},
                  hline.after = c(-1, seq(0,9, by = 3)))

table_aux <- resizebox(table_aux)
note <- c("For every variable, each row shows the mean, standard deviation, and number of observations.
          The Americas Barometer sample is for the year 2021 and the Latin Barometer sample for the year 2020. 
          The Americas Baromoter restricted to internet users and the Latin Barometer restricted to internet users are 
          the subset of respondents that have connection or a contract of internet at home.")
table_aux <- add_notes(table_aux, note)

# Save table
cat(table_aux,"\n", file = 'Tables/Appendix/SampleComparison.tex')

