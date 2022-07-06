# Analyzing COVID-19 variables from WhatsApp sample

# Read WhatsApp data 
survey <- read.csv('Datasets/WhatsApp/survey_final.csv')

# Read ICW function
source("Scripts/General Scripts/ICW_function.R")

# Create COVID-19 index --------------------------------------------------------
covid_data <- survey[c("affirm_covid_end", "affirm_lockdown_end", "agree_vaccine_end", "trust_vaccine_end")]
survey$covid_index <- scale(icwIndex(covid_data,  revcols = c(1:2))[[2]])

# Recode continuing suscription to yes or no (1,0)
survey$suscribe_whats_end <- ifelse(survey$suscribe_whats_end == 4,1,0)

# Run regressions --------------------------------------------------------------
treatments_separate <- c("curso_verificacion", "curso", "verificacion")

aux <- c("covid_index", "affirm_covid_end", "affirm_lockdown_end", "agree_vaccine_end", "trust_vaccine_end", "suscribe_whats_end")
aux_data <- survey[aux]

lm_list_ols <- list()
count <- 1

for (x in aux) {
  
  fmla1 <- as.formula(paste0(x, "~ ", paste(treatments_separate, collapse = " + "), " + factor(blockid1)"))
  nam1 <- paste("lm_", count, "_ols", sep = "")
  assign(nam1, lm(fmla1, data = survey))
  
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  
  count <- count + 1
}

# Robust s.e.
new_se <- lapply(lm_list_ols, robust_se)

# Additional statistics
treated_means <- round(colMeans(aux_data[which(survey$control == 0),], na.rm = T), 3)
means <- round(colMeans(aux_data, na.rm = T), 3)
sds <- round(colSds(as.matrix(aux_data), na.rm = T), 3)

maximums <- round(sapply(aux_data, max, na.rm = T), 1)
minimums <- round(sapply(aux_data, min, na.rm = T), 1)
range <- paste0("[", minimums, ",", maximums, "]")

# Stargazer aux
covariates_sep <- c("Course and verification", "Course", "Verification")

dep_var <- c("\\shortstack{COVID-19 index \\\\ of (-1,-1,1,1)}",
             "\\shortstack{COVID-19 is not\\\\ a real and \\\\serious problem\\\\ in Bolivia}",
             "\\shortstack{Confinements were\\\\ not necessary \\\\and economically \\\\unjustifiable}",
             "\\shortstack{Would you take \\\\ a COVID-19\\\\ vaccine?}",
             "\\shortstack{Do you think \\\\COVID-19 vaccines \\\\in Bolivia are \\\\ safe?}",
             "\\shortstack{Would like to \\\\ remain on \\\\ Chequea Bolivia's \\\\ WhatsApp list}")

title <- "Treatment effects on COVID-19 variables and treatment continuatiuon in the WhatsApp sample"
omit_var <- c("Constant", "blockid1")

size <- length(aux) + 1
cm <- 16

# Generate table ---------------------------------------------------------------
table1 <- stargazer(lm_list_ols,
                    se = new_se,
                    label = "table:covid19",
                    header = FALSE,
                    font.size="tiny",
                    dep.var.caption = "",
                    dep.var.labels.include = FALSE,
                    table.placement = "H",
                    column.labels=dep_var,
                    covariate.labels= covariates_sep,
                    omit = omit_var,
                    omit.stat=c("f", "ser","adj.rsq"),
                    add.lines = list(c("Treated outcome mean", treated_means), c("Outcome mean", means), 
                                     c("Outcome std. dev.", sds),
                                     c("Outcome range", range)),
                    column.sep.width = "0pt",
                    title = title,
                    type = "latex")

note.latex <- paste0("\\multicolumn{", size, "}{l} {\\parbox[t]{", cm, "cm}{ \\textit{Notes:} 
We report estimates from OLS regression including randomization block fixed effects. 
Robust standard errors are in parentheses. * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table1[grepl("Note", table1)] <- note.latex

cat(table1, file = "Tables/Appendix/Disaggregated/covid19.txt")

