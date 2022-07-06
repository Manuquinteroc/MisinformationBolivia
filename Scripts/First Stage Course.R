# First Stage Online sample

# ------------------------------------------------------------------------------
# Read dataframes from each excel sheet
path <- "Datasets/Alfabetizacion/Lista de avance grupo Febrero - Marzo.xlsx"
sheetnames <- excel_sheets(path)
mylist <- lapply(excel_sheets(path), read_excel, path = path)
names(mylist) <- sheetnames

# Treatment indicator
mylist$`F-Estudiantes`$treatment <- 1 
mylist$`F-Profesionales`$treatment <- 1 
mylist$`M-Profesionales`$treatment <- 0 
mylist$`M-Profesionales`$treatment <- 0

# Auxiliary functions to convert all columns to string type
chars <- function(x) {
  x <- x %>% mutate(across(everything(), as.character))
  return(x)
}

# Apply chars function to all dataframes
mylist <- lapply(mylist, chars)

# Append dataframes and merge identical columns
final <- bind_rows(mylist) %>% mutate(email = coalesce(Email,RecipientEmail), cel = coalesce(Númerodecelular, Celular),
                                      ambos = coalesce(`Hizo ambas`, `Hizo ambos`)) %>% 
  dplyr::select(-c("Email", "RecipientEmail", "...1", "Númerodecelular", "Celular", "Orden", "...17", 
                   "Hizo ambas", "Hizo ambos"))

# Rename variables with dates
original <- c("44230", "44231", "44232", "44236", "44252",
              "44254", "44255", "44256", "44257", "44258", "44259", "44260", "44263", "44264")

replacement <- c("2/3/21", "4/2/21", "5/2/21", "9/2/21", "25/2/21",
                 "2/27/2021", "2/28/2021", "3/1/2021", "3/2/2021", "3/3/2021", "3/4/2021", "3/5/2021", "3/8/2021", "3/9/2021")

names(final)[which(names(final) %in% original)] <- replacement

# Filter to unique observations 
final <- final %>% distinct(Identificación, .keep_all = T) %>% tidyr::drop_na(Identificación)

# Convert to numeric type
final$`Terminó Curso` <- as.numeric(final$`Terminó Curso`)
final$`Hizo encuesta` <- as.numeric(final$`Hizo encuesta`)
final$treatment <- as.numeric(final$treatment)
final$cel <- as.numeric(final$cel)
final$ambos <- as.numeric(final$ambos)
final$`2/3/21` <- as.numeric(gsub("\\%","", final$`2/3/21`))
final$`4/2/21` <- as.numeric(gsub("\\%","", final$`4/2/21`))
final$`5/2/21` <- as.numeric(gsub("\\%","", final$`5/2/21`))
final$`9/2/21` <- as.numeric(gsub("\\%","", final$`9/2/21`))
final$`25/2/21` <- as.numeric(gsub("\\%","", final$`25/2/21`))
final$`2/27/2021` <- as.numeric(gsub("\\%","", final$`2/27/2021`))
final$`2/28/2021` <- as.numeric(gsub("\\%","", final$`2/28/2021`))
final$`3/1/2021` <- as.numeric(gsub("\\%","", final$`3/1/2021`))
final$`3/2/2021` <- as.numeric(gsub("\\%","", final$`3/2/2021`))
final$`3/3/2021` <- as.numeric(gsub("\\%","", final$`3/3/2021`))
final$`3/4/2021` <- as.numeric(gsub("\\%","", final$`3/4/2021`))
final$`3/5/2021` <- as.numeric(gsub("\\%","", final$`3/5/2021`))
final$`3/8/2021` <- as.numeric(gsub("\\%","", final$`3/8/2021`))
final$`3/9/2021` <- as.numeric(gsub("\\%","", final$`3/9/2021`))
final$Fin <- as.numeric(gsub("\\%","", final$Fin))
final$Puntuación <- as.numeric(gsub("\\%","", final$Puntuación))

# Change 1's to 100
final$Fin[which(final$Fin == 1)] <- 100

final <- final %>% dplyr::rename(ended50 = `Terminó Curso`)

# Create ended course variable
final$endedCourse <- 0
final$endedCourse[which(final$Fin == 100)] <- 1

# Assign treatment arms within treatment group ---------------------------------
# Read data with specific treatments 
treatment_data <- read_dta("Datasets/Alfabetizacion/All_Assignment.dta")
treatment_data$Númerodecelular <- as.numeric(treatment_data$Númerodecelular) 

# Desegregate treatments
treatment_data$curso <- ifelse(treatment_data$treatment == "Febrero - Sin Verificaciones",1,0)
treatment_data$curso_verificacion <- ifelse(treatment_data$treatment == "Febrero - Con Verificaciones",1,0)
treatment_data$verificacion <- ifelse(treatment_data$treatment == "Marzo - Con Verificaciones",1,0)
treatment_data$control <- ifelse(treatment_data$treatment == "Marzo - Sin Verificaciones",1,0)

# Indexes for non-matching obs
ind_cel <- which(final$cel %in% treatment_data$Númerodecelular == F) 

# Fix cellphone numbers using email matching 
final$cel[ind_cel] <- treatment_data$Númerodecelular[which(treatment_data$Email %in% final$email[ind_cel])]

# Double-check non-matching
which(final$cel %in% treatment_data$Númerodecelular == F) # = 0

# Variables to merge
vars <- c("Númerodecelular", "blockid1", "curso", "verificacion", "curso_verificacion", "control")

# Merge dataframes
final <- left_join(final, treatment_data[vars], by = c("cel" = "Númerodecelular"))

# Analysis ---------------------------------------------------------------------
# Unconstrained sample 

final$ended50[which(is.na(final$ended50))] <- 0
final$ended50 <- final$ended50*100

final$endedCourse[which(final$control == 1 | final$verificacion == 1)] <- 0
final$endedCourse <- final$endedCourse*100

final$`Hizo encuesta`[which(is.na(final$`Hizo encuesta`))] <- 0

final$Puntuación[which(is.na(final$Puntuación))] <- 0

# Subset to baseline Sample ----------------------------------------------------
# Filter by baseline observations and baseline complete responses
baseline <- read_dta('Datasets/Alfabetizacion/final_data_alfabetizacion.dta', encoding = "UTF-8") %>% filter(is.na(missing_baseline) & baseline == 1)
ind <- which(!is.na(baseline$gender)) # delete five responses with all missing answers
baseline <- baseline[ind,]

baseline$Númerodecelular <- as.numeric(baseline$Númerodecelular)

ind_cel <- which(baseline$Númerodecelular %in% final$cel == F)

# Fix cellphone numbers using email matching 
final$email[which(final$email %in% baseline$email[ind_cel])]

baseline$Númerodecelular[ind_cel] <- final$cel[which(final$email %in% baseline$email[ind_cel])]

# Desegregate treatments
baseline$curso <- ifelse(baseline$treatment == "Febrero - Sin Verificaciones",1,0)
baseline$curso_verificacion <- ifelse(baseline$treatment == "Febrero - Con Verificaciones",1,0)
baseline$verificacion <- ifelse(baseline$treatment == "Marzo - Con Verificaciones",1,0)
baseline$control <- ifelse(baseline$treatment == "Marzo - Sin Verificaciones",1,0)

# Variables to merge
vars <- c("Númerodecelular")

# Merge dataframes
final_base <- left_join(baseline[vars], final, by = c("Númerodecelular" = "cel"))

# Analysis ---------------------------------------------------------------------
lm1 <- lm(ended50 ~ curso_verificacion + curso + verificacion + factor(blockid1), final_base)
lm2 <- lm(endedCourse ~ curso_verificacion + curso + verificacion + factor(blockid1), final_base)
lm3 <- lm(Puntuación ~ curso_verificacion + curso + verificacion + factor(blockid1), final_base)

# apply robust_se function to all regressions
new_se <- lapply(list(lm1,lm2,lm3), robust_se)

dep_var <- c("\\shortstack{Completed 50\\% \\\\ of the course}", 
             "\\shortstack{Completed 100\\% \\\\ of the course}",
             "Score")

omit_var <- c("Constant", "blockid1")
covariates <- c("Course and verification", "Course", "Verification")

ind_control <- which(final_base$control == 1)

Table1 <- stargazer(lm1, lm2, lm3,
                    se = new_se,
                    label = "table:FS_Online",
                    header = FALSE,
                    font.size ="footnotesize",
                    dep.var.caption = "",
                    dep.var.labels.include = FALSE,
                    table.placement = "H",
                    column.labels = dep_var,
                    covariate.labels = covariates,
                    omit = omit_var,
                    omit.stat = c("f", "ser","adj.rsq"),
                    add.lines = list(c("Control mean", 0, 
                                       round(mean(final_base$ended50[ind_control]),3),
                                       round(mean(final_base$endedCourse[ind_control]),3),
                                       round(mean(final_base$Puntuación[ind_control]),3)),
                                     c("Outcome range",  "[0, 100]",
                                       "[0, 100]", paste0("[", min(final_base$Puntuación), ", ", max(final_base$Puntuación), "]"))),
                    column.sep.width = "0pt",
                    title = "Summary statistics of Online course",
                    type = "latex")

# ------------------------------------------------------------------------------
# Match with our original sample
survey <- read.csv("Datasets/Alfabetizacion/survey course final.csv")

# See which cellphone numbers do not match
which(survey$Númerodecelular %in% final$cel == F) # 1 obs
# Check non-matching cellphones with emails 
survey$email[which(survey$Númerodecelular %in% final$cel == F)] %in% final$email # True 
# Fix cellphone
survey$Númerodecelular[which(survey$Númerodecelular %in% final$cel == F)] <- final$cel[which(final$email == survey$email[which(survey$Númerodecelular %in% final$cel == F)])]
# Double check all observations match
which(survey$Númerodecelular %in% final$cel == F) # 0 obs

# Merge dataframes 
survey <- left_join(survey, final[c("cel", "Puntuación", "Fin", "ended50", "endedCourse")], 
                    by = c("Númerodecelular" = "cel"))

# Run analysis for sample subset -----------------------------------------------
# Unconstrained sample 
survey$ended50[which(is.na(survey$ended50))] <- 0
survey$endedCourse[which(is.na(survey$endedCourse))] <- 0
survey$Puntuación[which(is.na(survey$Puntuación))] <- 0

lm1 <- lm(ended50 ~ curso_verificacion + curso + verificacion + factor(blockid1), survey)
lm2 <- lm(endedCourse ~ curso_verificacion + curso + verificacion + factor(blockid1), survey)
lm3 <- lm(Puntuación ~ curso_verificacion + curso + verificacion + factor(blockid1), survey)

# Apply robust_se function to all regressions
new_se <- lapply(list(lm1,lm2,lm3), robust_se)

dep_var <- c("\\shortstack{Completed 50\\% \\\\ of the course}", 
             "\\shortstack{Completed 100\\% \\\\ of the course}",
             "Score")

omit_var <- c("Constant", "blockid1")

covariates_separate <- c("Course and verification", "Course", "Verification")

ind_control <- which(survey$control == 1)

Table2 <- stargazer(lm1, lm2, lm3,
                    se = new_se,
                    header = FALSE,
                    font.size ="footnotesize",
                    dep.var.caption = "",
                    dep.var.labels.include = FALSE,
                    table.placement = "H",
                    column.labels = dep_var,
                    covariate.labels = covariates_separate,
                    omit = omit_var,
                    omit.stat = c("f", "ser","adj.rsq"),
                    add.lines = list(c("Control mean", 0, 
                                       round(mean(survey$ended50[ind_control]),3),
                                       round(mean(survey$endedCourse[ind_control]),3),
                                       round(mean(survey$Puntuación[ind_control]),3)),
                                     c("Outcome range",  "[0, 100]",
                                       "[0, 100]", paste0("[", min(survey$Puntuación), ", ", max(survey$Puntuación), "]"))),
                    column.sep.width = "0pt",
                    title = "Summary statistics of Online course (Restricted to sample)",
                    type = "latex")


note.latex <- "\\multicolumn{4}{l} {\\parbox[t]{10cm}{ \\textit{Notes:} 
We report estimates from OLS regression including randomization block fixed effects.
Robust standard errors are in parentheses. * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\"
Table2[grepl("Note", Table2)] <- note.latex


# Append tables as panels ------------------------------------------------------
TableA <- paste(Table1, collapse = "")
TableB <- paste(Table2, collapse = "")

aux1 <- sub("\\\\\\[-1.8ex].*", 
            "\\hline \\\\hline \\\\\\\\[-1.8ex] \\\\\\\\[-0.5ex] \\\\multicolumn{%}{l}{\\\\textbf{Panel A: Responded to Baseline sample }} \\\\\\\\ \\\\hline \\\\\\\\[-1ex]", 
            TableA)

aux2 <- sub(paste0(".*\\]  & ",  "\\\\shortstack\\{Completed 50"), paste0("& ", "\\\\shortstack\\{Completed 50"), TableA) 

aux2 <- sub("\\\\hline \\\\\\\\\\[-1.8ex] \\\\textit.*", "\\\\\\\\[-0.5ex] \\\\multicolumn{%}{l}{\\\\textbf{Panel B: Responded to Endline sample }} \\\\\\\\ \\\\hline \\\\\\\\[-1ex]", aux2) 

modified_tableA <- paste(aux1, aux2, collapse = "")

# Data for panel B 
aux1B <- sub(".*?Course ", "Course ", TableB) 

# Merge A and B
merged_AB <- paste(modified_tableA, aux1B, collapse = "")


merged_AB <- gsub("%", 4, merged_AB)

cat(merged_AB, file = "Tables/Appendix/First_stage_course.tex")


# ------------------------------------------------------------------------------
# Figures
# ggplot setup 
text.11 <- element_text(color = "black", size = 11)
final.text.11 <- element_text(color = "black", size = 12, hjust = 0.5, family="Arial")
text.9 <- element_text(color = "black", size = 9)
text.12 <- element_text(color = "black", size = 12)

th <- theme(strip.text.x = final.text.11,
            strip.placement = "inside",
            strip.background = element_rect(colour = "black", fill = "white"),
            axis.text.x = element_blank(),
            axis.text.y = text.11,
            axis.title.x = text.12,
            axis.title.y = text.12,
            legend.text = final.text.11,
            legend.title = final.text.11,
            axis.text = final.text.11, 
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y =  element_line(color = "grey",
                                               size = 0.1,
                                               linetype = 1),
            axis.ticks.length.x = unit(.25, "cm"),
            panel.border = element_rect(colour = "black", fill = NA),
            legend.position = "bottom")

legend_guide <- guides(color = guide_legend(title.position = "top", title.hjust = 0.5))

legend_guide1 <- guides(color = guide_legend(title.position = "top", title.hjust = 0.5))
legend_guide2 <- guides(linetype = guide_legend(title.position = "top", title.hjust = 0.5))

colors <- c("#E7B800", "#0072B2","#D55E00")

y_text <-c("Treatment effect (90% C.I.)")

# ------------------------------------------------------------------------------

# Save lm regressions and retrieve coeffs and sd
lm1 <- lm_robust(ended50 ~ curso_verificacion + curso + verificacion, fixed_effects = blockid1, se_type = "HC0", data = survey, alpha = .1)
lm2 <- lm_robust(endedCourse ~ curso_verificacion + curso + verificacion, fixed_effects = blockid1, se_type = "HC0", data = survey, alpha = .1)
lm3 <- lm_robust(Puntuación ~ curso_verificacion + curso + verificacion, fixed_effects = blockid1, se_type = "HC0", data = survey, alpha = .1)

c1 <- as.numeric(c(lm1$coefficients, lm2$coefficients, lm3$coefficients))
c2 <- as.numeric(c(lm1$conf.low, lm2$conf.low, lm3$conf.low))
c3 <- as.numeric(c(lm1$conf.high, lm2$conf.high, lm3$conf.high))
c4 <- rep(c("Course and verification", "Course", "Verification"), 3)
c5 <- c(rep("Completed 50% of the course", 3),
        rep("Completed 100% of the course", 3),
        rep("Score", 3))

data_aux <- cbind.data.frame(c1, c2, c3, c4,c5)

colnames(data_aux) <- c("coefs", "left", "right", "treatment", "variables")
positions <- unique(data_aux$treatment)
data_aux$treatment <- factor(data_aux$treatment, levels = positions)
data_aux$variables <- factor(data_aux$variables, levels = unique(data_aux$variables))

Figure1 <- ggplot(data_aux, aes(treatment, coefs), color = factor(treatment)) + 
  geom_point(aes(color = factor(treatment)), size = 4, fill = "#d3d3d3") + 
  geom_errorbar(aes(ymin = left, ymax = right, color = factor(treatment)), width = 0.2,
                cex = 0.75) +
  facet_grid(. ~ variables, labeller = labeller(variables = label_wrap_gen(21))) +
  scale_x_discrete(limits = positions) +
  theme_light() +
  th + 
  scale_color_manual(name = "Treatment",  labels = c("Course and verification", "Course", "Verification"),
                     values= colors) +
  geom_hline(yintercept=0, size = 0.8, alpha = 0.4, linetype = "dashed") +
  ylab(y_text) +
  xlab("") + 
  legend_guide

ggsave(Figure1, path = 'Figures', filename = "Figure1.pdf", device = cairo_pdf, 
       width = 7, height = 5, dpi = 300)

