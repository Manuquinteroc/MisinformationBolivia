# Database Manipulation: WhatsApp

# ------------------------------------------------------------------------------
# Read data
baseline <- read_dta("Datasets/WhatsApp/baseline_ready.dta")

# Read dataframes for each traetment group 
path <- "Datasets/WhatsApp/23032021 Primera_Aleatoriazacion_coded.xlsx"
sheetnames <- excel_sheets(path)
mylist <- lapply(excel_sheets(path), read_excel, path = path)
names(mylist) <- sheetnames

# Split list in dataframes per treatment
curso_df <- mylist$`Solo Curso`
cursover_df <- mylist$`Curso + Verificaciones`
ver_df <- mylist$Verificaciones

curso_df <- curso_df %>% dplyr::rename("WhatsApp" = "WhatsApp...3")
curso_df$WhatsApp <- as.character(curso_df$WhatsApp)
ver_df$WhatsApp <- as.character(ver_df$WhatsApp)

# Receive messages for curso group
curso_df$recibido_curso <- ifelse(curso_df$WhatsApp...27 != 1,1,0)
cursover_df$recibido_curso <- ifelse(rowSums(cursover_df[,29:40], na.rm = T) > 0, 1,0)
ver_df$recibido_curso <- 0

# recieve verification messages
curso_df$recibido_ver <- 0
cursover_df$recibido_ver <- ifelse(rowSums(cursover_df[,29:40], na.rm = T) > 0, 1,0) # cannot distinguish with course
ver_df$recibido_ver <- ifelse(rowSums(ver_df[,29:41], na.rm = T) > 0, 1,0)

# Code receive messages for course and verirication and verification groups 
curso_df$comment_curso <- ifelse(rowSums(!is.na(curso_df[ ,5:16])) > 0,1,0)
cursover_df$comment_curso <- ifelse(rowSums(!is.na(cursover_df[ ,5:16])) > 0,1,0)
ver_df$comment_curso <- 0

# Code if responded with messages to the course or the verification
curso_df$comment_ver <- 0
cursover_df$comment_ver <- ifelse(rowSums(!is.na(cursover_df[ ,17:28])) > 0,1,0)
ver_df$comment_ver <- ifelse(rowSums(!is.na(ver_df[ ,5:28])) > 0,1,0)

# Filter to baseline -----------------------------------------------------------
# Append dataframe
final <- bind_rows(curso_df, cursover_df, ver_df) %>% mutate(cel = coalesce(WhatsApp, WhatsApp...3)) %>%
  dplyr::select(-c("WhatsApp", "WhatsApp...3")) %>% dplyr::rename(WhatsApp = cel)


# Merge with baseline data set to subset to baseline respondents
final <- left_join(final, baseline[c("treatment", "whatsapp", "baseline_complete", "blockid1")], by = c("WhatsApp" = "whatsapp")) %>%
  filter(baseline_complete == 1)

# Number of non-matching phones
sum(final$WhatsApp %in% baseline$whatsapp == F) # 0

aux <- fastDummies::dummy_cols(final$treatment, remove_selected_columns = T) 
names(aux) <- c("course_ver", "course", "verification")

final <- cbind.data.frame(final, aux)

# Analysis ---------------------------------------------------------------------
aux <- c("recibido_curso", "recibido_ver", "comment_curso", "comment_ver")

dem_data <- final[aux]

dem_control <- round(colMeans(dem_data, na.rm = T), digits = 3)
dem_sd <- round(colSds(as.matrix(dem_data)), digits = 3)

lm_list <- list()
count <- 1

treatments_separate <- c("course_ver", "course", "verification")
covariates_separate <- c("Course and verification", "Course", "Verification")

for(x in aux) {
  fmla <- as.formula(paste0(x, "~ -1 +", paste(treatments_separate, collapse = " + "), " + factor(blockid1)"))
  nam <- paste("lm_", x, sep = "")

  assign(nam, lm(fmla, data = final))

  lm_list[[count]] <- get(nam, envir = globalenv())

  count <- count + 1
}

# Apply robust s.e.
new_se <- lapply(lm_list, robust_se)

dep_var <- c("\\shortstack{Message for \\\\ the course \\\\ received}",
             "\\shortstack{Verification \\\\ received}",
             "\\shortstack{Responded to \\\\the course}",
             "\\shortstack{Responded to \\\\verifications}")

omit_var <- c("Constant", "blockid1")

Table1 <- stargazer(lm_list, 
                     se = new_se,
                    label = "table:FS_WhatsApp",
                     header=FALSE,
                     font.size="footnotesize",
                     dep.var.caption = "",
                     dep.var.labels.include = FALSE,
                     table.placement = "H",
                     column.labels=dep_var,
                     covariate.labels= covariates_separate,
                     omit = omit_var,
                     omit.stat=c("f", "ser","adj.rsq"),
                     add.lines = list(c("Outcome mean", dem_control),
                                      c("Outcome std. dev.", dem_sd)),
                     column.sep.width = "0pt",
                     title = "Respondents that received and interacted with the course’s videos/messages and verifications",
                     type = "latex")

# Filter to sample with Endline ------------------------------------------------
survey <- read.csv("Datasets/Whatsapp/survey WhatsApp final.csv")

final$WhatsApp <- as.numeric(final$WhatsApp)

# See which cellphone numbers do not match
which(survey$whatsapp %in% final$WhatsApp == F) # 73 obs

# Merge dataframes 
survey <- left_join(survey, final[c("WhatsApp", "recibido_curso", "recibido_ver", "comment_curso", "comment_ver")], 
                    by = c("whatsapp" = "WhatsApp"))

# Analysis ---------------------------------------------------------------------
aux <- c("recibido_curso", "recibido_ver", "comment_curso", "comment_ver")

dem_data <- survey[aux]

dem_control <- round(colMeans(dem_data, na.rm = T), digits = 3)
dem_sd <- round(colSds(as.matrix(dem_data), na.rm = T), digits = 3)

lm_list <- list()
count <- 1

treatments_separate <- c("curso_verificacion", "curso", "verificacion")
covariates_separate <- c("Course and verification", "Course", "Verification")

for(x in aux) {
  fmla <- as.formula(paste0(x, "~ -1 +", paste(treatments_separate, collapse = " + "), " + factor(blockid1)"))
  nam <- paste("lm_", x, sep = "")
  
  assign(nam, lm(fmla, data = survey))
  
  lm_list[[count]] <- get(nam, envir = globalenv())
  
  count <- count + 1
}

# Apply robust s.e.
new_se <- lapply(lm_list, robust_se)

dep_var <- c("\\shortstack{Message for \\\\ the course \\\\ received}",
             "\\shortstack{Verification \\\\ received}",
             "\\shortstack{Responded to \\\\the course}",
             "\\shortstack{Responded to \\\\verifications}")

omit_var <- c("Constant", "blockid1")

Table2 <- stargazer(lm_list, 
                    se = new_se,
                    header=FALSE,
                    font.size="footnotesize",
                    dep.var.caption = "",
                    dep.var.labels.include = FALSE,
                    table.placement = "H",
                    column.labels=dep_var,
                    covariate.labels= covariates_separate,
                    omit = omit_var,
                    omit.stat=c("f", "ser","adj.rsq"),
                    add.lines = list(c("Outcome mean", dem_control),
                                     c("Outcome std. dev.", dem_sd)),
                    column.sep.width = "0pt",
                    title = "Respondents that received and interacted with the course’s videos/messages and verifications",
                    type = "latex")

note.latex <- paste("\\multicolumn{5}{l} {\\parbox[t]{12cm}{ \\textit{Notes:} 
We report estimates from OLS regression including randomization block fixed effects. 
Robust standard errors are in parentheses.
* denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
Table2[grepl("Note", Table2)] <- note.latex


# Append tables as panels -----------------------------------------------------
TableA <- paste(Table1, collapse = "")
TableB <- paste(Table2, collapse = "")

aux1 <- sub("\\\\\\[-1.8ex].*", 
            "\\hline \\\\hline \\\\\\\\[-1.8ex] \\\\\\\\[-0.5ex] \\\\multicolumn{%}{l}{\\\\textbf{Panel A: Responded to Baseline sample }} \\\\\\\\ \\\\hline \\\\\\\\[-1ex]", 
            TableA)

aux2 <- sub(".*?\\\\shortstack\\{Message ",  " & \\\\shortstack\\{Message ", TableA) 

aux2 <- sub("\\\\hline \\\\\\\\\\[-1.8ex] \\\\textit.*", "\\\\\\\\[-0.5ex] \\\\multicolumn{%}{l}{\\\\textbf{Panel B: Responded to Endline sample }} \\\\\\\\ \\\\hline \\\\\\\\[-1ex]", aux2) 

modified_tableA <- paste(aux1, aux2, collapse = "")

# Data for Panel C
aux1B <- sub(".*?Course ", "Course ", TableB) 

merged_AB <- paste(modified_tableA, aux1B, collapse = "")
merged_AB <- gsub("%", 4, merged_AB)

cat(merged_AB, file = 'Tables/Appendix/Table_WhatsMessages.txt')

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
lm1 <- lm_robust(recibido_curso ~ -1 + curso_verificacion + curso + verificacion + factor(blockid1), se_type = "HC0", data = survey, alpha =  0.1)
lm2 <- lm_robust(recibido_ver ~ -1 + curso_verificacion + curso + verificacion + factor(blockid1), se_type = "HC0", data = survey, alpha =  0.1)
lm3 <- lm_robust(comment_curso ~ -1 + curso_verificacion + curso + verificacion + factor(blockid1), se_type = "HC0", data = survey, alpha =  0.1)
lm4 <- lm_robust(comment_ver ~ -1 + curso_verificacion + curso + verificacion + factor(blockid1), se_type = "HC0",  data = survey, alpha =  0.1)

c1 <- as.numeric(c(lm1$coefficients[1:3], lm2$coefficients[1:3], lm3$coefficients[1:3], lm4$coefficients[1:3]))
c2 <- as.numeric(c(lm1$conf.low[1:3], lm2$conf.low[1:3], lm3$conf.low[1:3], lm4$conf.low[1:3]))
c3 <- as.numeric(c(lm1$conf.high[1:3], lm2$conf.high[1:3], lm3$conf.high[1:3], lm4$conf.high[1:3]))
c4 <- rep(c("Course and verification", "Course", "Verification"), 4)
c5 <- c(rep("Message for the course received",3),
        rep("Verification received",3),
        rep("Responded to the course",3),
        rep("Responded to verifications",3))

data_aux <- cbind.data.frame(c1, c2, c3, c4,c5)

colnames(data_aux) <- c("coefs", "left", "right", "treatment", "variables")
positions <- unique(data_aux$treatment)
data_aux$treatment <- factor(data_aux$treatment, levels = positions)
data_aux$variables <- factor(data_aux$variables, levels = unique(data_aux$variables))


Figure2 <- ggplot(data_aux, aes(treatment, coefs), color = factor(treatment)) + 
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

ggsave(Figure2, path = 'Figures', filename = "Figure2.pdf", device = cairo_pdf, 
       width = 7, height = 5,dpi = 300)
