# Heterogeneous effects general setup
# Auxiliary lists to store lm
lm_list_ols <- list()
count <- 1

for (x in aux) {
  
  if (interacted_bool[count] == T) {
    fmla1 <- as.formula(paste0(x, "~ ", paste(treatments_separate, collapse = " + "), " + ",
                               paste(paste0(treatments_separate, ":", interacted), collapse = " + "), " + ",
                               paste(control, collapse = " + "), "+ factor(blockid1)"))
  
  } else {
    fmla1 <- as.formula(paste0(x, "~ ", paste(paste0(treatments_separate, ":", interacted), collapse = " + "), " + ",
                               paste(control, collapse = " + "), "+ factor(blockid1)"))
  }
  
  nam1 <- paste("lm_", count, "_ols", sep = "")
  assign(nam1, lm(fmla1, data = survey))
  
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  
  count <- count + 1
}

# Robust s.e.
new_se <- lapply(lm_list_ols, robust_se)

# Calculate additional lines for tables: mean, sd, range 
means <- round(colMeans(aux_data, na.rm = T), 3)
sds <- round(colSds(as.matrix(aux_data), na.rm = T), 3)
# Range
maximums <- round(sapply(aux_data, max, na.rm = T), 1)
minimums <- round(sapply(aux_data, min, na.rm = T), 1)
range <- paste0("[", minimums, ",", maximums, "]")

# Generate table ---------------------------------------------------------------
table1 <- stargazer(lm_list_ols,
                    se = new_se,
                    header = FALSE,
                    font.size="tiny",
                    dep.var.caption = "",
                    dep.var.labels.include = FALSE,
                    table.placement = "H",
                    column.labels=dep_var,
                    covariate.labels= covariates_interacted,
                    omit = omit_var,
                    omit.stat=c("f", "ser","adj.rsq", "n"),
                    column.sep.width = "0pt",
                    # add.lines = list(c("Outcome mean", means ),
                    #                  c("Outcome std. dev.", sds),
                    #                  c("Outcome range", range)),
                    title = title,
                    type = "latex")

# note.latex <- paste0("\\multicolumn{", size, "}{l} {\\parbox[t]{", cm, "cm}{ \\textit{Notes:} 
# We report estimates from OLS regression including randomization block fixed effects. 
# Specifications further include the corresponding outcome variables at baseline as controls.
# Robust standard errors are in parentheses.
# The variable ", interaction_name, " is an indicator for responding the WhatsApp survey.
# * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
# table1[grepl("Note", table1)] <- note.latex


# ------------------------------------------------------------------------------
# Construct sum of coefficients with s.e. and significance

# Declare functions to calculate non-linear coefficients with s.e. and pvalues (pkg: multcomp)
sum_coefs_cv <- function(x) {
  aux <- summary(glht(x, "curso_verificacion +  curso_verificacion:WhatsApp = 0"))
  
  return(list(aux[[9]]$coefficients, se= aux[[9]]$sigma, pvalues= aux[[9]]$pvalues))
}

sum_coefs_c <- function(x) {
  aux <- summary(glht(x, "curso +  curso:WhatsApp = 0"))
  
  return(list(aux[[9]]$coefficients, se= aux[[9]]$sigma, pvalues= aux[[9]]$pvalues))
}

sum_coefs_v <- function(x) {
  aux <- summary(glht(x, "verificacion + verificacion:WhatsApp = 0"))
  
  return(list(aux[[9]]$coefficients, se= aux[[9]]$sigma, pvalues= aux[[9]]$pvalues))
}

# ----------
sum_coefs_cv_onlyW <- function(x) {
  aux <- summary(glht(x, "curso_verificacion:WhatsApp = 0"))
  
  return(list(aux[[9]]$coefficients, se= aux[[9]]$sigma, pvalues= aux[[9]]$pvalues))
}

sum_coefs_c_onlyW <- function(x) {
  aux <- summary(glht(x, "WhatsApp:curso = 0"))
  
  return(list(aux[[9]]$coefficients, se= aux[[9]]$sigma, pvalues= aux[[9]]$pvalues))
}

sum_coefs_v_onlyW <- function(x) {
  aux <- summary(glht(x, "WhatsApp:verificacion = 0"))
  
  return(list(aux[[9]]$coefficients, se= aux[[9]]$sigma, pvalues= aux[[9]]$pvalues))
}


# Initialize empty lists for each treatment and for each type (coef, se, pval)
lm_coef_cv <- list()
lm_se_cv <- list()
lm_pval_cv <- list()

lm_coef_c <- list()
lm_se_c <- list()
lm_pval_c <- list()

lm_coef_v <- list()
lm_se_v <- list()
lm_pval_v <- list()

# Calculate coefs, se, and pvals and store in lists
count <- 1
for (x in 1:length(aux)) {

    if (interacted_bool[count] == T) {
      
    lm_coef_cv[[count]] <- sum_coefs_cv(lm_list_ols[[count]])[[1]]
    lm_se_cv[[count]] <- sum_coefs_cv(lm_list_ols[[count]])[[2]]
    lm_pval_cv[[count]] <- sum_coefs_cv(lm_list_ols[[count]])[[3]]
    
    lm_coef_c[[count]] <- sum_coefs_c(lm_list_ols[[count]])[[1]]
    lm_se_c[[count]] <- sum_coefs_c(lm_list_ols[[count]])[[2]]
    lm_pval_c[[count]] <- sum_coefs_c(lm_list_ols[[count]])[[3]]
    
    lm_coef_v[[count]] <- sum_coefs_v(lm_list_ols[[count]])[[1]]
    lm_se_v[[count]] <- sum_coefs_v(lm_list_ols[[count]])[[2]]
    lm_pval_v[[count]] <- sum_coefs_v(lm_list_ols[[count]])[[3]]
    
  } else {
    
    lm_coef_cv[[count]] <- sum_coefs_cv_onlyW(lm_list_ols[[count]])[[1]]
    lm_se_cv[[count]] <- sum_coefs_cv_onlyW(lm_list_ols[[count]])[[2]]
    lm_pval_cv[[count]] <- sum_coefs_cv_onlyW(lm_list_ols[[count]])[[3]]
    
    lm_coef_c[[count]] <- sum_coefs_c_onlyW(lm_list_ols[[count]])[[1]]
    lm_se_c[[count]] <- sum_coefs_c_onlyW(lm_list_ols[[count]])[[2]]
    lm_pval_c[[count]] <- sum_coefs_c_onlyW(lm_list_ols[[count]])[[3]]
    
    lm_coef_v[[count]] <- sum_coefs_v_onlyW(lm_list_ols[[count]])[[1]]
    lm_se_v[[count]] <- sum_coefs_v_onlyW(lm_list_ols[[count]])[[2]]
    lm_pval_v[[count]] <- sum_coefs_v_onlyW(lm_list_ols[[count]])[[3]]
    
  }

    count <- count + 1
}

# Create a data frame with coefs, se, pvals for each treatment arm
data_aux_cv <- cbind.data.frame(do.call(rbind.data.frame, lm_coef_cv), do.call(rbind.data.frame, lm_se_cv), do.call(rbind.data.frame, lm_pval_cv))
data_aux_c <- cbind.data.frame(do.call(rbind.data.frame, lm_coef_c), do.call(rbind.data.frame, lm_se_c), do.call(rbind.data.frame, lm_pval_c))
data_aux_v <- cbind.data.frame(do.call(rbind.data.frame, lm_coef_v), do.call(rbind.data.frame, lm_se_v), do.call(rbind.data.frame, lm_pval_v))

# Rename columns for each dataset
names(data_aux_cv) <- c("coefs", "se", "pvals")
names(data_aux_c) <- c("coefs", "se", "pvals")
names(data_aux_v) <- c("coefs", "se", "pvals")

# Initialize empty list to store texreg object
list_of_texreg_cv <- list()
list_of_texreg_c <- list()
list_of_texreg_v <- list()

# Create the texreg object with createTexreg
count <- 1
for (i in 1:length(aux)) {
  name1 <- paste0("trcv", count, "<- ")
  name2 <- paste0("trc", count, "<- ")
  name3 <- paste0("trv", count, "<- ")
  
  content1 <- paste0("createTexreg(coef.names = 'Course and verification $+$ \\\\\\\\ Course and verification x WhatsApp Delivery'", ", coef = data_aux_cv$coefs[", count, "], se = data_aux_cv$se[", count, "], pvalues = data_aux_cv$pvals[", count, "])")
  content2 <- paste0("createTexreg(coef.names = 'Course $+$ \\\\\\\\ Course x WhatsApp Delivery'", ", coef = data_aux_c$coefs[", count, "], se = data_aux_c$se[", count, "], pvalues = data_aux_c$pvals[", count, "])")
  content3 <- paste0("createTexreg(coef.names = 'Verification $+$ \\\\\\\\ Verification x WhatsApp Delivery'", ", coef = data_aux_v$coefs[", count, "], se = data_aux_v$se[", count, "], pvalues = data_aux_v$pvals[", count, "])")
  
  eval(parse(text = paste0(name1, content1)))
  eval(parse(text = paste0(name2, content2)))
  eval(parse(text = paste0(name3, content3)))
  
  nam1 = substr(name1, 1, nchar(name1)-3)
  nam2 = substr(name2, 1, nchar(name2)-3)
  nam3 = substr(name3, 1, nchar(name3)-3)
  
  # Store Texreg objects in corresponding lists
  list_of_texreg_cv[[count]] <- get(nam1, envir = globalenv())
  list_of_texreg_c[[count]] <- get(nam2, envir = globalenv())
  list_of_texreg_v[[count]] <- get(nam3, envir = globalenv())
  
  count <- count + 1
  
}

# Create tables for each treatment arm  
table_aux1 <- texreg(list_of_texreg_cv,
                     stars=c(0.01, 0.05, 0.1), # change significance as in stargazer
                     digits = 3)

table_aux2 <- texreg(list_of_texreg_c,
                     stars=c(0.01, 0.05, 0.1),
                     digits = 3)

table_aux3 <- texreg(list_of_texreg_v,
                     stars=c(0.01, 0.05, 0.1),
                     digits = 3)

# Append tables as desired
aux1 <- sub(".*?Course", "Course", table_aux1)
aux1 <- sub("\\\\hline.*", "", aux1)

aux2 <- sub(".*?Course", "Course", table_aux2)
aux2 <- sub("\\\\hline.*", "", aux2)

aux3 <- sub(".*?Verification", "Verification", table_aux3)
aux3 <- sub("\\\\hline.*", "", aux3)

aux_table <- paste0(aux1, aux2, aux3, sep = "")
aux_table <- str_replace_all(aux_table, "\\n", "") # delete \n strings


# Merge with original table
table1 <- paste(table1, collapse = "")
aux_table <- paste(aux_table, collapse = "")

aux1_original <- sub("\\\\\\\\\\[-1.8ex] R\\$\\^\\{2\\}\\$.*", "\\\\\\\\", table1)

aux2_original <- sub(".*\\\\\\\\\\[-1.8ex] R\\$\\^\\{2\\}\\$", "\\\\\\\\ \\\\hline \\\\\\\\\\[-1.8ex] R\\$\\^\\{2\\}\\$", table1)

# Append final table
final_table <- paste0(aux1_original, aux_table, aux2_original, sep = "")

