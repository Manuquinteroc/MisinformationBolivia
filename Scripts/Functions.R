# Auxiliary functions

# ------------------------------------------------------------------------------
# Negate function
`%!in%` = Negate(`%in%`)

# Robust se function
robust_se <- function(x) {
  coeftest(x, vcov = vcovHC(x, type = "HC0"))[,2]
}


# ------------------------------------------------------------------------------
# Text functions to construct LaTeX tables

# Align left fucntion 
left_align <- function(table, hs){
  
  table <- paste(table, collapse = "")
  
  table <- gsub("\\\\begin\\{tabular", "\\\\hspace*{-%cm} \\\\begin{tabular", table) 
  table <- gsub("%", hs, table)
  
  return(table)
  
}

# Append panels of treatment effects + IPSW + LASSO -----------------------------
main_panel <- function(panelA, panelB, panelC, x, keyword, vs = NA, hs = NA) {
  
  TableA <- paste(panelA, collapse = "")
  TableB <- paste(panelB, collapse = "")
  TableC <- paste(panelC, collapse = "")
  
  aux1 <- sub("\\\\\\[-1.8ex].*", 
              "\\hline \\\\hline \\\\\\\\[-0.5ex] \\\\\\\\[-0.5ex] \\\\\\\\[-0.5ex] \\\\multicolumn{%}{l}{\\\\textbf{Panel A: Treatment effects with covariates in the outcome family }} \\\\\\\\[-0.5ex] \\\\\\\\ \\\\hline \\\\\\\\[-1ex]", 
              TableA)

  aux2 <- sub(paste0(".*\\]  & ",  keyword), paste0("& ", keyword), TableA) 
  
  aux2 <- sub("\\\\hline \\\\\\\\\\[-1.8ex] \\\\textit.*", paste0("\\\\\\\\[-0.5ex] \\\\\\\\[-0.5ex] \\\\\\\\[-0.5ex] \\\\multicolumn{%}{l}{\\\\textbf{Panel B: IPSW with covariates in the outcome family }} \\\\\\\\[-0.5ex] \\\\\\\\ \\\\hline ",
              "\\\\\\\\[-0.5ex] \\\\multicolumn{%}{l}{\\\\textbf{Panel B.1: Sample course Delivery Online}} \\\\\\\\[-1.8ex] \\\\\\\\ \\\\cline{1-2} \\\\\\\\[-0.5ex]"), aux2)
  
  modified_tableA <- paste(aux1, aux2, collapse = "")
  
  # data for panel B 
  aux1_B <- sub(".*?Course ", "Course ", TableB) 
  
  # Merge A and B
  merged_AB <- paste(modified_tableA, aux1_B, collapse = "")
  
  aux2_B <- sub("\\\\hline \\\\\\\\\\[-1.8ex] \\\\textit.*", paste0("\\\\\\\\[-0.5ex] \\\\\\\\[-0.5ex] \\\\\\\\[-0.5ex] \\\\multicolumn{%}{l}{\\\\textbf{Panel C: Treatment effects with covariates selected with LASSO }} \\\\\\\\[-0.5ex] \\\\\\\\ \\\\hline ",
                "\\\\\\\\[-0.5ex] \\\\multicolumn{%}{l}{\\\\textbf{Panel C.1: Sample course Delivery Online}} \\\\\\\\[-1.8ex] \\\\\\\\ \\\\cline{1-2} \\\\\\\\[-0.5ex]"), merged_AB)

  # data for panel C 
  aux1_C <- sub(".*?Course ", "Course ", TableC) 
  
  # Merge A, B, and C
  merged_ABC <- paste(aux2_B, aux1_C, collapse = "")
  
  merged_ABC <- gsub("%", x, merged_ABC)
  
  # fix vertical height 
  if (!is.na(vs)) {
    merged_ABC <- gsub("\\centering", "centering \\\\vspace*{-%cm}", merged_ABC) 
    merged_ABC <- gsub("%", vs, merged_ABC)
    
  }
  
  # fix horizontal alignment 
  if (!is.na(hs)) {
    merged_ABC <- gsub("\\\\begin\\{tabular", "\\\\hspace*{-%cm} \\\\begin{tabular", merged_ABC) 
    merged_ABC <- gsub("%", hs, merged_ABC)
    
  }
  
  return(merged_ABC)
  
}

# Auxiliary function to add notes to tex tables --------------------------------
add_notes <- function(xtable, note) {
  
  xtable <- gsub("\\\\end\\{table", "\\\\justify {\\\\scriptsize \\\\textit{Notes:} %} \\\\end\\{table", xtable)
  xtable <- gsub("%", note, xtable)  
  
  return(xtable) 
}

# Auxiliary function to resize box
resizebox <- function(xtable) {
  
  xtable <- gsub("begin\\{tabular", "\\resizebox{\\\\textwidth}{!}{ \\\\begin{tabular", xtable)
  xtable <- gsub("end\\{tabular", "end\\{tabular}", xtable)
  
  return(xtable) 
}

# Auxiliary functions to construct panel for WhatsApp delivery -----------------
# Declare functions to calculate non-linear coefficients with s.e. and p-values (pkg: multcomp)
sum_coefs_cv <- function(x) {
  aux <- summary(glht(x, "curso_verificacion +  curso_verificacion:WhatsApp = 0", vcov = vcovHC(x, type = "HC0")))
  
  return(list(aux[[9]]$coefficients, se = aux[[9]]$sigma, pvalues = aux[[9]]$pvalues))
}

sum_coefs_c <- function(x) {
  aux <- summary(glht(x, "curso +  curso:WhatsApp = 0", vcov = vcovHC(x, type = "HC0")))
  
  return(list(aux[[9]]$coefficients, se= aux[[9]]$sigma, pvalues= aux[[9]]$pvalues))
}

sum_coefs_v <- function(x) {
  aux <- summary(glht(x, "verificacion + verificacion:WhatsApp = 0", vcov = vcovHC(x, type = "HC0")))
  
  return(list(aux[[9]]$coefficients, se= aux[[9]]$sigma, pvalues= aux[[9]]$pvalues))
}

