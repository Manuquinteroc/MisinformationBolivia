# Algorithm to run LASSO to select desired variables 

# ------------------------------------------------------------------------------
# Need to specify on the father program the following:
# Dependent variables: aux = dep_vars_names
# controls for LASSO selection
# Treatment indicators, either treatmet_separate or treatment_pooled

# Out: "control_list" with the covariates that LASSO selected for each DV sotred in a ordered list

# ------------------------------------------------------------------------------
# Put treatment as treatments_separate 
treatment <- treatments_separate

# Change dependent variables name
dep_vars_names <- aux
dep_vars <- survey[, dep_vars_names]

# Analysis ---------------------------------------------------------------------

# Fix effect matrix
fe <- as.matrix(fastDummies::dummy_cols(survey$blockid1, remove_selected_columns = T))

# Algorithm to select LASSO variables and run specifications 
# 1.- Create model matrix and standardize all vars,
# 2.- Drop high correlation variables,
# 3.- Run cv.glmnet to get lamba,
# 4.- Run lasso using glmnet,
# 5.- Extract non-zero coefficients from lasso,
# 6.- Use 5 as covariates in the regression.

control_list <- list()

for (count in 1:length(aux)) {
  
  # Example for 1 DV
  # Step 1: Create model matrix and standardize all vars
  if (is.na(lagged[count])) { # if there is no lagged DV, then take all controls
    model_matrix_controls <- scale(as.matrix(survey[,controls])) # subset matrix to only controls,
    # Remove the lagged DV to force it as covariate
  } else { # otherwise remove lagged DV from control matrix
    
    model_matrix_controls <- scale(as.matrix(survey[,controls[which(controls != lagged[count])]])) # subset matrix to only controls,
    # Remove the lagged DV to force it as covariate
  }
  see <- as.data.frame(model_matrix_controls)
  
  # Step 2: Drop high correlation variables
  cor_matrix <- Rfast::cora(model_matrix_controls) # get vcov matrix
  
  cor_matrix_rm <- cor_matrix 
  cor_matrix_rm[upper.tri(cor_matrix_rm)] <- 0 # set upper triangle matrix equal to 0
  diag(cor_matrix_rm) <- 0 # diagonal equal to 0
  
  dim(cor_matrix_rm)

  model_controls_NoCor <- model_matrix_controls[ , !apply(cor_matrix_rm,    # Remove highly correlated variables
                                                          2,
                                                          function(x) any(abs(x) > 0.99, na.rm = TRUE))]
  dim(model_controls_NoCor)
  # Check near zero values: few unique values relative to the number of samples and
  # the ratio of the frequency of the most common value to the frequency of the second most common value is large.
  ind_nzv <- caret::nearZeroVar(model_controls_NoCor, freqCut  = 95/1) # Many FE are near 0, actually all of them: 117
  
  # If we need to remove a column do, otherwise continue same matrix of covariates
  if (length(ind_nzv) > 0 ){
    model_controls_NoCor <- model_controls_NoCor[,-ind_nzv] # remove NZV
  }
  
  # Append model dataframe as: T + lagged DV + FE
  
  if (is.na(lagged[count])) { # if there is no lagged DV then force no lagged variable
    model_matrix <- scale(as.matrix(cbind.data.frame(as.matrix(survey[,c(treatment)]), fe)))
    
  } else {
    model_matrix <- scale(as.matrix(cbind.data.frame(as.matrix(survey[,c(treatment, lagged[count])]), fe)))
  }
  
  fixed_covs_index <- dim(model_matrix)[2] # Aux to force these covariates into the LASSO model
  
  # Append columns of baseline controls to create:  lagged DV + FE + controls
  model_matrix <- cbind(model_matrix, model_controls_NoCor)

    # Step 3: Run cv.glmnet to get lamba
  penalties <- rep(TRUE, dim(model_matrix)[2])
  penalties[1:fixed_covs_index] <- FALSE
  
  # if missing values in the DV then subset to non-missing rows of matrix and DV
  if (any(is.na(dep_vars[,count]))) {
    
    model_matrix <- model_matrix[!is.na(dep_vars[,count]), ]
    y_var <- dep_vars[!is.na(dep_vars[,count]), count]
    
  } else { # keep DV as it is
    y_var <- dep_vars[, count]
  }
  
  set.seed(31)
  crossval <- cv.glmnet(x = model_matrix, y = y_var,
                        nfolds = 10, penalty.factor = penalties)
  
  lambda_min <- crossval$lambda.min
  
  # Step 4: fit glm LASSO model
  fit = glmnet(x = as.matrix(model_matrix), y = y_var, alpha = 1, 
               lambda = lambda_min, penalty.factor = penalties) # force treatment, lagged, and FE
  
  # Step 5: Extract non-zero coefficients from LASSO
  coef <- fit$beta # Retrieve coefficient's names and estimated values
  final_vars <- data.frame(name = coef@Dimnames[[1]][coef@i + 1], coefficient = coef@x) # Retrieve non-zero coefficeintes
  
  # IF the last name corresponds to a FE indicator, then no LASSO variables were selected
  if(grepl(".data", tail(final_vars$name, n=1))) {
    vars_names <- NA
  } else {
    index_start <- tail(which(grepl(".data", final_vars$name)), n=1)
    vars_names <- final_vars$name[(index_start + 1):length(final_vars$name)] # Retrieve names of covariates others that T + Lagged + FE
    
  }
  
  # Step 6: Run a normal regression with T + Lagged + FE + covs selected by LASSO
  # Save controls for each regression in a list: LASSO + Lagged
  
  # If NA in lagged or vars_names, drop them
  control_list[[count]] <- c(vars_names, lagged[count])[!is.na( c(vars_names, lagged[count]))]
  
  print(count)
  
}

# Output object we want is "control_list"

