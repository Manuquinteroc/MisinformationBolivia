# Inverse Covariance Weigthing Function
# df: dataframe with selected columns
# reverse: column indexes to change sign 
# Output: list with weights and the index variable

# ------------------------------------------------------------------------------

ICW_function <- function(df, reverse = NULL) {

  vars <- Rfast::colVars(as.matrix(df), na.rm = T)
  
  if (length(reverse) > 0 ) {
    df[, reverse] <- -1*df[, reverse]
  }
  
  sum_inverse <- (sum(1/vars))^-1 # sum of the inverse of variances to the inverse
  
  weights <- (1/vars)*sum_inverse # inverse vector of variances times sum of inverses to the inverse
  
  # Temporarily convert NA values
  x0 <- df
  x0[is.na(df)] <- 0
  
  index <- t(weights%*%t(x0)) # Calculate the index.  t(W*t(DF))
  
  if (all(colSums(is.na(df)) > 0 )) { # if all columns have NA values set NA values to index in those obs
    index[is.na(df[,1])] <- NA
  }
  
  return(list(weights, index)) # return weights and index
}


# ------------------------------------------------------------------------------
# Cyrus Samii Function from: https://cyrussamii.com/?p=2177

# Function that takes in data in matrix format and returns
# (i) IC weights and (ii) ICW index.
# Weights can be incorporated using the "wgts" argument.
# The "revcols" argument takes a vector indicating which columns,
# if any, should have values reversed (that is, standardized 
# values are multiplied by -1) prior to construction of the index. 

matStand <- function(x, sgroup = rep(TRUE, nrow(x))){
  for(j in 1:ncol(x)){
    x[,j] <- (x[,j] - mean(x[sgroup,j], na.rm = T))/sd(x[sgroup,j], na.rm = T)
  }
  return(x)
}

# Function that takes in data in matrix format and returns
# (i) IC weights and (ii) ICW index.
# Weights can be incorporated using the "wgts" argument.
# The "revcols" argument takes a vector indicating which columns,
# if any, should have values reversed (that is, standardized 
# values are multiplied by -1) prior to construction of the index. 

# This function works for ATE + HEs (It does not for PSM)
icwIndex <- function(	xmat,
                      wgts=rep(1, nrow(xmat)),
                      revcols = NULL,
                      ind_na = 1, # auxiliary index for PSM column with 463 responses of xmat
                      sgroup = rep(TRUE, nrow(xmat))){
  
  # Modify to accept NA values 
  X0 <- xmat
  
  X <- matStand(X0, sgroup)
  
  X[is.na(xmat)] <- 0
  
  
  if(length(revcols)>0){
    X[,revcols] <-  -1*X[,revcols]
  }
  
  i.vec <- as.matrix(rep(1,ncol(xmat)))
  Sx <- cov.wt(X, wt=wgts)[[1]]
  weights <- solve(t(i.vec)%*%solve(Sx)%*%i.vec)%*%t(i.vec)%*%solve(Sx)
  index <- t(solve(t(i.vec)%*%solve(Sx)%*%i.vec)%*%t(i.vec)%*%solve(Sx)%*%t(X))
  
  
  if (all(colSums(is.na(xmat)) > 0 )) { # if all columns have NA values set NA values to index in those obs
    index[is.na(xmat[,ind_na])] <- NA # Implement ind_na as an auxiliary index for a column that has 463 responses (ONLY PSM)
  }
  
  
  return(list(weights = weights, index = index))
}


