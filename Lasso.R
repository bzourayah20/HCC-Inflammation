#LASSO
{
  y <- Surv(metaRisk$OS_time, metaRisk$OS_status)
  #y <- na.omit(y)
  #genes <- new_expr[,1: 104]
  train <- as.data.frame(train)
  train <- log(train+1,2)
  train <- as.matrix(t(inflammationDEGs.hcc))
  #train <- na.omit(train)
  
  new_expr_clinical <- clinical
  new_expr_clinical$PBK <- new_expr$PBK
  new_expr_clinical$S100A9 <- new_expr$S100A9
  new_expr_clinical$OS_time <- new_expr$OS_time
  new_expr_clinical$OS_status <- new_expr$OS_status
  
  #perform k-fold cross-validation to find optimal lambda value
  
  cv_model <- cv.glmnet(as.matrix(train), y, alpha = 1, family = "cox")
  
  
  #find optimal lambda value that minimizes test MSE
  best_lambda <- cv_model$lambda.min
  best_lambda
  #produce plot of test MSE by lambda value
  plot(cvfit)
  fit <- glmnet(train, y, family = "cox", alpha = 1)
  plot(fit, coef = "lambda")
  cof <-as.data.frame(as.matrix(coef(fit, s = 0.05)))
  cof <- cof %>%
    filter(abs(cof$`1`)>0)
  cvfit <- cv.glmnet(train, y, family = "cox", type.measure = "C")
  
}
