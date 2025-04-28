
# Calculate R-squared




GLM_Analysis <- R6::R6Class("GLM_Analysis", lock_objects = FALSE, lock_class = FALSE,
                            public         = list(
                              data        = NULL,
                              n.model     = 1,
                              bayes       = FALSE,
                              # main.table  = data.frame( ),
                              result      = list( ),
                              # result$models =list(),
                              family      = "gaussian",
                              sepration   = c(),
                              R2          = data.frame(Formula= NA, R2 = NA ),
                              # plot        = NULL,
                              univariate  = FALSE,
                              n.model.univariate  = 0,
                              stepwise    = FALSE,
                              initialize = function(data, formula, family = "gaussian", bayes = FALSE,
                                                    univariate=FALSE, stepwise=FALSE, digits = 3){
                                self$digits      = digits
                                self$data      = data
                                self$family    = family
                                self$bayes     = bayes
                                self$univariate  = univariate
                                self$stepwise     = stepwise
                                
                                n.model        = 1
                                "%+%" <- function(x,y) paste0(x,y)
                                m1 <- glm(formula = formula, family = family, data = na.omit(data[,all.vars(formula)]))
                                deviance <- summary(m1)$deviance
                                null_deviance <- summary(m1)$null.deviance
                                rsquared <- 1 - (deviance / null_deviance)
                                rsquaredS = c(paste0( all.vars(formula ), collapse = ", "))
                                d.m =    dim( self$R2)
                                self$R2[d.m+1,1] =    rsquaredS   
                                self$R2[d.m+1,2] =    rsquared
                                
                                
                                
                             self$sepration[self$n.model] <- !(m1$converged)
                                
                                
                                m1.ci = private$CI(m1, Family = self$family)$type2.result
                                if(self$sepration[self$n.model] | self$bayes){
                                  m1 = arm::bayesglm(formula = formula, family = family, data = data)
                                  deviance <- summary(m1)$deviance
                                  null_deviance <- summary(m1)$null.deviance
                                  rsquared <- 1 - (deviance / null_deviance)
                                  rsquaredS = c(paste0( all.vars(formula ), collapse = ", "))
                                  d.m =    dim( self$R2)
                                  self$R2[d.m+1,1] =    rsquaredS   
                                  self$R2[d.m+1,2] =    rsquared
                                  
                                  
                                  m1.ci = private$CI(m1, Family = self$family)$type2.result
                                  cat("\nWe re-analyzed using bayesian glm due to Separation.\nIgnore  warning!\n")
                                  names(m1.ci) =c("Effect ","P value ") %+% "Model " %+% n.model %+% "*"
                                
                                  
                                  } else {
                                  names(m1.ci) =c("Effect ","P value ") %+% "Model " %+% n.model
                                }
                                
                                
                                self$result$main.table <- m1.ci
                                self$result$models <- c( self$result$models,m1)
                                
                                
                                if(isTRUE(univariate)){
                                  ind=  all.vars(update( formula, 1~.))
                                  dep=  all.vars(update( formula, .~1))
                                  self$n.model.univariate = length(ind)
                                  for (i in 1:length(ind)) {
                                    self$add(formula = as.formula(paste0(dep, "~", ind[i]))) 
                                  }
                                }
                                if(isTRUE(stepwise)){
                                  require(MASS)
                                  best= stepAIC(m1, trace = 0 )$formula
                                  self$add(formula =  best) 
                                  
                                }
                                
                                
                                
                                
                                
                              },
                              add = function(data = NULL, 
                                             formula, 
                                             family = self$family, 
                                             m = self$result$main.table, 
                                             n.model = self$n.model + 1 , 
                                             bayes = FALSE){
                                if(is.null(data)) data = self$data
                                self$n.model = n.model 
                                self$bayes = bayes
                                "%+%" <- function(x,y) paste0(x,y)
                                m1 =  glm(formula = formula, family = family, data = data)
                               
                                deviance <- summary(m1)$deviance
                                null_deviance <- summary(m1)$null.deviance
                                rsquared <- 1 - (deviance / null_deviance)
                                rsquaredS = c(paste0( all.vars(formula ), collapse = ", "))
                                d.m =    dim( self$R2)
                                self$R2[d.m+1,1] =    rsquaredS   
                                self$R2[d.m+1,2] =    rsquared
                                
                                self$sepration[self$n.model] <- !(m1$converged)
                                m1.ci = private$CI(m1, Family = self$family)$type2.result
                                if(self$sepration[self$n.model]| self$bayes){
                                  m1 = arm::bayesglm(formula = formula, family = family, data = data)
                                  
                                  deviance <- summary(m1)$deviance
                                  null_deviance <- summary(m1)$null.deviance
                                  rsquared <- 1 - (deviance / null_deviance)
                                  rsquaredS = c(paste0( all.vars(formula ), collapse = ", ") )
                                  d.m =    dim( self$R2)
                                  self$R2[d.m+1,1] =    rsquaredS   
                                  self$R2[d.m+1,2] =    rsquared   
                                  
                                  
                                  m1.ci = private$CI(m1, Family = self$family)$type2.result
                                  cat("\nWe re-analyzed using bayesian glm due to Separation.\nIgnore  warning!\n")
                                  names(m1.ci) =c("Effect ","P value ") %+% "Model " %+% n.model %+% "*"
                                } else {
                                  names(m1.ci) =c("Effect ","P value ") %+% "Model " %+% n.model
                                }
                                
                                m = base::merge(m,m1.ci, by="row.names",all=TRUE, suffixes = c("",""))
                                row.names(m) <- m$Row.names
                                m$Row.names <- NULL
                                self$result$main.table <- m
                                self$result$models <- c( self$result$models,m1)
                                
                              },
                              
                              
                              
                              
                              wd.Table =
                                function(x= self$result$main.table,..., filename=NULL, path = ""){
                                  caption = "- * Calculated by Bayesian Logistic regression due to data sparsity."
                                  if(!any(self$sepration))   caption= ""
                                  if("RDCOMClient" %in% rownames(installed.packages()) == FALSE)  { 
                                    # Sys.setenv("TAR" = "internal") # if you need it.
                                    # devtools::install_github("omegahat/RDCOMClient")
                                    install.packages('RDCOMClient', repos = 'http://www.omegahat.org/R') }
                                  R2wd::wdGet(filename,path , method="RDCOMClient")
                                  R2wd::wdBody("\n\n")
                                  R2wd::wdTable(as.data.frame(x),caption = caption, ...)
                                  cat("Done!\n")
                                },
                              write.cb=
                                function(x = self$result$main.table, 
                                         row.names=TRUE, col.names=TRUE, 
                                         comment=FALSE, 
                                         text=NULL, ...){ 
                                  text = " * Calculated by Bayesian Logistic regression due to data sparsity."
                                  if(!any(self$sepration))   text = ""
                                  datafile <- file("clipboard", open='wt')
                                  on.exit(close(datafile))
                                  if(comment == TRUE)   {
                                    if(is.null(comment(x))) warning("There is no comment for x! first add one by comment(x) = '...'") else
                                      writeLines(comment(x), con=datafile)}
                                  write.table(x, file = datafile, sep = "\t", row.names = row.names,
                                              col.names = col.names, ...)
                                  if(!is.null(text))   {writeLines(text , con=datafile)}
                                },
                              
                              merge.univariate = function(f = self$result$main.table, 
                                                          univariate=self$univariate, 
                                                          n.model.univariate = self$n.model.univariate,
                                                          n.model = self$n.model){
                                
                                if( (isFALSE(univariate)| (n.model.univariate == 0))) stop("univariate models do not exist. Run with 'univariate= TRUE'.")
                                n = dim(f)[2] 
                                
                                d = f[ c(3:(n.model.univariate*2 +2 ))]  
                                
                                
                                
                                d[1,] = NA
                                dd = data.frame(est = rep(NA,  n[1]), p.value = rep(NA,  n[1]))
                                for(j  in (1:dim(d)[2])[as.logical((1:dim(d)[2]) %% 2)]){
                                  k =  which(!is.na(d[,j]))
                                  dd[k,] = d[k, c(j,j+1)]
                                }
                                
                                if(n.model.univariate+1 < n.model)
                                  re = cbind(f[,1:2], dd, f[, (n.model.univariate*2 +3 ):(n.model*2)])
                                
                                if(n.model.univariate+1 == n.model)
                                  re=  cbind(f[,1:2], dd)
                                
                                re
                              }
                            ),
                            
                            private = list(
                              CI = function(M0,round=self$digits, Family=self$family){
                                # M0: glm or arm::bayesglm object.
                                Re2<- data.frame()
                                if( class(M0)[1]== "bayesglm" ){
                                  # cat("Bayesian method by bayesglm\n\n")
                                  s.M0=summary(M0)
                                  M0.s=coef(arm::sim(M0,n=10000))
                                  CI<- apply(M0.s,2,quantile,
                                             c(0.025,0.975))
                                  mean.sim <- apply(M0.s,2,mean)
                                  
                                  if(Family == "binomial"){
                                    Re=data.frame (OR= round( exp(mean.sim),round),round(t( exp(CI)),round),P.value=round(s.M0$ coefficients[,4],round+1))
                                    Re2= data.frame("OR"= ( paste0( Re[,1],"(", Re[,2],",", Re[,3], ")")), "P value"= Re[,4])
                                  }
                                  
                                  if(Family == "gaussian"){
                                    Re=data.frame ("beta"= round( (mean.sim),round),round(t( (CI)),round),P.value=round(s.M0$ coefficients[,4],round+1))
                                    Re2= data.frame("beta"= ( paste0( Re[,1],"(", Re[,2],",", Re[,3], ")")), "P value"= Re[,4])
                                  }
                                  
                                  row.names(Re2)=row.names(Re)
                                }
                                
                                
                                if( class(M0)[1]== "glm" ){
                                  # cat("Classic method by glm\n\n")
                                  s.M0=summary(M0)
                                  CI <-  (confint(M0))
                                  self$sepration[self$n.model] = (self$sepration[self$n.model] | any(is.na(CI)))
                                  mean.sim <- s.M0$coefficients[,1]
                                  
                                  
                                  if(Family == "binomial"){
                                    Re=data.frame (OR= round( exp(mean.sim),round),round(( exp(CI)),round),P.value=round(s.M0$ coefficients[,4],round+1))
                                    Re2= data.frame("OR"= (paste0( Re[,1],"(", Re[,2],",", Re[,3], ")")), "P value"= Re[,4])
                                  }
                                  
                                  if(Family == "gaussian"){
                                    Re=data.frame ("beta"= round( mean.sim,round),round(( (CI)),round),P.value=round(s.M0$ coefficients[,4],round+1))
                                    Re2= data.frame("beta"= ( paste0( Re[,1],"(", Re[,2],",", Re[,3], ")")), "P value"= Re[,4])
                                  }
                                  
                                  
                                  row.names(Re2)=row.names(Re)
                                }
                                list(type1.result=Re,type2.result=Re2)
                              } 
                              
                              
                              
                              
                              
                              
                            )
)
