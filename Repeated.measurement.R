Repeat.measurment =  function(data = NULL, 
                              formula  , 
                              ID,
                              melt.data = NULL,
                              comparison.formula = NULL,
                              glht.linfct.matrix = NULL,
                              adjust ="none"
){
  # data: wide data
  # formula: like 'cbind(y1, y2, y3) ~(Age+ Sex+ Group)*Time'
  # ID: A variable to identify the subjects 
  # comparison.formula: Formula for multiple comparison like  '~ Group|Time' or '~ Group*Time'
  # adjust: p value adjustment. see '?p.adjust()'
  
  "%+%" <- function(a, b) {
    if(!(is.atomic(a) | is.atomic(b)| is.matrix(a) | is.matrix(b)))
      stop("a and b must be matrix or atomic")
    if(is.atomic(a) & is.atomic(b)){
      z = paste0(a, b)
    } 
    if(is.matrix(a) & is.matrix(b)){
      if( all (dim(a) == dim(b))){
        z = paste0(a, b)
        dim(z) = dim(a)}
    } 
    if((is.matrix(a) & is.atomic(b))){
      if(length(b) == 1){
        z = paste0(a, b)
        dim(z) = dim(a)}
    } 
    if((is.matrix(b) & is.atomic(a))){
      if(length(a) == 1){
        z = paste0(a, b)
        dim(z) = dim(b)}
    } 
    z}
  
  
  
  
  library(car)
  remove_terms <- function(form, term) {
    fterms <- terms(form)
    fac <- attr(fterms, "factors")
    idx <- which(as.logical(fac[term, ]))
    new_fterms <- drop.terms(fterms, dropx = idx, keep.response = TRUE)
    return(formula(new_fterms))
  }
  DV.names = all.vars(update(formula, . ~ 1))
  IV.names = all.vars(update(formula, 1 ~ .)) 
  
  if(!all(IV.names == "Time"))  formulaNoTime=  remove_terms(formula,  "Time")
  if(all(IV.names == "Time"))  formulaNoTime=  update.formula(formula, .~1)
  
  Time = factor(c(DV.names))
  if(! "Time" %in% IV.names) stop("The name of within subjevt varible in the formula must be 'Time'.")
  IV.names = IV.names[which(IV.names != "Time")]
  
  formula2<- formula
  if(!is.null(data) ){ 
    data = na.omit(data[,c(ID, IV.names, DV.names)])
    melt.data = reshape2::melt(data,id= c(ID,IV.names), measure.vars = DV.names, variable.name = "Time")
    formula2 <-Reduce(paste, deparse(update(formula, value ~ .)))
    
  } else 
    if(is.null(melt.data)) {
      stop("data or melt.data is empty")
    }  
  IV.names = c(IV.names, "Time")
  
  options(contrasts = c("contr.sum","contr.poly"))
  ...fixed123456789 <<- formula2
  M1 <-nlme::lme(fixed= as.formula(...fixed123456789),
                 random= as.formula(paste0("~ 1|", ID)), data=melt.data,
                 method="REML",
                 correlation=nlme::corCompSymm(form=   as.formula(paste0("~ 1|", ID))))
  
  
  ML1 <- lm( as.formula(formulaNoTime),
             data=data)
  # M2<- lm( formula,data=data)
  Anova.M1 <- car::Anova(M1 , type = "III")
  (av.ok <- car::Anova(ML1, idata=data.frame(Time = as.factor(paste0("Time", 1:length(DV.names)))), 
                       idesign=~ Time, type = "III") )
  av.ok.s =  summary(av.ok)
  sphericity.tests= av.ok.s$sphericity.tests
  
  eta_squared = effectsize::eta_squared(M1)
  univariate.tests=  av.ok.s
  pval.adjustments= as.data.frame(av.ok.s$pval.adjustments)
  
  main.table =as.data.frame(Anova.M1)  
  main.table$Chisqstat= paste0(round(Anova.M1$Chisq,2), " (", Anova.M1$Df, ")")
  main.table<- data.frame("Chisq (df)"=  main.table[,4],
                          "P.value"=round(  main.table[,3],3))
  row.names(main.table) = row.names(Anova.M1)
  
  
  comparison        = data.frame()
  emm               = data.frame()
  comparison.object = data.frame()
  l=NULL
  if(!is.null(comparison.formula)){
    emm <- emmeans::emmeans(M1, comparison.formula)
    comparison.object = emmeans::contrast(emm, interaction = "pairwise", adjust = adjust) 
    comparison = data.frame(comparison.object)
    # pairs(emm)  # adjust argument not specified -> default p-value adjustment in this case is "tukey"  
    
    if(dim(comparison)[2] == 7 ) {
      by = unique(comparison[,2] )
      
      
      l = list()
      require(multcompView)                       
      for(i in 1:length(by)) {
        comparison.by =comparison[which(comparison[,2] == by[i]), c(1,7)]
        comparison.by.p =  comparison.by[,2]
        comparison.by.name =  strsplit(as.character(comparison.by[,1]), " - ")
        comparison.by.name2 = c()
        for (ii in 1:length(comparison.by.name)) {
          comparison.by.name2[ii] = paste0(comparison.by.name[[ii]], collapse = "-")
        }
        names(comparison.by.p) = comparison.by.name2
        l[[i]]<-multcompLetters(
          comparison.by.p
        )$Letters
        
      }
      l =do.call(rbind,l)
      
      row.names(l)= by
      
    }
    
    if(dim(comparison)[2] == 6 ) {
      
      
      l = c()
      require(multcompView)                       
      comparison.by <-comparison[ , c(1,6)]
      comparison.by.p =  comparison.by[,2]
      comparison.by.name =  strsplit(as.character(comparison.by[,1]), " - ")
      comparison.by.name2 = c()
      for (ii in 1:length(comparison.by.name)) {
        comparison.by.name2[ii] = paste0(comparison.by.name[[ii]], collapse = "-")
      }
      names(comparison.by.p) = comparison.by.name2
      l <-multcompLetters(
        comparison.by.p
      )$Letters
      
    }
    
    
  }
  
  
  ss2 = NULL
  if(!is.null(glht.linfct.matrix)){
    if(ncol(glht.linfct.matrix) != length(coef(M1))) {
      cat("coefficients names have been returned.\nError: ncol(linfct) is not equal to length(coef(model))" )
      return(names(coef(M1)))
      stop(  )   
    }
    library(multcomp)
    warpbreaks.mc= glht(M1, linfct = as.matrix(glht.linfct.matrix))
    ss = summary(warpbreaks.mc)
    ss2= round(cbind.data.frame("Estimate"=ss$test$coefficients, 
                                "Std. Error"= ss$test$sigma, 
                                "z value" = ss$test$tstat, 
                                "pvalues" = ss$test$pvalues) ,3) 
    
  }
  comparison_summray_data = NULL
  
  if(!is.null(comparison.formula)){
  comparison.formula.IV.names = all.vars(update(comparison.formula, 1 ~ .))
  # melt.data<<- melt.data
  
  Rnames <- row.names(l)
  
  comparison_summray_data <-
    melt.data  %>% group_by_at(.vars = comparison.formula.IV.names ) %>%
    summarise(
      y = paste0(round(mean(value, na.rm = TRUE),2)," \u00B1 ", round(sd(value, na.rm = TRUE),2)),
      .groups = 'drop'
    )
  comparison_summray_data <- reshape2::acast(comparison_summray_data, as.formula(
    paste0(comparison.formula.IV.names[2] ,"~",comparison.formula.IV.names[1])) )
  
  
  
  if(all(row.names(comparison_summray_data) == Rnames)) {
    comparison_summray_data = comparison_summray_data %+% l 
    row.names(comparison_summray_data) = row.names(l)
    colnames(comparison_summray_data) = colnames(l)
  } 
  }
  ...fixed123456789<<- NULL  
  rm(...fixed123456789, envir = globalenv()) 
  
  
  
  res = list(main.results = list( main.table = main.table, 
                                  comparison_summray_data = comparison_summray_data,
                                  comparison = comparison, 
                                  letter= l,
                                  glht.results = ss2 ,
                                  univariate.tests = univariate.tests ,
                                  pval.adjustments= pval.adjustments,
                                  eta_squared = eta_squared , 
                                  sphericity.tests= sphericity.tests 
  ),
  invisible.results = list(data = melt.data, 
                           comparison.formula = comparison.formula,
                           lme.model = M1, 
                           car.Anova = Anova.M1, 
                           emmeans = list(emmeans = emm, 
                                          contrast = comparison.object))                 
  
  )
  class(res) = "Repeat.measurment"
  res
}

#############################################################################################################################
Repeat.measurment.helper  = function(  data = NULL, 
  formula, 
  ID,
  group.name = "Group",
  adjust ="none",
  wd.Table = FALSE){
  comparison.formula2 = as.formula(paste0("~ Time|",group.name))
  comparison.formula1 = as.formula(paste0(  "~", group.name, "|Time" ))
  
  if(!is.factor(data[[group.name]])) stop("Group must be factor!")
  MM1 =Repeat.measurment (data =data, formula = formula, ID = ID,
                          comparison.formula =comparison.formula1, adjust=adjust)
  
  MM2 =Repeat.measurment (data =data, formula = formula, ID = ID,
                          comparison.formula = comparison.formula2, adjust=adjust)
  
  
  if(any(MM1$main.results$univariate.tests$sphericity.tests[,"p-value"]<0.05)){
    GG = MM1$main.results$univariate.tests$pval.adjustments[,c("GG eps","Pr(>F[GG])")]
    newGG =( MM1$main.results$univariate.tests$univariate.tests)
    newGG[rownames(GG),c( "num Df", "den Df")] = newGG[rownames(GG),c( "num Df", "den Df")]*GG[1,1]
    newGG[rownames(GG),c( "Pr(>F)")] = GG[rownames(GG),c( "Pr(>F[GG])" )] 
  }
  newGG = as.data.frame(newGG)[1:dim(newGG)[1], ]  
  
  
  multivariate.tests  =MM1$main.results$univariate.tests$multivariate.test
  multivariate.tests.res = list()
  for (i in 1:length(multivariate.tests)) {
    multivariate.tests.res[[i]] = myprint(multivariate.tests[[i]])["Pillai",c("test stat","approx F","num Df",
                                                                              "den Df", "Pr(>F)")]
  }
  names(multivariate.tests.res) = names(multivariate.tests)
  multivariate.tests.res = do.call(rbind,multivariate.tests.res)
  names(multivariate.tests.res) =
    c("Pillai statistic","Pillai F","Pillai num Df",                                               "Pillai den Df", "Pillai P-value") 
  
  multivariate.tests.res$`Pillai P-value` = round(multivariate.tests.res$`Pillai P-value`, 3)
  
  re = cbind(name = rownames(newGG),multivariate.tests.res,(newGG))
  
  eta = as.data.frame(MM1$main.results$eta_squared)
  eta$Parameter
  re$eta_squared = NA
  re[eta$Parameter,"eta_squared"] = round(eta$Eta2_partial,3)
  
  MM1$main.results$comparison_summray_data
  tt= t(MM2$main.results$comparison_summray_data)
  MM1$main.results$comparison$effect = NA
  MM1$main.results$comparison$effect = paste0(
    round(MM1$main.results$comparison$estimate,2)," (",
    round(MM1$main.results$comparison$estimate - 1.96*
            MM1$main.results$comparison$SE,2),", ",
    round(MM1$main.results$comparison$estimate + 1.96*
            MM1$main.results$comparison$SE,2),")"
  )
  MM1$main.results$comparison[,c( "p.value" )] = round(MM1$main.results$comparison[,c( "p.value" )],3)
  
  rr=cbind(  "Measurement time points" =as.character(MM1$main.results$comparison$Time),tt[MM1$main.results$comparison$Time,],
             MM1$main.results$comparison[,   
                                         c( "p.value","effect")])
  rr_dim1 = dim(rr)[1]
  rr_dim2 = dim(rr)[2]
  re_dim1 = dim(re)[1]
  re_dim2 = dim(re)[2]
  rrre = rep(NA,(rr_dim2+re_dim2)*max(rr_dim1,re_dim1))
  dim(rrre) = c(max(rr_dim1,re_dim1), rr_dim2+re_dim2)
  rrre = as.data.frame(rrre)
  rrre[1:rr_dim1, 1:rr_dim2] = rr
  rrre[1:re_dim1, rr_dim2+1:re_dim2] = re
  names(rr)[which(names(rr)=="effect" )] = "Mean Difference (95% CI)"
  names(re) =  c("Effect","Pillai's statistic", "Pillai F" ,       
                 "Pillai num Df","Pillai den Df","Pillai P-value","Sum Sq",          
                 "num Df","Error SS","den Df","F value" ,        
                 "P-value","Eta-Squared" )
  
  
  
  names(rrre) = c(names(rr), names(re))
  comment_rrre =
    paste0(
      "The P-values of within group comparisons: ",
      paste0(  MM2$main.results$comparison$Time_pairwise,
               " in group " ,
               MM2$main.results$comparison$Group,
               " is ",
               round(MM2$main.results$comparison$p.value,3)
               , collapse = ", "))
  
  
 library(dplyr)
rrre =  rrre   %>% mutate_if(is.numeric, round, 3)

rrre = cbind(Response = "", rrre)
rrre[1,1] =  paste0(all.vars(update.formula(formula, .~1)), collapse = "|")

for (i in 1:dim(rrre)[1]) {
  for (j in 1:dim(rrre)[2]) {
    if(is.na(rrre[i,j]))    rrre[i,j] = " "
  }
}
rrre$`Error SS` = NULL
rrre$`Sum Sq` = NULL

wd.Table2=
  function(x=rrre,... , filename=NULL, path = ""){
    if("RDCOMClient" %in% rownames(installed.packages()) == FALSE)  { 
      # Sys.setenv("TAR" = "internal") # if you need it.
      # devtools::install_github("omegahat/RDCOMClient")
      devtools::install_github('omegahat/RDCOMClient')
    }
    R2wd::wdGet(filename,path , method="RDCOMClient")
    R2wd::wdBody("\n\n")
    R2wd::wdTable(as.data.frame(x) ,... )
    cat("Done!\n")
  }

if(isTRUE(wd.Table))   wd.Table2(rrre,comment_rrre)
list(results = rrre, models = list(MM1,MM2))
  
}




###################Examples#######################
# Data  = data.frame(
#   R = 1:1000,
#   Age = abs(rnorm(1000,32,10)),
#   Group = factor(rbinom(1000,3,0.5)),
#   Sex = factor(rbinom(1000,1,0.5)),
#   y1 =  (rnorm(1000)),
#   y2 =  (rnorm(1000)) ,
#   y3 = abs(rnorm(1000))
# )
# 
# 
# MM1 =Repeat.measurment (data =Data, formula = cbind(y1, y2, y3) ~(Age+ Sex+ Group)*Time,
#                         ID = "R",
#                         comparison.formula = ~ Group|Time)
# MM1$main.results$letter
# 
# MM2 =Repeat.measurment (data =Data, formula = cbind(y1, y2, y3) ~(Age+ Sex+ Group)*Time, 
#                         ID = "R",
#                         comparison.formula = ~ Group*Time)
# 
# 
# MM2$main.results
# 
# Data$Group = as.factor(Data$Group)
# rrre =Repeat.measurment.helper (data =Data, formula = cbind(Before, After, Followup) ~(  Group)*Time,
#                         ID = "ID",
#                       group.name = "Group" ,
#                       wd.Table = T)
