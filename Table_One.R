
 
Table_One <-   R6::R6Class(
  "table.One",
   public =  list(
    data                 = NULL,
    group                = NULL,
     deps.quantitative    = NULL,
    deps.qualitative     = NULL,
    wilcox               = FALSE,
     results              = list(),
     initialize          = function(data, group, 
                          deps.quantitative=NULL, 
                          deps.qualitative=NULL, 
                          wilcox = FALSE) {
       self$data = data 
       self$group = group
       self$deps.quantitative = deps.quantitative
       self$deps.qualitative = deps.qualitative
       self$wilcox = wilcox
       if(!is.null(deps.quantitative)) self$add.quantitative()
       if(!is.null(deps.qualitative)) self$add.qualitative()
     },
      
     add.quantitative = function(data = NULL, 
                                 group = NULL, 
                                 deps.quantitative = NULL, 
                                 wilcox = NULL) {
       
     if(is.null(data))               data               = self$data
     if(is.null(group))              group              = self$group
     if(is.null(deps.quantitative))   deps.quantitative  = self$deps.quantitative
     if(is.null(wilcox))             wilcox             = self$wilcox
       
       private$ttest(data = data, group = group, deps = deps.quantitative, wilcox = wilcox )
     },
     add.qualitative = function(data = NULL, 
                                group = NULL, 
                                deps.qualitative  = NULL
                                ) {
       y = group
       x = deps.qualitative
       if(is.null(data)) data = self$data 
       if(is.null(y))       y = self$group
       if(is.null(x))       x = self$deps.qualitative
       
       for (i in 1:length(x)) {
         private$chisqTest(data, x[i], y)
       }
     } ,
    combine = function() {
      self$results$combined <- 
        as.data.frame(rbind(self$results$quantitative, self$results$qualitative))
    }
    ,
    
    wd.Table =
      function(x= NULL,..., filename=NULL, path = ""){
        if(is.null(x))
        x <- as.data.frame(rbind(self$results$quantitative, self$results$qualitative))
         if("RDCOMClient" %in% rownames(installed.packages()) == FALSE)  { 
          # Sys.setenv("TAR" = "internal") # if you need it.
          # devtools::install_github("omegahat/RDCOMClient")
          install.packages('RDCOMClient', repos = 'http://www.omegahat.org/R') }
        R2wd::wdGet(filename,path , method="RDCOMClient")
        R2wd::wdBody("\n\n")
        R2wd::wdTable(as.data.frame(x), ...)
        cat("Done!\n")
      }
    ),
  private  = list(
    ttest     = function(data, group, deps, wilcox = FALSE) {
      if(!(length(deps)== length(wilcox) | length(wilcox) == 1 ))
        stop("length of wilcox must be the same as deps or one!")
      # self$data = data 
      # self$group = group
      # self$deps = deps
      # `self$data` contains the data
      # `self$options` contains the options
      # `self$results` contains the results object (to populate)
      # table <- self$results$Table
      group.value <- c(data[[group]])
      group.level<- sort(unique(group.value))
      if(length(group.level) != 2) stop("level of group must be 2.")
      i = 0
      for (dep in  deps) {
        if(length(deps)== length(wilcox)) {i = i +1} else {i =1}
        d= data[[dep]]
        G1.shapiro <- shapiro.test(d[group.value==group.level[1]])$p.value
        G2.shapiro <- shapiro.test(d[group.value==group.level[2]])$p.value
        shapiro    <- (G1.shapiro > 0.05) & (G2.shapiro > 0.05)
        
        
        formula <- jmvcore::constructFormula(dep,  group)
        formula <- as.formula(formula)
        results <- t.test(formula, data =  data)
        test.r <- "t-test" 
        if(isTRUE(wilcox[i]))        {results <- wilcox.test(formula, data =  data); test.r <- "Mann-Whitney U" } 
        if(isTRUE(shapiro))  {results <- wilcox.test(formula, data =  data); test.r <- "Mann-Whitney U" }  
        # self$results$text$setContent(table)
         values =c(   
          name = dep,
          level = NA,
          var1 = paste0(sprintf("%.2f", round(mean(d[group.value==group.level[1]],na.rm=TRUE),2)), 
                        ' \u00B1 ',sprintf("%.2f",round(sd(d[group.value==group.level[1]],na.rm=TRUE),2)) ),
          var2 = paste0(sprintf("%.2f",round(mean(d[group.value==group.level[2]],na.rm=TRUE),2)), 
                        ' \u00B1 ',sprintf("%.2f",round(sd(d[group.value==group.level[2]],na.rm=TRUE),2)) ),
          p = sprintf("%.3f",round(results$p.value,3)),
          test = test.r)
        names(values)[3:4] <- group.level 
         self$results$quantitative <- rbind(self$results$quantitative,values)
         row.names(self$results$quantitative) <- NULL
         }
    },
    chisqTest = function(data=NULL, x, y){
      # y: Column variable.
      # x: row variable.
      deps.qualitative = x
        x <- c(data[[x]])
        y <- c(data[[y]])
      
      
      t <- table(x ,y)
      p.t <- prop.table(t,2)
      s.t <- matrix(paste0(t,"(", round(p.t*100,2), ")"),nrow = dim(t)[1])
      "%f%" <- function(a, b) paste0(sprintf(paste0("%.",b,"f"), a) )              
      s.t <-cbind(s.t, chisq.test(x,y, simulate.p.value = TRUE)$p.value %f% 3)
      row.names(s.t)<-  NULL  
      colnames(s.t)<-  c(colnames(t), "p")
      s.t <- cbind(name = deps.qualitative, level =  row.names(t) , s.t, test = "Chi-squared")
      self$results$qualitative <- rbind(self$results$qualitative,s.t)
    }
)
)
 

# D<-  Table_One$new(data = data, group =  "HbA1c.Cat8", 
#                    deps.qualitative = c("Sex", "Metforminuse","Sulfonylureause",
#                                         "Statinuse"),
#                    deps.quantitative =
#                      c("AIPlogTGHDLC","HOMAIR", 
#                        "PON1activity", "Age","BMI" ))
# # D$deps.quantitative()
# # D$add.qualitative()
# # D$add.qualitative(deps.qualitative = "rs115.Cat")
# D$combine()
# D$wd.Table()
# D$results

 
