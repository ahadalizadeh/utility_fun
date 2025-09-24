Cronbach.alpha = function(data, q){
  #Cronbach.alpha(Data,paste0("G",1:12))

library(ltm)
CCs = cronbach.alpha(data[,q], standardized = TRUE, CI = TRUE, 
               probs = c(0.025, 0.975), B = 1000, na.rm = FALSE)

CC = cronbach.alpha(data[,q], standardized = FALSE, CI = TRUE, 
                    probs = c(0.025, 0.975), B = 1000, na.rm = FALSE)
CCs = paste0(round(CCs$alpha,4)*100," (", paste0(round(CCs$ci,4)*100, collapse = ", " ),")"  ) 
CC = paste0(round(CC$alpha,4)*100," (", paste0(round(CC$ci,4)*100, collapse = ", " ),")"  ) 

data.frame(`Standardized.Cronbach.alpha` = CCs, `Cronbach.alpha` = CC )
}
