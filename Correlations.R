correlations = function(data){
source("https://raw.githubusercontent.com/ahadalizadeh/utility_fun/master/Pips.R")
library("Hmisc")
res2 <- rcorr(as.matrix(data))
pp = ((2%f%res2$r) %+% " (") %+% ((3 %f% res2$P) %+% ")")
colnames(pp)      =  colnames(res2$r )  
row.names(pp)  =  row.names(res2$r ) 
list(combine = pp, r = res2$r, p = res2$P)
}