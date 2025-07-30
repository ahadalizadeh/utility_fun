# my pipes 
 "%f%" <- function(a, b) {
     if(is.matrix(b)) {d = dim(b); m =1}
     if(is.vector(b)) {d = length(b); m =0}
      b =as.vector(b)
     res = c()
     for(i in 1:length(b))
     res[i]=  paste0(sprintf(paste0("%.",a,"f"), b[i]) )              
   if(m==1) res=  matrix(res, nrow =d[1] ,ncol =d[2] )  
    return(res) 
   }
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

"%num%" <- function(a=NULL,b=NULL) {
  # Change charecter to numeric. a or b can be null. 
  if(!is.null(a))   {aname <- deparse(substitute(a))
  m.temp.12544521 <<- as.numeric(as.character(a)) 
  te = paste0(aname ,"<- m.temp.12544521")
  temp = parse(text = te)
  eval( temp, envir = globalenv())
  rm(m.temp.12544521, envir = globalenv())}
  
  if(!is.null(b))    {bname <- deparse(substitute(b))
  m.temp.12544522 <<- as.numeric(as.character(b)) 
  te = paste0(bname ,"<- m.temp.12544522")
  temp = parse(text = te)
  eval( temp, envir = globalenv())
  rm(m.temp.12544522, envir = globalenv())}
}


#############################
"%fac%" <- function(a=NULL,b=NULL) {
  # Change charecter to numeric. a or b can be null. 
  if(!is.null(a))   {aname <- deparse(substitute(a))
  m.temp.12544521 <<- as.factor(a)
  te = paste0(aname ,"<- m.temp.12544521")
  temp = parse(text = te)
  eval( temp, envir = globalenv())
  rm(m.temp.12544521, envir = globalenv())}
  
  if(!is.null(b))    {bname <- deparse(substitute(b))
  m.temp.12544522 <<- as.factor(b)
  te = paste0(bname ,"<- m.temp.12544522")
  temp = parse(text = te)
  eval( temp, envir = globalenv())
  rm(m.temp.12544522, envir = globalenv())}
}
#########################################################
#####################easy to do

print.DS = function(res){
  d = paste0(dim(res$data), collapse = ", ")
  length.vars      = length(res$vars)
  length.responses = length(res$responses)
  length.results = length(res$results)
  if(length.vars==0) {vars = "NULL"} else{ vars = paste0(res$vars, collapse = ", ")}
  if(length.responses==0) {responses = "NULL"} else {responses = paste0(res$responses, collapse = ", ")}
  cat(glue::glue("Object of DS class:\n data with {d}  dimentions.\n vars vector: {vars}.\n response vector: {responses}.\n length of results: {length.results}"))
}

responses = function(res, ...,add=FALSE){
  responses <- unique(c(...))
  if("data.frame" %in% class(res) & !("DS" %in% class(res) )){
    if(any(!responses %in% names(res))) 
      stop(
        paste0("the response(s) is not exist:  ", 
               paste0(responses[which(!responses %in% names(res))], collapse  = ", ")))
    
    res = list(data = res, vars= NULL, responses = responses, results = NULL)
    class(res)<-     "DS"
  }
  if("DS" %in% class(res)){
    if(add) res$responses = unique(c(res$responses , responses)) else {
      res$responses = responses  
    }
  }
  class(res) = c("DS")
  res
}



groups = function(res, ..., add = FALSE){
  vars <- unique(c(...))
  if("data.frame" %in% class(res) & !("DS" %in% class(res))){
    if(any(!vars %in% names(res))) 
      stop(
        paste0("the var(s) is not exist:  ", 
               paste0(vars[which(!vars %in% names(res))], collapse  = ", ")))
    
    res = list(data = res, vars= vars, responses = NULL, results = NULL )
    class(res)<-     "DS"
  }
  if("DS" %in% class(res)){
    if(add) res$vars = unique(c(res$vars , vars)) else {
     res$vars = vars   
    }
  }
  class(res) = c("DS")
  res
}



compare_group = function(res){
  data = res$data
  respanse= res$responses
  vars= res$vars
 FH= lapply(1:length(vars),function(j){
   FHJ= lapply(1:length(respanse),function(i){
     YY =   Table_One$new( data, deps.quantitative = respanse[i], group = vars[j])
   return ( cbind.data.frame( group = vars[j], YY$results$quantitative))
  })
    do.call(rbind.data.frame,FHJ)
})
 if(is.null(res$results)) res$results =  FH  else {
 res$results = append(res$results, FH) 
  
 } 
res
}

compare_dist = function(res){
  data = res$data
  respanse= res$responses
  vars= res$vars
  FH= lapply(1:length(vars),function(j){
    FHJ= lapply(1:length(respanse),function(i){
      YY =   Table_One$new( data, deps.qualitative  = respanse[i], group = vars[j])
      return ( cbind.data.frame( group = vars[j], YY$results$qualitative))
    })
    do.call(rbind,FHJ)
  })
  if(is.null(res$results)) res$results =  FH  else {
    res$results = append(res$results, FH) 
   }
  res  
}
# FF = Data%>%    responses (   "J", "H"  )  %>%  groups( "gender", "Marital_status" )
# mydoc  = FF |> compare_group()
# mydoc3 = mydoc%>%    responses (  "gender" ,"education" )  %>%  groups(  "Marital_status" )
# mydoc4= mydoc4 |> compare_dist() 
# mydoc4  = mydoc4 |> compare_group()
