
TE = tempfile(fileext = ".py")
download.file("https://raw.githubusercontent.com/ahadalizadeh/utility_fun/refs/heads/master/Word.py", TE)
reticulate::source_python(TE)

R2doc = function(df, doc=NULL, columnsname= TRUE, rowsnames = TRUE, title="", caption="", save=TRUE, filename="Result", path=NULL){
df = as.data.frame(df)
  if(rowsnames)  {
   df = cbind.data.frame(Factors = row.names(df), df )
  }
df.names = NULL
if(columnsname)  {
  df.names = names(df) 
}
py2doc(df = df, doc=doc, columnsname= df.names,   title=title, 
       caption=caption, save=save, filename=filename, path=path )


}

cat("use:\nmydoc= R2doc(R2doc = function(df, doc=NULL, columnsname= TRUE, rowsnames = TRUE, title=\"\", caption=\"\", save=TRUE, filename=\"Result\", path=NULL)\n")
