
TE = tempfile(fileext = ".py")
download.file("https://raw.githubusercontent.com/ahadalizadeh/utility_fun/refs/heads/master/Word.py", TE)
reticulate::source_python(TE)
cat("Ready!")
cat("use: mydoc= py2doc(df=df ,title=\"International System of Units\",save=False)\n")
cat("     py2doc(df=df, doc=mydoc ,title=\"International Sys\", caption=\"Table One\")\n")
