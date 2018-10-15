major.version <- function(libraryJar) {
  if ( ! file.exists(libraryJar) ) stop("File does not exists.")
  tryCatch({
    fn <- unz(libraryJar,"library.properties")
    lines <- readLines(fn)
    close(fn)
    version <- sub("^version.number=","",lines[grepl("^version.number=",lines)])[1]
    if ( substring(version,4,4) == "." ) substr(version,1,3)
    else substr(version,1,4)
  },error=function(e) { stop("Unexpected file format.") } )
}

major.version("lib1.jar")
major.version("lib2.jar")

