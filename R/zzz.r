
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.tropo <- list(
    tropo.path = ""
  )
  toset <- !(names(op.tropo) %in% names(op))
  if(any(toset)) options(op.tropo[toset])
  
  invisible()
}