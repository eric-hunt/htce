
##-------------------------------------------------------------------------
##  zzz.R                                                                --
##-------------------------------------------------------------------------

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.htce <- list(
    htce.tabular_class = "data.frame"
  )
  toset <- !(names(op.htce) %in% names(op))
  if (any(toset)) options(op.htce[toset])
}
