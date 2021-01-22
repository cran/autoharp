## this should go under zzz.R, but I cant create a new R file, the building part will give error

.onLoad <- function(libname, pkgname) {
  #ahook_list <- autoharp_hooks[names(autoharp_hooks) != "autoharp.set"]
  #knitr::knit_hooks$set(ahook_list)
  knitr::knit_hooks$set(autoharp_hooks)
}

.onUnload <- function(libpath) {
  knitr::knit_hooks$restore()
  #knitr::knit_hooks$delete("autoharp.objs")
  #knitr::knit_hooks$delete("autoharp.scalars")
}

.onDetach <- function(libpath) {
  knitr::knit_hooks$restore()
  #knitr::knit_hooks$delete("autoharp.objs")
  #knitr::knit_hooks$delete("autoharp.scalars")
}



