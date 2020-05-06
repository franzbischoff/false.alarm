source("renv/activate.R")
setHook("rstudio.sessionInit", function(newSession) {
 if (newSession & is.null(rstudioapi::getActiveProject()))
   rstudioapi::openProject("heads_thesis.Rproj")
}, action = "append")
