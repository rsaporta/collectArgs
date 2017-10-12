'.' <- "dummy variable so that R CMD check will not complain"

## hmmm... this doesn't work for some reason, so using the above solution instead
# #' @importFrom utils suppressForeignCheck
# utils::suppressForeignCheck(c("."))

## This is a generic .onLoad function
# .onLoad <- function(libname, pkgname, verbose=getOption("verbose.onLoad", default=TRUE)) {
#   if (verbose)
#     cat("Running .onLoad() from ", pkgname, "  ||   [turn off this message with options(verbose.onLoad = FALSE) ]\n")
# 
#   ## -------------------- OPTIONS FOR PACKAGE ----------------------- ##
#   ## Options to Load.  
#   ##   All Values should be quoted strings
#   ##   Actual string values should have quotes inside the quotes
#   ##     eg  "\"an example string\"" or "'an example string'"
#   ## Code adapted from the data.table package
#   opts = c(  "test.collectArgs.some_string"    = "'a string'"
#            , "test.collectArgs.some_number"    = "Inf"
#            , "test.collectArgs.some_integer"   = "100L"
#           )
#   for (i in setdiff(names(opts),names(options()))) {
#       eval(parse(text=paste("options(",i,"=",opts[i],")",sep="")))
#   }
#   ## ---------------------------------------------------------------- ##
# 
# 
#   ## What else should happen when this package loads? 
#   "... more stuff ..."
#   
#   return(invisible(TRUE))
# }
# 
# 
