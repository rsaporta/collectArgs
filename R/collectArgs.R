# ======================================================== #
#   multi-function roxygen2 page using @rdname with NULL   #
# ======================================================== #

#' collectArgs and iterateWithArgs
#' 
#' Functions to cleanly collect arguments from within one function or environment (to then pass to another or to iterate over)
#' 
#' \code{collectArgs()} colects objects from an envrionment into a single list. Generally, the list will then be passed to other functions (usually with \code{\link[base]{do.call}})
#' 
#' \code{iterateWithArgs()} similarly collects the objects in an environment, with the difference that one specific object is selected to iterate over. For each iteration, the given value is passed along with all the other objects to \code{FUNC}.
#'
# # ' @section After Arguments and Value sections:
# # ' Despite its location, this actually comes after the Arguments and Value sections.
# # ' Also, don't need to use null, could annotate first function, and then
# # ' using function name as the groupBy name is more intuitive.
#' 
#' @param except A vector of string values. Objects to \emph{NOT} include in the collection
#'               Generally, the user will not want to pass objets created inside the function and hence will pass to except
#'               _NOTE_ pass the quoted string-name of the object, not the object itself.
#' @param incl.dots A single logical value. Should the \code{...} be collected as well?  
#'                 \emph{NOTE: Has no effect in functions without dots argument}
#'                  Default is \code{TRUE}.
#' @param all.names A single logical value. Passed to \code{ls()}. When \code{FALSE}, then objects whose name begins with a '.' are omitted from the collection
#' 
#' @param increment.level.verbose (For packages and functions that use \code{level.verbose}) If this flag is set to TRUE and there is an arg called \code{level.verbose} of type numeric or integer, it will be incremented by 1.
#' 
#' 
#' @param envir     An \code{environment} object. Passed to \code{ls()}. The environment from which to collect the objects. Defaults to \code{parent.frame}
#'
#' @return 
#' for \code{collectArgs}: A list of all of the objects in \code{envir} (less any objects excluded via the parameters). The names of the list are the names of object in \code{envir}.
#' 
#' for \code{iterateWithArgs}: A list of the return values of \code{FUNC}, the length of \code{arg_to_iterate_over}. Naming of the list will be handled by \code{\link[base]{do.call}}
#' 
#' @name collectArgs-and-iterateWithArgs
#' @examples
#' sample_function <- function(x, base, thresh=500, verbose=TRUE) {
#' 
#'   some_object    <- is.na(x) ## an example of an object that we will exclude
#'   another_object <- 1:10     ## an example of an object that we will exclude
#' 
#'   if (length(x) > 1) {
#'     return(iterateWithArgs(x, FUNC=sample_function, except=c("some_object", "another_object")))
#'   }
#' 
#'   ret <- (base ^ x)
#' 
#'   if (verbose)
#'     cat(base, "^", x, " is ", ifelse(ret > thresh, "", "NOT "), "larger than ", thresh, "\n")
#' 
#'   return(ret)
#' }
#' 
#' sample_function(5, base=2)
#' sample_function(5:10, base=2)
#' 
#' 
#'  some_function <- function(x, param1, param2, etc, ...) {
#' 
#'    ARGS <- collectArgs(except="x")
#'    return(
#'            lapply(x, function(x_i) 
#'               do.call(some_function, c(ARGS, x=x_i))
#'            )
#'          )
#'  }
NULL

#' @rdname collectArgs-and-iterateWithArgs
#' @importFrom stats setNames
#' @importFrom magrittr %>%
#' @export
collectArgs <- function(except=c(), incl.dots=TRUE, all.names=TRUE, increment.level.verbose=getOption("collectArgs.increment.level.verbose", default=FALSE), envir=parent.frame()) {
## FORMERLY, envir was set as:  
#     collectArgs <- function(except=c(), incl.dots=TRUE, all.names=TRUE, envir=as.environment(pos), pos=-1L) {

## GENERAL USAGE:
#  if (is.list(x)) {
#    ARGS <- collectArgs(except="x")
#    return(lapply(x, function(x_i) do.call(fwp, c(ARGS, x=x_i))))
#  }

  stopifnot(requireNamespace("magrittr"))

  force(envir)

  if (length(except) && !is.character(except))
    stop("Invalid value for 'except'; it is not a character. HINT: 'except' should be the quoted string-name of the object, not the object itself.")

  object_names <- ls(envir=envir, all.names=all.names)

  ## Check to make sure that no duplicate object names.  This should be impossible, but better safe than sorry.
  if (anyDuplicated(object_names))
    stop("There are duplicate object names in the environment. collectArgs will not be able to get() them all.")

  ret <- object_names %>% 
            setdiff("...") %>% 
            setdiff(except) %>% 
            setNames(., .) %>%
            lapply(function(x) get(x, envir=envir))
  
  if (increment.level.verbose) {
    if ("level.verbose" %in% names(ret) &&   ( is.numeric(ret[["level.verbose"]]) || is.numeric(ret[["level.verbose"]]) ))
        ret[["level.verbose"]] <- ret[["level.verbose"]] + 1L
  }

  if (incl.dots && exists("...", envir=envir))
      ret <- c(ret, eval(quote(list(...)), envir=envir))
  
  return(ret)
}


#' @rdname collectArgs-and-iterateWithArgs
#'
#' @param FUNC function or string of length 1. function to iterate over.  Normally the same function in which \code{iterateWithArgs} is being called
#' @param arg_to_iterate_over Object, not the string-name of the object.
#' @param nm.arg_to_iterate_over The string-name of the object. 
#' 
#'        Default is \code{as.character(substitute(arg_to_iterate_over))}
#'
#' @importFrom stats setNames
#' @export
iterateWithArgs <- function(arg_to_iterate_over, FUNC, nm.arg_to_iterate_over=as.character(substitute(arg_to_iterate_over)), except=c(), incl.dots=TRUE, increment.level.verbose=getOption("collectArgs.increment.level.verbose", default=FALSE), envir=parent.frame()) {

  stopifnot(requireNamespace("magrittr"))

  if (identical(parent.frame(), globalenv()))
    stop("cannot call iterateWithArgs() from the globalenv()")

  if (missing(nm.arg_to_iterate_over) && length(nm.arg_to_iterate_over) > 1) {
    if (is.list(arg_to_iterate_over))
      nm.arg_to_iterate_over <- nm.arg_to_iterate_over[nm.arg_to_iterate_over != "list"]
  }

  ## VALIDATE -------------------------------------------------------
  ## CONFIRM SAME LENGTH
  L.vals <- length(arg_to_iterate_over)
  L.nms  <- length(nm.arg_to_iterate_over)
  if (!(L.vals == L.nms || 1 %in% c(L.vals, L.nms)))
    stop("\nargument mismatch:\n\n      'arg_to_iterate_over' has length ", length(arg_to_iterate_over), "\n   'nm.arg_to_iterate_over' has length ", length(nm.arg_to_iterate_over))

  if (L.nms > 1 && !is.list(arg_to_iterate_over))
    warning("arg_to_iterate_over is not a list. -- Results might be off.")
  ## VALIDATE -------------------------------------------------------


  force(envir)

  if (length(except) && !is.character(except))
    stop("Invalid value for 'except'; it is not a character. HINT: 'except' should be the quoted string-name of the object, not the object itself.")

  ## Func can be determined from the stack, 
  ## However, this should be avoided, as it opens it up for bugs.
  ## This is useful for quick interactive development only
  if (missing(FUNC)) {
    calls <- sys.calls()
    FUNC <- calls[[length(calls) - 1L]] [[1L]] 
    message("using '", as.character(FUNC), "' in iterateWithArgs() -- note it's safer to add  FUNC='", FUNC, "' in your call.")
  }

  ## capture the function name for error messaging
  if (is.character(FUNC)) {
    func_nm <- FUNC
  } else {
    func_nm <- as.character(as.expression(substitute(FUNC)))
    if (nchar(func_nm) > 50 && grepl("^\\s*function\\s*\\(", func_nm))
      func_nm <- "*an anonymous function*"
  }


  if (any(nm.arg_to_iterate_over == ".")) {
    # stop("iterateWithArgs() cannot receive piped arguments without explicitly setting 'nm.arg_to_iterate_over'\neg use:  iterateWithArgs(x, nm.arg_to_iterate_over=\"x\", ..)")
    stop("iterateWithArgs() cannot receive piped arguments ")
  }

  FUNC <- match.fun(FUNC)

  ARGS <- collectArgs(except=c(nm.arg_to_iterate_over, except), incl.dots=incl.dots, envir=envir)

  .mk_list <- function(a) setNames(nm=nm.arg_to_iterate_over, object=list(a))
  
  ## RETURN
  ## We just need the lapply() statement.  Everything else is error-handling
  tryCatch(
    expr = lapply(arg_to_iterate_over, function(.x_i) {
                do.call(FUNC, c(ARGS, .mk_list(.x_i)))
           })
    , error=function(e) {
        func_info <- paste0("For function `", func_nm, "`, ")
        prefix <- "iterateWithArgs() failed with the following error:"
        hint <- ""
        if (grepl("^unused argument", e$message)) {
          prefix <- "iterateWithArgs() failed due to an 'unused argument' error. The full error is:"
          hint <- "HINT:  This is generally due to having introduced a variable in the\n       calling function, which in turn got picked up by collectArgs()\n       To fix this, add the variable to the 'except' argument of iterateWithArgs()"
        }
        fmt <- paste0(func_info, prefix, "\n%2$s\n    %1$s\n%2$s\n", hint)
        hr <- paste0(rep("-", getOption("width", default = 80)), collapse="")
        error_message <- if (!func_nm %in% as.character(e$call)) paste0("Error in ", as.character(as.expression(e$call)), ":\n      ", e$message) else e$message
        stop(sprintf(fmt, error_message, hr), call.=FALSE)
    }
  ) ## // end of tryCatch
} ## // end of iterateWithArgs()
