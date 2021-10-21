customWarning <- function(class.name, message, call = sys.call(-1L), ...)
{
    custom.warning <- createCustomWarningStructure(class.name, message = message, call = call, ...)
    warning(custom.warning)
}

createCustomWarningStructure <- function(class.name, message, call = sys.call(-1L), ...)
{
    structure(class = c(class.name, "condition"),
              list(message = message, call = call),
              ...)
}

evalHandlingConditions <- function(function.call, env)
{
    function.call[["warn"]] <- TRUE
    withCallingHandlers(eval(function.call, envir = env),
                        MissingValuesIgnored = handleCustomWarning)
}

handleCustomWarning <- function(condition)
{
    if (!is.null(findRestart("muffleWarning")))
        invokeRestart("muffleWarning")
}
