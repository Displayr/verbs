#' @rdname variabilityOperations
#' @title General-Purpose Calculation of Variance and Standard Deviation
#'
#' @description \code{Variance} (\code{StandardDeviation}) is computes variance
#'     (standard deviation) \code{\link{var}} but supporting additional
#'     pre-processing, matching and weighting of data before calculation.
#' @param ... Input objects to compute the variance or standard deviation;
#'     e.g. vectors, matrices, Variables, Variable Sets or Q Tables
#' @inheritParams Sum
#' @details If a single input is provided, then the variance or standard deviation
#'     of all elements in the input is calculated (after possible subsetting or
#'     weighting).  If multiple inputs are provided, then element-wise calculation
#'     of the variance is performed.
#'
#'     If the inputs have named elements, then these names can be used to
#'     match up each of the elements between inputs via the
#'     \code{match.rows} and \code{match.columns} arguments. If either
#'     of \code{match.rows} or \code{match.columns} is set to
#'     \code{"No"} then names are ignored and the length on that
#'     dimension needs to agree between inputs. Partial dimension
#'     agreement is also supported. For example if an n x p matrix is
#'     used as the first input, then the second input could be an n x
#'     1 column vector that is recycled to an n x p matrix before
#'     calculation.
#' @return If a single input is provided, a single number is returned,
#'     the (possibly weighted) variance or standard deviation of all
#'     the elements. If multiple inputs, the output will have the same
#'     dimension as the input or possibly reduced dimension if matching
#'     based on dimension names is performed. It contains the variance or
#'     standard deviation computed element-wise.
#' @export
#' @examples
#' # Examples
#' X <- rnorm(12)
#' all.equal(Variance(X), var(X))
#' X.with.na <- sample(c(NA, runif(12)), size = 100, replace = TRUE)
#' var(X.with.na)
#' var(X.with.na, na.rm = TRUE)
#' Variance(X.with.na)
#' Variance(X.with.na, remove.missing = FALSE)
#' X <- replicate(3,
#'                array(sample(c(NA, runif(12)), size = 12, replace = TRUE), dim = 3:4,
#'                      dimnames = list(sample(letters[1:3]), sample(LETTERS[1:4]))),
#'                simplify = FALSE)
#' do.call(Variance, X)
Variance <- function(...,
                     remove.missing = TRUE,
                     remove.rows = NULL, remove.columns = NULL,
                     match.elements = "Yes",
                     subset = NULL,
                     weights = NULL,
                     warn = FALSE)
{
    calculateVariance(..., standard.deviation = FALSE, remove.missing = remove.missing,
                      remove.rows = remove.rows, remove.columns = remove.columns,
                      match.elements = match.elements,
                      subset = subset, weights = weights, warn = warn,
                      function.name = sQuote("Variance"))

}

#' @rdname variabilityOperations
#' @export
#' @examples
#' X <- replicate(3,
#'                array(sample(c(NA, runif(12)), size = 12, replace = TRUE), dim = 3:4,
#'                      dimnames = list(sample(letters[1:3]), sample(LETTERS[1:4]))),
#'                simplify = FALSE)
#' all.equal(do.call(StandardDeviation, X), sqrt(do.call(Variance, X)))
StandardDeviation <- function(...,
                              remove.missing = TRUE,
                              remove.rows = NULL, remove.columns = NULL,
                              match.elements = "Yes",
                              subset = NULL,
                              weights = NULL,
                              warn = FALSE)
{
    calculateVariance(..., standard.deviation = TRUE, remove.missing = remove.missing,
                      remove.rows = remove.rows, remove.columns = remove.columns,
                      match.elements = match.elements,
                      subset = subset, weights = weights, warn = warn,
                      function.name = sQuote("StandardDeviation"))

}

calculateVariance <- function(...,
                              standard.deviation = FALSE,
                              remove.missing = TRUE,
                              remove.rows = NULL, remove.columns = NULL,
                              match.elements = "Yes",
                              subset = NULL,
                              weights = NULL,
                              warn = FALSE,
                              function.name = sQuote("Variance"))
{
    x <- list(...)
    n.inputs <- length(x)
    return.total.element.weights <- if (n.inputs == 1 && weightsRequired(weights)) "TotalWeight" else FALSE
    x <- processArguments(x,
                          remove.missing = remove.missing,
                          remove.rows = remove.rows, remove.columns = remove.columns,
                          subset = subset, weights = weights,
                          return.total.element.weights = return.total.element.weights,
                          check.statistics = TRUE,
                          warn = warn,
                          function.name = function.name)
    if (n.inputs == 1)
        output <- computeVariance(x[[1L]], sum.weights = attr(x[[1L]], "sum.weights"),
                                  weights = weights, remove.missing = remove.missing)
    else
    {
        match.elements[tolower(match.elements) == "yes"] <- "Yes - hide unmatched"
        match.elements <- checkMatchingArguments(match.elements,
                                                 function.name = function.name)
        .updateVarianceWithMissingOptions <- function(x, y) updateVariance(x, y, remove.missing = remove.missing)
        .updateVariance <- function(x, y)
            calculateBinaryOperation(x, y,
                                     operation = .updateVarianceWithMissingOptions,
                                     match.elements = match.elements,
                                     remove.missing = remove.missing,
                                     function.name = function.name,
                                     warn = warn)
        output <- Reduce(.updateVariance, x)
        if (warn)
        {
            throwWarningIfTransposedInput(output, function.name)
            unmatched.elements <- attr(output, "unmatched")
            if (!is.null(unmatched.elements))
                throwWarningAboutUnmatched(unmatched.elements, function.name)
            if (any(attr(output, "n.sum") < 2L))
                throwWarningAboutMinimumTwoValuesForVariance(function.name)
        }
        output <- sanitizeAttributes(output, attributes.to.keep = c("dim", "dimnames", "names"))
    }
    if (getDimensionLength(output) == 1L)
        output <- setNames(as.vector(output), nm = names(output))
    if (standard.deviation)
        output <- sqrt(output)
    output
}

#' @importFrom stats var
computeVariance <- function(x, sum.weights, weights, remove.missing = TRUE)
{

    if (!weightsRequired(weights))
        return(var(as.vector(x), na.rm = remove.missing))
    n <- length(x)
    # x has the weights already pre-multiplied in subsetAndWeightInputsIfNecessary
    weighted.mean <- sum(x, na.rm = remove.missing)/sum.weights
    (sum(x^2/weights, na.rm = remove.missing) - sum.weights * weighted.mean^2)/((n - 1L)/n * sum.weights)
}

updateVariance <- function(x, y, remove.missing)
{
    if (missing(x) || missing(y))
        stop("Both x and y are required for updateVariance")
    if (is.null(attr(x, "mean")))
    {
        output <- x^2 + y^2 - (x + y)^2/2
        if (!remove.missing || !any(vapply(list(x, y), anyNA, logical(1L))))
            return(output)
        only.single.obs <- createArrayOfNAs(x)
        non.missing.input <- lapply(list(x, y), function(x) !is.na(x) & is.na(output))
        if (any(unlist(non.missing.input)))
        {
            non.missing <- which(non.missing.input[[1L]] | non.missing.input[[2L]])
            for (i in non.missing)
                only.single.obs[i] <- if (!is.na(x[[i]])) x[[i]] else y[[i]]
            attr(output, "only.single.obs") <- only.single.obs
        }
        return(output)
    }
    n <- attr(x, "n.sum")
    x.bar.n  <- attr(x, "mean")
    output <- (n - 1L)/n * (x + n *(y - x.bar.n)^2/((n - 1L) * (n + 1L)))
    if (!is.null(only.single.obs <- attr(x, "only.single.obs")) && remove.missing)
    {
        corrected.output <- checkForTwoObservationsAndComputeVariance(output, y)
        output <- corrected.output[["updated.variance"]]
        y <- corrected.output[["updated.y"]]
    }
    missing.x <- is.na(x)
    missing.y <- is.na(y)
    if (remove.missing && any(output.na.from.y <- !missing.x & missing.y & is.na(output)))
        output[output.na.from.y] <- x[output.na.from.y]
    if (any(single.obs <- missing.x & !missing.y))
    {
        only.single.obs <- attr(output, "only.single.obs")
        if (is.null(only.single.obs))
            only.single.obs <- createArrayOfNAs(x)
        only.single.obs[single.obs] <- y[single.obs]
        attr(output, "only.single.obs") <- only.single.obs
    }
    output
}

getNamesOfVectorOrArray <- function(x)
{
    if (is.null(dim(x))) list(names(x)) else dimnames(x)
}


createArrayOfNAs <- function(x)
{
    output.names <- getNamesOfVectorOrArray(x)
    array(NA, dim = standardizedDimensions(x), dimnames = output.names)
}

checkForTwoObservationsAndComputeVariance <- function(x, y)
{
    single.obs <- attr(x, "only.single.obs")
    if (!identical(standardizedDimensions(single.obs), standardizedDimensions(x)))
        single.obs <- subsetFirstInputToMatchSecondInput(single.obs, x)
    if (all(is.na(single.obs)))
    {
        attr(x, "only.single.obs") <- NULL
        return(list(updated.variance = x, updated.y = y))
    }

    matched.inputs <- list(single.obs, y)
    if (any(variances.to.compute <- !is.na(matched.inputs[[1L]]) & !is.na(matched.inputs[[2L]])))
    {
        args <- lapply(matched.inputs, `[`, variances.to.compute)
        previous.mean <- attr(x, "mean")
        if (any(mean.val.from.all.nas <- is.nan(previous.mean[variances.to.compute])))
        {
            previous.mean[variances.to.compute] <- (args[[1L]] + args[[2L]]) / 2L
            attr(x, "mean") <- previous.mean
        }
        x[variances.to.compute] <- updateVariance(args[[1L]], args[[2L]], remove.missing = FALSE)
        single.obs[variances.to.compute] <- y[variances.to.compute] <- NA
        attr(x, "only.single.obs") <- if (all(is.na(single.obs))) NULL else single.obs

    }
    list(updated.variance = x, updated.y = y)
}

throwWarningAboutMinimumTwoValuesForVariance <- function(function.name)
{
    warning("To calculate ", function.name, " there needs to be at least two non-missing ",
            "values for each element when multiple inputs are used. For the elements that ",
            "have less than 2 non-missing values, the resulting calculated value has been ",
            "set as missing data.")
}
