
# This file is automatically generated, you probably don't want to edit this

rplotsOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "rplotsOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            vars = NULL,
            splitBy = NULL,
            violin = TRUE,
            boxplot = FALSE,
            dot = TRUE,
            dotType = "stack", ...) {

            super$initialize(
                package="walrus",
                name="rplots",
                requiresData=TRUE,
                ...)

            private$..vars <- jmvcore::OptionVariables$new(
                "vars",
                vars,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"))
            private$..splitBy <- jmvcore::OptionVariable$new(
                "splitBy",
                splitBy,
                default=NULL,
                suggested=list(
                    "nominal"),
                permitted=list(
                    "factor"))
            private$..violin <- jmvcore::OptionBool$new(
                "violin",
                violin,
                default=TRUE)
            private$..boxplot <- jmvcore::OptionBool$new(
                "boxplot",
                boxplot,
                default=FALSE)
            private$..dot <- jmvcore::OptionBool$new(
                "dot",
                dot,
                default=TRUE)
            private$..dotType <- jmvcore::OptionList$new(
                "dotType",
                dotType,
                options=list(
                    "jitter",
                    "stack"),
                default="stack")

            self$.addOption(private$..vars)
            self$.addOption(private$..splitBy)
            self$.addOption(private$..violin)
            self$.addOption(private$..boxplot)
            self$.addOption(private$..dot)
            self$.addOption(private$..dotType)
        }),
    active = list(
        vars = function() private$..vars$value,
        splitBy = function() private$..splitBy$value,
        violin = function() private$..violin$value,
        boxplot = function() private$..boxplot$value,
        dot = function() private$..dot$value,
        dotType = function() private$..dotType$value),
    private = list(
        ..vars = NA,
        ..splitBy = NA,
        ..violin = NA,
        ..boxplot = NA,
        ..dot = NA,
        ..dotType = NA)
)

rplotsResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "rplotsResults",
    inherit = jmvcore::Group,
    active = list(
        plots = function() private$.items[["plots"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Box & Violin Plots")
            self$add(jmvcore::Array$new(
                options=options,
                name="plots",
                title="Plots",
                items="(vars)",
                template=jmvcore::Image$new(
                    options=options,
                    title="$key",
                    renderFun=".plot",
                    clearWith=list(
                        "splitBy",
                        "violin",
                        "boxplot",
                        "dot",
                        "dotType"))))}))

rplotsBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "rplotsBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "walrus",
                name = "rplots",
                version = c(1,0,0),
                options = options,
                results = rplotsResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE)
        }))

#' Box & Violin Plots
#'
#' Box & Violin Plots
#'
#' @examples
#' data('eurosoccer', package='WRS2')
#'
#' # violin plots
#'
#' walrus::rplots(
#'     data = eurosoccer,
#'     vars = "GoalsGame",
#'     splitBy = "League")
#'
#'
#' # box plots
#'
#' walrus::rplots(
#'     data = eurosoccer,
#'     vars = "GoalsGame",
#'     splitBy = "League",
#'     violin = FALSE,
#'     boxplot = TRUE,
#'     dot = FALSE)
#'
#' @param data the data as a data frame
#' @param vars a vector of strings naming the variables in \code{data} of
#'   interest
#' @param splitBy a string naming the variable in \code{data} to split the
#'   data by
#' @param violin \code{TRUE} (default) or \code{FALSE}, provide violin plots
#' @param boxplot \code{TRUE} or \code{FALSE} (default), provide box plots
#' @param dot \code{TRUE} (default) or \code{FALSE}, plot each measurement as
#'   a dot
#' @param dotType \code{'jitter'} or \code{'stack'} (default); whether data
#'   dots are jittered or stacked
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$plots} \tab \tab \tab \tab \tab an array of images \cr
#' }
#'
#' @export
rplots <- function(
    data,
    vars,
    splitBy = NULL,
    violin = TRUE,
    boxplot = FALSE,
    dot = TRUE,
    dotType = "stack") {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("rplots requires jmvcore to be installed (restart may be required)")

    if ( ! missing(vars)) vars <- jmvcore::resolveQuo(jmvcore::enquo(vars))
    if ( ! missing(splitBy)) splitBy <- jmvcore::resolveQuo(jmvcore::enquo(splitBy))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(vars), vars, NULL),
            `if`( ! missing(splitBy), splitBy, NULL))

    for (v in splitBy) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])

    options <- rplotsOptions$new(
        vars = vars,
        splitBy = splitBy,
        violin = violin,
        boxplot = boxplot,
        dot = dot,
        dotType = dotType)

    analysis <- rplotsClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

