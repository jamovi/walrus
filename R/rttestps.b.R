
# This file is a generated template, your changes will not be overwritten

rttestPSClass <- R6::R6Class(
    "rttestPSClass",
    inherit = rttestPSBase,
    private = list(
        .init = function() {

            table <- self$results$ttest

            for (key in table$rowKeys)
                table$setRow(rowKey=key, values=list(var1=key[[1]], var2=key[[2]]))

        },
        .run = function() {

            table <- self$results$ttest

            data <- self$data
            for (col in colnames(data))
                data[[col]] <- jmvcore::toNumeric(data[[col]])

            for (key in table$rowKeys) {

                if (is.null(key[[1]]) || is.null(key[[2]]))
                    next()

                v1 <- data[[key[[1]]]]
                v2 <- data[[key[[2]]]]
                res <- WRS2::yuend(v1, v2, self$options$tr)

                table$setRow(rowKey=key, values=list(
                    t=res$test,
                    df=res$df,
                    p=res$p.value,
                    md=res$diff,
                    se=res$se,
                    es=res$effsize,
                    cil=res$conf.int[1],
                    ciu=res$conf.int[2]))
            }

        })
)
