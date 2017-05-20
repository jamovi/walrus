
rttestISClass <- R6::R6Class(
    "rttestISClass",
    inherit = rttestISBase,
    private = list(
        .init = function() {

            est <- self$options$method
            ttestTable <- self$results$ttest

            method = 'Unknown'
            if (est == 'onestep')
                method = 'One-step'
            else if (est == 'mom')
                method = 'Modified one-step'
            else if (est == 'mean')
                method = 'Mean'
            else if (est == 'median')
                method = 'Median'

            for (dep in ttestTable$rowKeys)
                ttestTable$addFootnote(rowKey=dep, col='test[mest]', paste(method, 'estimator used'))
        },
        .run = function() {

            group <- self$options$group
            deps  <- self$options$deps
            all   <- c(group, deps)

            if (is.null(group) || length(deps) == 0)
                return()

            trim <- self$options$tr

            data <- self$data
            for (dep in deps)
                data[[dep]] <- jmvcore::toNumeric(data[[dep]])

            ttestTable <- self$results$ttest

            for (dep in deps) {
                fmla <- jmvcore::constructFormula(dep, group)
                fmla <- formula(fmla)

                res <- try(WRS2::yuen(fmla, data, tr=trim))

                if ( ! jmvcore::isError(res)) {
                    ttestTable$setRow(rowKey=dep, values=list(
                        `t[yuen]`=res$test,
                        `df[yuen]`=res$df,
                        `p[yuen]`=res$p.value,
                        `md[yuen]`=res$diff,
                        `cil[yuen]`=res$conf.int[1],
                        `ciu[yuen]`=res$conf.int[2]))
                } else {
                    ttestTable$setRow(rowKey=dep, values=list(
                        `t[yuen]`=NaN,
                        `df[yuen]`='',
                        `p[yuen]`=''))
                    error <- jmvcore::extractErrorMessage(res)
                    ttestTable$addFootnote(rowKey=dep, col='t[yuen]', error)
                }

                res <- try(WRS2::yuen.effect.ci(fmla, data, tr=trim))

                if ( ! jmvcore::isError(res)) {
                    ttestTable$setRow(rowKey=dep, values=list(
                        `es[yuen]`=res$effsize,
                        `escil[yuen]`=res$CI[1],
                        `esciu[yuen]`=res$CI[2]))
                } else {
                    ttestTable$setRow(rowKey=dep, values=list(
                        `es[yuen]`=NaN,
                        `escil[yuen]`='',
                        `esciu[yuen]`=''))
                    error <- jmvcore::extractErrorMessage(res)
                    ttestTable$addFootnote(rowKey=dep, col='es[yuen]', error)
                }

                if (self$options$yuenbt) {

                    private$.checkpoint()

                    res <- try(WRS2::yuenbt(fmla, data, tr=trim, nboot=self$options$nboot))

                    if ( ! jmvcore::isError(res)) {
                        ttestTable$setRow(rowKey=dep, values=list(
                            `t[yuenbt]`=res$test,
                            `df[yuenbt]`='',
                            `p[yuenbt]`=res$p.value))
                    } else {
                        ttestTable$setRow(rowKey=dep, values=list(
                            `t[yuenbt]`=NaN,
                            `df[yuenbt]`='',
                            `p[yuenbt]`=''))
                        error <- jmvcore::extractErrorMessage(res)
                        ttestTable$addFootnote(rowKey=dep, col='t[yuenbt]', error)
                    }
                }

                if (self$options$mest) {

                    private$.checkpoint()

                    method <- self$options$method
                    res <- try(WRS2::pb2gen(fmla, data, est=method))

                    if ( ! jmvcore::isError(res)) {
                        ttestTable$setRow(rowKey=dep, values=list(
                            `t[mest]`=res$test,
                            `df[mest]`='',
                            `p[mest]`=res$p.value))

                    } else {
                        ttestTable$setRow(rowKey=dep, values=list(
                            `t[mest]`=NaN,
                            `df[mest]`='',
                            `p[mest]`=''))
                        error <- jmvcore::extractErrorMessage(res)
                        ttestTable$addFootnote(rowKey=dep, col='t[mest]', error)
                    }
                }

                private$.checkpoint()
            }

        })
)
