
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

            group  <- self$options$group
            deps   <- self$options$deps
            all    <- c(group, deps)

            if (is.null(group) || length(deps) == 0)
                return()

            groups <- self$options$groups
            trim <- self$options$tr

            data <- self$data
            for (dep in deps)
                data[[dep]] <- jmvcore::toNumeric(data[[dep]])
            data[[group]] <- as.factor(data[[group]])

            levels <- levels(data[[group]])
            nlevels <- nlevels(data[[group]])

            if (nlevels < 2)
                reject('Grouping variable must contain at least two groups')
            if (length(groups) > 2)
                reject('Only two groups may be specified')
            if (length(groups) == 1)
                reject('Two groups must be specified')
            if (nlevels == 2 && length(groups) == 0)
                groups <- levels
            if (nlevels > 2 && length(groups) < 2)
                reject('Grouping variable contains more than two groups, you must specify the groups of interest')
            if (groups[1] == groups[2])
                reject('Group 1 may not be the same as Group 2')
            if ( ! groups[1] %in% levels)
                reject("Group '{group}' does not exist in '{variable}'", group=groups[1], variable=group)
            if ( ! groups[2] %in% levels)
                reject("Group '{group}' does not exist in '{variable}'", group=groups[2], variable=group)

            data <- subset(data, (data[[group]] == groups[1]) | (data[[group]] == groups[2]))
            data <- droplevels(data)

            if (is.unsorted(match(groups, levels))) {
                groupVar <- as.numeric(data[[group]])
                groupVar[groupVar == 1] <- 3
                groupVar[groupVar == 2] <- 1
                groupVar[groupVar == 3] <- 2
                data[[group]] <- groupVar
            }

            ttestTable <- self$results$ttest

            for (dep in deps) {
                fmla <- jmvcore::constructFormula(dep, group)
                fmla <- formula(fmla)

                res <- try(WRS2::yuen(fmla, data, tr=trim))

                if ( ! jmvcore::isError(res)) {
                    mult <- ifelse(res$diff >= 0, 1, -1)
                    ttestTable$setRow(rowKey=dep, values=list(
                        `t[yuen]`=res$test * mult,
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
