
ranovaClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "ranovaClass",
    inherit = ranovaBase,
    private = list(
        # Member variables ----
        .groups = NULL,

        # Init functions ----
        .init = function() {

            if (is.null(self$options$dep) || length(self$options$factors) == 0)
                return()

            private$.initPhsTable()

            switch(length(self$options$factors),
                   private$.initOneWay(),
                   private$.initTwoWay(),
                   private$.initThreeWay())

        },
        .initPhsTable = function() {

            main <- self$results$main
            phs <- self$results$phs
            factors <- self$options$factors

            data <- private$.cleanData()

            for (f in private$.ff()) {
                term <- jmvcore::stringifyTerm(f)
                main$addRow(rowKey=f, values=list(name=term))
            }

            for (i in 1:length(factors)) {
                ph <- phs[[i]]
                ph$setTitle(paste0("Post Hoc Tests - ", factors[i]))
                rowNo <- 1
                lvls <- base::levels(data[[factors[i]]])
                combns <- combn(lvls, 2)

                for (col in 1:ncol(combns)) {
                    pair <- combns[, col]
                    ph$addRow(rowKey=rowNo, values=list(v1=pair[1], v2=pair[2]))
                    rowNo <- rowNo + 1
                }
            }
        },
        .initOneWay = function() {

            main <- self$results$main
            method <- self$options$method
            phs <- self$results$phs

            if (self$options$ph) {
                phs[[1]]$setVisible(TRUE)
                phs[[2]]$setVisible(FALSE)
                phs[[3]]$setVisible(FALSE)
            }

            if (method == 'median') {
                main$getColumn('df1')$setVisible(FALSE)
                main$getColumn('df2')$setVisible(FALSE)
                main$getColumn('es')$setVisible(FALSE)
                main$getColumn('escil')$setVisible(FALSE)
                main$getColumn('esciu')$setVisible(FALSE)
                main$getColumn('critval')$setVisible(TRUE)

                main$setNote('method', .('Median method.'))
            } else if (method == 'trim') {
                main$getColumn('df1')$setVisible(TRUE)
                main$getColumn('df2')$setVisible(TRUE)
                main$getColumn('es')$setVisible(TRUE)
                main$getColumn('escil')$setVisible(TRUE)
                main$getColumn('esciu')$setVisible(TRUE)

                main$setNote('method', jmvcore::format(.('Method of trimmed means (level {}).'), self$options$tr))
                main$setNote('note', .('For effect size CI computation (samples 599)'), init=FALSE)
            } else {  # bootstrap
                main$getColumn('s')$setTitle('F*')
                main$getColumn('critval')$setVisible(FALSE)
                main$getColumn('df1')$setVisible(FALSE)
                main$getColumn('df2')$setVisible(FALSE)
                main$getColumn('escil')$setVisible(FALSE)
                main$getColumn('esciu')$setVisible(FALSE)
                main$getColumn('expvar')$setVisible(TRUE)
                main$getColumn('es')$setVisible(TRUE)

                if (self$options$est == 'onestep')
                    estimator <- 'one-step'
                else if (self$options$est == 'mom')
                    estimator <- 'modified one-step'
                else if (self$options$est == 'median')
                    estimator <- 'median'
                else
                    estimator <- 'unknown'

                if (self$options$dist == 'maha')
                    distances <- 'Mahalanobis'
                else if (self$options$dist == 'proj')
                    distances <- 'projection'
                else
                    distances <- 'unknown'

                main$setNote('method',
                             jmvcore::format(.('Bootstrap method, {} estimator, {} samples, {} distances.'),
                                             estimator, self$options$nboot, distances))
            }
        },
        .initTwoWay = function() {

            main <- self$results$main
            method <- self$options$method
            phs <- self$results$phs
            factors <- self$options$factors

            ph <- phs[[3]]
            ph$setTitle(paste0("Post Hoc Tests - ", factors[1], " \U273B ", factors[2]))

            if (self$options$ph) {
                phs[[1]]$setVisible(TRUE)
                phs[[2]]$setVisible(TRUE)
                phs[[3]]$setVisible(TRUE)
            }

            grs <- self$groups
            nrGrs <- nrow(grs)

            for (rowNo in 1:nrGrs)
                ph$addRow(rowKey=rowNo,
                          values=list(v1=as.character(grs[[factors[1]]][rowNo]),
                                      v2=as.character(grs[[factors[2]]][rowNo])))

            main$getColumn('s')$setTitle('Q')

            if (method == 'median') {
                main$setNote('method', .('Median method.'))
            } else if (method == 'trim') {
                main$setNote('method', jmvcore::format(.('Method of trimmed means (level {}).'), self$options$tr))
            } else {  # bootstrap
                if (self$options$est == 'onestep')
                    estimator <- 'one-step'
                else if (self$options$est == 'mom')
                    estimator <- 'modified one-step'
                else if (self$options$est == 'median')
                    estimator <- 'median'
                else
                    estimator <- 'unknown'

                if (self$options$dist == 'maha')
                    distances <- 'Mahalanobis'
                else if (self$options$dist == 'proj')
                    distances <- 'projection'
                else
                    distances <- 'unknown'

                main$setNote('method',
                             jmvcore::format(.('Bootstrap method, {} estimator, {} samples, {} distances.'),
                                             estimator, self$options$nboot, distances))
            }
        },
        .initThreeWay = function() {

            main <- self$results$main
            method <- self$options$method
            phs <- self$results$phs
            factors <- self$options$factors

            if (self$options$ph) {
                phs[[4]]$setTitle(paste0("Post Hoc Tests - ", factors[1], " \U273B ", factors[2]))
                phs[[5]]$setTitle(paste0("Post Hoc Tests - ", factors[1], " \U273B ", factors[3]))
                phs[[6]]$setTitle(paste0("Post Hoc Tests - ", factors[2], " \U273B ", factors[3]))
                phs[[7]]$setTitle(paste0("Post Hoc Tests - ", factors[1], " \U273B ", factors[2], " \U273B ", factors[3]))

                for (i in 1:7) {
                    phs[[i]]$getColumn('adjp')$setVisible(TRUE)
                    phs[[i]]$setVisible(TRUE)
                }
            }

            grs <- unique(self$groups[, 1:2])
            for (rowNo in 1:nrow(grs))
                phs[[4]]$addRow(rowKey=rowNo,
                                values=list(v1=as.character(grs[[factors[1]]][rowNo]),
                                            v2=as.character(grs[[factors[2]]][rowNo])))

            grs <- unique(self$groups[, c(1, 3)])
            for (rowNo in 1:nrow(grs))
                phs[[5]]$addRow(rowKey=rowNo,
                                values=list(v1=as.character(grs[[factors[1]]][rowNo]),
                                            v2=as.character(grs[[factors[3]]][rowNo])))

            grs <- unique(self$groups[, 2:3])
            for (rowNo in 1:nrow(grs))
                phs[[6]]$addRow(rowKey=rowNo,
                                values=list(v1=as.character(grs[[factors[2]]][rowNo]),
                                            v2=as.character(grs[[factors[3]]][rowNo])))

            grs <- unique(self$groups[, 1:3])
            for (rowNo in 1:nrow(grs))
                phs[[7]]$addRow(rowKey=rowNo,
                                values=list(v1=paste0(as.character(grs[[factors[1]]][rowNo]),
                                                      " - ",
                                                      as.character(grs[[factors[2]]][rowNo])),
                                            v2=as.character(grs[[factors[3]]][rowNo])))

            main$getColumn('s')$setTitle('Q')

            if (method == 'median') {
                main$setNote('method', .('Median method.'))
            } else if (self$options$method == 'trim') {
                main$setNote('method', jmvcore::format(.('Method of trimmed means (level {}).'), self$options$tr))
            } else {  # bootstrap
                if (self$options$est == 'onestep')
                    estimator <- 'one-step'
                else if (self$options$est == 'mom')
                    estimator <- 'modified one-step'
                else if (self$options$est == 'median')
                    estimator <- 'median'
                else
                    estimator <- 'unknown'

                if (self$options$dist == 'maha')
                    distances <- 'Mahalanobis'
                else if (self$options$dist == 'proj')
                    distances <- 'projection'
                else
                    distances <- 'unknown'

                main$setNote('method',
                             jmvcore::format(.('Bootstrap method, {} estimator, {} samples, {} distances.'),
                                             estimator, self$options$nboot, distances))
            }
        },
        # Run functions ----
        .run = function() {

            dep <- self$options$dep
            factors <- self$options$factors

            if (is.null(dep) || length(factors) == 0)
                return()

            data <- private$.cleanData()
            colnames(data) <- jmvcore::toB64(colnames(data))
            factors <- jmvcore::toB64(factors)
            dep <- jmvcore::toB64(dep)
            formula <- paste(dep, '~', paste(factors, collapse='*'))
            formula <- as.formula(formula)

            switch(length(self$options$factors),
                   private$.popOneWay(formula, data),
                   private$.popTwoWay(formula, data),
                   private$.popThreeWay(formula, data))

        },
        .popOneWay = function(formula, data) {

            main <- self$results$main
            phs <- self$results$phs

            if (self$options$method == 'median') {
                if (! main$isFilled()) {
                    result <- try(WRS2::med1way(formula, data))

                    if (! jmvcore::isError(result)) {
                        main$setRow(rowNo=1,
                                    values=list(s=result$test,
                                                critval=result$crit.val,
                                                p=result$p.value))
                    } else {
                        message <- jmvcore::extractErrorMessage(result)
                        main$setError(message)
                    }
                }
                # Post-Hoc ----
                if (self$options$ph || ! phs$isFilled()) {
                    ph <- phs$get(index=1)
                    ftn <- .('Post-hoc test not available for this method.')
                    for (rowNo in seq_len(ph$rowCount)) {
                        ph$setRow(rowNo=rowNo,
                                  values=list(psi=NaN, p=NaN, cil=NaN, ciu=NaN))

                        ph$addFootnote(rowNo=rowNo, col='psi', ftn)
                        ph$addFootnote(rowNo=rowNo, col='p', ftn)
                        ph$addFootnote(rowNo=rowNo, col='cil', ftn)
                        ph$addFootnote(rowNo=rowNo, col='ciu', ftn)
                    }
                }
            } else if (self$options$method  == 'trim') {
                if (! main$isFilled()) {
                    result <- try(WRS2::t1way(formula, data, tr=self$options$tr, nboot=599))

                    if (! jmvcore::isError(result)) {
                        main$setRow(rowNo=1,
                                    values=list(s=result$test,
                                                df1=result$df1,
                                                df2=result$df2,
                                                p=result$p.value,
                                                es=result$effsize,
                                                escil=result$effsize_ci[1],
                                                esciu=result$effsize_ci[2]))
                    } else {
                        message <- jmvcore::extractErrorMessage(result)
                        main$setError(message)
                    }
                }

                # Post-Hoc ----
                if (self$options$ph || ! phs$isFilled()) {
                    result <- try(WRS2::lincon(formula, data, tr=self$options$tr)$comp)

                    if (! jmvcore::isError(result)) {
                        ph <- phs$get(index=1)
                        ftn <- .('CI are adjusted to control FWE, but not p-values.')
                        for (rowNo in seq_len(ph$rowCount)) {
                            ph$setRow(rowNo=rowNo,
                                      values=list(psi=result[rowNo, 'psihat'],
                                                  p=result[rowNo, 'p.value'],
                                                  cil=result[rowNo, 'ci.lower'],
                                                  ciu=result[rowNo, 'ci.upper']))

                            ph$addFootnote(rowNo=rowNo, col='cil', ftn)
                            ph$addFootnote(rowNo=rowNo, col='ciu', ftn)
                        }
                    } else {
                        message <- jmvcore::extractErrorMessage(result)
                        phs$setError(message)
                    }
                }
            } else {  # bootstrap
                if (! main$isFilled()) {
                    result <- try(WRS2::t1waybt(formula, data, nboot=self$options$nboot))

                    if (! jmvcore::isError(result)) {
                        main$setRow(rowNo=1,
                                    values=list(s=result$test,
                                                expvar=result$Var.Explained,
                                                p=result$p.value,
                                                es=result$Effect.Size))

                        if (result$nboot.eff !=  self$options$nboot)
                            main$setNote('note',
                                         jmvcore::format(.('Effective number of bootstrap samples was {}.'),
                                                         result$nboot.eff), init=FALSE)
                    } else {
                        message <- jmvcore::extractErrorMessage(result)
                        main$setError(message)
                    }
                }

                # Post-Hoc ----
                if (self$options$ph || ! phs$isFilled()) {
                    result <- try(WRS2::mcppb20(formula, data, nboot=self$options$nboot)$comp)

                    ph <- phs$get(index=1)
                    nlevs <- base::nlevels(data[[1]])
                    ftn <- .('Post-hoc test unnecessary if the factor has only 2 levels.')
                    for (rowNo in seq_len(ph$rowCount)) {
                        if (nlevs == 2) {
                            ph$setRow(rowNo=rowNo, values=list(psi=NaN, p=NaN, cil=NaN, ciu=NaN))

                            ph$addFootnote(rowNo=rowNo, col='psi', ftn)
                            ph$addFootnote(rowNo=rowNo, col='p', ftn)
                            ph$addFootnote(rowNo=rowNo, col='cil', ftn)
                            ph$addFootnote(rowNo=rowNo, col='ciu', ftn)
                        } else if (! jmvcore::isError(result)) {
                            ph$setRow(rowNo=rowNo,
                                      values=list(psi=result[rowNo, 'psihat'],
                                                  p=result[rowNo, 'p-value'],
                                                  cil=result[rowNo, 'ci.lower'],
                                                  ciu=result[rowNo, 'ci.upper']))
                        } else {
                            message <- jmvcore::extractErrorMessage(result)
                            phs$setError(message)
                        }
                    }
                }
            }
        },
        .popTwoWay = function(formula, data) {

            main <- self$results$main
            phs <- self$results$phs
            factors <- self$options$factors

            if (self$options$method == 'median') {

                if (! main$isFilled()) {
                    result <- try(WRS2::med2way(formula, data))

                    if (! jmvcore::isError(result)) {
                        main$setRow(rowNo=1, values=list(s=result$Qa, p=result$A.p.value))
                        main$setRow(rowNo=2, values=list(s=result$Qb, p=result$B.p.value))
                        main$setRow(rowNo=3, values=list(s=result$Qab, p=result$AB.p.value))
                    } else {
                        message <- jmvcore::extractErrorMessage(result)
                        main$setError(message)
                    }
                }

                # Post-Hoc ----
                if (self$options$ph || ! phs$isFilled()) {
                    result <- try(WRS2::mcp2a(formula, data, est='median')$effects)

                    if (! jmvcore::isError(result)) {
                        for (i in 1:3) {
                            ph <- phs[[i]]
                            values <- result[[i]]
                            if (i<3) {
                                lvls <- base::levels(self$data[[factors[i]]])
                                nc <- ncol(combn(lvls, 2))
                                for (n in 1:nc) {
                                    psi <- values$psihat[n]
                                    p <- values$p.value[n]
                                    ci <- values$conf.int
                                    if (length(dim(ci)) > 1)
                                        ci <- ci[n, ]
                                    cil <- ci[1]
                                    ciu <- ci[2]

                                    ph$setRow(rowNo=n, values=list(psi=psi, p=p, cil=cil, ciu=ciu))
                                }
                            } else {
                                nr <- nrow(self$groups)
                                for (n in 1:nr) {
                                    psi <- values$psihat[n]
                                    p <- values$p.value[n]
                                    ci <- values$conf.int
                                    if (length(dim(ci)) > 1)
                                        ci <- ci[n, ]
                                    cil <- ci[1]
                                    ciu <- ci[2]

                                    ph$setRow(rowNo=n, values=list(psi=psi, p=p, cil=cil, ciu=ciu))
                                }
                            }
                        }
                    } else {
                        message <- jmvcore::extractErrorMessage(result)
                        phs$setError(message)
                    }
                }
            } else if (self$options$method  == 'trim') {
                if (! main$isFilled()) {
                    result <- try(WRS2::t2way(formula, data, tr=self$options$tr))

                    if (! jmvcore::isError(result)) {
                        main$setRow(rowNo=1, values=list(s=result$Qa, p=result$A.p.value))
                        main$setRow(rowNo=2, values=list(s=result$Qb, p=result$B.p.value))
                        main$setRow(rowNo=3, values=list(s=result$Qab, p=result$AB.p.value))
                    } else {
                        message <- jmvcore::extractErrorMessage(result)
                        main$setError(message)
                    }
                }

                # Post-Hoc ----
                if (self$options$ph || ! phs$isFilled()) {
                    result <- try(WRS2::mcp2atm(formula, data, tr=self$options$tr)$effects)

                    if (! jmvcore::isError(result)) {
                        for (i in 1:3) {
                            ph <- phs[[i]]
                            values <- result[[i]]
                            if (i<3) {
                                lvls <- base::levels(self$data[[factors[i]]])
                                nc <- ncol(combn(lvls, 2))
                                for (n in 1:nc) {
                                    psi <- values$psihat[n]
                                    p <- values$p.value[n]
                                    ci <- values$conf.int
                                    if (length(dim(ci)) > 1)
                                        ci <- ci[n, ]
                                    cil <- ci[1]
                                    ciu <- ci[2]

                                    ph$setRow(rowNo=n, values=list(psi=psi, p=p, cil=cil, ciu=ciu))
                                }
                            } else {
                                nr <- nrow(self$groups)
                                for (n in 1:nr) {
                                    psi <- values$psihat[n]
                                    p <- values$p.value[n]
                                    ci <- values$conf.int
                                    if (length(dim(ci)) > 1)
                                        ci <- ci[n, ]
                                    cil <- ci[1]
                                    ciu <- ci[2]

                                    ph$setRow(rowNo=n, values=list(psi=psi, p=p, cil=cil, ciu=ciu))
                                }
                            }
                        }
                    } else {
                        message <- jmvcore::extractErrorMessage(result)
                        phs$setError(message)
                    }
                }
            } else { # bootstrap
                if (! main$isFilled()) {
                    result <- try(WRS2::pbad2way(formula,
                                                 data,
                                                 est=self$options$est,
                                                 nboot=self$options$nboot,
                                                 pro.dis=base::ifelse(self$options$dist == 'proj', TRUE, FALSE)))

                    if (! jmvcore::isError(result)) {
                        main$setRow(rowNo=1, values=list(s=NaN, p=result$A.p.value))
                        main$setRow(rowNo=2, values=list(s=NaN, p=result$B.p.value))
                        main$setRow(rowNo=3, values=list(s=NaN, p=result$AB.p.value))

                        ftn <- .('Adjusted critical values are used, so only p-values are available.')
                        for (rN in 1:3)
                            main$addFootnote(rowNo=rN, col='s', ftn)
                    } else {
                        message <- jmvcore::extractErrorMessage(result)
                        main$setError(message)
                    }
                }

                # Post-Hoc ----
                if (self$options$ph || ! phs$isFilled()) {
                    result <- try(WRS2::mcp2a(formula,
                                              data,
                                              est=self$options$est,
                                              nboot=self$options$nboot)$effects)

                    if (! jmvcore::isError(result)) {
                        for (i in 1:3) {
                            ph <- phs[[i]]
                            values <- result[[i]]
                            if (i<3) {
                                lvls <- base::levels(self$data[[factors[i]]])
                                nc <- ncol(combn(lvls, 2))
                                for (n in 1:nc) {
                                    psi <- values$psihat[n]
                                    p <- values$p.value[n]
                                    ci <- values$conf.int
                                    if (length(dim(ci)) > 1)
                                        ci <- ci[n, ]
                                    cil <- ci[1]
                                    ciu <- ci[2]

                                    ph$setRow(rowNo=n, values=list(psi=psi, p=p, cil=cil, ciu=ciu))
                                }
                            } else {
                                nr <- nrow(self$groups)
                                for (n in 1:nr) {
                                    psi <- values$psihat[n]
                                    p <- values$p.value[n]
                                    ci <- values$conf.int
                                    if (length(dim(ci)) > 1)
                                        ci <- ci[n, ]
                                    cil <- ci[1]
                                    ciu <- ci[2]

                                    ph$setRow(rowNo=n, values=list(psi=psi, p=p, cil=cil, ciu=ciu))
                                }
                            }
                        }
                    } else {
                        if (self$options$est == 'onestep')
                            message <- .('Select the correct estimator: Modified one-step or Median')
                        else
                            message <- jmvcore::extractErrorMessage(result)

                        phs$setError(message)
                    }
                }
            }
        },
        .popThreeWay = function(formula, data) {

            main <- self$results$main
            phs <- self$results$phs

            if (self$options$method == 'boot') {
                result <- try(private$.mzMain3way(formula, data))

                if (! jmvcore::isError(result)) {
                    ftn <- .('So only p-values are available.')
                    ftnos <- .('Select the correct estimator: Median or Modified one-step')
                    for (rNo in 1:7) {
                        main$setRow(rowNo=rNo, values=list(s=NaN, p=result[[rNo]][[1]]))
                        main$addFootnote(rowNo=rNo, col='s', ftn)

                        if (self$options$est == 'onestep')
                            main$addFootnote(rowNo=rNo, col='p', ftnos)
                    }
                } else {
                    message <- jmvcore::extractErrorMessage(result)
                    main$setError(message)
                }

                if (self$options$ph) {
                    ph <- NULL
                    ftnph <- .('Post-hoc test not available for this method.')
                    for (key in phs$itemKeys) {
                        ph <- phs$get(key)
                        for (rowNo in seq_len(ph$rowCount)) {
                            ph$setRow(rowNo=rowNo,
                                      values=list(psi=NaN, p=NaN, adjp=NaN, cil=NaN, ciu=NaN))

                            ph$addFootnote(rowNo=rowNo, col='psi', ftnph)
                            ph$addFootnote(rowNo=rowNo, col='p', ftnph)
                            ph$addFootnote(rowNo=rowNo, col='adjp', ftnph)
                            ph$addFootnote(rowNo=rowNo, col='cil', ftnph)
                            ph$addFootnote(rowNo=rowNo, col='ciu', ftnph)
                        }
                    }
                }
                return()
            } else if (self$options$method == 'median') {
                result <- try(private$.mzMain3way(formula, data))

                if (! jmvcore::isError(result)) {
                    ftn <- .('Compute the Harrell-Davis estimate of the qth quantile, so only p-values are available.')
                    for (rNo in 1:7) {
                        main$setRow(rowNo=rNo, values=list(s=NaN, p=result[[rNo]][[1]]))
                        main$addFootnote(rowNo=rNo, col='s', ftn)
                    }
                } else {
                    message <- jmvcore::extractErrorMessage(result)
                    main$setError(message)
                }
            } else { # trim
                result <- try(WRS2::t3way(formula, data, tr=self$options$tr))

                if (! jmvcore::isError(result)) {
                    main$setRow(rowNo=1, values=list(s=result$Qa, p=result$A.p.value))
                    main$setRow(rowNo=2, values=list(s=result$Qb, p=result$B.p.value))
                    main$setRow(rowNo=3, values=list(s=result$Qc, p=result$C.p.value))
                    main$setRow(rowNo=4, values=list(s=result$Qab, p=result$AB.p.value))
                    main$setRow(rowNo=5, values=list(s=result$Qac, p=result$AC.p.value))
                    main$setRow(rowNo=6, values=list(s=result$Qbc, p=result$BC.p.value))
                    main$setRow(rowNo=7, values=list(s=result$Qabc, p=result$ABC.p.value))
                } else {
                    message <- jmvcore::extractErrorMessage(result)
                    main$setError(message)
                }
            }

            # Post-Hoc for Median e Trim ----
            if (self$options$ph || ! phs$isFilled()) {
                result <- try(private$.mzPH3way(formula, data))

                if (! jmvcore::isError(result)) {
                    result <- as.data.frame(do.call(rbind, result))

                    r = 0
                    livelli = 0
                    for (i in 1:3) {
                        nl <- nlevels(unique(self$groups[, i]))
                        livelli = livelli + nl
                        for (n in 1:nl) {
                            r = r + 1
                            psi <- result$psihat[r]
                            p <- result$p.value[r]
                            adjp <- result$adj.p.value[r]
                            cil <- result$ci.lower[r]
                            ciu <- result$ci.upper[r]

                            phs[[i]]$setRow(rowNo=n,
                                            values=list(psi=psi,
                                                        p=p,
                                                        adjp=adjp,
                                                        cil=cil,
                                                        ciu=ciu))
                        }
                    }

                    grs <- unique(self$groups[, c(1, 2)])
                    for (n in 1:nrow(grs)) {
                        r = r + 1
                        psi <- result$psihat[r]
                        p <- result$p.value[r]
                        adjp <- result$adj.p.value[r]
                        cil <- result$ci.lower[r]
                        ciu <- result$ci.upper[r]

                        phs[[4]]$setRow(rowNo=n,
                                        values=list(psi=psi,
                                                    p=p,
                                                    adjp=adjp,
                                                    cil=cil,
                                                    ciu=ciu))
                    }

                    grs <- unique(self$groups[, c(1, 3)])
                    for (n in 1:nrow(grs)) {
                        r = r + 1
                        psi <- result$psihat[r]
                        p <- result$p.value[r]
                        adjp <- result$adj.p.value[r]
                        cil <- result$ci.lower[r]
                        ciu <- result$ci.upper[r]

                        phs[[5]]$setRow(rowNo=n,
                                        values=list(psi=psi,
                                                    p=p,
                                                    adjp=adjp,
                                                    cil=cil,
                                                    ciu=ciu))
                    }

                    grs <- unique(self$groups[, c(2, 3)])
                    for (n in 1:nrow(grs)) {
                        r = r + 1
                        psi <- result$psihat[r]
                        p <- result$p.value[r]
                        adjp <- result$adj.p.value[r]
                        cil <- result$ci.lower[r]
                        ciu <- result$ci.upper[r]

                        phs[[6]]$setRow(rowNo=n,
                                        values=list(psi=psi,
                                                    p=p,
                                                    adjp=adjp,
                                                    cil=cil,
                                                    ciu=ciu))
                    }

                    grs <- unique(self$groups[, c(1, 2, 3)])
                    for (n in 1:nrow(grs)) {
                        r = r + 1
                        psi <- result$psihat[r]
                        p <- result$p.value[r]
                        adjp <- result$adj.p.value[r]
                        cil <- result$ci.lower[r]
                        ciu <- result$ci.upper[r]

                        phs[[7]]$setRow(rowNo=n,
                                        values=list(psi=psi,
                                                    p=p,
                                                    adjp=adjp,
                                                    cil=cil,
                                                    ciu=ciu))
                    }
                } else {
                    message <- jmvcore::extractErrorMessage(result)
                    phs$setError(message)
                }
            }
        },
        .mzMain3way = function(formula, data, alpha=.05) {
            # R. R. Wilcox' robust statistics functions needed
            hd = function(x, q=.5, na.rm=TRUE, STAND=NULL) {
                # Three-way ANOVA for robust measures of locaton
                # To compare medians, use est = hd, in case which tied values are allowed.
                # Compute the Harrell-Davis estimate of the qth quantile
                # The default value for q is .5.
                if (na.rm)
                    x <- x[!is.na(x)] # Remove missing values

                n <- length(x)
                m1 <- (n+1) * q
                m2 <- (n+1) * (1-q)
                vec <- seq(along=x)
                w <- pbeta(vec/n, m1, m2) - pbeta((vec-1)/n, m1, m2)
                y <- sort(x)
                hd <- sum(w*y)

                return(hd)
            }

            mom = function(x, bend=2.24, na.rm=TRUE) {
                #  Compute MOM-estimator of location.
                #  The default bending constant is 2.24
                if (na.rm)
                    x <- x[!is.na(x)] # Remove missing values

                flag1 <- (x > median(x) + bend * mad(x))
                flag2 <- (x < median(x) - bend * mad(x))
                flag <- rep(TRUE, length(x))
                flag[flag1] <- FALSE
                flag[flag2] <- FALSE
                mom <- mean(x[flag])

                return(mom)
            }

            onestep = function(x, bend=1.28, na.rm=FALSE, MED=TRUE) {
                #  Compute one-step M-estimator of location using Huber's Psi.
                #  The default bending constant is 1.28
                #
                #  MED = TRUE: initial estimate is the median
                #  Otherwise use modified one-step M-estimator
                hpsi = function(x, bend=1.28) {
                    #   Evaluate Huber`s Psi function for each value in the vector x
                    #   The bending constant defaults to 1.28.
                    #
                    hpsi <- ifelse(abs(x) <= bend, x, bend*sign(x))
                    return(hpsi)
                }

                if (na.rm)
                    x <- x[!is.na(x)]

                if (MED)
                    init.loc = median(x)

                if (!MED)
                    init.loc = mom(x, bend=bend)

                y <- (x-init.loc)/mad(x)
                A <- sum(hpsi(y, bend))
                B <- length(x[abs(y) <= bend])
                onestep <- median(x) + mad(x)*A/B

                return(onestep)
            }

            mf <- model.frame(formula, data)
            # Create the three contrast matrices
            con <- private$.con3way(mf)
            x <- private$.fac2list(mf[, 1], mf[, -1])
            rl <- list()
            if (self$options$method == 'median') {
                for (i in 1:7)
                    rl[i] <- private$.pbadepth(x,
                                               con=con[[i]],
                                               alpha=alpha,
                                               est=hd,
                                               op=3,
                                               nboot=500,
                                               MC=FALSE)
            } else { # boot
                for (i in 1:7)
                    if (self$options$est == 'onestep')
                        rl[i] <- NaN
                    else
                        rl[i] <- private$.pbadepth(x,
                                                 con=con[[i]],
                                                 alpha=alpha,
                                                 est=base::ifelse(self$options$est == 'mom', mom, median),
                                                 op=base::ifelse(self$options$dist == 'proj', 3, 1),
                                                 nboot=self$options$nboot,
                                                 MC=FALSE)
            }
            return(rl)
        },
        .mzPH3way = function(formula, data, alpha = 0.05) {

            mf <- model.frame(formula, data)
            # Create the three contrast matrices
            con <- private$.con3way(mf)
            x <- private$.fac2list(mf[, 1], mf[, -1])
            rl <- list()
            if (self$options$method == 'median') {
                for (i in 1:7)
                    rl <- append(rl, private$.medpb(x,
                                                    con=con[[i]],
                                                    alpha=alpha,
                                                    est=median))
            } else { # trim
                for (i in 1:7)
                    rl <- append(rl, private$.lincon(x,
                                                     con=con[[i]],
                                                     alpha=alpha,
                                                     tr=self$options$tr))
            }
            return(rl)
        },
        .cleanData = function() {

            dep <- self$options$dep
            factors <- self$options$factors
            data <- self$data

            if (! is.null(dep))
                data[[dep]] <- jmvcore::toNumeric(data[[dep]])

            for (factor in factors)
                data[[factor]] <- as.factor(data[[factor]])

            return(data)
        },
        .ff = function() {

            factors <- self$options$factors

            if (length(factors) > 1) {
                formula <- as.formula(paste('~', paste(paste0('`', factors, '`'), collapse='*')))
                terms <- attr(stats::terms(formula), 'term.labels')
                modelTerms <- sapply(terms, function(x) as.list(strsplit(x, ':')), USE.NAMES=FALSE)
            } else {
                modelTerms <- as.list(factors)
            }

            for (i in seq_along(modelTerms)) {
                term <- modelTerms[[i]]
                quoted <- grepl('^`.*`$', term)
                term[quoted] <- substring(term[quoted], 2, nchar(term[quoted])-1)
                modelTerms[[i]] <- term
            }

            return(modelTerms)
        },

        ### --------------------------------------------------------------------
        # Reference:
        # A package of R. R. Wilcox' robust statistics functions.
        # For more information, see https://dornsife.usc.edu/labs/rwilcox/software/
        # Here the latest update of the Rallfun-v40.txt file (lic. and All func.)
        # https://dornsife.usc.edu/assets/sites/239/docs/Rallfun-v40.txt
        #
        # Some of these functions are used here, with some minor changes.
        ### --------------------------------------------------------------------
        .fac2list = function(x, g) {
            # data are stored in x
            # information about the level of the value in x is stored in g,
            # which can be a matrix with up to 4 columns sort the data in x
            # into groups based on values in g, store results in list mode.

            g = as.data.frame(g)
            ncg = ncol(g)

            y <- list()
            for (nc in 1:ncg)
                y[[nc]] <- as.factor(g[, nc])

            # take data in list mode and store it in a matrix
            lgy = length(y)
            nval = NA

            for (lg in 1:lgy)
                nval[lg] = length(y[[lg]])

            g <- matrix(NA, ncol=lgy, nrow=max(nval))

            for (lg in 1:lgy)
                g[1:nval[lg], lg] <- y[[lg]]

            Lp1 = ncg + 1
            if (ncg > 4)
                stop("Can have at most 4 factors")

            if (ncg == 1)
                res <- private$.selby(cbind(x, g), 2, 1)$x

            if (ncg > 1)
                res <- private$.selby2(cbind(x, g), c(2:Lp1), 1)$x

            return(res)
        },
        .con3way = function(mf) {

            cjMAT = function(J) {
                L = (J^2 - J) / 2
                cj = matrix(0, nrow=L, ncol=J)
                ic = 0
                for (j in 1:J) {
                    for (k in 1:J)
                        if (j<k) {
                            ic = ic + 1
                            cj[ic, j] = 1
                            cj[ic, k] = -1
                        }
                }
                return(cj)
            }

            kron = function(m1, m2) {
                # compute the Kronecker product of the two matrices m1 and m2
                # Vectors of length p are converted to a p by 1 matrix
                m1 <- as.matrix(m1)
                m2 <- as.matrix(m2)
                kron <- vector(mode="numeric", length=0)
                for (i in 1:nrow(m1)) {
                    m3 <- m1[i, 1] * m2
                    for (j in 2:ncol(m1))
                        m3 <- cbind(m3, m1[i, j]*m2)

                    if (i == 1)
                        kron <- m3

                    if (i >= 2)
                        kron <- rbind(kron, m3)
                }

                return(kron)
            }

            J <- nlevels(mf[, 2])
            K <- nlevels(mf[, 3])
            L <- nlevels(mf[, 4])

            cj = cjMAT(J)
            ck = cjMAT(K)
            cl = cjMAT(L)

            ij <- matrix(c(rep(1, J)), 1, J)
            ik <- matrix(c(rep(1, K)), 1, K)
            il <- matrix(c(rep(1, L)), 1, L)

            conA = t(kron(cj, kron(ik, il)))
            conB = t(kron(ij, kron(ck, il)))
            conC = t(kron(ij, kron(ik, cl)))

            conAB = kron(cj, kron(ck, il))
            conAC = kron(cj, kron(ik, cl))
            conBC = kron(ij, kron(ck, cl))

            conABC = kron(cj, kron(ck, cl))

            rl <- list(conA=conA, conB=conB, conC=conC,
                       conAB=t(conAB), conAC=t(conAC), conBC=t(conBC),
                       conABC=t(conABC))

            return(rl)
        },
        .elimna = function(m) {
            # remove any rows of data having missing values
            DONE = FALSE
            if (is.list(m) && is.matrix(m)) {
                z = pool.a.list(m)
                m = matrix(z, ncol=ncol(m))
                DONE = TRUE
            }

            if (!DONE) {
                if (is.list(m) && is.matrix(m[[1]])) {
                    for (j in 1:length(m))
                        m[[j]] = na.omit(m[[j]])

                    e = m
                    DONE = TRUE
                }
            }

            if (!DONE) {
                if (is.list(m) && is.null(dim(m))) { #!is.matrix(m))
                    for (j in 1:length(m))
                        m[[j]] = as.vector(na.omit(m[[j]]))

                    e = m
                    DONE = TRUE
                }
            }

            if (!DONE) {
                m <- as.matrix(m)
                ikeep <- c(1:nrow(m))
                for (i in 1:nrow(m))
                    if (sum(is.na(m[i, ]) >= 1))
                        ikeep[i] <- 0

                e <- m[ikeep[ikeep>=1], ]
            }

            return(e)
        },
        .selby = function(m, grpc, coln) {
            # A commmon situation is to have data stored in an n by p matrix
            # where one or more of the columns are  group identification numbers.
            # This function groups  all values in column coln according to the
            # group numbers in column grpc and stores the  results in list mode.
            # More than one column of data can sorted
            # grpc indicates the column of the matrix containing group id number
            if (is.null(dim(m)))
                stop("Data must be stored in a matrix or data frame")

            if (is.na(grpc[1]))
                stop("The argument grpc is not specified")

            if (is.na(coln[1]))
                stop("The argument coln is not specified")

            if (length(grpc) != 1)
                stop("The argument grpc must have length 1")

            x <- vector("list")
            grpn <- sort(unique(m[, grpc]))
            it <- 0
            for (ig in 1:length(grpn)) {
                for (ic in 1:length(coln)) {
                    it <- it + 1
                    flag <- (m[, grpc]==grpn[ig])
                    x[[it]] <- m[flag, coln[ic]]
                }
            }

            rl <- list(x=x, grpn=grpn)
            return(rl)
        },
        .selby2 = function(m, grpc, coln = NA) {
            # Create categories according to the grpc[1] and grpc[2] columns of the matrix m.
            # The function puts the values in column coln into a vector having list mode.
            if (is.na(coln))
                stop("The argument coln is not specified")

            if (length(grpc) > 4)
                stop("The argument grpc must have length less than or equal to 4")

            x <- vector("list")
            ic <- 0
            if (length(grpc) == 2) {
                cat1 <- private$.selby(m, grpc[1], coln)$grpn
                cat2 <- private$.selby(m, grpc[2], coln)$grpn

                for (i1 in 1:length(cat1)) {
                    for (i2 in 1:length(cat2)) {
                        temp <- NA
                        it <- 0
                        for (i in 1:nrow(m)) {
                            if (sum(m[i,
                                      c(grpc[1],
                                        grpc[2])]==c(cat1[i1],
                                                     cat2[i2]))==2) {
                                it <- it + 1
                                temp[it] <- m[i, coln]
                            }
                        }

                        if (!is.na(temp[1])) {
                            ic <- ic + 1
                            x[[ic]] <- temp
                            if (ic == 1)
                                grpn <- matrix(c(cat1[i1], cat2[i2]), 1, 2)

                            if (ic > 1)
                                grpn <- rbind(grpn, c(cat1[i1], cat2[i2]))
                        }
                    }
                }
            }

            if (length(grpc) == 3) {
                cat1 <- private$.selby(m, grpc[1], coln)$grpn
                cat2 <- private$.selby(m, grpc[2], coln)$grpn
                cat3 <- private$.selby(m, grpc[3], coln)$grpn
                x <- vector("list")
                ic <- 0
                for (i1 in 1:length(cat1)) {
                    for (i2 in 1:length(cat2)) {
                        for (i3 in 1:length(cat3)) {
                            temp <- NA
                            it <- 0
                            for (i in 1:nrow(m)) {
                                if (sum(m[i,
                                          c(grpc[1],
                                            grpc[2],
                                            grpc[3])]==c(cat1[i1],
                                                         cat2[i2],
                                                         cat3[i3]))==3) {
                                    it <- it + 1
                                    temp[it] <- m[i, coln]
                                }
                            }

                            if (!is.na(temp[1])) {
                                ic <- ic + 1
                                x[[ic]] <- temp
                                if (ic == 1)
                                    grpn <- matrix(c(cat1[i1], cat2[i2], cat3[i3]), 1, 3)

                                if (ic > 1)
                                    grpn <- rbind(grpn, c(cat1[i1], cat2[i2], cat3[i3]))
                            }
                        }
                    }
                }
            }

            if (length(grpc) == 4) {
                cat1 <- private$.selby(m, grpc[1], coln)$grpn
                cat2 <- private$.selby(m, grpc[2], coln)$grpn
                cat3 <- private$.selby(m, grpc[3], coln)$grpn
                cat4 <- private$.selby(m, grpc[4], coln)$grpn
                x <- vector("list")
                ic <- 0
                for (i1 in 1:length(cat1)) {
                    for (i2 in 1:length(cat2)) {
                        for (i3 in 1:length(cat3)) {
                            for (i4 in 1:length(cat4)) {
                                temp <- NA
                                it <- 0
                                for (i in 1:nrow(m)) {
                                    if (sum(m[i,
                                              c(grpc[1],
                                                grpc[2],
                                                grpc[3],
                                                grpc[4])]==c(cat1[i1],
                                                             cat2[i2],
                                                             cat3[i3],
                                                             cat4[i4]))==4) {
                                        it <- it + 1
                                        temp[it] <- m[i, coln]
                                    }
                                }

                                if (!is.na(temp[1])) {
                                    ic <- ic + 1
                                    x[[ic]] <- temp

                                    if (ic == 1)
                                        grpn <- matrix(c(cat1[i1], cat2[i2], cat3[i3], cat4[i4]), 1, 4)

                                    if (ic > 1)
                                        grpn <- rbind(grpn, c(cat1[i1], cat2[i2], cat3[i3], cat4[i4]))
                                }
                            }
                        }
                    }
                }
            }
            rl <- list(x=x, grpn=grpn)
            return(rl)
        },
        .medpb = function(x, alpha=.05, nboot=NA, grp=NA, est=median,
                          con=0, bhop=FALSE, method='hoch', SEED=FALSE, ...) {
            # Multiple comparisons for J independent groups using medians.
            # A percentile bootstrap method.
            # FWE controlled via argument method
            # method = hoch  Hochberg; s method is used by default
            # The data are assumed to be stored in x which either has list mode
            # or is a matrix. In the first case x[[1]] contains the data for the
            # first group, x[[2]] the data for the second group, etc.
            # Length(x) = the number of groups = J.
            # If stored in a matrix, the columns of the matrix correspond
            # to groups.medpb<
            # est is the measure of location and defaults to the median ...
            # can be used to set optional arguments associated with est
            # The argument grp can be used to analyze a subset of the groups
            # Example: grp = c(1, 3, 5) would compare groups 1, 3 and 5.
            # con can be used to specify linear contrasts; see the function lincon
            # Missing values are allowed.

            con <- as.matrix(con)

            if (is.data.frame(x))
                x = as.matrix(x)

            if (is.matrix(x)) {
                y <- list()
                for (n in 1:ncol(x))
                    y[[n]] <- x[, n]

                x <- y
            }

            if (is.list(x)) {
                J = length(x)
                nval = NA
                for (j in 1:J)
                    nval[j] = length(x[[j]])

                temp <- matrix(NA, ncol=J, nrow=max(nval))
                for (j in 1:J)
                    temp[1:nval[j], j] <- x[[j]]

                temp <- private$.elimna(temp)
                y <- list()
                for (n in 1:ncol(temp))
                    y[[n]] <- temp[, n]

                x <- y
            }

            if (!is.list(x))
                stop('Data must be stored in list mode or in matrix mode.')

            if (!is.na(sum(grp))) {  # Only analyze specified groups.
                xx <- list()
                for (i in 1:length(grp))
                    xx[[i]] <- x[[grp[i]]]

                x <- xx
            }

            J <- length(x)
            tempn <- 0
            mvec <- NA
            for (j in 1:J) {
                temp <- x[[j]]
                temp <- temp[!is.na(temp)] # Remove missing values.
                tempn[j] <- length(temp)
                x[[j]] <- temp
                mvec[j] <- est(temp, ...)
            }

            Jm <- J - 1
            # Determine contrast matrix
            if (sum(con^2) == 0) {
                ncon <- (J^2-J)/2
                con <- matrix(0, J, ncon)
                id <- 0
                for (j in 1:Jm) {
                    jp <- j + 1
                    for (k in jp:J) {
                        id <- id + 1
                        con[j, id] <- 1
                        con[k, id] <- 0 - 1
                    }
                }
            }

            ncon <- ncol(con)
            dvec <- alpha / c(1:ncon)

            if (nrow(con) != J)
                stop('Something is wrong with con; the number of rows does not match the number of groups.')

            # Determine nboot if a value was not specified
            if (is.na(nboot)) {
                nboot <- 5000

                if (J <= 8)
                    nboot <- 4000

                if (J <= 3)
                    nboot <- 2000
            }

            # Determine critical values
            if (!bhop) {
                if (alpha == .05) {
                    dvec <- c(.05, .025, .0169, .0127, .0102, .00851, .0073, .00639, .00568, .00511)
                    if (ncon > 10) {
                        avec <- .05 / c(11:ncon)
                        dvec <- c(dvec, avec)
                    }
                }

                if (alpha == .01) {
                    dvec <- c(.01, .005, .00334, .00251, .00201, .00167, .00143, .00126, .00112, .00101)
                    if (ncon > 10) {
                        avec <- .01 / c(11:ncon)
                        dvec <- c(dvec, avec)
                    }
                }

                if (alpha != .05 && alpha != .01)
                    dvec <- alpha/c(1:ncon)

            }

            if (bhop)
                dvec <- (ncon-c(1:ncon) + 1)*alpha/ncon

            bvec <- matrix(NA, nrow = J, ncol = nboot)

            # set seed of random number generator so that results can be duplicated.
            # if (SEED)
            #     set.seed(2)

            for (j in 1:J) {
                data <- matrix(sample(x[[j]], size = length(x[[j]])*nboot, replace = TRUE), nrow = nboot)
                bvec[j, ] <- apply(data, 1, est, ...) # Bootstrapped values for jth group
            }

            test <- NA
            bcon <- t(con)%*%bvec # ncon by nboot matrix
            tvec <- t(con)%*%mvec

            for (d in 1:ncon) {
                tv <- sum(bcon[d, ]==0) / nboot
                test[d] <- sum(bcon[d, ]>0) / nboot + .5 * tv
                if (test[d] > .5)
                    test[d] <- 1 - test[d]
            }

            test <- 2 * test
            output <- matrix(0, ncon, 7)
            dimnames(output) <- list(NULL, c('con.num', 'psihat', 'p.value',
                                             'p.crit', 'ci.lower', 'ci.upper',
                                             'adj.p.value'))
            temp2 <- order(0-test)
            zvec <- dvec[1:ncon]
            sigvec <- (test[temp2] >= zvec)
            output[temp2, 4] <- zvec
            icl <- round(dvec[ncon]*nboot/2) + 1
            icu <- nboot - icl - 1
            for (ic in 1:ncol(con)) {
                output[ic, 2] <- tvec[ic, ]
                output[ic, 1] <- ic
                output[ic, 3] <- test[ic]
                temp <- sort(bcon[ic, ])
                output[ic, 5] <- temp[icl]
                output[ic, 6] <- temp[icu]
            }

            num.sig <- sum(output[, 3] <= output[, 4])
            output[, 7] = p.adjust(output[, 3], method=method)

            #rl <- list(output = output, con = con, num.sig = num.sig)
            rl <- list(output)
            return(rl)
        },
        .lincon = function(x, con=0, tr=.2, alpha=.05, pr=TRUE) {
            # A heteroscedastic test of d linear contrasts using trimmed means.
            # This version uses an improved method for computing the quantiles
            # of a Studentized maximum modulus distriburtion.
            # The data are assumed to be stored in $x$ in list mode, a matrix
            # or a data frame. If in list mode, length(x) is assumed to
            # correspond to the total number of groups.
            # It is assumed all groups are independent.
            # con is a J by d matrix containing the contrast coefficients that are used.
            # If con is not specified, all pairwise comparisons are made.
            # Missing values are automatically removed.
            # pr = FALSE included to avoid errors using an earlier version of
            # this function when dealing with two-way and higher designs.
            # Adjusted p-values are based on the Studentized maximum modulus
            # distribution with the goal of controlling FWE.
            # To apply the Kaiser-Bowden method, use the function kbcon

            winvar = function(x, tr=.2, na.rm=FALSE, STAND=NULL) {
                # Compute the gamma Winsorized variance for the data in the vector x.
                # tr is the amount of Winsorization which defaults to .2.
                remx = x
                x <- x[!is.na(x)]
                y <- sort(x)
                n <- length(x)
                ibot <- floor(tr*n) + 1
                itop <- length(x) - ibot + 1
                xbot <- y[ibot]
                xtop <- y[itop]
                y <- ifelse(y <= xbot, xbot, y)
                y <- ifelse(y >= xtop, xtop, y)
                wv <- var(y)

                if (!na.rm)
                    if (sum(is.na(remx) > 0))
                        wv = NA

                return(wv)
            }

            qsmm = function(q, r, nu) {
                # r = number of comparisons
                if (!is.finite(nu))
                    return(qnorm(1-0.5*(1-q^(1/r))))

                res = uniroot(function(c, r, nu, q) {
                    psmm(c, r=r, nu=nu) - q
                },
                c(0, 100), r=r, nu=nu, q=q)

                return(res$root)
            }

            psmm = function(x, r, nu) {
                psmm.x <- function(x, c, r, nu) {
                    snu <- sqrt(nu)
                    sx <- snu * x
                    lgx <- log(snu) - lgamma(nu/2) + (1-nu/2) * log(2) + (nu-1) * log(sx) + (-sx^2/2)
                    re <- exp(r*log(2*pnorm(c*x)-1)+lgx)

                    return(re)
                }

                res <- integrate(psmm.x, 0, Inf, c=x, r=r, nu=nu)
                return(res$value)
            }

            if (tr == .5)
                stop('Use the R function medpb to compare medians')

            if (is.data.frame(x))
                x = as.matrix(x)

            if (is.matrix(x))
                x <- listm(x)

            if (!is.list(x))
                stop('Data must be stored in a matrix or in list mode.')

            con <- as.matrix(con)
            J <- length(x)
            sam = NA
            h <- vector('numeric', J)
            w <- vector('numeric', J)
            xbar <- vector('numeric', J)
            for (j in 1:J) {
                xx <- !is.na(x[[j]])
                val <- x[[j]]
                x[[j]] <- val[xx]  # Remove missing values
                sam[j] = length(x[[j]])
                h[j] <- length(x[[j]]) - 2 * floor(tr*length(x[[j]]))
                w[j] <- ((length(x[[j]]) - 1) * winvar(x[[j]], tr)) / (h[j] * (h[j]-1))
                xbar[j] <- mean(x[[j]], tr)
            }

            if (sum(con^2) == 0) {
                CC <- (J^2-J) / 2
                psihat <- matrix(0, CC, 9)
                dimnames(psihat) <- list(NULL, c('Group', 'Group', 'psihat',
                                                 'ci.lower', 'ci.upper', 'p.value',
                                                 'Est.1', 'Est.2', 'adj.p.value'))
                test <- matrix(NA, CC, 6)
                dimnames(test) <- list(NULL, c('Group', 'Group', 'test',
                                               'crit', 'se', 'df'))
                jcom <- 0
                for (j in 1:J) {
                    for (k in 1:J) {
                        if (j < k) {
                            jcom <- jcom + 1
                            test[jcom, 3] <- abs(xbar[j] - xbar[k]) / sqrt(w[j] + w[k])
                            sejk <- sqrt(w[j] + w[k])
                            test[jcom, 5] <- sejk
                            psihat[jcom, 1] <- j
                            psihat[jcom, 2] <- k
                            test[jcom, 1] <- j
                            test[jcom, 2] <- k
                            psihat[jcom, 3] <- (xbar[j] - xbar[k])
                            df <- (w[j] + w[k])^2 / (w[j]^2 / (h[j]-1) + w[k]^2 / (h[k]-1))
                            test[jcom, 6] <- df
                            psihat[jcom, 6] <- 2 * (1 - pt(test[jcom, 3], df))
                            psihat[jcom, 7] = xbar[j]
                            psihat[jcom, 8] = xbar[k]
                            crit = qsmm(1-alpha, CC, df)
                            test[jcom, 4] <- crit
                            psihat[jcom, 4] <- (xbar[j]-xbar[k]) - crit * sejk
                            psihat[jcom, 5] <- (xbar[j]-xbar[k]) + crit*sejk
                            psihat[jcom, 9] = 1-psmm(test[jcom, 3], CC, df)
                        }
                    }
                }
            }

            if (sum(con^2) > 0) {
                if (nrow(con) != length(x))
                    stop('The number of groups does not match the number of contrast coefficients.')

                CC = ncol(con)
                psihat <- matrix(0, ncol(con), 6)
                dimnames(psihat) <- list(NULL, c('con.num', 'psihat', 'ci.lower',
                                                 'ci.upper', 'p.value', 'adj.p.value'))
                test <- matrix(0, ncol(con), 5)
                dimnames(test) <- list(NULL, c('con.num', 'test', 'crit', 'se', 'df'))
                df <- 0
                for (d in 1:ncol(con)) {
                    psihat[d, 1] <- d
                    psihat[d, 2] <- sum(con[, d] * xbar)
                    sejk <- sqrt(sum(con[, d]^2 * w))
                    test[d, 1] <- d
                    test[d, 2] <- sum(con[, d] * xbar) / sejk
                    df <- (sum(con[, d]^2 * w))^2 / sum(con[, d]^4 * w^2 / (h-1))
                    crit = qsmm(1-alpha, CC, df)
                    test[d, 3] <- crit
                    test[d, 4] <- sejk
                    test[d, 5] <- df
                    psihat[d, 3] <- psihat[d, 2] - crit * sejk
                    psihat[d, 4] <- psihat[d, 2] + crit * sejk
                    psihat[d, 5] <- 2 * (1-pt(abs(test[d, 2]), df))
                    psihat[d, 6] = 1 - psmm(abs(test[d, 2]), CC, df)
                }
            }
            #rl <- list(n=sam, test=test, psihat=psihat)
            rl <- list(psihat)
            return(rl)
        },
        .pbadepth = function(x, est=onestep, con=0, alpha=.05, nboot=2000,
                             grp=NA, op=3, allp=TRUE, MM=FALSE, MC=FALSE,
                             cop=3, SEED=FALSE, na.rm=FALSE, ...) {
            # Test the hypothesis that C linear contrasts all have a value of zero.
            # By default, an M-estimator is used
            # Independent groups are assumed.
            # The data are assumed to be stored in x in list mode or in a matrix.
            # If stored in list mode, x[[1]] contains the data for the first
            # group, x[[2]] the data for the second group, etc.
            # Length(x) = the number of groups = J, say.
            # If stored in a matrix, columns correspond to groups.
            # By default, all pairwise differences are used, but contrasts
            # can be specified with the argument con.
            # The columns of con indicate the contrast coefficients.
            # Con should have J rows, J = number of groups.
            # For example, con[, 1] = c(1, 1, -1, -1, 0, 0) and con[, 2] = c(, 1, -1, 0, 0, 1, -1)
            # will test two contrasts: (1) the sum of the first two measures of
            # location is equal to the sum of the second two, and (2) the
            # difference between the first two is equal to the difference between
            # the measures of location for groups 5 and 6.
            # op controls how depth is measured
            #   op = 1, Mahalanobis
            #   op = 2, Mahalanobis based on MCD covariance matrix
            #   op = 3, Projection distance
            #
            # MC = TRUE, use a multicore processor when op = 3
            # for arguments MM and cop, see pdis.

            pdis = function(m, pts=m, MM=FALSE, cop=3, dop=1, center=NA, na.rm=TRUE) {
                # Compute projection distances for points in pts relative to points in m
                # That is, the projection distance from the center of m
                #   MM = F  Projected distance scaled using interquatile range.
                #   MM = T  Scale projected distances using MAD.
                # There are five options for computing the center of the
                # cloud of points when computing projections:
                #   cop = 1 uses Donoho-Gasko median
                #   cop = 2 uses MCD center
                #   cop = 3 uses median of the marginal distributions.
                #   cop = 4 uses MVE center
                #   cop = 5 uses skipped mean

                m <- private$.elimna(m) # Remove missing values
                pts = private$.elimna(pts)
                m <- as.matrix(m)
                nm = nrow(m)
                pts <- as.matrix(pts)

                if (ncol(m) > 1)
                    if (ncol(pts) == 1)
                        pts = t(pts)

                npts = nrow(pts)
                mp = rbind(m, pts)
                np1 = nrow(m) + 1

                if (ncol(m) == 1) {
                    m = as.vector(m)
                    pts = as.vector(pts)

                    if (is.na(center[1]))
                        center <- median(m)

                    dis <- abs(pts-center)
                    disall = abs(m-center)
                    temp = idealf(disall)

                    if (!MM)
                        pdis <- dis / (temp$qu-temp$ql)

                    if (MM)
                        pdis <- dis / mad(disall)
                } else {
                    if (is.na(center[1])) {
                        if (cop == 1)
                            center <- dmean(m, tr=.5, dop=dop)

                        if (cop == 2)
                            center <- cov.mcd(m)$center

                        if (cop == 3)
                            center <- apply(m, 2, median)

                        if (cop == 4)
                            center <- cov.mve(m)$center

                        if (cop == 5)
                            center <- smean(m)
                    }

                    dmat <- matrix(NA, ncol = nrow(mp), nrow = nrow(mp))
                    for (i in 1:nrow(mp)) {
                        B <- mp[i, ] - center
                        dis <- NA
                        BB <- B^2
                        bot <- sum(BB)
                        if (bot != 0) {
                            for (j in 1:nrow(mp)) {
                                A <- mp[j, ] - center
                                temp <- sum(A*B) * B / bot
                                dis[j] <- sqrt(sum(temp^2))
                            }

                            dis.m = dis[1:nm]
                            if (!MM) {
                                #temp <- idealf(dis)
                                temp <- idealf(dis.m)
                                dmat[, i] <- dis / (temp$qu-temp$ql)
                            }

                            if (MM)
                                dmat[, i] <- dis / mad(dis.m)
                        }
                    }
                    pdis <- apply(dmat, 1, max, na.rm=na.rm)
                    pdis = pdis[np1:nrow(mp)]
                }
                return(pdis)
            }

            idealf = function(x, na.rm=FALSE) {
                # Compute the ideal fourths for data in x
                if (na.rm)
                    x <- x[!is.na(x)]

                j <- floor(length(x)/4 + 5/12)
                y <- sort(x)
                g <- (length(x)/4) - j + (5/12)
                ql <- (1-g) * y[j] + g * y[j + 1]
                k <- length(x) - j + 1
                qu <- (1-g) * y[k] + g * y[k-1]
                rl <- list(ql=ql, qu=qu)

                return(rl)
            }

            con <- as.matrix(con)
            if (is.matrix(x) || is.data.frame(x))
                x = listm(x)

            if (!is.list(x))
                stop("Data must be stored in list mode or in matrix mode.")

            if (!is.na(grp)) {  # Only analyze specified groups.
                xx <- list()
                for (i in 1:length(grp))
                    xx[[i]] <- x[[grp[i]]]

                x <- xx
            }

            J <- length(x)
            mvec <- NA
            nvec = NA
            for (j in 1:J) {
                temp <- x[[j]]
                if (na.rm)
                    temp <- temp[!is.na(temp)] # Remove missing values

                x[[j]] <- temp
                mvec[j] <- est(temp, ...)
                nvec[j] = length(temp)
            }

            Jm <- J - 1
            d <- ifelse(con == 0, (J^2 - J) / 2, ncol(con))
            if (sum(con^2) == 0) {
                if (allp) {
                    con <- matrix(0, J, d)
                    id <- 0
                    for (j in 1:Jm) {
                        jp <- j + 1
                        for (k in jp:J) {
                            id <- id + 1
                            con[j, id] <- 1
                            con[k, id] <- 0 - 1
                        }
                    }
                }

                if (!allp) {
                    con <- matrix(0, J, Jm)
                    for (j in 1:Jm) {
                        jp <- j + 1
                        con[j, j] <- 1
                        con[jp, j] <- 0 - 1
                    }
                }
            }

            bvec <- matrix(NA, nrow=J, ncol=nboot)
            # set seed of random number generator so that results can be duplicated.
            # if (SEED)
            #     set.seed(2)

            for (j in 1:J) {
                data <- matrix(sample(x[[j]], size=length(x[[j]])*nboot, replace=TRUE), nrow=nboot)
                bvec[j, ] <- apply(data, 1, est, na.rm=na.rm, ...)
            }

            chkna = sum(is.na(bvec))
            # if (chkna > 0) {
            #     print("Bootstrap estimates of location could not be computed")
            #     print("This can occur when using an M-estimator")
            #     print("Might try est = tmean")
            # }

            bcon <- t(con) %*% bvec
            tvec <- t(con) %*% mvec
            tvec <- tvec[, 1]
            tempcen <- apply(bcon, 1, mean)
            vecz <- rep(0, ncol(con))
            bcon <- t(bcon)
            smat <- var(bcon-tempcen + tvec)
            temp <- bcon - tempcen + tvec
            bcon <- rbind(bcon, vecz)

            if (op == 1)
                dv <- mahalanobis(bcon, tvec, smat)

            if (op == 2) {
                smat <- cov.mcd(temp)$cov
                dv <- mahalanobis(bcon, tvec, smat)
            }

            if (op == 3)
                dv <- pdis(bcon, MM=MM, cop=cop)

            bplus <- nboot + 1
            sig.level <- 1 - sum(dv[bplus] >= dv[1:nboot]) / nboot
            #rl <- list(p.value=sig.level, psihat=tvec, con=con, n=nvec)
            rl <- list(c(p.value = sig.level))

            return(rl)
        }
    ),
    active = list(
        # Active binding for groups ----
        groups = function(value) {

            if (missing(value)) {
                if (is.null(private$.groups)) {
                    groups <- self$options$factors
                    if (length(groups) == 0)
                        return(NULL)

                    lvlsuse <- list()
                    for (gr in groups) {
                        lvls <- levels(self$data[[gr]])
                        if (length(lvls) == 2)
                            lvls <- lvls[1]

                        lvlsuse[[gr]] <- lvls
                    }

                    private$.groups <- rev(expand.grid(rev(lvlsuse)))
                }

                return(private$.groups)
            }
        }
    )
)
