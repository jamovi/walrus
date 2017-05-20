
# This file is a generated template, your changes will not be overwritten

rAnovaClass <- R6::R6Class(
    "rAnovaClass",
    inherit = rAnovaBase,
    private = list(
        .cleanData = function() {
            
            dep <- self$options$dep
            factors <- self$options$factors
            
            data <- self$data
            
            if ( ! is.null(dep))
                data[[dep]] <- jmvcore::toNumeric(data[[dep]])
            
            for (factor in factors)
                data[[factor]] <- as.factor(data[[factor]])
            
            data
        },
        .init = function() {
            
            main <- self$results$main
            
            factors <- self$options$factors
            method  <- self$options$method
            
            if (length(factors) == 0)
                return()
            
            data <- private$.cleanData()
            
            for (f in private$.ff()) {
                term <- jmvcore::stringifyTerm(f)
                main$addRow(rowKey=f, values=list(name=term))
            }
            
            for (factor in factors) {
                
                ph <- self$results$phs$get(factor)
            
                rowNo <- 1
                lvls <- base::levels(data[[factor]])
                combns <- combn(lvls, 2)
                
                for (col in 1:ncol(combns)) {
                    pair <- combns[,col]
                    ph$addRow(rowKey=rowNo, values=list(v1=pair[1], v2=pair[2]))
                    rowNo <- rowNo + 1
                }
            }

            if (length(factors) == 1) {

                if (method == 'trim') {

                    main$setNote('method', jmvcore::format('Method of trimmed means, trim level {}', self$options$tr))

                } else if (method == 'median') {

                    main$setNote('method', 'Median method')

                } else {  # bootstrap
                    
                    # bootstrap of 1 way designs is more like the trim method, and
                    # doesn't look anything like the bootstrap 2 way stuff
                    
                    jmvcore::reject('Bootstrap method is unavailable for 1-way designs')

                    main$getColumn('s')$setTitle('F*')
                }

            } else if (length(factors) == 2) {

                main$getColumn('s')$setTitle('Q')

                if (method == 'trim') {

                    main$setNote('method', jmvcore::format('Method of trimmed means, trim level {}', self$options$tr))

                } else if (method == 'median') {

                    main$setNote('method', 'Median method')

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
                    
                    main$setNote('method', jmvcore::format(
                        'Bootstrap method, {} estimator, {} samples, {} distances',
                        estimator, self$options$nboot, distances))
                    
                    main$setNote('nostat', 'Test statistics are not available for 2-way bootstrapped designs')
                }

            } else if (length(factors) == 3) {

                main$getColumn('s')$setTitle('Q')

                if (method == 'trim') {

                    main$setNote('method', jmvcore::format('Method of trimmed means, trim level {}', self$options$tr))

                } else if (method == 'median') {

                    jmvcore::reject('Median method is unavailable for 3-way designs')

                } else {  # bootstrap

                    jmvcore::reject('Bootstrap method is unavailable for 3-way designs')
                }
            } else {

                jmvcore::reject("More than 3 factors is not supported")
            }

        },
        .run = function() {
            
            dep <- self$options$dep
            factors <- self$options$factors
            method  <- self$options$method
            
            if (is.null(dep) || length(factors) == 0)
                return()
            
            main <- self$results$main
            phTables <- self$results$phs
            
            data <- private$.cleanData()
            colnames(data) <- jmvcore::toB64(colnames(data))
            
            factors <- jmvcore::toB64(factors)
            dep     <- jmvcore::toB64(dep)
            
            fmla <- paste(dep, '~', paste(factors, collapse='*'))
            fmla <- as.formula(fmla)
            
            if (length(factors) == 1) {
                
                if (method == 'trim') {
                    
                    result <- WRS2::t1way(fmla, data, tr=self$options$tr)
                    
                    main$setRow(rowNo=1, values=list(
                        s=result$test, p=result$p.value))
                    
                    private$.checkpoint()
                    
                    result <- try(WRS2::lincon(fmla, data, tr=self$options$tr)$comp)
                    
                    if ( ! jmvcore::isError(result)) {
                        
                        ph <- phTables$get(index=1)
                        for (rowNo in seq_len(ph$rowCount)) {
                            
                            psi <- result[rowNo,'psihat']
                            p   <- result[rowNo,'p.value']
                            cil <- result[rowNo,'ci.lower']
                            ciu <- result[rowNo,'ci.upper']
                            
                            ph$setRow(rowNo=rowNo, values=list(
                                psi=psi, p=p, cil=cil, ciu=ciu))
                        }
                        
                    } else {
                        message <- jmvcore::extractErrorMessage(result)
                        phTables$setError(message)
                    }
                    
                } else if (method == 'median') {
                    
                    result <- WRS2::med1way(fmla, data)
                    
                    main$setRow(rowNo=1, values=list(
                        s=result$test, p=result$p.value))
                    
                    phTables$setError('Post hoc tests are not available for the median method')
                    
                } else {  # bootstrap
                    
                    if ( ! main$isFilled()) {
                    
                        result <- WRS2::t1waybt(fmla, data, nboot=self$options$nboot)
                        
                        main$setRow(rowNo=1, values=list(
                            s=result$test, p=result$p.value))
                        
                        private$.checkpoint()
                    }
                    
                    result <- try(WRS2::mcppb20(fmla, data, nboot=self$options$nboot)$comp)
                    if ( ! jmvcore::isError(result)) {
                        
                        ph <- phTables$get(index=1)
                        for (rowNo in seq_len(ph$rowCount)) {
                            
                            psi <- result[rowNo,'psihat']
                            p   <- result[rowNo,'p-value']
                            cil <- result[rowNo,'ci.lower']
                            ciu <- result[rowNo,'ci.upper']
                            
                            ph$setRow(rowNo=rowNo, values=list(
                                psi=psi, p=p, cil=cil, ciu=ciu))
                        }
                        
                    } else {
                        message <- jmvcore::extractErrorMessage(result)
                        phTables$setError(message)
                    }
                    
                }
                
            } else if (length(factors) == 2) {
                
                if (method == 'trim') {
                    
                    result <- WRS2::t2way(fmla, data, tr=self$options$tr)
                    
                    main$setRow(rowNo=1, values=list(
                        s=result$Qa, p=result$A.p.value))
                    main$setRow(rowNo=2, values=list(
                        s=result$Qb, p=result$B.p.value))
                    main$setRow(rowNo=3, values=list(
                        s=result$Qab, p=result$AB.p.value))
                    
                    private$.checkpoint()
                    
                    result <- try(WRS2::mcp2atm(fmla, data, tr=self$options$tr)$effects)
                    
                    if ( ! jmvcore::isError(result)) {
                        
                        for (key in phTables$itemKeys) {
                            b64 <- jmvcore::toB64(key)
                            values <- result[[b64]]
                            table <- phTables$get(key)
                            for (rowNo in seq_len(table$rowCount)) {
                                psi <- values$psihat[rowNo]
                                ci <- values$conf.int
                                if (length(dim(ci)) > 1)
                                    ci <- ci[rowNo,]
                                cil <- ci[1]
                                ciu <- ci[2]
                                p   <- values$p.value[rowNo]
                                table$setRow(rowNo=rowNo, values=list(
                                    psi=psi, p=p, cil=cil, ciu=ciu))
                            }
                        }
                        
                    } else {
                        
                        message <- jmvcore::extractErrorMessage(result)
                        phTables$setError(message)
                    }
                    
                } else if (method == 'median') {
                    
                    result <- WRS2::med2way(fmla, data)
                    
                    main$setRow(rowNo=1, values=list(
                        s=result$Qa, p=result$A.p.value))
                    main$setRow(rowNo=2, values=list(
                        s=result$Qb, p=result$B.p.value))
                    main$setRow(rowNo=3, values=list(
                        s=result$Qab, p=result$AB.p.value))
                    
                    phTables$setError('Post hoc tests are not available for the median method')
                    
                } else {  # bootstrap
                    
                    nboot <- self$options$nboot
                    est <- self$options$est
                    if (self$options$dist == 'proj')
                        pro.dis <- TRUE
                    else
                        pro.dis <- FALSE
                    
                    print(main)
                    print(main$isFilled())
                    
                    if ( ! main$isFilled()) {
                    
                        result <- WRS2::pbad2way(fmla, data, est=est, nboot=nboot, pro.dis=pro.dis)
                    
                        main$setRow(rowNo=1, values=list(
                            s=NaN, p=result$A.p.value))
                        main$setRow(rowNo=2, values=list(
                            s=NaN, p=result$B.p.value))
                        main$setRow(rowNo=3, values=list(
                            s=NaN, p=result$AB.p.value))
                        
                        private$.checkpoint()
                    }
                    
                    result <- try(WRS2::mcp2a(fmla, data, est=est, nboot=nboot)$effects)
                    
                    if ( ! jmvcore::isError(result)) {
                        
                        for (key in phTables$itemKeys) {
                            b64 <- jmvcore::toB64(key)
                            values <- result[[b64]]
                            table <- phTables$get(key)
                            for (rowNo in seq_len(table$rowCount)) {
                                psi <- values$psihat[rowNo]
                                ci <- values$conf.int
                                if (length(dim(ci)) > 1)
                                    ci <- ci[rowNo,]
                                cil <- ci[1]
                                ciu <- ci[2]
                                p   <- values$p.value[rowNo]
                                table$setRow(rowNo=rowNo, values=list(
                                    psi=psi, p=p, cil=cil, ciu=ciu))
                            }
                        }
                        
                    } else {
                        
                        message <- jmvcore::extractErrorMessage(result)
                        phTables$setError(message)
                    }
                }
                
            } else if (length(factors) == 3) {
                
                ph <- NULL
                
                if (method == 'trim') {
                    
                    result <- WRS2::t3way(fmla, data, tr=self$options$tr)
                    main$setRow(rowNo=1, values=list(
                        s=result$Qa, p=result$A.p.value))
                    main$setRow(rowNo=2, values=list(
                        s=result$Qb, p=result$B.p.value))
                    main$setRow(rowNo=3, values=list(
                        s=result$Qc, p=result$C.p.value))
                    main$setRow(rowNo=4, values=list(
                        s=result$Qab, p=result$AB.p.value))
                    main$setRow(rowNo=5, values=list(
                        s=result$Qac, p=result$AC.p.value))
                    main$setRow(rowNo=6, values=list(
                        s=result$Qbc, p=result$BC.p.value))
                    main$setRow(rowNo=7, values=list(
                        s=result$Qabc, p=result$ABC.p.value))
                    
                    phTables$setError('Post hoc tests are not available for 3-way designs')
                    
                } else if (method == 'median') {
                    
                    jmvcore::reject('Median method is unavailable for 3-way designs')
                    
                } else {  # bootstrap
                    
                    jmvcore::reject('Bootstrap method is unavailable for 3-way designs')
                }
            }
            

        },
        .ff=function() {
            
            factors <- self$options$factors
            
            if (length(factors) > 1) {
                formula <- as.formula(paste('~', paste(paste0('`', factors, '`'), collapse='*')))
                terms   <- attr(stats::terms(formula), 'term.labels')
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
            
            modelTerms
        })
)
