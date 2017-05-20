
# This file is a generated template, your changes will not be overwritten

rdescClass <- R6::R6Class(
    "rdescClass",
    inherit = rdescBase,
    private = list(
        .init = function() {
            
            vars <- self$options$vars
            table <- self$results$table
            
            if (is.null(self$options$splitBy)) {
                
                for (i in seq_along(vars))
                    table$addRow(rowKey=i, values=list("var"=vars[i]))
                
            } else {
                
                levels <- levels(self$data[[self$options$splitBy]])
                rowNo <- 1
                
                for (var in vars) {
                    
                    for (level in levels) {
                        table$addRow(rowKey=rowNo, values=list("var"=var, "level"=level))
                        rowNo <- rowNo + 1
                    }
                }
            }
        },
        .run = function() {

            vars <- self$options$vars

            table <- self$results$table
            data <- self$data
            
            if (is.null(self$options$splitBy)) {

                for (i in seq_along(vars)) {
                    
                    v <- jmvcore::toNumeric(data[[vars[i]]])
                    v <- na.omit(v)
    
                    r <- private$.means(v)
    
                    table$setRow(rowNo=i, values=list(
                        `m[m]`=r$m, `se[m]`=r$mse, `m[tr]`=r$mtr, `se[tr]`=r$mtrse,
                        `m[w]`=r$w, `se[w]`=r$wse, `m[med]`=r$med, `se[med]`=r$medse,
                        `m[est]`=r$mest, `se[est]`=r$mestse))
                }
                
            } else {
                
                f <- factor(data[[self$options$splitBy]])
                levels <- levels(f)
                rowNo <- 1
                
                for (i in seq_along(vars)) {
                    
                    v <- jmvcore::toNumeric(data[[vars[i]]])
                    df <- data.frame(v=v, f=f)
                    df <- jmvcore::naOmit(df)
                    
                    means <- tapply(df$v, df$f, private$.means)
                    
                    for (j in seq_along(levels)) {
                        
                        r <- means[[levels[j]]]
                        
                        table$setRow(rowNo=rowNo, values=list(
                            `m[m]`=r$m, `se[m]`=r$mse, `m[tr]`=r$mtr, `se[tr]`=r$mtrse,
                            `m[w]`=r$w, `se[w]`=r$wse, `m[med]`=r$med, `se[med]`=r$medse,
                            `m[est]`=r$mest, `se[est]`=r$mestse))
                        
                        rowNo <- rowNo + 1
                    }
                }
            }
        },
        .means = function(v) {
            
            tr   <- self$options$tr
            wl   <- self$options$wl
            bend <- self$options$bend
            
            m <- jmvcore::tryNaN(mean(v))
            mse <- jmvcore::tryNaN(WRS2::trimse(v, tr=0))
            
            mtr <- jmvcore::tryNaN(mean(v, trim=tr))
            mtrse <- jmvcore::tryNaN(WRS2::trimse(v, tr=tr))
            
            w <- jmvcore::tryNaN(WRS2::winmean(v, tr=wl))
            wse <- jmvcore::tryNaN(WRS2::winse(v, tr=wl))
            
            med <- jmvcore::tryNaN(median(v))
            medse <- jmvcore::tryNaN(WRS2::msmedse(v, sewarn=FALSE))
            
            mest <- jmvcore::tryNaN(WRS2::mest(v, bend=bend))
            mestse <- jmvcore::tryNaN(WRS2::mestse(v, bend=bend))
            
            r <- list("m"=m, "mse"=mse, "mtr"=mtr, "mtrse"=mtrse, "w"=w, "wse"=wse, 
                      "med"=med, "medse"=medse, "mest"=mest, "mestse"=mestse)
            
            return(r)
            
        })
)
