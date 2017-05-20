
# This file is a generated template, your changes will not be overwritten

rdescClass <- R6::R6Class(
    "rdescClass",
    inherit = rdescBase,
    private = list(
        .run = function() {

            vars <- self$options$vars
            tr   <- self$options$tr
            wl   <- self$options$wl
            bend <- self$options$bend
            
            table <- self$results$table
            data <- self$data
            
            for (var in vars) {
                v <- jmvcore::toNumeric(data[[var]])
                v <- na.omit(v)
                
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
                
                table$setRow(rowKey=var, values=list(
                    `m[m]`=m, `se[m]`=mse, `m[tr]`=mtr, `se[tr]`=mtrse,
                    `m[w]`=w, `se[w]`=wse, `m[med]`=med, `se[med]`=medse,
                    `m[est]`=mest, `se[est]`=mestse))
            }
            
        })
)
