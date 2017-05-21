
# This file is a generated template, your changes will not be overwritten

rplotsClass <- R6::R6Class(
    "rplotsClass",
    inherit = rplotsBase,
    private = list(
        .run = function() {

            vars <- self$options$vars
            factor <- self$options$splitBy
            
            for (var in vars) {
                
                image <- self$results$plots$get(key=var)
                
                v <- jmvcore::toNumeric(self$data[[var]])
                df <- data.frame(v=v)
                
                if ( ! is.null(self$options$splitBy))
                    df$f <- factor(self$data[[factor]])
                else
                    df$f <- factor(rep('var', length(v)))
                
                df <- jmvcore::naOmit(df)
                
                image$setState(df)
                
            }
        },
        .plot = function(image, ggtheme, theme, ...) {
            
            if (is.null(image$state))
                return(FALSE)
            
            factor <- self$options$splitBy
            
            p <- ggplot2::ggplot(data=image$state, ggplot2::aes(x = f, y = v)) +
                ggtheme + ggplot2::theme(legend.position = "none") +
                ggplot2::labs(x=factor, y=image$key)
            
            if (self$options$violin)
                p <- p + ggplot2::geom_violin(fill=theme$fill[1], color=theme$color[1])
            
            if (self$options$dot) {
                if (self$options$dotType == 'jitter')
                    p <- p + ggplot2::geom_jitter(color=theme$color[1], width=0.1, alpha=0.4)
                else
                    p <- p + ggplot2::geom_dotplot(binaxis = "y", stackdir = "center", color=theme$color[1], 
                                                   alpha=0.4, stackratio=0.9, dotsize=0.7)
            }
                
            
            if (self$options$boxplot)
                p <- p + ggplot2::geom_boxplot(color=theme$color[1], width=0.2, alpha=0.9, fill=theme$fill[2], 
                                               outlier.colour=theme$color[1])
            
            
            
            if (is.null(self$options$splitBy)) {
                p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                                        axis.ticks.x = ggplot2::element_blank(),
                                        axis.title.x = ggplot2::element_blank())
            }
            
            suppressWarnings({
                suppressMessages({
                    print(p)
                }) # suppressMessages
            }) # suppressWarnings
            
            TRUE
        })
)
