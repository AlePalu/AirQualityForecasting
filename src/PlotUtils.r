## plot posterior density of parameters togheter with 95% credible intervals
## sample is an MCMC sample obtained from fitAR(...)
## vars is a list of parameters we want to extract
## cI: if TRUE will plot also the credible intervals
## alpha: level of the credible interval to plot, default 0.025 (for a 95% credible interval)
plotPosteriorDensity <- function(sample, vars, cI = FALSE, alpha = 0.025){
    # extract samples to plot
    plotData = rstan::extract(sample, vars) %>% 
               as_tibble() %>% 
               map_df(as_data_frame, .id = 'param')

    print(head(plotData))
    
    plots = list()
    for(var in vars){
        data = plotData[plotData$param == var,]
        # plot density
        p = ggplot(data, aes(value)) + 
                    geom_density(fill = "lightblue", alpha = 0.4) + 
                    labs(title = var)
        if(cI){
            qu = quantile(data$value, prob = c(alpha, 1 - alpha))
            p = p + geom_vline(xintercept = qu, lty = 2, color = "blue", size = 1)
        }
        plots <- c(plots, list(p))
        # plot traceplot
        p = ggplot(data, aes(x = 1:length(value), y = value)) + 
                    geom_line(color = "grey") + 
                    labs(x = "iterations", y = "trace")
        plots <- c(plots, list(p))
    }
    grid.arrange(grobs = plots, nrow = length(vars), ncol = 2)
}

## plot forecast matrix
## testData:  data against which we want to compare the forecast
## trainData: data used in ARforecsat function
## forecast is the result of a forecast function
## title is just the title to show in the plot
plotForecast <- function(testData, trainData, forecast, order, title, regressors = NULL,
                         png = FALSE, fileTitle = NULL, limits = c(-50, 160)){

    forecastDF = data.frame(yF   = colMeans(forecast),
                            xF   = (1:(dim(forecast)[2]))+ 24,
                            ylow = apply(forecast, 2, quantile, p = 0.05),
                            yup  = apply(forecast, 2, quantile, p = 0.95))
    
    trueDF = data.frame(yT = testData,
                        xT = (1:(length(testData)))+24)
    
    ## just to plot the last day in train set
    trainDF = data.frame(yTr = c(trainData[(length(trainData)-24):length(trainData)], testData[1]),
                         xTr = 0:25)
    
    plots = list()
    ## plot forecast
    p = ggplot(forecastDF) + geom_point(aes(x = xF, y = yF), color = "red", size = 2) +
        geom_line(aes(x = xF, y = yF), color = "red") + 
        geom_ribbon(aes(ymin = ylow, ymax = yup, x = xF), alpha = 0.2, fill = "#ff8a82") + 
        geom_vline(xintercept = 25, lty = 2, color = "orange", size = 1) +
        
        ## plot true data
        geom_line(data = trueDF, aes(x = xT, y = yT)) + 
        geom_point(data = trueDF, aes(x = xT, y = yT), color = "black", size = 2) +
        
        ## plot the day before
        geom_line(data = trainDF, aes(x = xTr, y = yTr), color = "#5cad5f") +
        geom_point(data = trainDF, aes(x = xTr, y = yTr), color = "#5cad5f", size = 2) +
        
        theme(axis.text.y = element_blank()) +
        scale_y_continuous(limits = c(limits[1], limits[2]),breaks = seq(limits[1], limits[2], by = 20)) +
        ggtitle(title)
    
    plots = c(plots, list(p))

    if (!is.null(regressors)) {
        for (var in ls(regressors)) {
            varDF = data.frame(xV = (1:length(regressors[[var]]))+24,
                               yV = regressors[[var]])

            p = ggplot(varDF) +
                geom_point(aes(x = xV, y = yV), color = "black", size = 2) +
                geom_line(aes(x = xV, y = yV)) +
                ggtitle(var) +
                theme(axis.text.y = element_blank()) +
                geom_vline(xintercept = 25, lty = 2, color = "orange", size = 1) +
                xlim(0, 24 + (dim(forecast)[2] + order))

            plots = c(plots, list(p))            
        }
    }

    if (png == FALSE) {
        x11()
        grid.arrange(grobs = plots, nrow = (1 + length(regressors)), ncol = 1)
    } else {
        if (is.null(fileTitle)) {
            return()
        } else {
            ## save plot on a file
            file = sprintf("frames/%s.png", fileTitle)
            png(file, width = 1200, height = 800)
            picture = grid.arrange(grobs = plots, nrow = (1 + length(regressors)), ncol = 1)
            print(picture)
            dev.off()
        }
    }
}

plotPosteriorBoxplot <- function(sample, names){
    dat <- stack(as.data.frame(sample))
    dat$ind <- rep(names, each = dim(sample)[1])

    x11()
    ggplot(dat) +
        geom_boxplot(aes(x = ind, y = values, fill = ind)) +
        geom_hline(aes(yintercept = 0), col = 2, lty = 2) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold")) +
        theme(legend.position = "null") +
        xlab("") + 
        ylab("")

}
