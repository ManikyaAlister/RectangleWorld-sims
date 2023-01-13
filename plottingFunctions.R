library(tidyverse)
library(ggplot2)



# ********************************
#     plotColourfulDistribution
# ********************************
#' @title plotColourfulDistribution
#' @description Like plotDistribution, except includes the probabilities of 
#' things and also makes points red if they are negative and green if positive
#' @param trueRectangle length 4 vector of true rectangle (default: none)
#' @param obs observations to be plotted with it (default: none)
#' @param allPts set of all of the points along with their probabilities 
#' @param xrange vector describing the x axis (default 0:10)
#' @param yrange vector describing the y axis (default 0:10)
#' @param title character string for a title for the graph (default:"Sampling distribution")
#' @param subtitle character string for a title for the graph (default:"Yellow rectangle is the true hypothesis")

plotColourfulDistribution = function(obs=NA, trueRectangle=c(0,0,0,0), allPts,
                                     xrange = 0:10, yrange=0:10,
                                     title="Sampling distribution", 
                                     subtitle="Yellow rectangle is the true hypothesis"){
  xlow <- min(xrange)
  xhigh <- max(xrange)
  ylow <- min(yrange)
  yhigh <- max(yrange)
  nPts <- nrow(allPts)
  trueR <- data.frame(x1=trueRectangle[1],y1=trueRectangle[2],
                      x2=trueRectangle[3],y2=trueRectangle[4])
  # makes it so all observations are black
  
  if (!is.null(nrow(obs))) {
    allPts$posterior[obs$index] <- 0
  }
  
  pRect <- ggplot() +
    xlim(xlow,xhigh) +
    ylim(ylow,yhigh) +  
    theme(panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = "black"), 
          panel.grid.minor = element_line(size = 0.05, linetype = 'solid',
                                          colour = "black"),
          panel.background = element_rect(fill = "black", colour = "black",
                                          size = 2, linetype = "solid")) +
    labs(title=title, subtitle=subtitle)
  
  pRect <- pRect + 
    geom_rect(data=allPts, 
              mapping=aes(xmin=x-0.5, xmax=x+0.5, ymin=y-0.5, ymax=y+0.5, 
                          fill=posterior),show.legend=FALSE) +
    #geom_text(data=allPts, mapping=aes(x=x, y=y, label=abs(p)),
    #          show.legend=FALSE) +
    scale_fill_gradient2(low="red",mid="black",high="green")
  
  pRect <- pRect +
    geom_rect(data=trueR, mapping=aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2), color="yellow",
              fill=NA,linetype="dashed")
  
  
  # add in observations if they exist
  if (!is.null(nrow(obs))) {
    nPos <- sum(obs$category=="positive")
    nNeg <- sum(obs$category=="negative")
    if (nPos > 0 & nNeg > 0) {
      pRect <- pRect +
        geom_point(data=obs, mapping=aes(x=x, y=y, color=category, shape=category), size=6,
                   show.legend=FALSE) +
        scale_shape_manual(values=c(4,19)) +
        scale_color_manual(values=c("#FF0000FF","#00A600"))          
    } else if (nNeg > 0) {
      pRect <- pRect +
        geom_point(data=obs, mapping=aes(x=x, y=y), color="#FF0000FF", shape=4, size=6,
                   show.legend=FALSE)        
    } else {
      pRect <- pRect +
        geom_point(data=obs, mapping=aes(x=x, y=y), color="#00A600", shape=19, size=6,
                   show.legend=FALSE)        
    }
  }
  
  return(pRect)
}



# **********************
#     plotDistribution
# **********************
#' @title plotDistribution
#' @description Plots a heatmap where the color of the points reflects the probability
#' of them. Superimposes the true rectangle and any observations, if given
#' @param trueRectangle length 4 vector of true rectangle (default: none)
#' @param obs observations to be plotted with it (default: none)
#' @param allPts set of all of the points along with their probabilities 
#' @param xrange vector describing the x axis (default 0:10)
#' @param yrange vector describing the y axis (default 0:10)
#' @param whichDist Either "prior" or "posterior" (default "posterior")
#' @param title character string for a title for the graph (default:"Hypothesis distribution")
#' @param subtitle character string for a title for the graph (default:"Yellow rectangle is the true hypothesis")

plotDistribution = function(obs=NA, trueRectangle=c(0,0,0,0), allPts,
                            xrange = 0:10, yrange=0:10, whichDist="posterior",
                            title="Hypothesis distribution", 
                            subtitle="Yellow rectangle is the true hypothesis"){
  xlow <- min(xrange)
  xhigh <- max(xrange)
  ylow <- min(yrange)
  yhigh <- max(yrange)
  nPts <- nrow(allPts)
  trueR <- data.frame(x1=trueRectangle[1],y1=trueRectangle[2],
                      x2=trueRectangle[3],y2=trueRectangle[4])
  
  if (!is.null(nrow(obs))) {
    allPts$posterior[obs$index] <- 0
  }

  pRect <- ggplot() +
    xlim(xlow,xhigh) +
    ylim(ylow,yhigh) +  
    theme(panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = "black"), 
          panel.grid.minor = element_line(size = 0.05, linetype = 'solid',
                                          colour = "black"),
          panel.background = element_rect(fill = "black", colour = "black",
                                          size = 2, linetype = "solid")) +
    labs(title=title, subtitle=subtitle)
  
    if (whichDist=="prior") {
      pRect <- pRect + 
        geom_rect(data=allPts, 
                  mapping=aes(xmin=x-0.5, xmax=x+0.5, ymin=y-0.5, ymax=y+0.5, 
                              fill=prior),show.legend=FALSE)       
    } else {
      pRect <- pRect + 
        geom_rect(data=allPts, 
                  mapping=aes(xmin=x-0.5, xmax=x+0.5, ymin=y-0.5, ymax=y+0.5, 
                              fill=posterior),show.legend=FALSE)  
    }

    
    pRect <- pRect +
      geom_rect(data=trueR, mapping=aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2), 
                color="yellow", fill=NA,linetype="dashed")

    
  # add in observations if they exist
  if (!is.null(nrow(obs))) {
    nPos <- sum(obs$category=="positive")
    nNeg <- sum(obs$category=="negative")
    
    if (nPos > 0 & nNeg > 0) {
      pRect <- pRect +
        geom_point(data=obs, mapping=aes(x=x, y=y, color=category, shape=category), size=6,
                   show.legend=FALSE) +
        scale_shape_manual(values=c(4,19)) +
        scale_color_manual(values=c("#FF0000FF","#00A600"))     
      
    } else if (nNeg > 0) {
      pRect <- pRect +
        geom_point(data=obs, mapping=aes(x=x, y=y), color="#FF0000FF", shape=4, size=6,
                   show.legend=FALSE)        
    } else {
      pRect <- pRect +
        geom_point(data=obs, mapping=aes(x=x, y=y), color="#00A600", shape=19, size=6,
                   show.legend=FALSE)        
    }
  }
  
  return(pRect)
}



# ******************************
#     plotHypotheses
# ******************************
#' @title plotHypotheses
#' @description Plots multiple rectangles, each indicating a different hypothesis, along with
#' observations (if provided) and the true rectangle (if provided)
#' Returns a variable with the plot in it
#' @param xrange vector describing the x axis (default 0:10)
#' @param yrange vector describing the y axis (default 0:10)
#' @param r length 4 vector of true rectangle to be plotted: (x1,y1,x2,y2)
#' @param obs observations to be plotted with it. Default is NA, in which case none are plotted
#' @param rects set of rectangles to be plotted. Default is NA, in which case only
#' @param title character string for a title for the graph (default:"Hypothesis distribution")
#' @param subtitle character string for a title for the graph (default:"Yellow rectangle is the true hypothesis")

#' the true rectangle is plotted

plotHypotheses = function(r,obs=NA,rects=NA, xrange = 0:10,yrange=0:10,
                          title="Best hypotheses", 
                          subtitle="Solid line is true, dashed are best"){
  jitsize <- 0.05
  xlow <- min(xrange)
  xhigh <- max(xrange)
  ylow <- min(yrange)
  yhigh <- max(yrange)
  d <- data.frame(x1=r[1],y1=r[2],x2=r[3],y2=r[4])
  
  # basic plot
  pRect <- ggplot() +
    xlim(xlow-jitsize,xhigh+jitsize) +
    ylim(ylow-jitsize,yhigh+jitsize) +
    theme(panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"), 
          panel.grid.minor = element_line(size = 0.05, linetype = 'solid',
                                          colour = "grey"),
          panel.background = element_rect(fill = "white", colour = "black",
                                          size = 2, linetype = "solid")) +
    labs(title=title, subtitle=subtitle)
  
  # add in other rectangles if they exist
  if (!is.null(nrow(rects))) {
    
    # add jitter so can see them
    xjit <- runif(n=nrow(rects),min=-1*jitsize,max=jitsize)
    yjit <- runif(n=nrow(rects),min=-1*jitsize,max=jitsize)
    rects$x1 <- rects$x1+xjit
    rects$x2 <- rects$x2+xjit
    rects$y1 <- rects$y1+yjit
    rects$y2 <- rects$y2+yjit
    
    pRect <- pRect +
      geom_rect(data=rects, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),
                color="blue", fill=NA, linetype="dashed") 
  }  

  # add in true rectangle
  pRect <- pRect + 
    geom_rect(data=d, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color="black", fill="blue",
              alpha=0.1)
  
  # add in observations if they exist
  if (!is.null(nrow(obs))) {
    nPos <- sum(obs$category=="positive")
    nNeg <- sum(obs$category=="negative")
    if (nPos > 0 & nNeg > 0) {
      pRect <- pRect +
        geom_point(data=obs, mapping=aes(x=x, y=y, color=category, shape=category), size=6,
                   show.legend=FALSE) +
        scale_shape_manual(values=c(4,19)) +
        scale_color_manual(values=c("#FF0000FF","#00A600"))          
    } else if (nNeg > 0) {
      pRect <- pRect +
        geom_point(data=obs, mapping=aes(x=x, y=y), color="#FF0000FF", shape=4, size=6,
                   show.legend=FALSE)        
    } else {
      pRect <- pRect +
        geom_point(data=obs, mapping=aes(x=x, y=y), color="#00A600", shape=19, size=6,
                   show.legend=FALSE)        
    }
  
  } 
  
  return(pRect)
}


# **********************
#     plotHeatmap
# **********************
#' @title plotHeatmap
#' @description Plots a heatmap corresponding to a pts x hyp matrix
#' where the colour reflects the probability of them. 
#' @param probPts pts x hyp matrix to be drawn
#' @param title character string for a title for the graph (default:"Hypothesis vs Points")
#' @param subtitle character string for a title for the graph (default:"Lighter colours are more probable")

plotHeatmap = function(probPts, title="Hypothesis vs Points", 
                            subtitle="Lighter colours are more probable"){
  
  nPts <- nrow(probPts)
  nHyp <- ncol(probPts)
  dd <- expand.grid(1:nPts,1:nHyp)
  colnames(dd) <- c("p","h")
  dd$points <- paste0("p",dd$p)
  dd$hypotheses <- paste0("h",dd$h)
  dd <- dd %>% select(points,hypotheses)
  ppp <- as.vector(probPts)
  
  pHeat <- ggplot(data=dd,aes(points, hypotheses, fill= ppp)) +    
    geom_tile(show.legend=FALSE) + 
    scale_fill_viridis(discrete=FALSE) +
    theme(panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = "black"), 
          panel.grid.minor = element_line(size = 0.05, linetype = 'solid',
                                          colour = "black"),
          panel.background = element_rect(fill = "black", colour = "black",
                                          size = 2, linetype = "solid")) +
    labs(title=title, subtitle=subtitle)

  return(pHeat)
}


# Plotting fit to alphas --------------------------------------------------


#' Plot the mean posterior probability across multiple learner guesses for a set of alphas
#'
#' @param posteriorsAlphas A data frame containing the posterior probability of a given rectangle
#' for multiple alphas. The data frame should contain three columns: alpha, posterior, and index. It can be 
#' generated from the function GetMultipleAlphaPosteriors(). 
#' @param generatingAlpha The "true" alpha that was used to generate the rectangles.
#'
#' @return Bar chart with each alpha on the x axis and mean probability that each alpha produced the participants guesses (collapsed across all guesses)
#' @export
#'
#' @examples
plotMeanPosteriorAlphas = function(posteriorsAlphas,
                                   generatingAlpha,
                                   recoveory = TRUE) {
  posteriorsAlphas %>%
    group_by(alpha) %>%
    summarise(sum = sum(posterior), mean = mean(posterior)) %>%
    mutate(alpha = as.factor(alpha)) %>%
    ggplot() +
    geom_col(aes(x = alpha, y = mean), fill = "blue",alpha = 0.5) +
    geom_vline(xintercept = "1", colour = "red") +
    theme_classic()
}


#' Plot the posterior probability of a rectangle(s) for a set of alphas
#'
#' @param posteriorAlphas A data frame containing the posterior probability of a given rectangle
#' for multiple alphas. The data frame should contain three columns: alpha, posterior, and index. It can be 
#' generated from the function GetMultipleAlphaPosteriors(). 
#' @param generatingAlpha The "true" alpha that was used to generate the rectangles. 
#'
#' @return ggplot object 
#' @export
#'
#' @examples
plotRawPosteriorAlphas = function(posteriorAlphas, generatingAlpha){
  posteriorAlphas %>%
    ggplot()+
    geom_point(aes(x = alpha, y = posterior, colour = as.factor(index)))+
    geom_vline(xintercept = generatingAlpha, colour = "red") 
}

