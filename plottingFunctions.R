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
# Abdy's version
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
                                   alphas = c(-5,-2, -1, -0.5,-0.1,0,0.1,0.5, 1, 2, 5),
                                   title = "",
                                   recovery = TRUE,
                                   stat = "mean") {
  
  generatingAlpha = as.character(generatingAlpha)
  posteriorsAlphas %>%
    filter(alpha %in% alphas) %>%
    group_by(alpha) %>%
    summarise(sum = sum(posterior), mean = mean(posterior), median = median(posterior)) %>%
    mutate(alpha = as.factor(alpha)) %>%
    ggplot() +
    {if(stat == "mean")geom_col(aes(x = alpha, y = mean), fill = "blue",alpha = 0.5)} +
    {if(stat == "median")geom_col(aes(x = alpha, y = median), fill = "blue",alpha = 0.5)} +
    {if(stat == "sum")geom_col(aes(x = alpha, y = sum), fill = "blue",alpha = 0.5)} +
    {if(recovery)geom_vline(xintercept = generatingAlpha, colour = "red")} +
    #ylim(c(0,0.005))+
    labs(subtitle = title, x = "Alpha") +
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

plotPosteriors = function(sum_data, all_data, statistic, block, exp, scales = "free", man_check = FALSE){
  plot <- sum_data %>% 
    mutate(alpha = as.factor(alpha)) %>% 
    ggplot()+
    {if(statistic == "mean")geom_col(aes(x = alpha, y = mean))} +
    {if(statistic == "median")geom_col(aes(x = alpha, y = median, fill = alpha), alpha = 1)}+
    {if(statistic == "sum")geom_col(aes(x = Alpha, y = sum))}+
    {if(statistic == "prob")geom_col(aes(x = Alpha, y = Probability))}+
    theme_classic()+
    #geom_jitter(data = all_data, aes(x = alpha, y = posterior, colour = alpha), alpha = 0.5)+
    scale_color_brewer(palette = "RdYlGn")+
    scale_fill_brewer(palette = "RdYlGn")+
    labs(x = "", y = "")+
    #{if(scales == "free")facet_wrap(~cond+clue, ncol = 4, scales = "free")}+
    #{if(scales == "fixed")facet_wrap(~cond+clue, ncol = 4)}+
    theme(#axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          #axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          text = element_text(size = 18),
          #plot.margin = margin(0, 0, 0, 0, "cm"),
          legend.position = "none",
          
          )
  plot
  # if(man_check){
  #   ggsave(plot = plot, filename = here(paste0("experiment-",exp,"/modelling/05_plots/b-",block,"-",statistic,"posterior-mc.png")))
  # }else{
  #   ggsave(plot = plot, filename = here(paste0("experiment-",exp,"/modelling/05_plots/b-",block,"-",statistic,"posterior.png")))
  # }
  # plot
}

sizeHist = function(data){
  data %>%
    mutate(index = as.character(index)) %>%
    group_by(cond, size_resp) %>%
    arrange(size_resp) %>%
    ggplot()+
    geom_bar(aes(x = as.factor(size_resp)))+
    geom_vline(xintercept = "228", colour = "red")+
    ylim(c(0,50))+
    facet_wrap(~clue+cond, ncol = 4, scales = "free")
}


#' For plotting the size histograms and the models together
#'
#' @param data 
#' @param condition 
#' @param plotAlpha 
#' @param prob_constant 
#'
#' @return
#' @export
#'
#' @examples
sizeHistModel = function(data, condition, plotAlpha, prob_constant = 250){
  
  data <- data %>%
    mutate(cover_cond = case_when(
      cond == "HS" | cond == "HN" ~ "helpful",
      cond == "MS" | cond == "MN" ~ "misleading", 
      cond == "UN" | cond == "US" ~ "uninformative",
      cond == "RS" | cond == "RN" ~ "random"
    ))
  
  
  data %>%
    filter(cond == condition & cover_cond == plotAlpha) %>%
    ggplot(aes(x = factor(size))) +
    geom_col(aes(y = count, fill = cond)) +
    geom_line(aes(y = prob*prob_constant, colour = factor(cover_cond), group = factor(cover_cond)), linewidth = 0.8, colour = "grey28")+ # get on same scale
    #geom_line(aes(y = prob*prob_constant, colour = factor(cover_cond), group = factor(cover_cond)), linewidth = 0.8)+ # get on same scale
    scale_fill_manual(values = c("HS" = "seagreen", "HN" = "seagreen", "RS" = "lightblue", "RN" = "lightblue", "MS" = "red", "MN" = "red", "UN" = "orange", "US" = "orange"))+
    #geom_line(data = all_dists, aes(x = size, y = posterior, colour = Alpha))+
    #scale_colour_manual(values = c("helpful" = "darkseagreen", "random" = "blue", "misleading" = "red3", "uninformative" = "yellow"))+
    labs(y = "Count")+
    #scale_y_continuous(sec.axis = sec_axis(~.*0.0005, name="Posterior")) +
    theme_classic()+
    ylim(c(0,86))+
    theme(axis.text.x=element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x=element_blank(),
          #axis.text.y=element_blank(),
          #axis.ticks.y=element_blank(),
          text = element_text(size = 12),
          #strip.background = element_blank(),
          #plot.margin = margin(-1, 0, -1, 0, "cm"),
          strip.text = element_text(margin = margin(0,0,0,0, "cm"), size = 5),
          legend.position = "none")
  #facet_wrap(~cond+Experiment, ncol = 2)
}

plotHeatMaps = function(d, all_conditions, experiment){
  # function to vectorise isInRect function
  applyIsInRect <- function(df, rectangle) {
    inRectangle <- apply(df[,c("x","y")], 1, function(p) isInRectangle(p, rectangle))
    return(inRectangle)
  }
  
  # Set up grid of all possible points
  x  = seq(0.5, 9.5)
  y = seq(0.5, 9.5)
  pts = expand.grid(x,y)
  colnames(pts) = c("x","y")
  
  # Find out how many conditions there are
  nConds = length(all_conditions[,1])
  
  # Empty list to fill with plots for arrange
  plotList = list()
  
  # Loop through each condition, creating a separate heat map for each
  for (condId in 1:nConds){
    # Get condition data (block, conditions, clue number)
    b <- all_conditions[condId,"targetBlocks"]
    condition <- all_conditions[condId,"conditions"]
    clueNum <- all_conditions[condId,"clues"]
    
    # Filter data based on those conditions
    data <- d %>%
      filter(cond == condition & clue == clueNum & block == b)
    
    # Find out how many participant responses there are
    nResp <- length(data[,1])
    
    # Empty data frame to fill with the points contained with a participant response
    ptsIn <- NULL
    
    # Loop through each participant response, seeing which grid cells/points were contained within each response
    for (j in 1:nResp) {
      rect <- c(data[j,"x1"], data[j,"y1"], data[j,"x2"], data[j,"y2"])
      isIn <- applyIsInRect(pts, rect)
      ptsIn <- cbind(ptsIn, isIn)
    }
    # Calculate how many times a point was contained within a given response
    sums <- rowSums(ptsIn)
    
    # Convert to probability
    probs <- sums/sum(sums)
    
    # Combine into a single data frame
    ptProbs  <- cbind(pts, probs) 
    
    # Save data
    if (experiment == "sim") {
    save(ptProbs, file = here(paste0("experiment-scenarios/heatmap/data/derived/point-probs/pp-",condition,"-b-",b,"-c-",clueNum,".Rdata")))
    } else {
      save(ptProbs, file = here(paste0("experiment-",experiment,"/data/derived/point-probs/pp-",condition,"-b-",b,"-c-",clueNum,".Rdata")))
      
    }
    
    # Load clues pertaining to condition
    load(here(paste0("experiment-scenarios/target-blocks/data/target-block-",b,"-Cartesian.Rdata")))
    
    # Rename columns for plot legend
    ptProbs <- rename(ptProbs, Probability = probs)
    
    # Get observations pertaining to condtition
    obs <- targetBlock$observations[1:clueNum,]
    
    if(condition == "HS"){
      fullCond <- "Helpful, Cover Story"
    } else if (condition == "HN") {
      fullCond <- "Helpful, No Cover Story"
    } else if(condition == "MS"){
      fullCond <- "Misleading, Cover Story"
    } else if (condition == "MN") {
      fullCond <- "Misleading, No Cover Story"
    } else if(condition == "US"){
      fullCond <- "Uninformative, Cover Story"
    } else if (condition == "UN") {
      fullCond <- "Uninformative, No Cover Story"
    } else if(condition == "RS"){
      fullCond <- "Random, Cover Story"
    } else if (condition == "RN") {
      fullCond <- "Helpful, No Cover Story"}
    
    #st = fullCond
    
    #colourScale <- c("white","lightpink", "hotpink","lightblue","blue","navyblue")
    colourScale <- c("white","hotpink", "navy")
    #colourScale <- c("navy","hotpink")
    
    
    # Plot heat map for that condition
    heatMap <- ptProbs %>% ggplot() +
      geom_raster(aes(x = ptProbs[,1], y = ptProbs[,2], fill = Probability))+
      scale_fill_gradientn(colours = colourScale)+
      geom_rect(aes(xmin = data[1,"ground_truth_x1"], ymin = data[1,"ground_truth_y1"], xmax = data[1,"ground_truth_x2"], ymax = data[1,"ground_truth_y2"]), alpha = 0, colour = "yellow", linetype = 4, linewidth = 0.2)+
      geom_point(data = obs, aes(x = x, y = y, colour = category), size = 7)+
      scale_colour_manual(values = c("positive" = "green", "negative" = "red"))+
      #{if ( clueNum == 1)labs(subtitle = st)} +
      guides(color = FALSE)+
      theme_void()+
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            text = element_text(size = 4),
            plot.margin = margin(0, 0, 0, 0, "cm"),
            legend.position = "none")+
      labs(x = "", y = "")
    if (experiment == "sim"){
      ggsave(filename = here(paste0("experiment-scenarios/heatmap/plots/heatmap-",condition,"-b-",b,"-c-",clueNum,".png")), width = 5, height = 5)
      
    }
    ggsave(filename = here(paste0("experiment-",experiment,"/modelling/05_plots/heatmap-",condition,"-b-",b,"-c-",clueNum,".png")), width = 9, height = 6)
    
    # Save plot to list 
    plotList[[condId]] <- heatMap
    # track progress in console
    print(condId)
  }
  # Create and save plot 
  #plot <- ggarrange(plotlist = plotList, common.legend = TRUE, ncol = 4, nrow = 8, widths = c(1,1, 1,1), heights = c(2,2,2,2), legend = c("bottom","left"))
  #ggsave(plot = plot, filename = here(paste0("experiment-",experiment,"/modelling/05_plots/heatmap-all-b",b,".png")))
  #plot
}
