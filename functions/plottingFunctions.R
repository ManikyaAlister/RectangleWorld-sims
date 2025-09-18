library(dplyr)
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
                                     subtitle="Yellow rectangle is the true hypothesis",
                                     manual_scale = NULL){
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
                          fill=posterior),show.legend=FALSE) 
    
    #geom_text(data=allPts, mapping=aes(x=x, y=y, label=abs(p)),
    #          show.legend=FALSE) +
    if (is.null(manual_scale)) {
      
      pRect <- pRect + 
        scale_fill_gradient2(low="red",mid="black",high="green")
      
    } else {
      pRect <- pRect + 
        scale_fill_gradient2(
          limits = manual_scale,
          low = "red",
          mid = "black",   
          high = "green"   
        )
    }
      

    
  
  pRect <- pRect +
    geom_rect(data=trueR, mapping=aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2), color="yellow",
              fill=NA,linetype="dashed")+
    
  
  
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
        geom_point(data=obs, mapping=aes(x=x, y=y), color="white", shape=4, size=6,
                   show.legend=FALSE)        
    } else {
      pRect <- pRect +
        geom_point(data=obs, mapping=aes(x=x, y=y), color="white", shape=19, size=6,
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
                            subtitle="Yellow rectangle is the true hypothesis",
                            facet = FALSE,
                            manual_scale = NULL,
                            title_size = 40,
                            legend = FALSE){
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
  
  if (facet){
    # Normalize posterior within each facet
    allPts <- allPts %>%
      group_by(cover_story, condition) %>%
      mutate(posterior = (posterior - min(posterior)) / (max(posterior) - min(posterior))) %>%
      ungroup()
  }
  
  pRect <- ggplot() +
    theme_bw()+
    xlim(xlow,xhigh) +
    ylim(ylow,yhigh) +  
    theme(panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = "black"), 
          panel.grid.minor = element_line(size = 0.05, linetype = 'solid',
                                          colour = "black"),
          panel.background = element_rect(fill = "black", colour = "black",
                                          size = 2, linetype = "solid"),
          strip.background = element_rect(fill= "white"),
          axis.text = element_blank(),  # Remove axis text
          axis.ticks = element_blank(), # remove ticks
          plot.title = element_text(size = title_size),
          strip.text = element_text(size = title_size)
    
    ) +
  
      

    labs(title=title, subtitle=subtitle)
  
  if (!is.null(manual_scale)) {
    pRect <- pRect + 
      scale_fill_gradient(
        limits = manual_scale,
        low = "black",   # or whatever color you want at 0
        high = "lightblue"    # or whatever color you want at 1
      )
  }
  
  
    if (whichDist=="prior") {
      pRect <- pRect + 
        geom_rect(data=allPts, 
                  mapping=aes(xmin=x-0.5, xmax=x+0.5, ymin=y-0.5, ymax=y+0.5, 
                              fill=prior),show.legend=FALSE)       
    } else {
      pRect <- pRect + 
        geom_rect(data=allPts, 
                  mapping=aes(xmin=x-0.5, xmax=x+0.5, ymin=y-0.5, ymax=y+0.5, 
                              fill=posterior))  
    }
  
  # if legend == false (default), remove legend
  if (!legend) {
    pRect <- pRect + 
      theme(legend.position = "none")
  } else {
    pRect <- pRect + 
      theme(legend.title = element_text(size = title_size-3),
            legend.text = element_text(size = title_size-8),
            legend.key.width = unit(2.5, 'cm'),
            legend.key.height = unit(1.5,'cm'))+
      labs(fill = "Probability  ")
    
  }


    
    pRect <- pRect +
      geom_rect(data=trueR, mapping=aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2), 
                color="yellow", fill=NA,linetype="dashed", size = 2)+
      theme(axis.title = element_blank())

    if (facet) {
      pRect <- pRect + 
        facet_grid(cover_story ~ condition) 
    }
    
  # add in observations if they exist
  if (!is.null(nrow(obs))) {
    nPos <- sum(obs$category=="positive")
    nNeg <- sum(obs$category=="negative")
    
    if (nPos > 0 & nNeg > 0) {
      pRect <- pRect +
        geom_point(data=obs, mapping=aes(x=x, y=y, color=category, shape=category), size=12,
                   show.legend=FALSE) +
         scale_shape_manual(values=c(15,15)) + # to make them crosses and dots
        scale_color_manual(values=c("#FF0000FF","#00A600"))     
      
    } else if (nNeg > 0) {
      pRect <- pRect +
        geom_point(data=obs, mapping=aes(x=x, y=y), color="#FF0000FF", shape=15, size=12,
                   show.legend=FALSE)        
    } else {
      pRect <- pRect +
        geom_point(data=obs, mapping=aes(x=x, y=y), color="#00A600", shape=15, size=12,
                   show.legend=FALSE)        
    }
  }
  
  return(pRect)
}

# ********************************
#     plotHypothesisHistogram
# ********************************
#' @title plotHypothesisHistogram
#' @description Makes a bar plot with hypothesis sizes where the y axis is
#' the probability, the x axis is the size, and it indicates how many exist
#' @param hyps hypotheses to be plotted
#' @param title character string for a title for the graph (default:"Hypothesis distribution")
#' @param subtitle character string for a title for the graph (default:"Darker bars have more hypotheses")

plotHypothesisHistogram = function(hyps, title="Hypothesis distribution", 
                                   subtitle="Darker bars have more hypotheses"){
  
  d <- hyps %>%
    filter(posterior>0) %>%
    mutate(sz = as.character(size)) %>%
    group_by(sz) %>%
    summarise(post = mean(posterior), sd=sd(posterior), n=n()) %>%
    ungroup() %>%
    mutate(size = as.numeric(sz))
  
  d$sz <- reorder(d$sz,d$size)
  m <- max(d$post)
  
  pHDist <- d %>% 
    #mutate(label = paste0("n=",n)) %>%
    ggplot(mapping=aes(x=sz,y=post,fill=n)) + 
    geom_col(show.legend=FALSE,color="black") + 
    geom_text(aes(y=0.03*m,label=n),vjust=0,size=3) + 
    theme_bw() + 
    theme(axis.ticks.y=element_blank(),
          axis.text.y=element_blank()) +
    scale_fill_distiller(palette="Purples",direction=-1) +
    #scale_x_continuous(breaks=d$sz) +
    #scale_x_discrete()
    labs(title=title, subtitle=subtitle,
         x="Hypothesis size",
         y="Probability")
  
  return(pHDist)
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
  colnames(d) <- c("x1", "y1", "x2", "y2") # rename columns because if r is a data frame it wont rename. 
  
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
# same as above but clues numbered
plotHypothesesTeach = function(r,obs=NA, xrange = 0:10,yrange=0:10,
                               title="Best hypotheses", 
                               subtitle="Solid line is true, dashed are best"){
  jitsize <- 0.05
  xlow <- min(xrange)
  xhigh <- max(xrange)
  ylow <- min(yrange)
  yhigh <- max(yrange)
  d <- data.frame(x1=r[1],y1=r[2],x2=r[3],y2=r[4])
  colnames(d) <- c("x1", "y1", "x2", "y2") # rename columns because if r is a data frame it wont rename. 
  
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
    obs$clue_num = 1:length(obs$x)
    pRect <- pRect + 
      geom_text(data = obs, aes(x = x, y = y, label = clue_num))
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

plotPosteriors = function(p_data, statistic, man_check = FALSE, recovery_data, title = "", subtitle = ""){
  plot <- p_data %>% 
    mutate(alpha = as.factor(alpha)) %>% 
    ggplot()+
    geom_col(aes(x = alpha, y = eval(parse(text = statistic)), fill = alpha)) +
    geom_line(data = recovery_data, aes(x = as.factor(alpha), y = eval(parse(text = statistic))), group = 1) +
    theme_classic()+
    scale_fill_brewer(palette = "RdYlGn")+
    labs(x = "", y = "", title = title, subtitle = subtitle)+
    theme(
          #axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          text = element_text(size = 18),
          axis.text = element_text(size = 22),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          axis.text.x = element_text(angle = 45, vjust = .5),
          legend.position = "none",
          
          )
  plot
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
sizeHistModel = function(data, condition, plotAlpha, ylim = 95, dif_priors = FALSE){
  
  data <- data %>%
    mutate(cover_cond = case_when(
      cond == "HS" | cond == "HN" ~ "helpful",
      cond == "MS" | cond == "MN" ~ "misleading", 
      cond == "UN" | cond == "US" ~ "uninformative",
      cond == "RS" | cond == "RN" ~ "random"
    ))
  
  plot  <- data %>%
    filter(cond == condition & cover_cond == plotAlpha) %>%
    ggplot(aes(x = factor(size))) +
    geom_col(aes(y = Percent, fill = cond)) +
    scale_fill_manual(values = c("HS" = "darkgreen", "HN" = "darkgreen", "RS" = "skyblue3", "RN" = "skyblue3", "MS" = "darkred", "MN" = "darkred", "UN" = "orange", "US" = "orange"))+
    theme_classic()+
    ylim(c(0,ylim))+
    theme(axis.text.x=element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x=element_blank(),
          text = element_text(size = 25),
          axis.text = element_text(size = 22),
          strip.text = element_text(margin = margin(0,0,0,0, "cm"), size = 5),
          legend.key.width = unit(5, "cm")
          #legend.position = "none"
          )

  if (dif_priors){
    
    if (condition == "HS" ){
      title = "Helpful"
    } else if (condition == "MS") {
      title = "Misleading Naive"
    } else if (condition == "US") {
      title = "Misleading Aware"
    } else if (condition == "RS"){
      title = "Random"
    }
    
    data <- data %>%
      mutate(prior_type = factor(prior_type, levels = c("flat", "empirical"))) 
     
    plot <- plot + 
      geom_line(aes(y = prob, colour = prior_type, group = prior_type, linetype = prior_type), size = 4.5) +
      scale_linetype_manual(values = c("empirical" = "solid", "flat" = "longdash")) +
      scale_colour_manual(values = c("empirical" = "purple", "flat" = "darkgrey")) + 
      labs(title = title) +
      guides(
        linetype = guide_legend(title = "Prior Type"),
        fill = "none",
        colour = guide_legend(title = "Prior Type")
      )
      
  } else {
    plot <- plot +geom_line(aes(y = prob, group = factor(cover_cond)), size = 0.8, colour = "grey28") # get on same scale

  }
  
  plot
  
  
}

sizeDensModel = function(data, condition, plotAlpha, prob_constant = 250, ylim = 95){
  
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
    geom_density(aes(x = size, fill = cond, group = cond)) +
    #geom_line(aes(y = prob*prob_constant, colour = factor(cover_cond), group = factor(cover_cond)), size = 0.8, colour = "grey28")+ # get on same scale
    scale_fill_manual(values = c("HS" = "darkgreen", "HN" = "darkgreen", "RS" = "lightblue", "RN" = "lightblue", "MS" = "darkred", "MN" = "darkred", "UN" = "orange", "US" = "orange"))+
    labs(y = "Count")+
    theme_classic()+
   # ylim(c(0,ylim))+
    theme(axis.text.x=element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x=element_blank(),
          text = element_text(size = 12),
          strip.text = element_text(margin = margin(0,0,0,0, "cm"), size = 5),
          legend.position = "none")
}




plotHeatMaps = function(all_conditions, experiment, target_blocks = c(2,8), zeroA = 6, H = 10, save = TRUE, file_label = "", filtered = FALSE, facet = TRUE){
  # get load experiment obs function 
  source(here("functions/genericFunctions.R"))
  # get update points function 
  source(here("functions/calculatingFunctions.R"))
  
  nConds <- length(all_conditions[,1])
  # upload pre-calculated positive point probabilities for an alpha of zero (just for plotting clarity)
  # Load the pre-calculated data if not loaded already 
  if(!exists("xrange")){
    fileSeg <- paste0("x0to", H, "y0to", H)
    fn <- paste0("datafiles/", fileSeg, ".RData")
    load(here(fn)) 
  }
  
  plot_list <- NULL
  # Loop through each condition, creating a separate heat map for each
  for (condId in 1:nConds){
    # Get condition data (block, conditions, clue number)
    b <- all_conditions[condId,"blocks"]
    condition <- all_conditions[condId,"conditions"]
    clueNum <- all_conditions[condId,"clues"]
    
    # Load data
    if (experiment == "sim") {
      load(here(paste0("experiment-scenarios/heatmap/data/derived/hyp-probs/hp-",condition,"-b-",b,"-c-",clueNum,file_label,".Rdata")))
    } else {
      load(here(paste0("experiment-",experiment,"/data/derived/hyp-probs/hp-",condition,"-b-",b,"-c-",clueNum,file_label,".Rdata")))
      
    }
    
    
    
    
    # get the provider helpfulness in each condition
    if (condition == "HS" | condition == "HN") {
      provider <- "Helpful"
    } else if (condition == "RS" | condition == "RN") {
      provider <- "random"
    } else if (condition == "MS" | condition == "MN") {
      provider <- "misleading"
    } else if (condition == "US" | condition == "UN") {
      provider <- "uninformative"
      recursion = TRUE
    }
    
    # Load clues pertaining to condition
    
    obs <- loadExperimentObs(b, clueNum, target_blocks, provider)
    trueR <- loadExperimentTrueRect(b, clueNum, target_blocks, provider)
    
    # Rename columns for plot legend
    #colnames(ptProbs) <- c("x", "y", "posterior")
    
    
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
    
    #colourScale <- c("white","hotpink", "navy")
    
    # find the index of alpha = 0
    zeroA = which.min(abs(alphas))
    
    # make index column 
    pts$index = 1:nrow(pts)
    
    # get index of the observations 
    merged_df <- merge(obs, pts, by.x = c("x", "y"), by.y = c("x", "y"))
    obs$index = merged_df$index
    
    # make "selected" column (necessary for update points)
    #ptProbs$selected = FALSE
    #ptProbs$selected[obs$index] = TRUE
    
    t <- NULL
    # if not saving, plot title needs to be annotated
    if (!save){
      t <- paste0("block ", b, " ", condition)
    }
    # plot hypothesis heat map 
    tempPts <- updatePoints(posProbPts[,,zeroA],obs[clueNum,],
                            posterior=hyp$posterior,pts=pts)
    
    heatMap <- plotDistribution(allPts=tempPts,xrange=xrange,yrange=yrange,
                                obs=obs[1:clueNum,],whichDist="posterior", title = NULL, subtitle = t, trueRectangle = trueR)
              plot_list[[condId]] <- heatMap

    
    if (save) {
      if (experiment == "sim"){
        ggsave(filename = here(paste0("experiment-scenarios/heatmap/plots/heatmap-",condition,"-b-",b,"-c-",clueNum,".png")), width = 5, height = 5, plot = heatMap)
        
      } else {
        if (filtered) {
          ggsave(filename = here(paste0("experiment-",experiment,"/modelling/11.1_filtered-analyses-conservative/plots/heatmap-",condition,"-b-",b,"-c-",clueNum,".png")), width = 5, height = 5, plot = heatMap)
        } else {
          ggsave(filename = here(paste0("experiment-",experiment,"/modelling/05_plots/heatmap-",condition,"-b-",b,"-c-",clueNum,".png")), width = 5, height = 5, plot = heatMap)
          
        }
      }
      
    } else {
      # Save plot to list 
      plot_list[[condId]] <- heatMap
    }

    
    # track progress in console
    print(paste0(condId," out of ", nConds))
  }
  ggarrange(plotlist = plot_list, common.legend = TRUE)
  #ggsave(filename = here(paste0("experiment-",experiment,"/modelling/05_plots/heatmap-all-b-",b,".png")), width = 5, height = 5, plot = heatMap)
  
  
  }
  
plotHeatMapsLite = function(all_conditions, experiment, target_blocks = c(2,8), zeroA = 6, H = 10){
  # get load experiment obs function 
  source(here("functions/genericFunctions.R"))
  # get update points function 
  source(here("functions/calculatingFunctions.R"))
  
  nConds <- length(all_conditions[,1])
  # upload pre-calculated positive point probabilities for an alpha of zero (just for plotting clarity)
  # Load the pre-calculated data if not loaded already 
  if(!exists("xrange")){
    fileSeg <- paste0("x0to", H, "y0to", H)
    fn <- paste0("datafiles/", fileSeg, ".RData")
    load(here(fn)) 
  }
  
  # Loop through each condition, creating a separate heat map for each
  for (condId in 1:nConds){
    # Get condition data (block, conditions, clue number)
    b <- all_conditions[condId,"blocks"]
    condition <- all_conditions[condId,"conditions"]
    clueNum <- all_conditions[condId,"clues"]
    
    # Load data
    if (experiment == "sim") {
      load(here(paste0("experiment-scenarios/heatmap/data/derived/hyp-probs/hp-",condition,"-b-",b,"-c-",clueNum,".Rdata")))
    } else {
      load(here(paste0("experiment-",experiment,"/data/derived/hyp-probs/hp-",condition,"-b-",b,"-c-",clueNum,".Rdata")))
      
    }
    
    
    # get the provider helpfulness in each condition
    if (condition == "HS" | condition == "HN") {
      provider <- "helpful"
    } else if (condition == "RS" | condition == "RN") {
      provider <- "random"
    } else if (condition == "MS" | condition == "MN") {
      provider <- "misleading"
    } else if (condition == "US" | condition == "UN") {
      provider <- "uninformative"
      recursion = TRUE
    }
    
    # Load clues pertaining to condition
    
    obs <- loadExperimentObs(b, clueNum, target_blocks, provider)
    
    # Rename columns for plot legend
    #colnames(ptProbs) <- c("x", "y", "posterior")
    
    
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
    
    #colourScale <- c("white","hotpink", "navy")
    
    # find the index of alpha = 0
    zeroA = which.min(abs(alphas))
    
    # make index column 
    pts$index = 1:nrow(pts)
    
    # get index of the observations 
    merged_df <- merge(obs, pts, by.x = c("x", "y"), by.y = c("x", "y"))
    obs$index = merged_df$index
    
    # make "selected" column (necessary for update points)
    #ptProbs$selected = FALSE
    #ptProbs$selected[obs$index] = TRUE
    
    # plot hypothesis heat map 
    tempPts <- updatePoints(posProbPts[,,zeroA],obs[clueNum,],
                            posterior=hyp$posterior,pts=pts)
    
    heatMap <- plotDistribution(allPts=tempPts,xrange=xrange,yrange=yrange,
                                obs=obs[1:clueNum,],whichDist="posterior", title = NULL, subtitle = NULL)
    
    print(filtered)
    if (experiment == "sim"){
      ggsave(filename = here(paste0("experiment-scenarios/heatmap/plots/heatmap-",condition,"-b-",b,"-c-",clueNum,".png")), width = 5, height = 5, plot = heatMap)
      
    } else {
      if (filtered) {
        ggsave(filename = here(paste0("experiment-",experiment,"/modelling/11_filtered-analyses/plots/heatmap-",condition,"-b-",b,"-c-",clueNum,".png")), width = 9, height = 6, plot = heatMap)
      } else {
        ggsave(filename = here(paste0("experiment-",experiment,"/modelling/05_plots/heatmap-",condition,"-b-",b,"-c-",clueNum,".png")), width = 9, height = 6, plot = heatMap)
        
      }
    }
    
    # Save plot to list 
    
    # track progress in console
    print(paste0(condId," out of ", nConds))
  }}

  
plotHeatMapsFacet = function(all_conditions,
                             experiment,
                             clues = 4,
                             target_blocks = c(2, 8),
                             zeroA = 6,
                             H = 10,
                             save = TRUE,
                             file_label = "",
                             filtered = FALSE,
                             t = NULL,
                             legend = TRUE) {
  
  #clueNum = clues
  
  # get load experiment obs function
  source(here("functions/genericFunctions.R"))
  # get update points function
  source(here("functions/calculatingFunctions.R"))
  
  nConds <- length(all_conditions[, 1])
  # upload pre-calculated positive point probabilities for an alpha of zero (just for plotting clarity)
  # Load the pre-calculated data if not loaded already
  if (!exists("xrange")) {
    fileSeg <- paste0("x0to", H, "y0to", H)
    fn <- paste0("datafiles/", fileSeg, ".RData")
    load(here(fn))
  }
  
  tempPts_all <- NULL
  
  # Loop through each condition, creating a separate heat map for each
  for (clueNum in clues) {
    for (condId in 1:nConds) {
      tempPts <- NULL 
      hyp = NULL
      # Get condition data (block, conditions, clue number)
      b <- all_conditions[condId, "blocks"]
      condition <- all_conditions[condId, "conditions"]
      # Load data
      if (experiment == "sim") {
        load(here(
          paste0(
            "experiment-scenarios/heatmap/data/derived/hyp-probs/hp-",
            condition,
            "-b-",
            b,
            "-c-",
            clueNum,
            file_label,
            ".Rdata"
          )
        ))
      } else {
        load(here(
          paste0(
            "experiment-",
            experiment,
            "/data/derived/hyp-probs/hp-",
            condition,
            "-b-",
            b,
            "-c-",
            clueNum,
            file_label,
            ".Rdata"
          )
        ))
        
      }
      
      
      # Load hypothesis probabilities
      if (experiment == "sim") {
        load(here(paste0("experiment-scenarios/heatmap/data/derived/hyp-probs/hp-",condition,"-b-",b,"-c-",clueNum,file_label,".Rdata")))
      } else {
        load(here(paste0("experiment-",experiment,"/data/derived/hyp-probs/hp-",condition,"-b-",b,"-c-",clueNum,file_label,".Rdata")))
      }
      
      
      # get the provider helpfulness in each condition
      if (condition == "HS" | condition == "HN") {
        provider <- "Helpful"
      } else if (condition == "MS" | condition == "MN") {
        provider <- "Misleading\nNaive"
      } else if (condition == "US" | condition == "UN") {
        provider <- "Misleading\nAware"
        recursion = TRUE
      } else if (condition == "RS" | condition == "RN") {
        provider <- "Random"
      }
      
      # Load clues pertaining to condition
      
      obs <- loadExperimentObs(b, clueNum, target_blocks, provider)
      trueR <- loadExperimentTrueRect(b, clueNum, target_blocks, provider)
      
      #colourScale <- c("white","hotpink", "navy")
      
      # find the index of alpha = 0
      zeroA = which.min(abs(alphas))
      
      # make index column 
      pts$index = 1:nrow(pts)
      
      # get index of the observations 
      merged_df <- merge(obs, pts, by.x = c("x", "y"), by.y = c("x", "y"))
      obs$index = merged_df$index
      
      # make "selected" column (necessary for update points)
      #ptProbs$selected = FALSE
      #ptProbs$selected[obs$index] = TRUE
      
      #t <- NULL
      # if not saving, plot title needs to be annotated
      if (!save){
        t <- paste0("block ", b, " ", condition)
      }
      
      
      # plot hypothesis heat map 
      tempPts <- updatePoints(posProbPts[,,zeroA],obs[1:clueNum,],
                              posterior=hyp$posterior,pts=pts)
    
      
      tempPts <- tempPts %>%
        mutate(clue = clueNum, block = b, condition = provider)
      
      cover_story = ifelse(grepl("S", condition), "Cover Story", "No Cover Story")
      
      if (experiment == "sim") {
        tempPts  <- tempPts  %>%
          mutate(cover_story = clueNum) # want to facet by clue number instead of cover story for the simulation plot 
      } else {
        tempPts  <- tempPts  %>%
          mutate(cover_story = cover_story)
      }
      
      tempPts <- tempPts %>%
        mutate(condition = factor(
          condition,
          levels = c("Helpful", "Misleading\nNaive", "Misleading\nAware", "Random")
        ))
      
      tempPts_all <- rbind(tempPts_all, tempPts)
      
   }
  }

  
  heatMap <- plotDistribution(
    allPts = tempPts_all,
    xrange = xrange,
    yrange = yrange,
    obs = obs[1:clueNum, ],
    whichDist = "posterior",
    title = t,
    subtitle = NULL,
    trueRectangle = trueR,
    facet = TRUE,
    legend = legend
  )
  
  heatMap
}

  