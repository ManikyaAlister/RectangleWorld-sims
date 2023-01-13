source(here("getLearnerHypDistributions.R"))
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


plotPosteriorAlphas(a1n100o4pr25_posteriors, 1)

#' Plot the mean posterior probability across multiple learner guesses for a set of alphas
#'
#' @param posteriorsAlphas A data frame containing the posterior probability of a given rectangle
#' for multiple alphas. The data frame should contain three columns: alpha, posterior, and index. It can be 
#' generated from the function GetMultipleAlphaPosteriors(). 
#' @param generatingAlpha The "true" alpha that was used to generate the rectangles.
#'
#' @return ggplot object 
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
