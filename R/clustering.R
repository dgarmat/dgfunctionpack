


#' Clustering Screeplot to help determine k
#'
#' @param df 
#' @param nc 
#' @param seed 
#' @param nstart 
#'
#' @return
#' @export
#' @import ggplot2
#'
#' @examples
#' wssplot(iris[1:4])
wssplot <- function(df, nc = 15, seed = 1234, nstart = 1){
  wss <- (nrow(df) - 1) * sum(apply(df, 2, var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(df, centers = i, nstart = nstart)$withinss)}
  qplot(x = 1:nc, y = wss,  xlab="Number of Clusters",
        ylab="Within groups sum of squares") + geom_line() +
    theme(axis.title.y = element_text(size = rel(.8), angle = 90)) +
    theme(axis.title.x = element_text(size = rel(.8), angle = 00)) +
    theme(axis.text.x = element_text(size = rel(.8))) +
    theme(axis.text.y = element_text(size = rel(.8)))
}


