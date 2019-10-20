# From https://rpubs.com/dav1d00/ggpareto

ggpareto <- function(x) {
  
  title <- deparse(substitute(x))
  
  x <- data.frame(modality = na.omit(x))
  
  library(dplyr)
  
  Df <- x %>% group_by(modality) %>% summarise(frequency=n()) %>% 
    arrange(desc(frequency))
  
  Df$modality <- ordered(Df$modality, levels = unlist(Df$modality, use.names = F))
  
  Df <- Df %>% mutate(modality_int = as.integer(modality), 
                      cumfreq = cumsum(frequency), cumperc = cumfreq/nrow(x) * 100)
  nr <- nrow(Df)
  N <- sum(Df$frequency)
  
  Df_ticks <- data.frame(xtick0 = rep(nr +.55, 11), xtick1 = rep(nr +.59, 11), 
                         ytick = seq(0, N, N/10))
  
  y2 <- c("  0%", " 10%", " 20%", " 30%", " 40%", " 50%", " 60%", " 70%", " 80%", " 90%", "100%")
  
  library(ggplot2)
  
  g <- ggplot(Df, aes(x=modality, y=frequency)) + 
    geom_bar(stat="identity", aes(fill = modality_int)) +
    geom_line(aes(x=modality_int, y = cumfreq, color = modality_int)) +
    geom_point(aes(x=modality_int, y = cumfreq, color = modality_int), pch = 19) +
    scale_y_continuous(breaks=seq(0, N, N/10), limits=c(-.02 * N, N * 1.02)) + 
    scale_x_discrete(breaks = Df$modality) +
    guides(fill = FALSE, color = FALSE) + 
    annotate("rect", xmin = nr + .55, xmax = nr + 1, 
             ymin = -.02 * N, ymax = N * 1.02, fill = "white") +
    annotate("text", x = nr + .8, y = seq(0, N, N/10), label = y2, size = 3.5) +
    geom_segment(x = nr + .55, xend = nr + .55, y = -.02 * N, yend = N * 1.02, color = "grey50") +
    geom_segment(data = Df_ticks, aes(x = xtick0, y = ytick, xend = xtick1, yend = ytick)) +
    labs(title = paste0("Pareto Chart of ", title), y = "absolute frequency") +
    theme_bw()
  
  return(graph = g)
  #return(list(graph = g, Df = Df[, c(3, 1, 2, 4, 5)]))
}
