retweet_count <- function(tweets) {
  cascade <- tweets %>% mutate(retweet_count = 0) %>% arrange(tweet_date)
  if (nrow(cascade) > 1) {
    for (k in 1:nrow(cascade)) {
      idx <- which(cascade$parent_tid == cascade$tid[k])
      cascade$retweet_count[k] <- length(idx)
    }
  }
  return(cascade)
}

structural_virality <- function(tweets){
  cascade <- tweets %>% mutate(structural_virality = 1) %>% mutate(parent_tid_temp = replace(parent_tid, parent_tid == -1, 1))
  cascade <- cascade %>% arrange(tweet_date)
  if (nrow(cascade) > 1) {
    for (k in 2:nrow(cascade)) {
      subset <- cascade[1:k,]
      graph_df <- subset %>% dplyr::select(parent_tid, tid)
      g_df <- graph.data.frame(d = graph_df, directed = FALSE)
      dist <- mean_distance(g_df, directed = F)
      cascade$structural_virality[k] <- dist
    }
  } 
  return(cascade %>% dplyr::select(-parent_tid_temp))
}
