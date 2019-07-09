
select_split <- function(x, ...){
  y <- enquos(...)
  a <- x %>% select(!!!y) 
  class(a) <- c("tsner_matrix", "data.frame", "tbl", "tbl_df")
  b <- x %>% select(-c(!!!y)) 
  class(b) <- c("tsner_grouping", "data.frame", "tbl", "tbl_df")
  list(a, b)
}

is.tsner_matrix <- function(x){
  "tsner_matrix" %in% class(x[[1]])
}

is.tsner_fit <- function(x){
  "tsner_fit" %in% class(x[[1]])
}


tsner <- function(x, ...){
  if(!is.tsner_matrix(x)){
    simpleError("\nObject to tsner is not a valid tsner_matrix\nPlease use select_split to produce a tsner_matrix\n")
  }
  m <- Rtsne(x[[1]], ...)
  ys <- data.frame(m$Y)
  names(ys) <- paste0("y", 1:ncol(ys))
  y <- bind_cols(x[[2]], x[[1]], ys)
  attributes(y) <- list(names = names(y),
                        perplexity = m$perplexity,
                        N = m$N,
                        min_kl = m$itercosts[length(m$itercosts)],
                        max_iter = m$max_iter,
                        theta = m$theta,
                        origD = m$origD,
                        eta = m$eta,
                        cols = names(x[[1]]))
  class(y) <- c("tsner_fit", "list")
  y
}

foo <- function(g, x){
  y <- as.matrix(x[, g])
  x %>% mutate(att = y,
               taxa = g)
  
}


plotr <- function(x, ...){
  if(!is.tsner_fit(x)){
    simpleError("\nObject to plotr is not a valid tsner_fit\nPlease use tsner to embed\n")
  }
  j <- attributes(x)$cols
  k <- lapply(j, function(z) foo(z, data.frame(x)))
  k <- bind_rows(k)
  class(k) <- c("data.frame", "tbl", "tbl_df", "tsner_plot_layers")
  k
}



k <- A %>% select_split(Zn, Fe, Al, Rb) %>%
  tsner(perplexity = 9, max_iter = 2000) %>%
  plotr() %>%
  ggplot(aes(x = y1, y = y2, colour = region, size = att, alpha = att/2, frame = taxa)) +
  geom_point() +
  geom_point(size = 0.5, alpha = 0.8)

plotly::ggplotly(k)
