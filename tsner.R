
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
  "tsner_fit" %in% class(x)
}


tsner <- function(x, ...){
  if(!is.tsner_matrix(x)){
    stop("\nObject to tsner is not a valid tsner_matrix\nPlease use select_split to produce a tsner_matrix\n")
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

plotr.foo <- function(g, x, ...){
  y <- as.matrix(x[, g])
  x %>% mutate(att = y,
               taxa = g)
        
  
}


plotr <- function(x, ...){
  if(!is.tsner_fit(x)){
    stop("\nObject to plotr is not a valid tsner_fit\nPlease use tsner to embed\n")
  }
  j <- attributes(x)$cols
  k <- lapply(j, function(z) plotr.foo(z, data.frame(x)))
  k <- bind_rows(k)
  class(k) <- c("data.frame", "tbl", "tbl_df", "tsner_plot_layers")
  k
}


order_taxa <- function(x, ...){
  y <- reshape2::melt(x[, c(unique(x$taxa), "y1")], id.vars = c("y1")) %>%
    mutate(z = y1*value) %>%
    group_by(variable) %>%
    summarise(z.m = median(z)) %>%
    arrange(z.m)
  x %>% mutate(taxa = factor(taxa, y$variable))
}

x <- reshape2::melt(data.frame(p), id.vars = c("group", "region", "id", "y1", "y2")) %>%
  mutate(z = y1*value) %>%
  group_by(variable) %>%
  summarise(z.m = mean(z)) %>%
  arrange(z.m)



library(Rtsne)
set.seed(123)
d <- expand.grid(metal = c(-0.5,-0.4,.5,1),
                 region = c(-1.1,-1,0,1.5,2.2),
                 id = c("a", "b", "c", "d", "e", "f", "g","h","i","j","k", "aa", "bb", "cc", "dd", "ee", "ff", "gg","hh","ii","jj","kk"))
d$group <- c(rep(0, 11*20), rep(1, 11*20))

d$conc <- 0.3*d$metal -.9*d$region + -0.2*d$group + 1.2*d$metal*d$region - 2.2*d$metal*d$group + rnorm(nrow(d), 3, 1)

d <- d %>% mutate(ranef = rep(rnorm(22, 0, 2), each = 20),
                  conc = conc + ranef)

# did we recover the simulated parameters?
m1.lin <- lmer(conc ~ metal*region+ metal*group + (1|id), d)
summary(m1.lin)

d <- d %>%
  mutate(metal = factor(metal, labels = c("Al", "Zn", "Fe", "Rb")),
         region = factor(region, labels = c("Sciatic", "Spinal_cord", "Cerebellum", "Hippocampus", "Cortex")))

ggplot(d, aes(x = metal, y = conc, colour = region)) +
  geom_boxplot() +
  facet_wrap(~group)

m2.lin <- lmer(conc ~ metal*region*group + (1|id), d)

d <- d %>%
  mutate(f = rep(ranef(m1.lin)$id[,1], each = 20),
         fconc = conc - f)

set.seed(321)
p <- select(d, group, metal, region, fconc, id) %>% 
  tidyr::spread(metal, fconc) %>%
  group_by(region) %>%
  mutate(Al = scale(Al),
         Rb = scale(Rb),
         Zn = scale(Zn),
         Fe = scale(Fe)) %>%
  ungroup() %>%
  select_split(Al, Zn, Fe, Rb) %>%
  tsner(perplexity = 10, max_iter = 2000) %>%
  plotr() %>%
  order_taxa() %>%
  ggplot(aes(x = y1, y = y2, colour = factor(group), size = att, alpha = att, frame = taxa)) +
  geom_point() +
  theme_minimal()
  
plotly::ggplotly(p)


