library(lme4)
library(lmerTest)
library(Rtsne)
set.seed(42)
d <- expand.grid(metal = c(-0.5,0,.5,1),
                 region = c(-1.1,-1,0,1.5,2.2),
                 id = c("a", "b", "c", "d", "e", "f", "g","h","i","j","k", "aa", "bb", "cc", "dd", "ee", "ff", "gg","hh","ii","jj","kk"))
d$group <- c(rep(0, 11*20), rep(1, 11*20))

d$conc <- 0.3*d$metal -.9*d$region + -0.2*d$group + 1.2*d$metal*d$region - 2.2*d$metal*d$group + rnorm(nrow(d), 3, 1)

d <- d %>% mutate(ranef = rep(rnorm(22, 0, 2), each = 20),
                  conc = conc + ranef)


ggplot(d, aes(x = factor(metal), y = conc, colour = factor(region))) +
  geom_boxplot() +
  facet_wrap(~group)


m1.lin <- lmer(conc ~ metal*region+ metal*group + (1|id), d)
summary(m1.lin)

d <- d %>%
  mutate(metal = factor(metal, labels = c("Al", "Zn", "Fe", "Rb")),
         region = factor(region, labels = c("Sciatic", "Spinal_cord", "Cerebellum", "Hippocampus", "Cortex")))
m2.lin <- lmer(conc ~ metal*region*group + (1|id), d)

A <- select(d, group, metal, region, conc, id) %>% 
  tidyr::spread(metal, conc)


set.seed(888)
m1 <- Rtsne(A[,4:7], dim = 2,
            perplexity = 7,
            max_iter = 10000, 
            theta = 0.05)

# make a data frame that assigns an attribute one 
# column/taxa at a time
foo <- function(x){
  g <- as.matrix(A[, x])
  A %>% mutate(att = g,
               Metal = x,
               y1 = m1$Y[, 1],
               y2 = m1$Y[, 2])
  
}
metals <- c("Al", "Zn", "Fe", "Rb")
k <- lapply(metals, foo)
k <- dplyr::bind_rows(k)
k$Metal <- factor(k$Metal, levels = metals)

p1 <- ggplot(k, aes(x = y1, y = y2, colour = region, shape = factor(group), size = att, alpha = att/2, frame = Metal, scale = 1.5)) +
  geom_point() +
  geom_point(size = .5, alpha = .8, show.legend = FALSE) +
  theme_minimal()

#plotly::plotly_json(p1)

plotly::ggplotly(p1)


d <- d %>%
  mutate(f = rep(ranef(m1.lin)$id[,1], each = 20),
         fconc = conc - f)


A <- select(d, group, metal, region, fconc, id) %>% 
  tidyr::spread(metal, fconc)


set.seed(888)
m1 <- Rtsne(A[,4:7], dim = 2,
            perplexity = 7,
            max_iter = 10000, 
            theta = 0.05)


k <- lapply(metals, foo)
k <- dplyr::bind_rows(k)
k$Metal <- factor(k$Metal, levels = metals)

p2 <- ggplot(k, aes(x = y1, y = y2, colour = region, size = att, shape = factor(group), alpha = att/2, frame = Metal, scale = 1.5)) +
  geom_point() +
  geom_point(size = .5, alpha = .8, show.legend = FALSE) +
  theme_minimal()

#plotly::plotly_json(p1)

plotly::ggplotly(p2)
plotly::ggplotly(p1)

superwide <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}


A <- select(d, group,  metal, region, conc, id) %>% 
  spread(metal, conc) %>%
  superwide(region, c(Al, Zn, Fe, Rb))

set.seed(888)
m1 <- Rtsne(A[,-c(1,2)], dim = 2,
            perplexity = 7,
            max_iter = 10000, 
            theta = 0.05)

metals_regions <- names(A[,-c(1,2)])
k <- lapply(metals_regions, foo)
k <- dplyr::bind_rows(k)
k$Metal <- factor(k$Metal, levels = metals_regions)

p3 <- ggplot(k, aes(x = y1, y = y2, colour = factor(group), size = att, alpha = att/2, frame = Metal, scale = 1.5)) +
  geom_point() +
  geom_point(size = .5, alpha = .8, show.legend = FALSE) +
  theme_minimal()

#plotly::plotly_json(p1)

plotly::ggplotly(p3)


A <- select(d, group,  metal, region, conc, id) %>% 
  tidyr::spread(metal, conc)  %>%
  group_by(region) %>%
  mutate(Al = scale(Al),
         Zn = scale(Zn),
         Fe = scale(Fe),
         Rb = scale(Rb)) %>%
  ungroup()

set.seed(888)
m1 <- Rtsne(A[,-c(1,2)], dim = 2,
            perplexity = 7,
            max_iter = 10000, 
            theta = 0.05)

k <- lapply(metals, foo)
k <- dplyr::bind_rows(k)
k$Metal <- factor(k$Metal, levels = metals)

p4 <- ggplot(k, aes(x = y1, y = y2, colour = region, shape = factor(group), size = att, alpha = att/2, frame = Metal, scale = 1.5)) +
  geom_point() +
  geom_point(size = .5, alpha = .8, show.legend = FALSE) +
  theme_minimal()

#plotly::plotly_json(p1)

plotly::ggplotly(p4)

p4.1 <- ggplot(k, aes(x = y1, y = y2, colour = id, shape = factor(group), size = att, alpha = att/2, frame = Metal, scale = 1.5)) +
  geom_point() +
  geom_point(size = .5, alpha = .8, show.legend = FALSE) +
  theme_minimal()

#plotly::plotly_json(p1)

plotly::ggplotly(p4.1)

A <- select(d, group,  metal, region, fconc, id) %>% 
  tidyr::spread(metal, fconc)  %>%
  group_by(region) %>%
  mutate(Al = scale(Al),
         Zn = scale(Zn),
         Fe = scale(Fe),
         Rb = scale(Rb)) %>%
  ungroup()

set.seed(888)
m1 <- Rtsne(A[,-c(1,2)], dim = 2,
            perplexity = 7,
            max_iter = 10000, 
            theta = 0.05)

k <- lapply(metals, foo)
k <- dplyr::bind_rows(k)
k$Metal <- factor(k$Metal, levels = metals)

p5 <- ggplot(k, aes(x = y1, y = y2, colour = region, shape = factor(group), size = att, alpha = att/2, frame = Metal, scale = 1.5)) +
  geom_point() +
  geom_point(size = .5, alpha = .8, show.legend = FALSE) +
  theme_minimal()

#plotly::plotly_json(p1)

plotly::ggplotly(p5)

