vizPCA <- function(df, v, rv) {
  if (!requireNamespace("ggalt", "tidyverse"))
    install.packages("tidyverse", "ggalt")
  library(tidyverse)
  library(ggalt)
  theme_set(theme_classic())
  var <- df %>% select(all_of(v))
  feature <- df[[substitute(rv)]]
  pca <- prcomp(var)
  var_pc <- data.frame(pca$x, feature = feature)
  var_pcs <- vector("list", length(var))
  p <-
    ggplot(var_pc, aes(x = PC1, y = PC2, col = feature)) +
    geom_point(aes(shape = feature), size = 2) +   # draw points
    labs(title = "Clustering",
         subtitle = "With principal components PC1 and PC2 as X and Y axis") +
    # change axis limits ----
  coord_cartesian(xlim = 1.2 * c(min(var_pc$PC1), max(var_pc$PC1)),
                  ylim = 1.2 * c(min(var_pc$PC2), max(var_pc$PC2)))
  for (i in 1:length(var)) {
    var_pcs[[i]] <-
      var_pc[var_pc$feature == unique(var_pc$feature)[i],]
    p <-
      p + geom_encircle(data = var_pcs[[i]],
                        aes(x = PC1, y = PC2))
  }
  return(p)
}

vizPCA(iris, 1:4, Species)