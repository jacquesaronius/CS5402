library(ggplot2)
library(factoextra)
args = commandArgs(trailingOnly=TRUE)
d = read.csv(args[1])
cluster = kmeans(d, centers = 3, iter.max = 10, nstart = 1,
                 algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
                               "MacQueen"), trace=FALSE)
#ggplot(cluster) + geom_line()
fviz_cluster(cluster, data = d)