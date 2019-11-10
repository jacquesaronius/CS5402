library(ggplot2)
d = read.csv('5402_dataset.csv')
df = data.frame(d)
df$idu <- as.numeric(row.names(df))
df2 = df[1:30,]
ggplot(mapping = aes(x = idu, y = AIT202), data = df2) + geom_line()
