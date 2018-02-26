#plotting all the constrains
p1 = ggplot(df, aes(x= DF.L3.C1, y = DF.L3.C2)) + geom_point(aes(col=label))
p2 = ggplot(df, aes(x= DF.L3.C1, y = DF.L3.C3)) + geom_jitter(aes(col=label))
p3 = ggplot(df, aes(x= DF.L3.C1, y = DF.L3.C4)) + geom_point(aes(col=label))
p4 = ggplot(df, aes(x= DF.L3.C2, y = DF.L3.C1)) + geom_jitter(aes(col=label))
p5 = ggplot(df, aes(x= DF.L3.C2, y = DF.L3.C3)) + geom_jitter(aes(col=label))
p6 = ggplot(df, aes(x= DF.L3.C2, y = DF.L3.C4)) + geom_jitter(aes(col=label))
p7 = ggplot(df, aes(x= DF.L3.C3, y = DF.L3.C1)) + geom_jitter(aes(col=label))
p8 = ggplot(df, aes(x= DF.L3.C3, y = DF.L3.C2)) + geom_jitter(aes(col=label))
p9 = ggplot(df, aes(x= DF.L3.C3, y = DF.L3.C4)) + geom_jitter(aes(col=label))

plot_grid(p1, p2, p3, p4,p5, p6, p7, p8,p9,
          labels = c("C1,2", "C1,3", "C1,4","C2,1", "C2,3", "C2,4","C3,1", "C3,2", "C3,4"),
          ncol = 3, nrow = 3)

h2o.shutdown()

