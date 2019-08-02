setwd("d:/Git/DataScienceWithR")

df <- mtcars
mean_mpg <- mean(df$mpg)
descr_df <- describe(df[, -c(8, 9)])
boxplot <- ggplot(df, aes(x = factor(am), y = disp))+
    geom_boxplot()+
    xlab("Transmission")+
    ylab("Displacement (cu.in.)")+
    ggtitle(("Box   plot"))

write.csv(df, "df.csv")
write.csv(descr_df, "descr_df.csv")

my_mean <- mean(10^6:10^9)
save(my_mean, file = "my_mean.RData")
