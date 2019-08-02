?mtcars
df <- mtcars
df$vs <- factor(df$vs, labels = c("V", "S"))
df$am <- factor(df$am, labels = c("Auto", "Manual"))

# Простейшие описательные статистики
median(df$mpg)
mean(df$disp)
sd(df$hp)
range(df$cyl)

mean(df$mpg[df$cyl == 6])

mean(df$mpg[df$cyl == 6 & df$vs == "V"])

sd(df$hp[df$cyl != 3 & df$am == "Auto"])

# ЗАДАЧА
#  Рассчитайте среднее значение времени разгона (qsec) 
# для автомобилей, число цилиндров (cyl) у которых 
# не равняется 3 и показатель количества миль на галлон 
# топлива (mpg) больше 20.
result <- mean(df$qsec[df$cyl != 3 & df$mpg > 20])

# Расчет статистик по группам
?aggregate

mean_hp_vs <- aggregate(x = df$hp, by = list(df$vs), FUN = mean)
colnames(mean_hp_vs) <- c("VS", "Mean HP")

aggregate(hp ~ vs, df, mean)

aggregate(hp ~ vs + am, df, mean)
aggregate(x = df$hp, by = list(df$vs, df$am), FUN = mean)

# Несколько статистик по группам
aggregate(x = df[, -c(8, 9)], by = list(df$am), FUN = median)
aggregate(x = df[, c(1, 3)], by = list(df$am, df$vs), FUN = sd)

aggregate(cbind(mpg, disp) ~ am + vs, df, sd)

