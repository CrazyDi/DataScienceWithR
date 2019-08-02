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

