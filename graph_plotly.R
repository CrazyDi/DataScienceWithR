install.packages("plotly")
library("plotly")
purchases <- read.csv("d:/Git/DataScienceWithR/data/E96/purchases.csv", sep = ";")


price.hist <- ggplot(purchases) +
    geom_histogram(aes(totalcents), fill = "white", color = "black") +
    scale_x_log10("Item price, RUB", labels = function(x) {
        format(x / 100, scientific = F, big.mark = " ")
    }) +
    ylab("Times purchased")
price.hist

interactive.price.hist <- ggplotly(price.hist)
interactive.price.hist

data("mtcars")

plot_ly(mtcars,
        x = mtcars$mpg,
        y = mtcars$disp,
        text = rownames(mtcars),
        group_by = as.factor(mtcars$cyl),
        mode="markers",
        type="scatter")

teapot <- read.csv("d:/Git/DataScienceWithR/data/teapot.csv", sep = ";")
plot_ly(z = ~volcano, type="surface")
library("data.table")
mesh <- data.table(
    x = rnorm(40),
    y = rnorm(40),
    z = rnorm(40)
)
plot_ly(mesh, type="mesh3d", x = ~x, y = ~y, z = ~z, alphahull = 0)

points <- data.table(
    x = c(0.2, 0.8, 0, 1),
    y = c(0, 0, 1, 1),
    z = c(0, 0, 0, 0)
)

i.s <- c(0, 2)
j.s <- c(1, 1)
k.s <- c(2, 3)
plot_ly(points, x = ~x, y = ~y, z = ~z, i = ~i.s, j = ~j.s, k = ~k.s, type = "mesh3d")


teapot <- data.table(teapot)
plot_ly(teapot[1:6, ], x = ~x, y = ~y, z = ~z, i = c(0, 3), j = c(1, 4), k = c(2, 5), type = "mesh3d")
i.s <- seq(0, nrow(teapot) - 1, 3)
j.s <- seq(1, nrow(teapot) - 1, 3)
k.s <- seq(2, nrow(teapot) - 1, 3)
plot_ly(teapot, x = ~x, y = ~y, z = ~z, i = ~i.s, j = ~j.s, k = ~k.s, type = "mesh3d")
