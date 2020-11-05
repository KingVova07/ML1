naive_Bayes <- function(Py, n, m, mu, sigma, point){
  p <- rep(0, m)
  for (i in 1:m) {
    p[i] <- Py[i]
    for (j in 1:n) {
      p[i] <- p[i] * exp((-(point[j] - mu[i, j])^2 * (1/sigma[i, j]))/2) / sqrt(2 * pi * sigma[i, j])
    }
  }
  return(x[which.max(p)])
}

LOO <- function(){
  xl <- iris[ , 3:5]
  l <- dim(xl)[1]
  c = dim(xl)[2]
  loo <-  0
  for (i in 1:l){
    print(i)
    new_xl = xl[-i, ]
    z = xl[i, 1:c-1]
    zclass = xl[i,c]
    row <- dim(new_xl)[1]
    col <- dim(new_xl)[2]
    
    num_x <- table(new_xl[3])
    x <- unique(new_xl[, 3])
    
    n <- col - 1 
    m <- dim(num_x)
    
    Py <- rep(0, m) 
    mu <- matrix(0, m, n)
    sigma <- matrix(0, m, n)
    
    for (i in 1:m) {
      Py[i] <- num_x[i] / row
    }
    
    
    for (i in 1:m) {
      for (j in 1:n) {
        mu[i, j] <- mean(new_xl[new_xl[, 3] == x[i], ][ , j])
        sigma[i, j] <- var(new_xl[new_xl[, 3] == x[i], ][ , j])
      }
    }
    
    class = naive_Bayes(Py, n, m, mu, sigma, z)
    print(zclass)
    if (class != zclass){
      loo = loo + 1 
    }
  }
  return (loo)
}

xl <- iris[ , 3:5]
num_x <- table(xl[3])
x <- unique(xl[, 3])
colors <- c("xlosa" = "red", "versicolor" = "green", "virginica" = "blue")

row <- dim(xl)[1]
col <- dim(xl)[2]
n <- col - 1 
m <- dim(num_x)

Py <- rep(0, m) 
mu <- matrix(0, m, n)
sigma <- matrix(0, m, n)

for (i in 1:m) {
  Py[i] <- num_x[i] / row
}

for (i in 1:m) {
  for (j in 1:n) {
    mu[i, j] <- mean(xl[xl[, 3] == x[i], ][ , j])
    sigma[i, j] <- var(xl[xl[, 3] == x[i], ][ , j])
  }
}

plot(
  xl[ , 1], xl[ , 2], 
  pch = 21, bg = colors[xl[, 3]], col = colors[xl[, 3]],
  xlab = "Длина лепестка", ylab = "Ширина лепестка",
  main = "Карта классификации (Наивный нормальный Байесовский классификатор)",
  xlim = c(1, 7), ylim = c(0, 2.5)
)

# карта классификации
for (i in seq(0.8, 7.2, 0.1)) {
  for (j in seq(-0.3, 2.9, 0.1)) {
    points(i, j, pch = 1, col = colors[naive_Bayes(Py, n, m, mu, sigma, c(i, j))])
  }
}

print (LOO())




