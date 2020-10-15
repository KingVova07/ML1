colors <- c("setosa" = "red", "versicolor" = "green3",
            "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species],
     col = colors[iris$Species])



euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}


sortObjectsByDist <- function(xl, z)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  
  distances <- matrix(NA, l, 2)
  
  for (i in 1:l)
  {
    distances[i, ] <- c(i, euclideanDistance(xl[i, 1:n], z))
  }
  
  
  orderedXl <- xl[order(distances[, 2]), ]
  
  return (orderedXl);
}



Rightangle <- function(u){
  return (1/2)
}


Triangle <- function(u){
  return (1 - u)
}

Epanech <- function(u){
  return ((3/4)*(1-u^2))
}

Quartic <- function(u){
  return ((15/16)* (1 - u^2))
}

Gauss <- function(u){
  return ((1/sqrt(2*pi))*exp((-1/2)*u^2))
}


algo <- function(xl,z,h, y, ker = Quartic){
  l = dim(xl)[1]
  n = dim(xl)[2] - 1
  weights <- c()
  weights[xl[ ,n+1]] <- 0
  i = 1
  #print(y)
  #h = 0.5
  while(euclideanDistance(xl[i, 1:n], z) < h && i < l ){
    tmp = (euclideanDistance(xl[i, 1:n], z))/(h)
    weights[xl[i,n+1]] = weights[xl[i,n+1]] + y*ker(tmp)
    i <- i + 1
  }
  if (i == 1) return ("white") 
  return (x[which.max(weights)])
}

potentional <- function(xl){
  l = dim(xl)[1]
  c = dim(xl)[2]
  y =  matrix(0, l, 1)
  h = 0.4
  eps = 10
  err = 150
  while(err >= eps){
    err = 0
    for (i in 1:l){
      new_xl = xl[-i, ]
      z = xl[i, 1:c-1]
      new_xl = sortObjectsByDist(new_xl,z)
      class <- algo(new_xl,z,h,y[i])
     # print (class)
      if (class != xl[i,c]){
        err = err + 1
        y[i] = y[i] + 1
      }
    }
    print(err)
  }
  return (y)
}


ans <- function(xl,z, y, h){
  l = dim(xl)[1]
  n = dim(xl)[2] - 1
  weights <- c()
  weights[xl[ ,n+1]] <- 0
  i = 1
  flag = 0
  while(i < l){
    #print(i)
    if (euclideanDistance(xl[i, 1:n], z) < h[i]) {
      weights[xl[i,n+1]] = weights[xl[i,n+1]] + y[i]
      flag = 1
    }
    i = i + 1
  }
  if (flag == 0) return ("white") 
  return (x[which.max(weights)])
}




x = iris[,5]
x = unique(x)
xl <- iris[, 3:5]
l = dim(xl)[1]
h = matrix (0.4, 1 , l)
y = potentional(xl)
print(y)

for (i in seq(0.8, 7.1, 0.1)) {
  for (j in seq(-0.3, 2.9, 0.1)) {
    z <- c(i,j)
    class = ans(xl, z,y,h)
    points(i, j, pch = 1, col = colors[class], asp = 20)
  }
}

points(iris[, 3:4], pch = 21, bg = colors[iris$Species], asp = 1)
