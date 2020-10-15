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


Parzen <- function(xl,z,h, ker = Triangle){
  l = dim(xl)[1]
  n = dim(xl)[2] - 1
  weights <- c()
  weights[xl[ ,n+1]] <- 0
  i = 1
  #h = 0.5
  while(euclideanDistance(xl[i, 1:n], z) < h && i < l ){
    #print(i)
    tmp = (euclideanDistance(xl[i, 1:n], z))/(h)
    weights[xl[i,n+1]] = weights[xl[i,n+1]] + ker(tmp)
    i <- i + 1
  }
  if (i == 1) return ("white") 
  return (x[which.max(weights)])
}



LOO <- function(xl){
  l = dim(xl)[1]
  c = dim(xl)[2]
  Err = matrix(0, 400, 1)
  for (i in 1:l){
    print (i)
    z = xl[i, 1:c-1]
    new_xl = xl[-i, ]
    new_xl = sortObjectsByDist(new_xl,z)
    j = 1
    for (k in seq(0.1, 5, 0.1)){
      class <- Parzen(new_xl, z,k)
      if (class != xl[i,c]){
        Err[j] = Err[j] + 1
      }
      j = j + 1
    }
  }
  return (Err)
}


x = iris[,5]
x = unique(x)
xl <- iris[, 3:5]
#loo = LOOh(xl)
#print (loo)
#h = which.min(loo)
#print (h)
h = 0.1
if (FALSE){
  for (i in 1:10){
    x1 <- runif(1, 1.0, 7.0)
    x2 <- runif(1, 0, 2.5)
    z <- c(x1,x2)
    orderedXl <- sortObjectsByDist(xl, z)
    class = Parzen(orderedXl, z,h)
    points(z[1], z[2], pch = 22, bg = colors[class], asp = 20)
  }
}
for (i in seq(0.8, 7.1, 0.1)) {
  for (j in seq(-0.3, 2.9, 0.1)) {
    z <- c(i,j)
    orderedXl <- sortObjectsByDist(xl, z)
    class = Parzen(orderedXl, z,h)
    points(i, j, pch = 1, col = colors[class], asp = 20)
  }
}

points(iris[, 3:4], pch = 21, bg = colors[iris$Species], asp = 1)
