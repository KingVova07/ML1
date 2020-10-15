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


KNN <- function(xl, z,k)
{
  
  n <- dim(xl)[2] - 1
  
  
  classes <- xl[1:k, n + 1]

  counts <- table(classes)
  class <- names(which.max(counts))
  
  return (class)
}



LOO <- function(xl){
  l = dim(xl)[1]
  c = dim(xl)[2]
  Err = matrix(0, l-1, 1)
  for (i in 1:l){
    z = xl[i, 1:c-1]
    new_xl = xl[-i, ]
    new_xl = sortObjectsByDist(new_xl,z)
    for (k in 1:l-1){
        class <- KNN(new_xl, z,k)
        if (class != xl[i,c]){
          Err[k] = Err[k] + 1
        }
    }
  }

  return (Err)
}



xl <- iris[, 3:5]
loo = LOO(xl)
k = which.min(loo)

for (i in 1:10){
  x1 <- runif(1, 1.0, 7.0)
  x2 <- runif(1, 0, 2.5)
  z <- c(x1,x2)
  orderedXl <- sortObjectsByDist(xl, z)
  class <- KNN(orderedXl, z,k)
  points(z[1], z[2], pch = 22, bg = colors[class])
}






