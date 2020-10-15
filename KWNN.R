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


KWNN <- function(xl, z,k,q)
{

  
  n <- dim(xl)[2]
  classes <- xl[1:k, n]
  
  weights <- c()
  weights[xl[ ,n]] <- 0
  
  for (i in 1:k){
    weights[classes[i]] <-  weights[classes[i]] + q^i
  }

  return (x[which.max(weights)])
}



LOO <- function(xl,q){
  l = dim(xl)[1]
  c = dim(xl)[2]
  Err = matrix(0, l-1, 1)
  for (i in 1:l){
    #print(i)
    z = xl[i, 1:c-1]
    new_xl = xl[-i, ]
    new_xl = sortObjectsByDist(new_xl, z)
    for (k in 1:(l-1)){
      #print(k)
      class <- KWNN(new_xl, z, k,q)
      #print(class)
      if (class != xl[i,c]){
        Err[k] = Err[k] + 1
      }
    }
  }
  
  #for (i in 1:l-1){
    #print (Err[i])
   #} 
  return (Err)
}





x = iris[,5]
x = unique(x)
i = 1
xl <- iris[, 3:5]
k = matrix(0, 100, 3)
min = 1
for (q in seq(0.01, 1, 0.01)) {
  print(i)
  loo <- LOO(xl,q)
  k[i,1] <- which.min(loo)
  k[i,2] <- loo[which.min(loo)]
  k[i,3] <- q
  if (k[i,3] < k[min,3]){
    min <- i
  }
  i = i + 1 
}
for (i in 1:10){
  x1 <- runif(1, 1.0, 7.0)
  x2 <- runif(1, 0, 2.5)
  z <- c(x1,x2)
  orderedXl <- sortObjectsByDist(xl, z)
  class <- KWNN(orderedXl, z,k[min,1],k[min,3])
  points(z[1], z[2], pch = 22, bg = colors[class], asp = 20)
}
if(FALSE)
{
  
}

