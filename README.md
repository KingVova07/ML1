# ML1
<h1>Задачи Классификации</h1>
 Задача классификации в машинном обучении — это задача отнесения объекта к одному из заранее определенных классов на основании его формализованных признаков. Каждый из объектов в этой задаче представляется в виде вектора в N-мерном пространстве, каждое измерение в котором представляет собой описание одного из признаков объекта.
<h2>Метрические алгоритмы классификации</h2>
 <p>Метрические методы обучения - методы, основанные на анализе сходства объектов. Для формализации понятия сходства вводится функция расстояния между объектами . Чем меньше расстояние между объектами, тем больше объекты похожи друг на друга. Метрические классификаторы опираются на гипотезу компактности, которая предполагает, что схожим объектам чаще соответствуют схожие ответы.</p>
 <h2>Метод ближайшего соседа</h2>
 Алгоритм ближайшего соседа(1NN) является самым простым алгоритмом клссификации.
Данный алгоритм классификации относит классифицируемый объект u к тому классу y, к которому относится его ближайший сосед. Для начала нужно задать метрическую функцию, затем нужно отсортировать выборку по расстояниям от точек до классфицируемой точки, после чего классифицируемый элемент относим к классу, к которому принадлежит ближайший элемент(первый в отсортированной выборке).
Вот реализация этого алгоритма на языке R:

```R
NN <- function(xl, z)
{
  orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2] - 1
  classes <- orderedXl[1, n + 1]
  return (classes)
}
```
Вот пример работы этого алгоритма.
![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/1nn.png)
и Карта классификации
![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/1NN%20map%20class.png)

Но как увеличить точность метода?

 <h2>Метод ближайших соседей</h2>
 kNN расшифровывается как k Nearest Neighbor или k Ближайших Соседей — это один из самых простых алгоритмов классификации, также иногда используемый в задачах регрессии. Благодаря своей простоте, он является хорошим примером, с которого можно начать знакомство с областью Machine Learning. Данный алгоритм классификации относит классифицируемый объект z к тому классу y, к которому относится большинство из k его ближайших соседей.
Имеется некоторая выборка , состоящая из объектов x(i), i = 1, ..., l (например, выборка ирисов Фишера), и класифицируемый объект, который обозначим z. Чтобы посчитать расстояние от классифицируемого объекта до остальных точек, нужно использовать функцию расстояния(Евклидово расстояние).
Далее отсортируем объекты согласно посчитаного расстояния до объекта z:

```R
sortObjectsByDist <- function(xl, z, metricFunction =
                                  euclideanDistance)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1

  distances <- matrix(NA, l, 2)
  
  for (i in 1:l)
  {
    distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
  }
  

  orderedXl <- xl[order(distances[, 2]), ]
  
  return (orderedXl);
}
```

Применяем сам метод kNN, то есть создаем функцию, которая сортирует выборку согласно нашего классифицируемого объекта z и относит его к классу ближайших соседей:

```R
KNN <- function(xl, z,k)
{
  
  n <- dim(xl)[2] - 1
  
  
  classes <- xl[1:k, n + 1]

  counts <- table(classes)
  class <- names(which.max(counts))
  
  return (class)
}
```
Вот пример работы KNN. На графике отображено 10 точек, которым присвоен класс! В данном случае k = 10
![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/KNN%20k%20=%2010.png)

Но как правильно выбрать k?

 <h2>Скользящий контроль(leave-one-out)</h2>
 LeaveOneOut (или LOO) - простая перекрестная проверка, которая необходима, чтобы оценить при каких значениях k алгоритм knn оптимален, и на сколько он ошибается.
Нужно проверить, как часто будет ошибаться алгоритм, если по одному выбирать элементы из обучающей выборки.
Алгоритм состоит в следующем: извлечь элемент, обучить оставшиеся элементы, классифицировать извлеченный, затем вернуть его обратно. Так нужно поступить со всеми элементами выборки. Вот реализация этого алгоритма на языке R:

```R
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

  return (which.min(Err))
}
```

График зависимости LOO от k:
![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/LOO(k).png)

Cамый меньший LOO при k = 6, сделаем карту классификации для k = 6
![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/KNN%20map%20class.png)

Есть ли другие версии этого алгоритма?

<h2>Алгоритм k взвешенных ближайших соседей (kwNN)</h2>
<p>Имеется некоторая выборка Xl, состоящая из объектов x(i), i = 1, ..., l ( выборка ирисов Фишера). Данный алгоритм классификации относит объект u к тому классу y, у которого максимальна сумма весов w_i его ближайших k соседей x(u_i), то есть объект относится к тому классу, который набирает больший суммарный вес среди k ближайших соседей.
В алгоритме kwNN используется весовая функция, которая оценивает степень важности при классификации заданного элемента к какому-либо классу, что и отличает его от алгоритма kNN.
kwNN отличается от kNN, тем что учитывает порядок соседей классифицируемого объекта, улучшая качество классификации.</p>
<p><b>Примеры весов.</b> w(i) = (k+1−i)/k, w(i) = q^i - геометрическая прогрессия со
знаменателем q ∈ (0, 1), который можно подбирать по критерию LOO. В своей реализации этого алгоритма я возьму весовую функцию w(i) = q^i.</p>

Вот реализация алгоритма на R:

```R
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
```

Искав лучшую k для всех q от 0.01 до 1 с шагом 0.01, пришел к выводу что при k = 30 и q = 0.95, найменьшее LOO(k,q), вот карта классификации для таких характеристик.
![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/map%20KWNN%20q=0.95%20k%20=%2030.png)
