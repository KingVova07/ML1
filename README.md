# ML1

## Оглавление
1. [Метрические алгоритмы классификации](#Метрические-алгоритмы-классификации)
    1. [Метод ближайшего соседа](#Метод-ближайшего-соседа)
    2. [Метод ближайших соседей](#Метод-ближайших-соседей)
    3. [Скользящий контроль(leave-one-out)](#Скользящий-контроль(leave-one-out))
    4. [Алгоритм k взвешенных ближайших соседей (kwNN)](#Алгоритм-k-взвешенных-ближайших-соседей-(kwNN))
    5. [Метод Парзеновского окна](#Метод-Парзеновского-окна)
    6. [Метод потенциальных функций](#Метод-потенциальных-функций)
2. [Байесовские классификаторы.](#Байесовские-классификаторы.)
    1. [Линии уровня нормального распределения](#Линии-уровня-нормального-распределения)
    2. [Наивный байесовский классификатор](#Наивный-байесовский-классификатор)
    3. [Подстановочный алгоритм plug-in](#Подстановочный-алгоритм-plug-in)
    4. [Линейный дискриминант Фишера](#Линейный-дискриминант-Фишера)
3. [Линейные методы классификации](#Линейные-методы-классификации)
    1. [Стохастический градиентный спуск](#Стохастический-градиентный-спуск)
    2. [Адаптивный линейный элемент](#Адаптивный-линейный-элемент)
    3. [Персептрон Розенблатта](#Персептрон-Розенблатта)
    4. [Логистическая регрессия](#Логистическая-регрессия)
    5. [Сравнение алгоритмов](#Сравнение-алгоритмов)


<h1> Таблица точности алгоритмов на ирисах Фишера</h1>

<table>
<tr>
<th>Метод</th>
<th>Параметры</th>
<th>Погрешность</th>
</tr>
<tr>
<td>1nn</td>
<td>k = 1</td>
<td>0.04667</td>
</tr>
<tr>
 <td><strong>knn</strong></td>
<td><strong>k = 6</strong></td>
<td><strong>0.0333</strong></td>
</tr>
<tr>
<td>kwnn</td>
<td>k = 30, q = 0.95</td>
<td>0.0333</td>
</tr>
<tr>
<td>PW</td>
<td>ker = Rightangle, h = 0.4</td>
<td>0.04</td>
</tr>
<tr>
<td>PW</td>
<td>ker = Triangle, h = 0.4</td>
<td>0.04</td>
</tr>
<tr>
<td>PW</td>
<td>ker = Epanech, h = 0.4</td>
<td>0.04</td>
</tr>
<tr>
<td>PW</td>
<td>ker = Quatric, h = 0.4</td>
<td>0.04</td>
 </tr>
 <tr>
<td>PW</td>
<td>ker = Gauss, h = 0.1</td>
<td>0.04</td>
 </tr>
 <tr>
<td>Potentional</td>
<td>ker = Rightangle, h = 0.4, eps = 6</td>
<td>0.04</td>
 </tr>
  <tr>
<td>Potentional</td>
<td>ker = Triangle, h = 0.4, eps = 6</td>
<td>0.04</td>
 </tr>
  <tr>
<td>Potentional</td>
<td>ker = Epanech, h = 0.4, eps = 6</td>
<td>0.04</td>
 </tr>
  <tr>
<td>Potentional</td>
<td>ker = Quatric, h = 0.4, eps = 6</td>
<td>0.04</td>
 </tr>
 </table>
 
 
<h1>Задачи Классификации</h1>

[Оглавление](#Оглавление)

 Задача классификации в машинном обучении — это задача составления алгоритма для отнесения объекта к одному из заранее определенных классов на основании его формализованных признаков. Каждый из объектов в этой задаче представляется в виде вектора в N-мерном пространстве, каждое измерение в котором представляет собой описание одного из признаков объекта.
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
<h3>Вот пример работы этого алгоритма.</h3>

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/1nn.png)

<h3>и Карта классификации</h3>

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/1NN%20map%20class.png)

Но как увеличить точность метода?

 <h2>Метод ближайших соседей</h2>
 
 [Оглавление](#Оглавление)
 
 
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
<h3>Вот пример работы KNN. На графике отображено 10 точек, которым присвоен класс! В данном случае k = 10</h3>

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/KNN%20k%20=%2010.png)

Но как правильно выбрать k?

 <h2>Скользящий контроль(leave-one-out)</h2>
 
 [Оглавление](#Оглавление)
 
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

<h3>График зависимости LOO от k:</h3>

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/LOO(k).png)

<h3>Cамый меньший LOO при k = 6, сделаем карту классификации для k = 6<h3>
 
![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/KNN%20map%20class.png)

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/LOO_6nn_near.png?raw=true)

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

<h3>Искав лучшую k для всех q от 0.01 до 1 с шагом 0.01, пришел к выводу что при k = 30 и q = 0.95, найменьшее LOO(k,q), вот карта классификации для таких характеристик.</h3>

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/map%20KWNN%20q=0.95%20k%20=%2030.png)

<h2>Эффективность KWNN</h2>

<h3>При некоторой выборки KWNN эффективнее чем KNN, приведу пример k = 3, q = 0.5</h3>

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/knn%20%3C%20kwnn.png?raw=true)

<h3>Верхний скриншот пример работы KNN, он определил точку (1,2) неверно</h3>

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/kwnn%20%3E%20knn.png?raw=true)

<h3>Нижний скриншот пример работы KWNN, он определил точку верно</h3>

<h2>Метод Парзеновского окна</h2>

[Оглавление](#Оглавление)

Имеется некоторая выборка Xl, состоящая из объектов x(i), i = 1, ..., l (в нашем случае выборка ирисов Фишера).
В данном алгоритме используется такая весовая функция, которая зависит от расстояния между классфицируемым объектом и его соседями, тогда как в алгоритме взвешенных kNN весовая функция зависела от ранга соседа.
Весовая функция для данного алгоритма выглядит так:

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/Весовая%20функция%20парзеновского%20окна.png?raw=true)

Самые распространенные ядра:

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/Ядра%20классификаций.png?raw=true)

Вот программная реализация этих ядер:

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/Программная%20реализация%20ядер.png?raw=true)

Реализация алгоритма, а так же Loo для нахождение оптимального h:

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/Программная%20реализация%20PW.png?raw=true)

Приведу карты классификации, а так же LOO для определения точности

<h3><div align="center">Прямоугольное ядро</div></h3>

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/Parzen,%20Rightangle,%20h%20=%200.32.png?raw=true)

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/LOO%20Rightangle.png?raw=true)


<h3><div align="center">Треугольное ядро</div></h3>

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/Triangle_h__0_4.png?raw=true)

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/LOO%20Triangle.png?raw=true)

<h3><div align="center">Епанечниково ядро</div></h3>

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/Parzen,%20Quatric,%20h%20=%200,4.png?raw=true)

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/LOO%20Epanech.png?raw=true)

<h3><div align="center">Квартическое ядро</div></h3>

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/Parzen,%20Quatric,%20h%20=%200,4.png?raw=true)

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/LOO%20Quartic.png?raw=true)

<h3><div align="center">Гауссовское ядро</div></h3>

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/Parzen,%20Gauss,%20h%20=%200.1.png?raw=true)

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/LOO%20GAUSS.png?raw=true)

<h2> Метод потенциальных функций </h2>
Если в методе парзеновского окна центр окна поместить в классифицируемый
объект, то получим метод потенциальных функций:

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/Весовая%20функция%20potentioal.png?raw=true)

Теперь ширина окна hi зависит не от классифицируемого объекта, а от
обучающего xi.
Реализация алгоритма

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/Программная%20реализация%20Potentional.png?raw=true)

А вот карта классификаций и карта потенциалов

<h3><div align="center">Прямоугольное ядро</div></h3>

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/Potentional,%20rightangle,%20eps%20=%207.png?raw=true)

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/potential_epanech_core_v2.png?raw=true)

<h3><div align="center">Треугольное ядро</div></h3>

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/Potentional,%20eps%20=%207,%20triangle.png?raw=true)

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/potential_quarter_core.png?raw=true)

<h3><div align="center">Епанечниково ядро</div></h3>

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/Potentional,%20Epanech,%20h%20=%200.4.png?raw=true)

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/potential_epanech_core_v2.png?raw=true)

<h3><div align="center">Квартическое ядро</div></h3>

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/Quartic,%20h%20=%200.4%20,%20eps%20=%207.png?raw=true)

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/potential_quarter_core.png?raw=true)

<h2> STOLP </h2>

[Оглавление](#Оглавление)

Выделяют несколько выдов объектов обучения:

<p>Эталонные — типичные представители классов. Если классифицируемый объект близок к эталону, то, скорее всего, он принадлежит тому же классу.</p>
<p>Неинформативные — плотно окружены другими объектами того же класса. Если их удалить из выборки, это практически не отразится на качестве классификации..</p>
<p>Выбросы — находятся в окружении объектов чужого класса. Как правило, их удаление только улучшает качество классификации..</p>

Алгорим STOLP исключает из выборки выбросы и неинформативные объекты, оставляя лишь нужное количество эталонных. Таким образом улучшается качество классификации, сокращается объем данных и уменьшается время классификации объектов. Другими словами STOLP — алгоритм сжатия данных.

Он использует функцию отступа:

![screenshot_of_sample](https://raw.githubusercontent.com/KingVova07/ML1/a66c0ac3bcc3947069f429dfee2fa1ce1a288b70/function%20of%20margin.svg)

W является весовой функцией и зависит от выбранного алгоритма классификации.

Посмотрим на отсортированный список отступов для всех элементов:

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/margin.jpg?raw=true)

Вот здесь программная реализация для того чтобы посчитать отступы 

```R
for (i in 1:len){    
  z<-xl[-i,]
  right <- KwNN(z, xl[i,],k,1)
  wrong <- k-right
  margin[i] <- right-wrong  
}
```
Затем нам необходимо выбросить шумы

```R
for (i in 1:len){
  if (margin[i]<0){ 
    xl<-xl[-i,]
    len<-len-1
  }
}
```

Выбираем по одному эталону с каждого класса

```R
for (i in 1:len){
  if (margin[i]==k && colo[xl[i,3]]==0){
    new_xl[q,]<-xl[i,]
    q<-q+1
    colo[xl[i,3]]=1
  }
}
```

И потом присоединяются объекты, на которых происходит ошибка

```R
while (dim(new_xl)[1]!=len){
    wrong<-0
    right<-0
    for (i in 1:dim(new_xl)[1]){
        margin1[i]<-0
    }
    for (i in 1:dim(new_xl)[1]){ 
        zi<-sample(1:len, 1)  
        z<-xl[zi,]
        right <- KwNN(new_xl, z,k,1)
        wrong <- k-right
        margin1[i] <- right-wrong       
    }
    if (length(which(margin1<=0))<dim(new_xl)[1]*0.1){
        break
    }
    Standart<-rbind(Standart,z)
}
```

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/Stolp.jpg?raw=true)



<h1> Байесовские классификаторы.</h1>
Байесовский подход основан на следующей теореме: если плотности распределения классов известны, то алгоритм классификации, имеющий минимальную вероятность ошибок, можно выписать в явном виде.
Чтобы классифицировать точку, для начала, нужно вычислить функции правдоподобия каждого из классов, затем вычислить апостериорные вероятности классов. Классифицируемый объект относится к тому классу, у которого апостериорная вероятность максимальна.
<strong>Байесовское решающее правило:</strong>

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/Байесовское%20решающее%20правило.gif?raw=true)

<h2>Линии уровня нормального распределения</h2>

[Оглавление](#Оглавление)

Это случай байесовской классификации, когда предполагается, что плотности распределения всех классов являются многомерными нормальными.
Многомерное нормальное(гауссовское) распределение выглядит следующим образом:

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/form%20Gauss%20raspr.png?raw=true)

Предполагается, что матрица Σ симметричная, невырожденная, положительно определенная.

<strong>Вот программная реализация:</strong>

```R
Gauss_distribution <- function(Sigma, mu, x) {
  
  n <- 2
  
  numerator <- exp((-1/2) %*% t(x - mu) %*% solve(Sigma) %*% (x - mu))
  denominator <- sqrt(det(Sigma) * (2 * pi)^n)
  
  #print(numerator)
  #print(denominator)
  
  return(numerator/denominator)
}

Sigma <- matrix(NA, 2, 2)
mu <- c(0, 0)

Sigma[1, 1] <- 1
Sigma[2, 2] <- 2
Sigma[1, 2] <- 1
Sigma[2, 1] <- 0

x <- seq(-5, 5, 0.1)
y <- seq(-5, 5, 0.1)

par(bg = 'black', fg = 'white')
plot(-5:5, -5:5, type = "n",asp = 1)


for (i in x) {
  
  for (j in y) {
    
    color <- adjustcolor("white", Gauss_distribution(Sigma, mu, c(i, j)))
    points(i, j, pch = 21,col = color, bg = color)
    
  }
  
}


z = outer(x, y, function(x, y) {
  
  sapply(1:length(x), function(i) Gauss_distribution(Sigma, mu, c(x[i], y[i])))
  
})

contour(x,y,z,add = T ,asp = 1,lwd = 1)
```

<strong>Геометрия нормальной плотности:</strong>
В случае, когда признаки некореллированы, ![screenshot_of_sample](https://raw.githubusercontent.com/KingVova07/ML1/1a9300b889e45b5cfa705cf39a44c920ad69038a/некоррелированы.svg), то есть то плотности распределения имеют форму эллипсоидов, параллельных осям координат:

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/S1.png?raw=true)
![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/График%20некоррелированых.png?raw=true)

В случае, когда признаки имеют одинаковые дисперсии, ![screenshot_of_sample](https://raw.githubusercontent.com/KingVova07/ML1/1a9300b889e45b5cfa705cf39a44c920ad69038a/одинаковые%20дисперсии.svg) , линии уровня имеют форму сфер:

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/S2.png?raw=true)
![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/График%20одинаковых%20дисперсий.png?raw=true)

В случае, когда признаки коррелированы, а матрица не диагональна, то линии уровня – эллипсоиды, повернутые относительно исходной системы координат:

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/S3.png?raw=true)
![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/График%20коррелированых.png?raw=true)

Общая формула будет иметь такой вид:

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/formula%20for%20lines.png?raw=true)

<h2>Наивный байесовский классификатор</h2>

[Оглавление](#Оглавление)

<h3>Точность данного алгоритма на ирисах фишера = 0.94</h3>

Предполагается, что оценивать n одномерных плотностей легче, чем одну n-мерную, поэтому алгоритм называется "наивным". Все объекты выборки X описываются n числовыми признаками fj, где j=1,..,n. Эти признаки являются независимыми случайными величинами. Функции правдоподобия классов выглядят так:
py(x) = py1(ξ1)⋅⋅⋅pyn(ξn), где pyj(ξj) плотность распределения значений j-го признака для класса y.
Оценка априорной вероятности:

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/Оценка%20априорной%20вероятности.png?raw=true)

Эмпирическая оценка n-мерной плотности:

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/Эмпирическая%20оценка%20n-мерной%20плотности.png?raw=true)

Подставив в байесовское решающее правило эмпирические оценки одномерных плотностей признаков получим алгоритм:

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/naivnyy_bayes.jpg?raw=true)

<strong>Суть работы алгоритма:</strong>
Для начала определяем априорные вероятности каждого класса класса. Затем восстанавливаем матрицы математического ожидания и ковариационной матрицы.
Вот программная реализация:
```R
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
```
<h3><strong>Далее определяем саму функцию алгоритма:</strong></h3>

```R
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
```
<strong>Реализация алгоритма довольна проста, это является преимуществом, также алгоритм оптимален для независимых признаков.</strong>

<h3> Карта, когда все классы равнозначны</h3>

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/naive_Bayes.png?raw=true)

<h3> Карта, когда первый класс важнее чем остальные</h3>

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/Lambda1.png?raw=true)

<h3> Карта, когда второй класс важнее чем остальные</h3>

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/Lambda2.png?raw=true)

<h3> Карта, когда третий класс важнее чем остальные</h3>

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/Lambda3.png?raw=true)

<h2>Подстановочный алгоритм plug-in</h2>

[Оглавление](#Оглавление)

Нормальный дискриминантный анализ - это специальный случай байесовской классификации, предполагающий, что плотности всех классов являются многомерными нормальными.
Случайная величина x имеет многомерное нормальное распределение, если ее плотность задается выражением:

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/Плотность.gif?raw=true)

где 𝜇 ∈ ℝ - математическое ожидание (центр), а 𝛴 ∈ ℝ - ковариационная матрица (симметричная, невырожденная, положительно определённая).
Алгоритм заключается в том, чтобы найти неизвестные параметры 𝜇 и 𝛴 для каждого класса y и подставить их в формулу оптимального байесовского классификатора. В отличие от линейного дискриминанта Фишера(ЛДФ), в данном алгоритме мы предполагаем, что ковариационные матрицы не равны. Оценка параметров нормального распределения производится на основе параметров функций правдоподобия:

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/мю.gif?raw=true)

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/сигма.gif?raw=true)

Программная реализация восстановления данных параметров:
Для математического ожидания 𝜇, то есть находим центр нормального распределения элементов класса:

```R
 for (col in 1:cols){
   mu[1, col] = mean(objects[,col])
  }
```

Для восстановления ковариационной матрицы 𝛴:

```R
  for (i in 1:rows){
    sigma <- sigma + (t(objects[i,] - mu) %*% (objects[i,] - mu)) / (rows - 1)
  }
```

Результаты работы подстановочного алгоритма:

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/эллипс.jpg?raw=true)

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/парабола.jpg?raw=true)

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/гипербола.jpg?raw=true)

<h2>Линейный дискриминант Фишера</h2>

[Оглавление](#Оглавление)

Алгоритм ЛДФ отличается от подстановочного алгоритма тем, что ковариационые матрицы классов равны, поэтому для их восстановления необходимо использовать все объекты выборки. В этом случае разделяющая кривая вырождается в прямую.
Если оценить неизвестную 𝛴(ковариационная матрица, то есть их равенство), с учетом смещенности, то получим следующую формулу:

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/формула%20лдф.gif?raw=true)

Восстановление ковариационных матриц в коде алгоритма:

```R
    for (i in 1:rows1){
        sigma = sigma + (t(points1[i,] - mu1) %*% (points1[i,] - mu1))
    }

    for (i in 1:rows2){
        sigma = sigma + (t(points2[i,] - mu2) %*% (points2[i,] - mu2))
    }
```

Разделяющая плоскость здается формулой:

![screenshot_of_sample](https://raw.githubusercontent.com/KingVova07/ML1/476194473b380b9f7701cfb03d89d5a6f7b2ea90/плоскость%20лдф.svg)

коэффициенты которой находятся следующим образом:

![screenshot_of_sample](https://raw.githubusercontent.com/KingVova07/ML1/476194473b380b9f7701cfb03d89d5a6f7b2ea90/альфа%20лдф.svg)

![screenshot_of_sample](https://raw.githubusercontent.com/KingVova07/ML1/476194473b380b9f7701cfb03d89d5a6f7b2ea90/бета%20лдф.svg)

Программная реализация данной функции нахождения коэффициентов ЛДФ выглядит следующим образом:

```R
inverseSigma <- solve(Sigma)
alpha <- inverseSigma %*% t(mu1 - mu2)
beta <- (mu1 %*% inverseSigma %*% t(mu1) - mu2 %*% inverseSigma %*% t(mu2)) / 2
```

Результат работы алгоритма на ирисах фишера выглядит следующим образом:

![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/LDF_map.png?raw=true)


<h1>Линейные методы классификации</h1>
Пусть ![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/Множества%20X%20и%20Y.gif?raw=true) , тогда алгоритм ![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/Линейный%20алгоритм.gif?raw=true)  называется линейным алгоритмом.
В данном пространстве классы разделяет гиперплоскость, которая задается уравнением: ![screenshot_of_sample](https://raw.githubusercontent.com/KingVova07/ML1/9693483b10c5371601046108acf68960d7a902f3/функция.svg).
Если x находится по одну сторону гиперплоскости с её направляющим вектором w, то объект x относится к классу +1, в противном случае - к классу -1.
Эмпирический риск представлен следующей формулой: ![screenshot_of_sample](https://github.com/KingVova07/ML1/blob/master/Эмпирический%20риск.gif?raw=true)
Для того, чтобы минимизировать его и подобрать оптимальный вектор весов w, рекомендуется пользоваться методом стохастического градиента.

Существует величина ![screenshot_of_sample](https://raw.githubusercontent.com/KingVova07/ML1/9796da75f3695147443e9d19253f2e0470f1942f/отступ%20обьекта.svg), которая называется отступом объекта относительно алгоритма клссификации. Если данный отступ отрицательный, тогда алгоритм совершает ошибку.

L(M) - функция потерь.

