getwd()
my_data  <- read.csv("perfume_data.csv")
my_data

data <- scale(my_data)
head(data)

library("cluster")
library("ggplot2")
library("factoextra")

"""
Функція fviz_dist() з пакету factoextra візуалізує матрицю відстаней
Аргумент gradient , список з трьох елементів, визначає кольори для зображення малих, середніх та великих значень відстаней.

Статистика Гопкінса показує, що він є більш-менш явно розділеним на кластери
"""
data.dist <- get_dist(data, method = "pearson")
fviz_dist(data.dist, gradient = list(low = "salmon", mid = "white", high = "yellow"))

library(hopkins)
hopkins(data)

"""
Ієрархічна кластеризація
Найкраща кількість кластерів – це кількість вертикальних ліній в дендрограмі,
що перетинаються горизонтальними лініями, відстань між якими є перевернутою
максимальною вертикальною відстанню без перетину кластеру.
"""
data.dist <- get_dist(data, stand = TRUE)
data.hc1 <- hclust(data.dist) # кластеризація
fviz_dend(data.hc1)


fviz_dend(data.hc1, k = 2, # розділяємо на 2 кластери
 cex = 0.5, # розмір шрифту для назв об'єктів
 k_colors = c("purple3", "orange1"), # кольори для різних кластерів
 color_labels_by_k = TRUE, # назви груп також будуть різного кольору
 rect = TRUE, # прямокутник, що виділяє кожну групу
 main = "Дендрограма для кластеризації за методом дальнього сусіда")

fviz_dend(data.hc1, k = 4, # розділяємо на 4 кластери
 cex = 0.5, # розмір шрифту для назв об'єктів
 k_colors = c("purple3", "orange1"), # кольори для різних кластерів
 color_labels_by_k = TRUE, # назви груп також будуть різного кольору
 rect = TRUE, # прямокутник, що виділяє кожну групу
 main = "Дендрограма для кластеризації за методом дальнього сусіда")

data_cl1 <- hclust(data.dist, method = "ward.D2")
fviz_dend(data_cl1, cex = 0.2)
          
          
fviz_dend(data_cl1, k = 2,
 cex = 1,
 k_colors = c("cyan2", "palevioletred", "yellow1"),
 color_labels_by_k = TRUE,
 rect = TRUE,
 main = "Дендрограма для кластеризації за методом Варда (2 кластери)")


fviz_dend(data_cl1, k = 4,
 cex = 1,
 k_colors = c("cyan2", "palevioletred", "yellow1"),
 color_labels_by_k = TRUE,
 rect = TRUE,
 main = "Дендрограма для кластеризації за методом Варда (4 кластери)")


#Ітераційний метод кластеризації - k-медоїдів
data_cl2 <- pam(data.dist, 2)
data_cl2$data = data.dist
fviz_cluster(data_cl2, palette = "jco")


fviz_cluster(data_cl2, ellipse.type = "t", palette = "jco")

#PAM є робастим до шумів і викидів
data_pam <- eclust(data, "pam", k = 2, stand = FALSE)


#Зобразимо діаграму, яка зображує значення силуету для кожної точки та середнє значення силуету.
arrests.kmeans2 <- kmeans(data, centers = 2, nstart = 20)
sil2 <- silhouette(arrests.kmeans2$cluster, dist(data))
fviz_silhouette(sil2)

"""
Додатнє значення силуету означає, що об’єкт розміщений у потрібному кластері, і чим ближче
значення до 1, тим краще він розміщений. Наявні кілька від’ємних значень силуетів, що означає що об’єкт знаходиться не в тому кластері і є нульві силуети, тобто розміщені на межі між двома кластерами.
"""

#Визначення оптимальної кількості кластерів

#1) Метод ліктя
#Метод ліктя спирається на порівняння внутрішньокластерної суми квадратів відстаней (WSS), для зручності ця величина зображується як функція від кількості кластерів.

library(NbClust)
fviz_nbclust(data, kmeans, method = "wss") +
 geom_vline(xintercept = 2, linetype = 2) +
 labs(subtitle = "Метод ліктя")

#Оскільки останнє суттєве зменшення внутрішньокластерної суми квадратів досягається в точці 2, можна припустити що оптимальна кількість кластерів - 2. 

#2) Метод середнього силуету
#Метод середнього силуету дещо інакше характеризує якість кластеризації, і визначає, наскільки “вдало” кожна точка розміщена в своєму кластері. Оптимальною кількістю кластерів є така, що максимізує середнє значення силуету.

fviz_nbclust(my_data, kmeans, method = "silhouette") +
labs(subtitle = "Метод середнього силуету")

#У методі середньогго силету оптимальною кількістю кластерів є така, що максимізує середнє значення силуету, а отже знову точка 2.

#3. Метод статистики розриву
#Оптимальною кількістю кластерів, згідно логіці цього методу, є така, яка максимізує значення статистики розриву. Це означатиме, що структура кластерів суттєво відрізняється від випадкового рівномірного розподілу точок (об’єктів).

fviz_nbclust(my_data, kmeans, nstart = 25, method = "gap_stat",
 nboot = 500 
)+
 labs(subtitle = "Метод статистики розриву")

#Всі три емтоди дали однаковий результат - 2, отже робимо висновок що оптимальна кількість кластерів все ж 2.

library(fpc)
#к метоїдів  і Варда
dd <- dist(data, method ="euclidean")
data_pam <- eclust(data, "pam", k = 2, stand = FALSE)
clust.arrests.ward2 <- cutree(hclust(dd, method = "ward.D2"), 2)
km_stats3 <-  cluster.stats(dd,data_pam$cluster,
                alt.clustering = clust.arrests.ward2)
km_stats3$corrected.rand

#Варда і середнього
ward_data <- cutree(hclust(dd, method = "ward.D2"), 2)
data.kmeans3<-kmeans(my_data, centers = 2, nstart = 10)
km_stats3 <- cluster.stats(dd,data.kmeans3$cluster)
km_stats2 <- cluster.stats(dd, data.kmeans3$cluster,alt.clustering = ward_data)
km_stats2$corrected.rand

#середн і метоїди

data.kmeans3<-kmeans(my_data, centers = 2, nstart = 10)
data_pam <- eclust(data, "pam", k = 2, stand = FALSE)

km_stats5<-cluster.stats(dd,data_pam$cluster,data.kmeans3$cluster)
km_stats5$corrected.rand
