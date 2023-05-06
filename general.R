# Загрузка необходимых пакетов
install.packages('readxl')
install.packages('ade4')
install.packages('dplyr')
install.packages('factoextra')
install.packages('cluster')
install.packages('fpc')

# Распаковка необходимых пакетов
library(readxl)
library(ade4)
library(dplyr)
library(factoextra)
library(cluster)
library(fpc)

# Считываем базу данных по стратегиям вакцинации 
vac_db_1 <- read_excel('db_for_clustering_1.xlsx') # данные со стэком помесячных данных в одну строку
vac_db_1 <- tibble::column_to_rownames(vac_db_1, var = "Country") # столбец "Country" - 
# названия наблюдений

# Рассчёт матрицы расстояний между объектами
distance <- dist.binary(vac_db_1, method = 1) # для подсчёта расстояний используем индекс Жаккарда

# Иерархическая кластеризация; метод полной связи
clusters_1 <- hclust(distance, method = 'complete')
plot(clusters_1, labels = NULL, hang = 0.1, check = TRUE,
     axes = TRUE, frame.plot = FALSE, ann = TRUE,
     main = "Cluster Dendrogram",
     sub = NULL, xlab = NULL, ylab = "Height") # зарисовка дендрограммы

# Подбор лучшего разрезания дендрограммы на кластеры по силуэту
groups2 <- cutree(clusters_1, k = 2)
groups3 <- cutree(clusters_1, k = 3)
groups4 <- cutree(clusters_1, k = 4)
groups5 <- cutree(clusters_1, k = 5)
groups6 <- cutree(clusters_1, k = 6)
groups7 <- cutree(clusters_1, k = 7)
groups8 <- cutree(clusters_1, k = 8)
groups9 <- cutree(clusters_1, k = 9)
groups10 <- cutree(clusters_1, k = 10)
sil_2 <- silhouette(groups2, dist = distance)
sil_3 <- silhouette(groups3, dist = distance)
sil_4 <- silhouette(groups4, dist = distance)
sil_5 <- silhouette(groups5, dist = distance)
sil_6 <- silhouette(groups6, dist = distance)
sil_7 <- silhouette(groups7, dist = distance)
sil_8 <- silhouette(groups8, dist = distance)
sil_9 <- silhouette(groups9, dist = distance)
sil_10 <- silhouette(groups10, dist = distance)
fviz_silhouette(sil_2)
fviz_silhouette(sil_3)
fviz_silhouette(sil_4)
fviz_silhouette(sil_5)
fviz_silhouette(sil_6)
fviz_silhouette(sil_7)
fviz_silhouette(sil_8)
fviz_silhouette(sil_9)
fviz_silhouette(sil_10)

# Рассматриваем разбиение на 4 кластера, оцениваем для этих кластеров стабильность по clusterboot()
cboot_1 <- clusterboot(distance, 
                       clustermethod = disthclustCBI, 
                       method = 'complete', 
                       k = 4, 
                       seed = 38)
groups_cboot <- cboot_1$result$partition
groups_cboot == groups4 # Проверка, что мы нигде не опрофанились и разбиение на кластеры совпадает
cboot_1$bootmean # Вектор стабильности кластеров => 
# 2 и 4 кластеры стабильны, 
# 1 и 3 кластеры менее стабильны, 
# но алгоритм находит достаточно общих паттернов в этих кластерах, 
# чтобы их можно было рассматривать как кластеры
cboot_1$bootbrd # вектор, показывающий, сколько раз "распался" каждый кластер; 
# самый нестабильный - 3 кластер, остальные достаточно стабильны для интерпретации

# Сделаем то же самое, но для 6 кластеров
cboot_1_6 <- clusterboot(distance, 
                         clustermethod = disthclustCBI, 
                         method = 'complete', 
                         k = 6, 
                         seed = 38)
groups_cboot_6 <- cboot_1_6$result$partition
groups_cboot_6 == groups6 # Проверка, что мы нигде не опрофанились и разбиение на кластеры совпадает
cboot_1_6$bootmean # Вектор стабильности кластеров => 
# 1 и 6 кластеры высоко стабильны - похожи на "настоящие" кластеры
# 2 и 5 чуть менее стабильны, но значение всё равно высокое 
# 3 кластер удовлетворительно стабилен, 
# 4 кластер (состоящий только из Эстонии) нестабилен
cboot_1_6$bootbrd # вектор, показывающий, сколько раз "распался" каждый кластер; 
# выводы аналогичны

# И по стабильности, и по силуэту выигрывает разбиение на 6 кластеров (если игнорировать Эстонию).
# Тогда можно попробовать исключить Эстонию из выборки (считая её выбросом)
# и провести кластеризацию заново.
vac_db_1_woEst <- vac_db_1[-c(5), ]
# Рассчёт матрицы расстояний между объектами
distance_woEst <- dist.binary(vac_db_1_woEst, method = 1) # для подсчёта расстояний используем индекс Жаккарда
# Иерархическая кластеризация; метод полной связи
clusters_1_woEst <- hclust(distance_woEst, method = 'complete')
plot(clusters_1_woEst, labels = NULL, hang = 0.1, check = TRUE,
     axes = TRUE, frame.plot = FALSE, ann = TRUE,
     main = "Cluster Dendrogram",
     sub = NULL, xlab = NULL, ylab = "Height") # зарисовка дендрограммы
# Подбор лучшего разрезания дендрограммы на кластеры по силуэту
groups2_woEst <- cutree(clusters_1_woEst, k = 2)
groups3_woEst <- cutree(clusters_1_woEst, k = 3)
groups4_woEst <- cutree(clusters_1_woEst, k = 4)
groups5_woEst <- cutree(clusters_1_woEst, k = 5)
groups6_woEst <- cutree(clusters_1_woEst, k = 6)
groups7_woEst <- cutree(clusters_1_woEst, k = 7)
groups8_woEst <- cutree(clusters_1_woEst, k = 8)
groups9_woEst <- cutree(clusters_1_woEst, k = 9)
sil_2_woEst <- silhouette(groups2_woEst, dist = distance_woEst)
sil_3_woEst <- silhouette(groups3_woEst, dist = distance_woEst)
sil_4_woEst <- silhouette(groups4_woEst, dist = distance_woEst)
sil_5_woEst <- silhouette(groups5_woEst, dist = distance_woEst)
sil_6_woEst <- silhouette(groups6_woEst, dist = distance_woEst)
sil_7_woEst <- silhouette(groups7_woEst, dist = distance_woEst)
sil_8_woEst <- silhouette(groups8_woEst, dist = distance_woEst)
sil_9_woEst <- silhouette(groups9_woEst, dist = distance_woEst)
fviz_silhouette(sil_2_woEst)
fviz_silhouette(sil_3_woEst)
fviz_silhouette(sil_4_woEst)
fviz_silhouette(sil_5_woEst)
fviz_silhouette(sil_6_woEst)
fviz_silhouette(sil_7_woEst)
fviz_silhouette(sil_8_woEst)
fviz_silhouette(sil_9_woEst)
# Получаем, что лучшее разбиение - на 5 кластеров, построим это на дендрограмме:
rect.hclust(clusters_1_woEst, k = 5, border = 2:5)
# Теперь оценим полученные кластеры на стабильность
cboot_1_woEst <- clusterboot(distance_woEst, 
                             clustermethod = disthclustCBI, 
                             method = 'complete', 
                             k = 5, 
                             seed = 38)
groups_cboot_woEst <- cboot_1_woEst$result$partition
groups_cboot_woEst == groups5_woEst # Проверка, что мы нигде не опрофанились и разбиение на кластеры совпадает
cboot_1_woEst$bootmean # Вектор стабильности кластеров => 
# 1 и 4 кластеры высоко стабильны - похожи на "настоящие" кластеры 
# (те же кластеры, что и в прошлом разбиении на 6 групп)
# 2 и 5 чуть менее стабильны, но значение всё равно высокое 
# 3 кластер удовлетворительно стабилен
cboot_1_woEst$bootbrd # вектор, показывающий, сколько раз "распался" каждый кластер; 
# выводы аналогичны, хотя частота распадений даже более удовлетворительна, чем показатели стабильности

# Тогда на данном шаге выбираем разбиение на 5 кластеров с исключением Эстонии
# Рассмотрим подробно стратегии вакцинации в этих кластерах
vac_db_1_woEst$groups <- factor(groups_cboot_woEst)

vac_db_1_woEst %>% filter(groups == 1) %>% View
vac_db_1_woEst %>% filter(groups == 2) %>% View
vac_db_1_woEst %>% filter(groups == 3) %>% View
vac_db_1_woEst %>% filter(groups == 4) %>% View
vac_db_1_woEst %>% filter(groups == 5) %>% View

vac_db_1_woEst %>% group_by(groups) %>% 
  summarise_all(.funs = funs(mean)) %>% 
  View

# Оценим теперь избыточную смертность в каждом кластере
data_em <- read.csv('excess-mortality-timeseries.csv')
cluster1_em <- data_em %>% filter(year == 2021 & 
                                    (country_name == 'Austria' & time > 4 & time < 31 | 
                                       country_name == 'Ireland' & time > 1 & time < 8| 
                                       country_name == 'Sweden' & time > 4 & time < 31))
cluster1_exc_deaths <- sum(cluster1_em$excess.deaths) # число изб.смертей в 1 кластере

cluster2_em <- data_em %>% filter(year == 2021 & (country_name == 'Belgium' & time > 4 & time < 31 |
                                                    country_name == 'Croatia' & time > 4 & time < 31 |
                                                    country_name == 'Luxembourg' & time > 4 & time < 31))
cluster2_exc_deaths <- sum(cluster2_em$excess.deaths) # число изб.смертей во 2 кластере

cluster3_em <- data_em %>% filter(year == 2021 & (country_name == 'Czechia' & time > 4 & time < 31 |
                                                    country_name == 'Finland' & time > 4 & time < 31 |
                                                    country_name == 'Germany' & time > 4 & time < 31 |
                                                    country_name == 'Lithuania' & time > 4 & time < 31 |
                                                    country_name == 'Netherlands' & time > 4 & time < 31 |
                                                    country_name == 'Portugal' & time > 4 & time < 31 |
                                                    country_name == 'Spain' & time > 4 & time < 31
))
cluster3_exc_deaths <- sum(cluster3_em$excess.deaths) # число изб.смертей в 3 кластере

cluster4_em <- data_em %>% filter(year == 2021 & (country_name == 'France' & time > 4 & time < 31 |
                                                    country_name == 'Latvia' & time > 4 & time < 31 |
                                                    country_name == 'Poland' & time > 4 & time < 31 |
                                                    country_name == 'Romania' & time > 4 & time < 31))
cluster4_exc_deaths <- sum(cluster4_em$excess.deaths) # число изб.смертей в 4 кластере

cluster5_em <- data_em %>% filter(year == 2021 & (country_name == 'Iceland' & time > 4 & time < 31 |
                                                    country_name == 'Malta' & time > 4 & time < 31))
cluster5_exc_deaths <- sum(cluster5_em$excess.deaths) # число изб.смертей в 5 кластере

# Численность населения по кластерам (на 1 января 2020 года):
pops <- read_xlsx('WPP2022_Europe.xlsx')
pops <- pops %>% filter(Year == 2020)
pops_cluster1 <- pops %>% filter(Country == 'Austria' | 
                                   Country == 'Ireland' | 
                                   Country == 'Sweden')
pops_cluster2 <- pops %>% filter(Country == 'Belgium' |
                                   Country == 'Croatia' |
                                   Country == 'Luxembourg')
pops_cluster3 <- pops %>% filter(Country == 'Czechia' |
                                   Country == 'Finland' |
                                   Country == 'Germany' |
                                   Country == 'Lithuania' |
                                   Country == 'Netherlands' |
                                   Country == 'Portugal' |
                                   Country == 'Spain')
pops_cluster4 <- pops %>% filter(Country == 'France' |
                                   Country == 'Latvia' |
                                   Country == 'Poland' |
                                   Country == 'Romania')
pops_cluster5 <- pops %>% filter(Country == 'Iceland' |
                                   Country == 'Malta')
population_cluster1 <- sum(pops_cluster1$Population)*1000
population_cluster2 <- sum(pops_cluster2$Population)*1000
population_cluster3 <- sum(pops_cluster3$Population)*1000
population_cluster4 <- sum(pops_cluster4$Population)*1000
population_cluster5 <- sum(pops_cluster5$Population)*1000

# Избыточная смертность по кластерам
excess_mortality_cluster1 <- cluster1_exc_deaths/population_cluster1 * 100000
excess_mortality_cluster2 <- cluster2_exc_deaths/population_cluster2 * 100000
excess_mortality_cluster3 <- cluster3_exc_deaths/population_cluster3 * 100000
excess_mortality_cluster4 <- cluster4_exc_deaths/population_cluster4 * 100000
excess_mortality_cluster5 <- cluster5_exc_deaths/population_cluster5 * 100000
