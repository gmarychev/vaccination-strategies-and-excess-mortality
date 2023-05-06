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

# Социальные работники
scp_vac_db <- read_excel('db_for_clustering_2_scp.xlsx') 
scp_vac_db <- tibble::column_to_rownames(scp_vac_db, var = "Country") # столбец "Country" - 
# названия наблюдений

# Рассчёт матрицы расстояний между объектами
scp_distance <- dist.binary(scp_vac_db, method = 1) # для подсчёта расстояний используем индекс Жаккарда
scp_distance[is.na(scp_distance)] <- 0 # расстояние между нулевыми векторами = 0

# Иерархическая кластеризация; метод полной связи
scp_clusters <- hclust(scp_distance, method = 'complete')
plot(scp_clusters, labels = NULL, hang = 0.1, check = TRUE,
     axes = TRUE, frame.plot = FALSE, ann = TRUE,
     main = "Cluster Dendrogram",
     sub = NULL, xlab = NULL, ylab = "Height") # зарисовка дендрограммы

# Подбор лучшего разрезания дендрограммы на кластеры по силуэту
scp_groups2 <- cutree(scp_clusters, k = 2)
scp_groups3 <- cutree(scp_clusters, k = 3)
scp_groups4 <- cutree(scp_clusters, k = 4)
scp_groups5 <- cutree(scp_clusters, k = 5)
scp_groups6 <- cutree(scp_clusters, k = 6)
scp_groups7 <- cutree(scp_clusters, k = 7)
scp_groups8 <- cutree(scp_clusters, k = 8)
scp_groups9 <- cutree(scp_clusters, k = 9)
scp_groups10 <- cutree(scp_clusters, k = 10)
scp_groups11 <- cutree(scp_clusters, k = 11)
scp_groups12 <- cutree(scp_clusters, k = 12)
scp_sil_2 <- silhouette(scp_groups2, dist = scp_distance)
scp_sil_3 <- silhouette(scp_groups3, dist = scp_distance)
scp_sil_4 <- silhouette(scp_groups4, dist = scp_distance)
scp_sil_5 <- silhouette(scp_groups5, dist = scp_distance)
scp_sil_6 <- silhouette(scp_groups6, dist = scp_distance)
scp_sil_7 <- silhouette(scp_groups7, dist = scp_distance)
scp_sil_8 <- silhouette(scp_groups8, dist = scp_distance)
scp_sil_9 <- silhouette(scp_groups9, dist = scp_distance)
scp_sil_10 <- silhouette(scp_groups10, dist = scp_distance)
scp_sil_11 <- silhouette(scp_groups11, dist = scp_distance)
scp_sil_12 <- silhouette(scp_groups12, dist = scp_distance)

fviz_silhouette(scp_sil_2)
fviz_silhouette(scp_sil_3)
fviz_silhouette(scp_sil_4)
fviz_silhouette(scp_sil_5)
fviz_silhouette(scp_sil_6)
fviz_silhouette(scp_sil_7)
fviz_silhouette(scp_sil_8)
fviz_silhouette(scp_sil_9)
fviz_silhouette(scp_sil_10)
fviz_silhouette(scp_sil_11)
fviz_silhouette(scp_sil_12)

# Все разбиения без отрицательного силуэта выходят с одиночными кластерами
# Лучшее - 9 кластеров, из которых 5 одиночных
# Рассматриваем разбиение на 9 кластеров, оцениваем для этих кластеров стабильность по clusterboot()
scp_cboot <- clusterboot(scp_distance, 
                         clustermethod = disthclustCBI, 
                         method = 'complete', 
                         k = 9, 
                         seed = 38)
scp_groups_cboot <- scp_cboot$result$partition
scp_groups_cboot == scp_groups9 # Проверка, что мы нигде не опрофанились и разбиение на кластеры совпадает
scp_cboot$bootmean # Вектор стабильности кластеров => 
# высоко стабильны: 2
# стабильны: нет
# слабо стабильны: 1, 3, 4, 5, 6, 8, 9
# не стабильны: 7
scp_cboot$bootbrd # вектор, показывающий, сколько раз "распался" каждый кластер; 
# все кластеры, кроме 2 слишком часто распадаются
# Попробуем исключить кластеры-одиночки:
# 4 - Дания, 5 - Эстония, 6 - Франция, 7 - Ирландия, 8 - Мальта

scp_vac_db_wo5 <- scp_vac_db[-c(5,6,8,11,15), ]
# Рассчёт матрицы расстояний между объектами
scp_distance_wo5 <- dist.binary(scp_vac_db_wo5, method = 1) # для подсчёта расстояний используем индекс Жаккарда
scp_distance_wo5[is.na(scp_distance_wo5)] <- 0 
# Иерархическая кластеризация; метод полной связи
scp_clusters_wo5 <- hclust(scp_distance_wo5, method = 'complete')
plot(scp_clusters_wo5, labels = NULL, hang = 0.1, check = TRUE,
     axes = TRUE, frame.plot = FALSE, ann = TRUE,
     main = "Cluster Dendrogram",
     sub = NULL, xlab = NULL, ylab = "Height") # зарисовка дендрограммы
scp_groups2_wo5 <- cutree(scp_clusters_wo5, k = 2)
scp_groups3_wo5 <- cutree(scp_clusters_wo5, k = 3)
scp_groups4_wo5 <- cutree(scp_clusters_wo5, k = 4)
scp_sil_2_wo5 <- silhouette(scp_groups2_wo5, dist = scp_distance_wo5)
scp_sil_3_wo5 <- silhouette(scp_groups3_wo5, dist = scp_distance_wo5)
scp_sil_4_wo5 <- silhouette(scp_groups4_wo5, dist = scp_distance_wo5)
fviz_silhouette(scp_sil_2_wo5)
fviz_silhouette(scp_sil_3_wo5)
fviz_silhouette(scp_sil_4_wo5) # Да, на 4 кластера получается лучшее разбиение
rect.hclust(scp_clusters_wo5, k = 4, border = 2:5)
scp_cboot_4_wo5 <- clusterboot(scp_distance_wo5, 
                               clustermethod = disthclustCBI, 
                               method = 'complete', 
                               k = 4, 
                               seed = 38)
scp_groups_cboot_4 <- scp_cboot_4_wo5$result$partition
scp_groups_cboot_4 == scp_groups4_wo5
scp_cboot_4_wo5$bootmean # все, кроме 1, высоко стабильны; 1 слабо стабилен
# Тем не менее продолжим анализ этих кластеров, так как 1 кластер всё ещё имеет определённые паттерны
# Рассмотрим подробно стратегии вакцинации в этих кластерах
scp_vac_db_wo5$groups <- factor(scp_groups_cboot_4)
scp_vac_db_wo5 %>% filter(groups == 1) %>% View
scp_vac_db_wo5 %>% filter(groups == 2) %>% View
scp_vac_db_wo5 %>% filter(groups == 3) %>% View
scp_vac_db_wo5 %>% filter(groups == 4) %>% View

# Оценим теперь избыточную смертность в каждом кластере
scp_cluster1_em <- data_em %>% filter(year == 2021 & 
                                        (country_name == 'Austria' & time > 4 & time < 31 | 
                                           country_name == 'Sweden' & time > 4 & time < 31))
scp_cluster1_exc_deaths <- sum(scp_cluster1_em$excess.deaths) # число изб.смертей в 1 кластере

scp_cluster2_em <- data_em %>% filter(year == 2021 & (country_name == 'Belgium' & time > 4 & time < 31 |
                                                        country_name == 'Croatia' & time > 4 & time < 31 |
                                                        country_name == 'Germany' & time > 4 & time < 31 |
                                                        country_name == 'Iceland' & time > 4 & time < 31 |
                                                        country_name == 'Latvia' & time > 4 & time < 31 |
                                                        country_name == 'Lithuania' & time > 4 & time < 31 |
                                                        country_name == 'Luxembourg' & time > 4 & time < 31 |
                                                        country_name == 'Netherlands' & time > 4 & time < 31 |
                                                        country_name == 'Norway' & time > 4 & time < 31 |
                                                        country_name == 'Portugal' & time > 4 & time < 31 |
                                                        country_name == 'Romania' & time > 4 & time < 31))
scp_cluster2_exc_deaths <- sum(scp_cluster2_em$excess.deaths) # число изб.смертей во 2 кластере

scp_cluster4_em <- data_em %>% filter(year == 2021 & (country_name == 'Poland' & time > 4 & time < 31 |
                                                        country_name == 'Spain' & time > 4 & time < 31
))
scp_cluster4_exc_deaths <- sum(scp_cluster4_em$excess.deaths) # число изб.смертей в 3 кластере

scp_cluster3_em <- data_em %>% filter(year == 2021 & (country_name == 'Czechia' & time > 4 & time < 31 |
                                                        country_name == 'Finland' & time > 4 & time < 31 |
                                                        country_name == 'Slovakia' & time > 4 & time < 31 |
                                                        country_name == 'Slovenia' & time > 4 & time < 31 ))
scp_cluster3_exc_deaths <- sum(scp_cluster3_em$excess.deaths) # число изб.смертей в 4 кластере

# Численность населения по кластерам (на 1 января 2020 года):
scp_pops_cluster1 <- pops %>% filter(Country == 'Austria' |
                                       Country == 'Sweden')
scp_pops_cluster2 <- pops %>% filter(Country == 'Belgium' |
                                       Country == 'Croatia' |
                                       Country == 'Germany' |
                                       Country == 'Iceland' |
                                       Country == 'Latvia' |
                                       Country == 'Lithuania' |
                                       Country == 'Luxembourg' |
                                       Country == 'Netherlands' |
                                       Country == 'Norway' |
                                       Country == 'Portugal' |
                                       Country == 'Romania')
scp_pops_cluster3 <- pops %>% filter(Country == 'Czechia'  |
                                       Country == 'Finland' |
                                       Country == 'Slovakia' |
                                       Country == 'Slovenia')
scp_pops_cluster4 <- pops %>% filter(Country == 'Poland' |
                                       Country == 'Spain' )

scp_population_cluster1 <- sum(scp_pops_cluster1$Population)*1000
scp_population_cluster2 <- sum(scp_pops_cluster2$Population)*1000
scp_population_cluster3 <- sum(scp_pops_cluster3$Population)*1000
scp_population_cluster4 <- sum(scp_pops_cluster4$Population)*1000

# Избыточная смертность по кластерам
scp_excess_mortality_cluster1 <- scp_cluster1_exc_deaths/scp_population_cluster1 * 100000
scp_excess_mortality_cluster2 <- scp_cluster2_exc_deaths/scp_population_cluster2 * 100000
scp_excess_mortality_cluster3 <- scp_cluster3_exc_deaths/scp_population_cluster3 * 100000
scp_excess_mortality_cluster4 <- scp_cluster4_exc_deaths/scp_population_cluster4 * 100000
