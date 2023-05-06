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

# Постояльцы домов престарелых
ltcfeld_vac_db <- read_excel('db_for_clustering_2_ltcfeld.xlsx') # данные со стэком помесячных данных в одну строку
ltcfeld_vac_db <- tibble::column_to_rownames(ltcfeld_vac_db, var = "Country") # столбец "Country" - 
# названия наблюдений

# Рассчёт матрицы расстояний между объектами
ltcfeld_distance <- dist.binary(ltcfeld_vac_db, method = 1) # для подсчёта расстояний используем индекс Жаккарда

# Иерархическая кластеризация; метод полной связи
ltcfeld_clusters <- hclust(ltcfeld_distance, method = 'complete')
plot(ltcfeld_clusters, labels = NULL, hang = 0.1, check = TRUE,
     axes = TRUE, frame.plot = FALSE, ann = TRUE,
     main = "Cluster Dendrogram",
     sub = NULL, xlab = NULL, ylab = "Height") # зарисовка дендрограммы

# Подбор лучшего разрезания дендрограммы на кластеры по силуэту
ltcfeld_groups2 <- cutree(ltcfeld_clusters, k = 2)
ltcfeld_groups3 <- cutree(ltcfeld_clusters, k = 3)
ltcfeld_groups4 <- cutree(ltcfeld_clusters, k = 4)
ltcfeld_groups5 <- cutree(ltcfeld_clusters, k = 5)
ltcfeld_groups6 <- cutree(ltcfeld_clusters, k = 6)
ltcfeld_groups7 <- cutree(ltcfeld_clusters, k = 7)
ltcfeld_groups8 <- cutree(ltcfeld_clusters, k = 8)
ltcfeld_groups9 <- cutree(ltcfeld_clusters, k = 9)
ltcfeld_groups10 <- cutree(ltcfeld_clusters, k = 10)
ltcfeld_groups11 <- cutree(ltcfeld_clusters, k = 11)
ltcfeld_groups12 <- cutree(ltcfeld_clusters, k = 12)
ltcfeld_sil_2 <- silhouette(ltcfeld_groups2, dist = ltcfeld_distance)
ltcfeld_sil_3 <- silhouette(ltcfeld_groups3, dist = ltcfeld_distance)
ltcfeld_sil_4 <- silhouette(ltcfeld_groups4, dist = ltcfeld_distance)
ltcfeld_sil_5 <- silhouette(ltcfeld_groups5, dist = ltcfeld_distance)
ltcfeld_sil_6 <- silhouette(ltcfeld_groups6, dist = ltcfeld_distance)
ltcfeld_sil_7 <- silhouette(ltcfeld_groups7, dist = ltcfeld_distance)
ltcfeld_sil_8 <- silhouette(ltcfeld_groups8, dist = ltcfeld_distance)
ltcfeld_sil_9 <- silhouette(ltcfeld_groups9, dist = ltcfeld_distance)
ltcfeld_sil_10 <- silhouette(ltcfeld_groups10, dist = ltcfeld_distance)
ltcfeld_sil_11 <- silhouette(ltcfeld_groups11, dist = ltcfeld_distance)
ltcfeld_sil_12 <- silhouette(ltcfeld_groups12, dist = ltcfeld_distance)
fviz_silhouette(ltcfeld_sil_2)
fviz_silhouette(ltcfeld_sil_3)
fviz_silhouette(ltcfeld_sil_4)
fviz_silhouette(ltcfeld_sil_5)
fviz_silhouette(ltcfeld_sil_6)
fviz_silhouette(ltcfeld_sil_7)
fviz_silhouette(ltcfeld_sil_8)
fviz_silhouette(ltcfeld_sil_9)
fviz_silhouette(ltcfeld_sil_10)
fviz_silhouette(ltcfeld_sil_11)
fviz_silhouette(ltcfeld_sil_12)
# Рассматриваем разбиение на 2 кластера, оцениваем для этих кластеров стабильность по clusterboot()
ltcfeld_cboot <- clusterboot(ltcfeld_distance, 
                             clustermethod = disthclustCBI, 
                             method = 'complete', 
                             k = 2, 
                             seed = 38)
ltcfeld_groups_cboot <- ltcfeld_cboot$result$partition
ltcfeld_groups_cboot == ltcfeld_groups2 # Проверка, что мы нигде не опрофанились и разбиение на кластеры совпадает
ltcfeld_cboot$bootmean # Вектор стабильности кластеров => 
# 1 кластер высоко стабилен, 2 вообще не стабилен
ltcfeld_cboot$bootbrd # вектор, показывающий, сколько раз "распался" каждый кластер; 
# выводы аналогичные
# Это разбиение нельзя использовать, так как второй кластер не стабилен, поэтому рассмотрим разбиение на 6 кластеров
ltcfeld_cboot_6 <- clusterboot(ltcfeld_distance, 
                               clustermethod = disthclustCBI, 
                               method = 'complete', 
                               k = 6, 
                               seed = 38)
ltcfeld_groups_cboot_6 <- ltcfeld_cboot_6$result$partition
ltcfeld_groups_cboot_6 == ltcfeld_groups6 # Проверка, что мы нигде не опрофанились и разбиение на кластеры совпадает
ltcfeld_cboot_6$bootmean # Вектор стабильности кластеров => 
# 2, 3, 4 кластеры высоко стабильны, 1 и 6 стабилен, 5 не стабилен
ltcfeld_cboot_6$bootbrd # вектор, показывающий, сколько раз "распался" каждый кластер; 
# выводы аналогичны
# разбиение на 6 групп сильно лучше разбиения на 2, но 5 кластер состоит только из Эстонии: 
# исключим её как выброс и рассмотрим 5 кластеров
ltcfeld_vac_db_woEst <- ltcfeld_vac_db[-c(6), ]
# Рассчёт матрицы расстояний между объектами
ltcfeld_distance_woEst <- dist.binary(ltcfeld_vac_db_woEst, method = 1) # для подсчёта расстояний используем индекс Жаккарда
# Иерархическая кластеризация; метод полной связи
ltcfeld_clusters_woEst <- hclust(ltcfeld_distance_woEst, method = 'complete')
plot(ltcfeld_clusters_woEst, labels = NULL, hang = 0.1, check = TRUE,
     axes = TRUE, frame.plot = FALSE, ann = TRUE,
     main = "Cluster Dendrogram",
     sub = NULL, xlab = NULL, ylab = "Height") # зарисовка дендрограммы
rect.hclust(ltcfeld_clusters_woEst, k = 5, border = 2:5)
ltcfeld_cboot_5_woEst <- clusterboot(ltcfeld_distance_woEst, 
                                     clustermethod = disthclustCBI, 
                                     method = 'complete', 
                                     k = 5, 
                                     seed = 38)
ltcfeld_groups_cboot_5 <- ltcfeld_cboot_5_woEst$result$partition
ltcfeld_cboot_5_woEst$bootmean
# Рассмотрим подробно стратегии вакцинации в этих кластерах
ltcfeld_vac_db_woEst$groups <- factor(ltcfeld_groups_cboot_5)
ltcfeld_vac_db_woEst %>% filter(groups == 1) %>% View
ltcfeld_vac_db_woEst %>% filter(groups == 2) %>% View
ltcfeld_vac_db_woEst %>% filter(groups == 3) %>% View
ltcfeld_vac_db_woEst %>% filter(groups == 4) %>% View
ltcfeld_vac_db_woEst %>% filter(groups == 5) %>% View

ltcfeld_vac_db_woEst %>% group_by(groups) %>% 
  summarise_all(.funs = funs(mean)) %>% 
  View

# Оценим теперь избыточную смертность в каждом кластере
ltcfeld_cluster1_em <- data_em %>% filter(year == 2021 & 
                                            (country_name == 'Austria' & time > 4 & time < 31 | 
                                               country_name == 'Ireland' & time > 1 & time < 8))
ltcfeld_cluster1_exc_deaths <- sum(ltcfeld_cluster1_em$excess.deaths) # число изб.смертей в 1 кластере

ltcfeld_cluster2_em <- data_em %>% filter(year == 2021 & (country_name == 'Belgium' & time > 4 & time < 31 |
                                                            country_name == 'Croatia' & time > 4 & time < 31 |
                                                            country_name == 'Iceland' & time > 4 & time < 31 |
                                                            country_name == 'Luxembourg' & time > 4 & time < 31 |
                                                            country_name == 'Malta' & time > 4 & time < 31 ))
ltcfeld_cluster2_exc_deaths <- sum(ltcfeld_cluster2_em$excess.deaths) # число изб.смертей во 2 кластере

ltcfeld_cluster3_em <- data_em %>% filter(year == 2021 & (country_name == 'Czechia' & time > 4 & time < 31 |
                                                            country_name == 'Germany' & time > 4 & time < 31 |
                                                            country_name == 'Netherlands' & time > 4 & time < 31 |
                                                            country_name == 'Slovakia' & time > 4 & time < 31 |
                                                            country_name == 'Slovenia' & time > 4 & time < 31
))
ltcfeld_cluster3_exc_deaths <- sum(ltcfeld_cluster3_em$excess.deaths) # число изб.смертей в 3 кластере

ltcfeld_cluster4_em <- data_em %>% filter(year == 2021 & (country_name == 'Denmark' & time > 4 & time < 31 |
                                                            country_name == 'Finland' & time > 4 & time < 31 |
                                                            country_name == 'Lithuania' & time > 4 & time < 31 |
                                                            country_name == 'Norway' & time > 4 & time < 31 |
                                                            country_name == 'Poland' & time > 4 & time < 31 |
                                                            country_name == 'Portugal' & time > 4 & time < 31 |
                                                            country_name == 'Romania' & time > 4 & time < 31 |
                                                            country_name == 'Spain' & time > 4 & time < 31 |
                                                            country_name == 'Sweden' & time > 4 & time < 31))
ltcfeld_cluster4_exc_deaths <- sum(ltcfeld_cluster4_em$excess.deaths) # число изб.смертей в 4 кластере

ltcfeld_cluster5_em <- data_em %>% filter(year == 2021 & (country_name == 'France' & time > 4 & time < 31 |
                                                            country_name == 'Latvia' & time > 4 & time < 31))
ltcfeld_cluster5_exc_deaths <- sum(ltcfeld_cluster5_em$excess.deaths) # число изб.смертей в 5 кластере

# Численность населения по кластерам (на 1 января 2020 года):
ltcfeld_pops_cluster1 <- pops %>% filter(Country == 'Austria' |
                                           Country == 'Ireland')
ltcfeld_pops_cluster2 <- pops %>% filter(Country == 'Belgium' |
                                           Country == 'Croatia' |
                                           Country == 'Iceland' |
                                           Country == 'Luxembourg' |
                                           Country == 'Malta')
ltcfeld_pops_cluster3 <- pops %>% filter(Country == 'Czechia'|
                                           Country == 'Germany'|
                                           Country == 'Netherlands'|
                                           Country == 'Slovakia'|
                                           Country == 'Slovenia')
ltcfeld_pops_cluster4 <- pops %>% filter(Country == 'Denmark' |
                                           Country == 'Finland'|
                                           Country == 'Lithuania'|
                                           Country == 'Norway' |
                                           Country == 'Poland' |
                                           Country == 'Portugal' |
                                           Country == 'Romania' |
                                           Country == 'Spain' |
                                           Country == 'Sweden')
ltcfeld_pops_cluster5 <- pops %>% filter(Country == 'France' |
                                           Country == 'Latvia')
ltcfeld_population_cluster1 <- sum(ltcfeld_pops_cluster1$Population)*1000
ltcfeld_population_cluster2 <- sum(ltcfeld_pops_cluster2$Population)*1000
ltcfeld_population_cluster3 <- sum(ltcfeld_pops_cluster3$Population)*1000
ltcfeld_population_cluster4 <- sum(ltcfeld_pops_cluster4$Population)*1000
ltcfeld_population_cluster5 <- sum(ltcfeld_pops_cluster5$Population)*1000

# Избыточная смертность по кластерам
ltcfeld_excess_mortality_cluster1 <- ltcfeld_cluster1_exc_deaths/ltcfeld_population_cluster1 * 100000
ltcfeld_excess_mortality_cluster2 <- ltcfeld_cluster2_exc_deaths/ltcfeld_population_cluster2 * 100000
ltcfeld_excess_mortality_cluster3 <- ltcfeld_cluster3_exc_deaths/ltcfeld_population_cluster3 * 100000
ltcfeld_excess_mortality_cluster4 <- ltcfeld_cluster4_exc_deaths/ltcfeld_population_cluster4 * 100000
ltcfeld_excess_mortality_cluster5 <- ltcfeld_cluster5_exc_deaths/ltcfeld_population_cluster5 * 100000
