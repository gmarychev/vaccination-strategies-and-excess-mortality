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

# Теперь проведём кластеризацию отдельно по каждому показателю (приоритетной группе)
# Пожилые
eld_vac_db <- read_excel('db_for_clustering_2_eld.xlsx') # данные со стэком помесячных данных в одну строку
eld_vac_db <- tibble::column_to_rownames(eld_vac_db, var = "Country") # столбец "Country" - 
# названия наблюдений

# Рассчёт матрицы расстояний между объектами
eld_distance <- dist.binary(eld_vac_db, method = 1) # для подсчёта расстояний используем индекс Жаккарда

# Иерархическая кластеризация; метод полной связи
eld_clusters <- hclust(eld_distance, method = 'complete')
plot(eld_clusters, labels = NULL, hang = 0.1, check = TRUE,
     axes = TRUE, frame.plot = FALSE, ann = TRUE,
     main = "Cluster Dendrogram",
     sub = NULL, xlab = NULL, ylab = "Height") # зарисовка дендрограммы

# Подбор лучшего разрезания дендрограммы на кластеры по силуэту
eld_groups2 <- cutree(eld_clusters, k = 2)
eld_groups3 <- cutree(eld_clusters, k = 3)
eld_groups4 <- cutree(eld_clusters, k = 4)
eld_groups5 <- cutree(eld_clusters, k = 5)
eld_groups6 <- cutree(eld_clusters, k = 6)
eld_groups7 <- cutree(eld_clusters, k = 7)
eld_groups8 <- cutree(eld_clusters, k = 8)
eld_groups9 <- cutree(eld_clusters, k = 9)
eld_groups10 <- cutree(eld_clusters, k = 10)
eld_groups11 <- cutree(eld_clusters, k = 11)
eld_groups12 <- cutree(eld_clusters, k = 12)
eld_sil_2 <- silhouette(eld_groups2, dist = eld_distance)
eld_sil_3 <- silhouette(eld_groups3, dist = eld_distance)
eld_sil_4 <- silhouette(eld_groups4, dist = eld_distance)
eld_sil_5 <- silhouette(eld_groups5, dist = eld_distance)
eld_sil_6 <- silhouette(eld_groups6, dist = eld_distance)
eld_sil_7 <- silhouette(eld_groups7, dist = eld_distance)
eld_sil_8 <- silhouette(eld_groups8, dist = eld_distance)
eld_sil_9 <- silhouette(eld_groups9, dist = eld_distance)
eld_sil_10 <- silhouette(eld_groups10, dist = eld_distance)
eld_sil_11 <- silhouette(eld_groups11, dist = eld_distance)
eld_sil_12 <- silhouette(eld_groups12, dist = eld_distance)
fviz_silhouette(eld_sil_2)
fviz_silhouette(eld_sil_3)
fviz_silhouette(eld_sil_4)
fviz_silhouette(eld_sil_5)
fviz_silhouette(eld_sil_6)
fviz_silhouette(eld_sil_7)
fviz_silhouette(eld_sil_8)
fviz_silhouette(eld_sil_9)
fviz_silhouette(eld_sil_10)
fviz_silhouette(eld_sil_11)
fviz_silhouette(eld_sil_12)
# Рассматриваем разбиение на 2 кластера, оцениваем для этих кластеров стабильность по clusterboot()
eld_cboot <- clusterboot(eld_distance, 
                         clustermethod = disthclustCBI, 
                         method = 'complete', 
                         k = 2, 
                         seed = 38)
eld_groups_cboot <- eld_cboot$result$partition
eld_groups_cboot == eld_groups2 # Проверка, что мы нигде не опрофанились и разбиение на кластеры совпадает
eld_cboot$bootmean # Вектор стабильности кластеров => 
# 1 кластер стабилен, 2 не очень стабилен, но есть общие паттерны
eld_cboot$bootbrd # вектор, показывающий, сколько раз "распался" каждый кластер; 
# самый нестабильный - 2 кластер, а вот первые более-менее стабилен для интерпретации
# Это разбиение можно использовать, но лучше проверить ещё разбиение на 6 кластеров
eld_cboot_6 <- clusterboot(eld_distance, 
                           clustermethod = disthclustCBI, 
                           method = 'complete', 
                           k = 6, 
                           seed = 38)
eld_groups_cboot_6 <- eld_cboot_6$result$partition
eld_groups_cboot_6 == eld_groups6 # Проверка, что мы нигде не опрофанились и разбиение на кластеры совпадает
eld_cboot_6$bootmean # Вектор стабильности кластеров => 
# 2, 3, 4 и 5 кластеры высоко стабильны, 1 стабилен, 6 не очень стабилен, но паттерны есть
eld_cboot_6$bootbrd # вектор, показывающий, сколько раз "распался" каждый кластер; 
# выводы аналогичны
# разбиение на 6 групп сильно лучше разбиения на 2, но 6 кластер состоит только из Латвии: 
# исключим её как выброс и рассмотрим 5 кластеров
eld_vac_db_woLat <- eld_vac_db[-c(12), ]
# Рассчёт матрицы расстояний между объектами
eld_distance_woLat <- dist.binary(eld_vac_db_woLat, method = 1) # для подсчёта расстояний используем индекс Жаккарда
# Иерархическая кластеризация; метод полной связи
eld_clusters_woLat <- hclust(eld_distance_woLat, method = 'complete')
plot(eld_clusters_woLat, labels = NULL, hang = 0.1, check = TRUE,
     axes = TRUE, frame.plot = FALSE, ann = TRUE,
     main = "Cluster Dendrogram",
     sub = NULL, xlab = NULL, ylab = "Height") # зарисовка дендрограммы
rect.hclust(eld_clusters_woLat, k = 5, border = 2:5)
eld_cboot_5_woLat <- clusterboot(eld_distance_woLat, 
                                 clustermethod = disthclustCBI, 
                                 method = 'complete', 
                                 k = 5, 
                                 seed = 38)
eld_groups_cboot_5 <- eld_cboot_5_woLat$result$partition
eld_cboot_5_woLat$bootmean
# Рассмотрим подробно стратегии вакцинации в этих кластерах
eld_vac_db_woLat$groups <- factor(eld_groups_cboot_5)
eld_vac_db_woLat %>% filter(groups == 1) %>% View
eld_vac_db_woLat %>% filter(groups == 2) %>% View
eld_vac_db_woLat %>% filter(groups == 3) %>% View
eld_vac_db_woLat %>% filter(groups == 4) %>% View
eld_vac_db_woLat %>% filter(groups == 5) %>% View

eld_vac_db_woLat %>% group_by(groups) %>% 
  summarise_all(.funs = funs(mean)) %>% 
  View

# Оценим теперь избыточную смертность в каждом кластере
eld_cluster1_em <- data_em %>% filter(year == 2021 & 
                                        (country_name == 'Austria' & time > 4 & time < 31 | 
                                           country_name == 'Portugal' & time > 4 & time < 31))
eld_cluster1_exc_deaths <- sum(eld_cluster1_em$excess.deaths) # число изб.смертей в 1 кластере

eld_cluster2_em <- data_em %>% filter(year == 2021 & (country_name == 'Belgium' & time > 4 & time < 31 |
                                                        country_name == 'Ireland' & time > 1 & time < 8 |
                                                        country_name == 'Luxembourg' & time > 4 & time < 31 |
                                                        country_name == 'Spain' & time > 4 & time < 31 |
                                                        country_name == 'Sweden' & time > 4 & time < 31 ))
eld_cluster2_exc_deaths <- sum(eld_cluster2_em$excess.deaths) # число изб.смертей во 2 кластере

eld_cluster3_em <- data_em %>% filter(year == 2021 & (country_name == 'Croatia' & time > 4 & time < 31 |
                                                        country_name == 'Lithuania' & time > 4 & time < 31
))
eld_cluster3_exc_deaths <- sum(eld_cluster3_em$excess.deaths) # число изб.смертей в 3 кластере

eld_cluster4_em <- data_em %>% filter(year == 2021 & (country_name == 'Czechia' & time > 4 & time < 31 |
                                                        country_name == 'Denmark' & time > 4 & time < 31 |
                                                        country_name == 'Finland' & time > 4 & time < 31 |
                                                        country_name == 'Germany' & time > 4 & time < 31 |
                                                        country_name == 'Iceland' & time > 4 & time < 31 |
                                                        country_name == 'Malta' & time > 4 & time < 31 |
                                                        country_name == 'Netherlands' & time > 4 & time < 31 |
                                                        country_name == 'Norway' & time > 4 & time < 31 |
                                                        country_name == 'Slovenia' & time > 4 & time < 31 |
                                                        country_name == 'Slovakia' & time > 4 & time < 31))
eld_cluster4_exc_deaths <- sum(eld_cluster4_em$excess.deaths) # число изб.смертей в 4 кластере

eld_cluster5_em <- data_em %>% filter(year == 2021 & (country_name == 'Estonia' & time > 4 & time < 31 |
                                                        country_name == 'France' & time > 4 & time < 31 |
                                                        country_name == 'Poland' & time > 4 & time < 31 |
                                                        country_name == 'Romania' & time > 4 & time < 31))
eld_cluster5_exc_deaths <- sum(eld_cluster5_em$excess.deaths) # число изб.смертей в 5 кластере

# Численность населения по кластерам (на 1 января 2020 года):
eld_pops_cluster1 <- pops %>% filter(Country == 'Austria' |
                                       Country == 'Portugal')
eld_pops_cluster2 <- pops %>% filter(Country == 'Belgium' |
                                       Country == 'Ireland' |
                                       Country == 'Luxembourg' |
                                       Country == 'Spain' |
                                       Country == 'Sweden')
eld_pops_cluster3 <- pops %>% filter(Country == 'Croatia' |
                                       Country == 'Lithuania')
eld_pops_cluster4 <- pops %>% filter(Country == 'Czechia' |
                                       Country == 'Denmark' |
                                       Country == 'Finland'  |
                                       Country == 'Germany' |
                                       Country == 'Iceland' |
                                       Country == 'Malta' |
                                       Country == 'Netherlands' |
                                       Country == 'Norway' |
                                       Country == 'Slovenia' |
                                       Country == 'Slovakia')
eld_pops_cluster5 <- pops %>% filter(Country == 'Estonia' |
                                       Country == 'France' |
                                       Country == 'Poland' |
                                       Country == 'Romania')
eld_population_cluster1 <- sum(eld_pops_cluster1$Population)*1000
eld_population_cluster2 <- sum(eld_pops_cluster2$Population)*1000
eld_population_cluster3 <- sum(eld_pops_cluster3$Population)*1000
eld_population_cluster4 <- sum(eld_pops_cluster4$Population)*1000
eld_population_cluster5 <- sum(eld_pops_cluster5$Population)*1000

# Избыточная смертность по кластерам
eld_excess_mortality_cluster1 <- eld_cluster1_exc_deaths/eld_population_cluster1 * 100000
eld_excess_mortality_cluster2 <- eld_cluster2_exc_deaths/eld_population_cluster2 * 100000
eld_excess_mortality_cluster3 <- eld_cluster3_exc_deaths/eld_population_cluster3 * 100000
eld_excess_mortality_cluster4 <- eld_cluster4_exc_deaths/eld_population_cluster4 * 100000
eld_excess_mortality_cluster5 <- eld_cluster5_exc_deaths/eld_population_cluster5 * 100000