## sparkle and ML
## from http://spark.rstudio.com/mllib.html

getwd()
dat <-read.csv("./Crime_Philadelphia/crime.csv")

library(sparklyr)
library(ggplot2)
library(dplyr)
sc <- spark_connect(master = "local", version = "1.6.2")
devtools::install_github("SKKU-SKT/ggplot2.SparkR")


crime <-spark_read_csv(sc, dat, "./Crime_Philadelphia/crime.csv", header = TRUE,  overwrite = TRUE)




phil_tbl <- copy_to(sc,dat,"dat",overwrite = TRUE) 
head(phil_tbl)

ggplot(Phil16, aes(x = Lon, y = Lat)) + stat_sum()
head(summarize(groupBy(dat, dat$Lat, dat$Lon), count = n(dat$Lat,dat$Lon)))
head(dat)

dat$Dispatch_Date <- as.Date(dat$Dispatch_Date)
dat$year <- format(as.Date(dat$Dispatch_Date,
                           format="%m/%d/%Y"),"%Y")

Phil16 <-filter(dat, year == 2016)
phil16_tbl <- copy_to(sc,Phil16,"Phil16",overwrite = TRUE) 

kmeans_model <- phil_tbl %>%
  select(Lon, Lat, Hour) %>%
  ml_kmeans(centers = 10)

# print our model fit
print(kmeans_model)

# predict the associated class
predicted <- sdf_predict(kmeans_model, phil_tbl) %>%
  collect
table(predicted$Species, predicted$prediction)

# plot cluster membership
sdf_predict(kmeans_model) %>%
  collect() %>%
  ggplot(aes(Petal_Length, Petal_Width)) +
  geom_point(aes(Petal_Width, Petal_Length, col = factor(prediction + 1)),
             size = 2, alpha = 0.5) + 
  geom_point(data = kmeans_model$centers, aes(Petal_Width, Petal_Length),
             col = scales::muted(c("red", "green", "blue")),
             pch = 'x', size = 12) +
  scale_color_discrete(name = "Predicted Cluster",
                       labels = paste("Cluster", 1:3)) +
  labs(
    x = "Petal Length",
    y = "Petal Width",
    title = "K-Means Clustering",
    subtitle = "Use Spark.ML to predict cluster membership with the iris dataset."
  )

# Prepare beaver dataset
beaver <- beaver2
beaver$activ <- factor(beaver$activ, labels = c("Non-Active", "Active"))
copy_to(sc, beaver, "beaver")
beaver_tbl <- tbl(sc, "beaver")

glm_model <- beaver_tbl %>%
  mutate(binary_response = as.numeric(activ == "Active")) %>%
  ml_logistic_regression(binary_response ~ temp)

print(glm_model)

pca_model <- tbl(sc, "iris") %>%
  select(-Species) %>%
  ml_pca()
print(pca_model)

rf_model <- iris_tbl %>%
  ml_random_forest(Species ~ Petal_Length + Petal_Width, type = "classification")

rf_predict <- sdf_predict(rf_model, iris_tbl) %>%
  ft_string_indexer("Species", "Species_idx") %>%
  collect

table(rf_predict$Species_idx, rf_predict$prediction)

partitions <- tbl(sc, "iris") %>%
  sdf_partition(training = 0.75, test = 0.25, seed = 1099)

fit <- partitions$training %>%
  ml_linear_regression(Petal_Length ~ Petal_Width)

estimate_mse <- function(df){
  sdf_predict(fit, df) %>%
    mutate(resid = Petal_Length - prediction) %>%
    summarize(mse = mean(resid ^ 2)) %>%
    collect
}

sapply(partitions, estimate_mse)


ft_string2idx <- iris_tbl %>%
  ft_string_indexer("Species", "Species_idx") %>%
  ft_index_to_string("Species_idx", "Species_remap") %>%
  collect

table(ft_string2idx$Species, ft_string2idx$Species_remap)


ft_string2idx <- iris_tbl %>%
  sdf_mutate(Species_idx = ft_string_indexer(Species)) %>%
  sdf_mutate(Species_remap = ft_index_to_string(Species_idx)) %>%
  collect

ft_string2idx %>%
  select(Species, Species_idx, Species_remap) %>%
  distinct

#### example workflow

mtcars_tbl <- copy_to(sc, mtcars, "mtcars", overwrite = TRUE)

# transform our data set, and then partition into 'training', 'test'
partitions <- mtcars_tbl %>%
  filter(hp >= 100) %>%
  sdf_mutate(cyl8 = ft_bucketizer(cyl, c(0,8,12))) %>%
  sdf_partition(training = 0.5, test = 0.5, seed = 888)

# fit a linear mdoel to the training dataset
fit <- partitions$training %>%
  ml_linear_regression(mpg ~ wt + cyl)

# summarize the model
summary(fit)


# Score the data
pred <- sdf_predict(fit, partitions$test) %>%
  collect

# Plot the predicted versus actual mpg
ggplot(pred, aes(x = mpg, y = prediction)) +
  geom_abline(lty = "dashed", col = "red") +
  geom_point() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_fixed(ratio = 1) +
  labs(
    x = "Actual Fuel Consumption",
    y = "Predicted Fuel Consumption",
    title = "Predicted vs. Actual Fuel Consumption"
  )


#########################################
########################################
########## Must disconnect!!!!!!!!!!!!!!
##########################################
###########################################

spark_disconnect(sc)

