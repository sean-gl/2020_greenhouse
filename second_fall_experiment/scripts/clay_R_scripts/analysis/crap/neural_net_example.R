# https://blogs.rstudio.com/tensorflow/posts/2017-12-20-time-series-forecasting-with-recurrent-neural-networks/

dir.create("~/Downloads/jena_climate", recursive = TRUE)
download.file(
  "https://s3.amazonaws.com/keras-datasets/jena_climate_2009_2016.csv.zip",
  "~/Downloads/jena_climate/jena_climate_2009_2016.csv.zip"
)
unzip(
  "~/Downloads/jena_climate/jena_climate_2009_2016.csv.zip",
  exdir = "~/Downloads/jena_climate"
)

library(tibble)
library(readr)

data_dir <- "~/Downloads/jena_climate"
fname <- file.path(data_dir, "jena_climate_2009_2016.csv")
data <- read_csv(fname)

glimpse(data)

library(ggplot2)
ggplot(data, aes(x = 1:nrow(data), y = `T (degC)`)) + geom_line()
ggplot(data[1:1440,], aes(x = 1:1440, y = `T (degC)`)) + geom_line()


# subset data
data <- data[1:10000,]
# split off response variable as vector
response <- data$`p (mbar)`
data <- data.matrix(data[,3:ncol(data)])


train_n <- 0.5 * nrow(data)
train_ind <- sample(1:nrow(data), train_n, replace = F)
# scale the data and then resample trainig/test sets
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data_scaled <- scale(data, center = mean, scale = std)


require(keras)
model <- keras_model_sequential()

model %>% 
  layer_dense(units = 8, activation = 'relu', input_shape = c(13)) %>% 
  layer_dense(units = 3, activation = 'softmax')

# Compile the model
model %>% compile(
  loss = 'mean_squared_error',
  optimizer = 'adam',
  metrics = 'accuracy'
)
summary(model)

# Store the fitting history in `history` 
history <- model %>% fit(
  train_data, 
  response[train_ind], 
  epochs = 200,
  batch_size = 5, 
  validation_split = 0.2
)

# Plot the history
plot(history)
