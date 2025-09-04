##
##    Programme:  Experiments_in_Tiff.r
##
##    Objective:  Builds on output derived from Dep_Play_Code_Version2.py
##
##                GeoMAD is the Digital Earth Africa (DE Africa) surface reflectance geomedian and triple 
##                Median Absolute Deviation data service. It is a cloud-free composite of satellite data compiled over specific timeframes. 
##                The geomedian component combines measurements collected over the specified timeframe to produce one representative, 
##                multispectral measurement for every pixel unit of the African continent. The end result is a comprehensive dataset 
##                that can be used to generate true-colour images for visual inspection of anthropogenic or natural landmarks. The full 
##                spectral dataset can be used to develop more complex algorithms. 
##
##                For each pixel, invalid data is discarded, and remaining 
##                observations are mathematically summarised using the geomedian statistic. Flyover coverage provided by collecting data 
##                over a period of time also helps scope intermittently cloudy areas. Variations between the geomedian and the individual 
##                measurements are captured by the three Median Absolute Deviation (MAD) layers. These are higher-order statistical 
##                measurements calculating variation relative to the geomedian. 
##
##                The MAD layers can be used on their own or together with 
##                geomedian to gain insights about the land surface, and understand change over time. Calculating GeoMAD over different 
##                timeframes and sensors provides a range of insights to the environment. An annual timeframe allows better correction 
##                for cloud cover and reduces artifacts for comparison over multiple years. A semiannual timeframe, for example six-month 
##                blocks, better captures seasonal variation within one year, but can also be used to compare equivalent periods from 
##                different years. Likewise, Landsat sensors allows full utility of the surface reflectance archive dating back to 1984, 
##                while more recent Sentinel-2 data provides higher-frequency flyovers and better resolution. The Digital Earth Africa 
##                GeoMAD service currently provides annual and six-month semiannual datasets, with separate services for Landsat and 
##                Sentinel-2 sensors.
##
##                https://registry.opendata.aws/deafrica-geomad/b
##                https://keras.io/
##
##    Author:     James Hogan, Senior Marine Resource Economist, 1 September 2025
##
##

   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
      options(max.print=999999)
      options(scipen = 100)
     
   ##
   ##    Core libraries
   ##
      library(tensorflow)
      library(GPUmatrix)
      library(Matrix)
      library(keras3)
      
      library(tictoc)
      library(terra)
      library(sf)
      library(parallel)   

   ##
   ##    Set working directory
   ##
      setwd("c:\\Git_Projects\\Ocean_Accounts")

      NewCal <- rast("Data_Spatial/dep_s2_geomad_2017.tif")
      describe("Data_Spatial/dep_s2_geomad_2017.tif")
      plot(NewCal)
      head(values(NewCal))

   ##
   ##    Ok, with the python programme, DEP_Play_Code_Version2.py, I can extract GeoMAD datasets from DEP data sources.
   ##       The programme will pop them into my Data_Spatial directory where I can pick them up and read them. Now lets
   ##       incorporate the code from my kindle book "Deep Learning in R" to see if can group these images into a land
   ##       use metric. At least a basic one.
   ##
      ##
      ##    Load the data from the book 
      ##
         mnist <- dataset_mnist()
         train_images <- mnist$train$x
         train_labels <- mnist$train$y
         test_images <- mnist$test$x
         test_labels <- mnist$test$y

         train_images[1,,]
         train_images[2,,]

      ##
      ##    Create the layers
      ##
#         network <- keras_model_sequential() %>% 
#                    layer_dense(units = 512, activation = "relu", input_shape = c(28 * 28)) %>%
#                    layer_dense(units = 10, activation = "softmax")
                    
         network <- keras_model_sequential()
         layer_dense(network, units = 512, activation = "relu", input_shape = c(28 * 28))
         layer_dense(network, units =  10, activation = "softmax")                    

      ##
      ##    Define the loss function and the optimiser and the metrics to monitor during training (accuracy)
      ##
#         network %>% compile(optimizer = "rmsprop",
#                             loss = "categorical_crossentropy",
#                             metrics = c("accuracy"))
         compile(network,
                 optimizer = "rmsprop",
                 loss = "categorical_crossentropy",
                 metrics = c("accuracy"))

      ##
      ##    Reshape the images so rather than a 28 x 28 array, it becomes a 1 X 784 vector
      ##    Rescale the data to be a proportion of 1
      ##
         train_images[1,,]
         train_images <- array_reshape(train_images, c(60000, 28 * 28))
         train_images[1,]
         
         train_images <- train_images / 255
      
         Test_Images <- test_images
         test_images <- array_reshape(test_images, c(10000, 28 * 28))
         test_images <- test_images / 255

      ##
      ## Categorically recode the labels
      ##
         train_labels <- to_categorical(train_labels)
         test_labels  <- to_categorical(test_labels)

      ##
      ## Train the model
      ##
#         network %>% fit(train_images, train_labels, epochs = 5, batch_size = 128)
        fit(network, train_images, train_labels, epochs = 5, batch_size = 128)
         
      ##
      ## Test the model
      ##
#         metrics <- network %>% evaluate(test_images, test_labels)
         metrics <- evaluate(network, test_images, test_labels)

      ##
      ## Predict the model
      ##
        predict_on_batch(network, test_images[1:10,])


##
##    IMDB Examples - Sentiment classification of text into positive or negative reviewas
##
   imdb <- dataset_imdb(num_words = 10000)
   
   train_data <- imdb$train$x
   train_labels <- imdb$train$y
   test_data <- imdb$test$x
   test_labels <- imdb$test$y

   vectorize_sequences <- function(sequences, dimension = 10000) 
                           {
                             results <- matrix(0, nrow = length(sequences), ncol = dimension)      
                             for (i in 1:length(sequences))
                               results[i, sequences[[i]]] <- 1                                     
                             results
                           }

   x_train <- vectorize_sequences(train_data)
   x_test  <- vectorize_sequences(test_data)

   y_train <- as.numeric(train_labels)
   y_test  <- as.numeric(test_labels)

   ##
   ##    Overfitting example - the training loss descreases and accuracy increase with each epoch, 
   ##      but the validation accuracy and loss increase rather than decrease showing the "more accurate" model is 
   ##      overfitting the data rather than capturing relevant detail.
   ##

   model <- keras_model_sequential() %>%
            layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>%
            layer_dense(units = 16, activation = "relu") %>%
            layer_dense(units = 1, activation = "sigmoid")
 
   model %>% compile(optimizer = "rmsprop",
                     loss = "binary_crossentropy",
                     metrics = c("accuracy"))
                     
   val_indices       <- 1:10000
   
   x_val             <- x_train[val_indices,]
   partial_x_train   <- x_train[-val_indices,]
   y_val             <- y_train[val_indices]
   partial_y_train   <- y_train[-val_indices]

   history <- model %>% fit(partial_x_train,
                            partial_y_train,
                            epochs = 20,
                            batch_size = 512,
                            validation_data = list(x_val, y_val))
   results <- model %>% evaluate(x_test, y_test)
   results
   plot(history)
   
   ##
   ##    Retraining the model with fewer epochs to reduce overfitting. Number of epochs based on above plot
   ##
   
   model <- keras_model_sequential() %>%
            layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>%
            layer_dense(units = 16, activation = "relu") %>%
            layer_dense(units = 1, activation = "sigmoid")

   model %>% compile(optimizer = "rmsprop",
                    loss = "binary_crossentropy",
                    metrics = c("accuracy"))

   history <- model %>% fit(x_train, y_train, epochs = 5, batch_size = 512)
   results <- model %>% evaluate(x_test, y_test)
   results
   plot(history)

##
##    Reuters Examples - Classifying text into 46 mutually exclusive topics
##
   reuters <- dataset_reuters(num_words = 10000)
   c(c(train_data, train_labels), c(test_data, test_labels)) %<-% reuters
   length(train_data)
   length(test_data)
   
   ##
   ##    decoding back to words
   ##
      word_index <- dataset_reuters_word_index()
      reverse_word_index <- names(word_index)
      names(reverse_word_index) <- word_index
      
      decoded_newswire <- sapply(train_data[[1]], function(index) {
        word <- if (index >= 3) reverse_word_index[[as.character(index - 3)]]   
        if (!is.null(word)) word else "?"
      })
   
   ##
   ##    Recode the data into tensors
   ##
      vectorize_sequences <- function(sequences, dimension = 10000) {
        results <- matrix(0, nrow = length(sequences), ncol = dimension)
        for (i in 1:length(sequences))
          results[i, sequences[[i]]] <- 1
        results
      }

      x_train <- vectorize_sequences(train_data)         
      x_test <- vectorize_sequences(test_data)
      
   ##
   ##    Recode the data into tensors
   ##
      one_hot_train_labels <- to_categorical(train_labels)
      one_hot_test_labels <- to_categorical(test_labels)

   ##
   ##   the softmax activation outputs a probability distribution for the 46 label catagories
   ##

      model <- keras_model_sequential() %>%
               layer_dense(units = 64, activation = "relu", input_shape = c(10000)) %>%
               layer_dense(units = 64, activation = "relu") %>%
               layer_dense(units = 46, activation = "softmax")

      model %>% compile(optimizer = "rmsprop",
                        loss = "categorical_crossentropy",
                        metrics = c("accuracy"))
      val_indices <- 1:1000

      x_val <- x_train[val_indices,]
      partial_x_train <- x_train[-val_indices,]

      y_val <- one_hot_train_labels[val_indices,]
      partial_y_train = one_hot_train_labels[-val_indices,]
      
      history <- model %>% fit(partial_x_train,
                               partial_y_train,
                               epochs = 20,
                               batch_size = 512,
                               validation_data = list(x_val, y_val))
      results <- model %>% evaluate(x_test, one_hot_test_labels)
      results
   plot(history)
   ##
   ##   Overfitting after 10 - cut down epochs
   ##

      model <- keras_model_sequential() %>%
               layer_dense(units = 64, activation = "relu", input_shape = c(10000)) %>%
               layer_dense(units = 64, activation = "relu") %>%
               layer_dense(units = 46, activation = "softmax")

      model %>% compile(optimizer = "rmsprop",
                        loss = "categorical_crossentropy",
                        metrics = c("accuracy"))
      val_indices <- 1:1000

      x_val <- x_train[val_indices,]
      partial_x_train <- x_train[-val_indices,]

      y_val <- one_hot_train_labels[val_indices,]
      partial_y_train = one_hot_train_labels[-val_indices,]
      
      history <- model %>% fit(partial_x_train,
                               partial_y_train,
                               epochs = 9,
                               batch_size = 512,
                               validation_data = list(x_val, y_val))
      results <- model %>% evaluate(x_test, one_hot_test_labels)
      results
   plot(history)
   
   ##
   ##   Estimate the proportion of something..?
   ##
      test_labels_copy <- test_labels
      test_labels_copy <- sample(test_labels_copy)
      length(which(test_labels == test_labels_copy)) / length(test_labels)
      
      predictions <- model %>% predict(x_test)
      head(predictions)

##
##    The Boston Housing Price estimation - regression analysis
##
   dataset <- dataset_boston_housing()
   c(c(train_data, train_targets), c(test_data, test_targets)) %<-% dataset
   
   ##
   ##    Rescale the data to adjust the units
   ##
      mean <- apply(train_data, 2, mean)                                  
      std  <- apply(train_data, 2, sd)
      
      train_data <- scale(train_data, center = mean, scale = std)         
      test_data  <- scale(test_data,  center = mean, scale = std)


      build_model <- function() {
        model <- keras_model_sequential() %>%
                 layer_dense(units = 64, activation = "relu", input_shape = dim(train_data)[[2]]) %>%
                 layer_dense(units = 64, activation = "relu") %>%
                 layer_dense(units = 1)
                 
        model %>% compile(optimizer = "rmsprop",
                          loss      = "mse",
                          metrics   = c("mae"))
      }

   ##
   ##    K-fold validation because the data is small
   ##
      k <- 4
      indices <- sample(1:nrow(train_data))
      folds   <- cut(indices, breaks = k, labels = FALSE)

      num_epochs <- 100
      all_scores <- c()
      
      for (i in 1:k) 
      {
         cat("processing fold #", i, "\n")

         val_indices <- which(folds == i, arr.ind = TRUE)
         val_data <- train_data[val_indices,]

         val_targets <- train_targets[val_indices]
         partial_train_data <- train_data[-val_indices,]                      
         partial_train_targets <- train_targets[-val_indices]

         model <- build_model()                                               

         model %>% fit(partial_train_data, partial_train_targets,             
         epochs = num_epochs, batch_size = 1, verbose = 0)

         results <- model %>% evaluate(val_data, val_targets, verbose = 0)    
         all_scores <- c(all_scores, results$mae)
      }
   all_scores
   mean(all_scores)

##
##
##
   num_epochs <- 500
   all_mae_histories <- NULL
   for (i in 1:k) {
     cat("processing fold #", i, "\n")

     val_indices <- which(folds == i, arr.ind = TRUE)             
     val_data <- train_data[val_indices,]
     val_targets <- train_targets[val_indices]

     partial_train_data <- train_data[-val_indices,]               
     partial_train_targets <- train_targets[-val_indices]

     model <- build_model()                                        

     history <- model %>% fit(                                     
       partial_train_data, partial_train_targets,
       validation_data = list(val_data, val_targets),
       epochs = num_epochs, batch_size = 1, verbose = 0
     )
     mae_history <- history$metrics$val_mae
     all_mae_histories <- rbind(all_mae_histories, mae_history)
   }
   
   average_mae_history <- data.frame(epoch = seq(1:ncol(all_mae_histories)),
                                     validation_mae = apply(all_mae_histories, 2, mean)
   )

   
   library(ggplot2)
      ggplot(average_mae_history, aes(x = epoch, y = validation_mae)) + 
         geom_line()


      ggplot(average_mae_history, aes(x = epoch, y = validation_mae)) +    
         geom_smooth()

   num_epochs <- 125
   all_mae_histories <- NULL
   for (i in 1:k) {
     cat("processing fold #", i, "\n")

     val_indices <- which(folds == i, arr.ind = TRUE)             
     val_data <- train_data[val_indices,]
     val_targets <- train_targets[val_indices]

     partial_train_data <- train_data[-val_indices,]               
     partial_train_targets <- train_targets[-val_indices]

     model <- build_model()                                        

     history <- model %>% fit(                                     
       partial_train_data, partial_train_targets,
       validation_data = list(val_data, val_targets),
       epochs = num_epochs, batch_size = 1, verbose = 0
     )
     mae_history <- history$metrics$val_mae
     all_mae_histories <- rbind(all_mae_histories, mae_history)
   }
