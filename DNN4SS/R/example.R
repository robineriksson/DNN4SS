## Robin Eriksson 2018

##' Example on MNIST dataset
##' All credit to https://tensorflow.rstudio.com/keras/
##'
##' @export
exampleMnist <- function(){
    ## PREPARE THE DATA

    ## The MNIST dataset is included with Keras and can be accessed
    ## using the dataset_mnist() function. Here we load the dataset
    ## then create variables for our test and training data:
    mnist <- keras::dataset_mnist()
    x_train <- mnist$train$x
    y_train <- mnist$train$y
    x_test <- mnist$test$x
    y_test <- mnist$test$y

    ## The x data is a 3-d array (images,width,height) of grayscale
    ## values. To prepare the data for training we convert the 3-d
    ## arrays into matrices by reshaping width and height into a
    ## single dimension (28x28 images are flattened into length 784
    ## vectors). Then, we convert the grayscale values from integers
    ## ranging between 0 to 255 into floating point values ranging
    ## between 0 and 1:

    ## reshape
    x_train <- keras::array_reshape(x_train, c(nrow(x_train), 784))
    x_test <- keras::array_reshape(x_test, c(nrow(x_test), 784))

    ## rescale
    x_train <- x_train / 255
    x_test <- x_test / 255

    ## Note that we use the array_reshape() function rather than the
    ## dim<-() function to reshape the array. This is so that the data
    ## is re-interpreted using row-major semantics (as opposed to R’s
    ## default column-major semantics), which is in turn compatible
    ## with the way that the numerical libraries called by Keras
    ## interpret array dimensions.

    ## The y data is an integer vector with values ranging from 0 to
    ## 9. To prepare this data for training we one-hot encode the
    ## vectors into binary class matrices using the Keras
    ## to_categorical() function:

    y_train <- keras::to_categorical(y_train, 10)
    y_test <- keras::to_categorical(y_test, 10)


    ## DEFINING THE MODEL

    ## We begin by creating a sequential model and then adding layers
    ## using the pipe (%>%) operator:

    model <- keras::keras_model_sequential()
    model %>%
        keras::layer_dense(units = 256, activation = "relu", input_shape = c(784)) %>%
        keras::layer_dropout(rate = 0.4) %>%
        keras::layer_dense(units = 128, activation = "relu") %>%
        keras::layer_dropout(rate = 0.3) %>%
        keras::layer_dense(units = 10, activation = "softmax")

    ## The input_shape argument to the first layer specifies the shape
    ## of the input data (a length 784 numeric vector representing a
    ## grayscale image). The final layer outputs a length 10 numeric
    ## vector (probabilities for each digit) using a softmax
    ## activation function.

    summary(model)

    ## Next, compile the model with appropriate loss function,
    ## optimizer, and metrics:

    model %>% keras::compile(
                  loss ="categorical_crossentropy",
                  optimizer = keras::optimizer_rmsprop(),
                  metrics = c("accuracy")
                  )

    ## TRAINING AND EVALUATION

    ## Use the fit() function to train the model for 30 epochs using
    ## batches of 128 images:

    history <- model %>% keras::fit(
                                    x_train, y_train,
                                    epochs = 30, batch_size = 128,
                                    vaildation_split = 0.2
                                )

    ## The history object returned by fit() includes loss and accuracy
    ## metrics which we can plot:

    plot(history)

    ## Evaluate the model’s performance on the test data:
    model %>% keras::evaluate(x_test, y_test)

    ## Generate predictions on new data
    model %>% keras::predict_classes(x_test)
}


##' MLP for binary classification
##' credit: https://tensorflow.rstudio.com/keras/articles/sequential_model.html
##' @export
exampleBinaryClass <- function(){
    ## generate dummy data
    x_train <- matrix(runif(1000*20), nrow = 1000, ncol = 20)
    y_train <- matrix(round(runif(1000, min = 0, max = 1)), nrow = 1000, ncol = 1)
    x_test <- matrix(runif(100*20), nrow = 100, ncol = 20)
    y_test <- matrix(round(runif(100, min = 0, max = 1)), nrow = 100, ncol = 1)

    ## create model
    model <- keras::keras_model_sequential()

    ## define the model
    model %>%
        keras::layer_dense(units = 64, activation = "relu", input_shape = c(20)) %>%
        keras::layer_dropout(rate = 0.5) %>%
        keras::layer_dense(units = 64, activation = "relu") %>%
        keras::layer_dropout(rate = 0.5) %>%
        keras::layer_dense(units = 1, activation = "sigmoid")

    ## Compile the model
    model %>% keras::compile(
                         loss = "binary_crossentropy",
                         optimizer = "rmsprop",
                         metrics = c("accuracy")
                     )

    ## train
    history <- model %>% keras::fit(x_train, y_train, epochs = 20, batch_size = 128)

    ## evaluate
    score <- model %>% keras::evaluate(x_test, y_test, batch_size = 128)

    print(score)

    return(list(history = history, score = score, model = model))
}

##' mean of normdist Estimator
##'
##' @export
exampleMeanEst <- function(Nsample = 20, Nobs = 1000, Ntest = 100, epochs = 20){
    ## generate dummy data
    ##y_train <- matrix(sample(x=c(1,2,3), size = Nobs, replace = TRUE), nrow = Nobs, ncol = 1)
    y_train <- matrix(runif(Nobs, min = 0, max = 10), nrow = Nobs, ncol = 1)
    x_train <- matrix(rnorm(Nobs*Nsample, mean = y_train, sd = 1), nrow = Nobs, ncol = Nsample)
    ##y_test <- matrix(sample(x=c(1,2,3), size = Nobs, replace = TRUE), nrow = Nobs/10, ncol = 1)
    y_test <- matrix(runif(Ntest, min = 0, max = 10), nrow = Ntest, ncol = 1)
    x_test <- matrix(rnorm(Ntest*Nsample, mean = y_train, sd = 1), nrow = Ntest, ncol = Nsample)


    ##y_train <- keras::to_categorical(y_train)
    ##y_test <- keras::to_categorical(y_test)

    ## create model
    model <- keras::keras_model_sequential()

    ## define the model
    model %>%
        keras::layer_dense(units = 50, activation = "relu", input_shape = c(Nsample)) %>%
        keras::layer_dropout(rate = 0.1) %>%
        keras::layer_dense(units = 50, activation = "relu") %>%
        keras::layer_dropout(rate = 0.1) %>%
        keras::layer_dense(units = 50, activation = "relu") %>%
        keras::layer_dropout(rate = 0.1) %>%
        keras::layer_dense(units = 50, activation = "relu") %>%
        keras::layer_dropout(rate = 0.1) %>%
        keras::layer_dense(units = 50, activation = "relu") %>%
        keras::layer_dropout(rate = 0.1) %>%
        keras::layer_dense(units = 1, activation = "linear")



    ## Compile the model
    model %>% keras::compile(
                         loss = "mse",
                         optimizer = "rmsprop",
                         metrics = c("mae")
                     )


    ## train
    history <- model %>% keras::fit(x_train,
                                    y_train,
                                    epochs = epochs,
                                    batch_size = 100,
                                    vaildation_split = 0.2
                                    )

    ## evaluate
    score <- model %>% keras::evaluate(x_test, y_test, batch_size = 100)

    print(score)

    pred <- model %>% keras::predict_on_batch(x_test)
    df <- data.frame(pred,y_test)

    return(list(history = history, score = score, model = model, df = df))
}
