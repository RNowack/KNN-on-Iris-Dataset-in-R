
# Load and inspect data
data(iris)
view(iris)
summary(iris)
plot(iris)
colnames(iris)

x <- iris[,1:4]
y <- iris[,5]

featurePlot(x=x, y=y, plot="box")


ggplot(iris)+ geom_point(aes(x = Petal.Length, y = Petal.Width), stroke = 2)+
  facet_wrap(~ Species) +labs(x = "Petal Length", y = "Petal Width")+theme_bw()

# Train test split with random sampling
set.seed(123)  # for reproducibility
split_1  <- initial_split(iris, prop = 0.7)
train_1  <- training(split_1)
test_1   <- testing(split_1)

# Create partition
train_partition = createDataPartition(iris$Species, list = FALSE)

# Split data into train and test sets
data_train = data[train_partition,]
data_val = data[-train_partition,]

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

predictions = knn(train, test, cl, params)


# a) linear algorithms
set.seed(7)
fit.lda <- train(Species~., data=train_1, method="lda", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(Species~., data=train_1, method="knn", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Species~., data=train_1, method="rf", metric=metric, trControl=control)

# summarize accuracy of models
results <- resamples(list(lda=fit.lda, knn=fit.knn, rf=fit.rf))
summary(results)

# summarize Best Model
print(fit.lda)

# estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, train_1)
confusionMatrix(predictions, train_1$Species)
