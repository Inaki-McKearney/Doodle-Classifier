library(caret)
library(class)
library(ggplot2)
library(plyr)
library(reshape2)

# Check working directory
if (basename(getwd()) != "Classification"){
  print("Incorrect working directory, please run from 'Classification' folder")
  quit()
}

set.seed(3060)

# Read in all data from csv
feature_files <- dir(path="doodle_data/feature_files/", pattern = "\\.csv$", full.names = T)
dataset <- ldply(feature_files,read.table,sep="\t")
colnames(dataset) <- c("label", "index", "nr_pix", "height", "width", "span", "rows_with_5", "cols_with_5", "neigh1", "neigh5", "left2tile", "right2tile", "verticalness", "top2tile", "bottom2tile", "horizontalness", "left_diagonals", "right_diagonals", "nr_regions", "max_pixels")



##########  Q2.1  ##########

train.X <- as.data.frame(lapply(dataset[,c(3:10)], scale))

train.label <- dataset$label

ks = seq(1,59,2)

accuracies <- data.frame()

for (k in ks){
  knn.pred=knn(train.X,train.X,train.label,k=k)
  accuracies <- rbind(accuracies, data.frame(k, errorType="Training Errors", accuracy=mean(mean(knn.pred==train.label))))
}

print(accuracies[,c(1,3)])

accuracy_plt <- ggplot(accuracies,aes(x=k, y=accuracy)) + 
                        geom_line(colour="purple") + 
                        geom_point(size=3) +
                        ggtitle("Effect of k on KNN Classifier Accuracy Over the Training Set")
accuracy_plt
ggsave('../report_images/section_2/q1_knn_accuracy_overfit.png',dpi=600)



##########  Q2.2  ##########

objects <- dataset[sample(nrow(dataset)),]
features = c("nr_pix", "height", "width", "span", "rows_with_5", "cols_with_5", "neigh1", "neigh5")

kfolds <- 5
objects$folds <- cut(seq(1,nrow(objects)),breaks=kfolds,labels=FALSE)

objects[features] <- lapply(objects[features], scale)

ks = seq(1,59,2)

for(k in ks){
  fold_test_accuracies <- rep(NA, kfolds)
  for(fold in 1:kfolds) 
  {
    train  = objects[objects$folds != fold,] 
    test = objects[objects$folds == fold,]
    
    knn.pred = knn(train[,features], 
                      test[,features], 
                      train$label, k=k)
    fold_test_accuracies[fold] <- mean(knn.pred == test$label)
  }
  accuracies <- rbind(accuracies, data.frame(k, errorType="Testing Errors", accuracy=mean(fold_test_accuracies)))
}
accuracies

bayes_error_estimate <- 1-max(accuracies[accuracies$errorType == "Testing Errors",]$accuracy)

training_error <- ggplot(accuracies,aes(x=1/k, y=1-accuracy, colour=errorType)) + 
                        geom_hline(yintercept=bayes_error_estimate, linetype="dashed", color = "purple") +
                        geom_line() + 
                        geom_point(size=3) + 
                        ylab("Error Rate") +
                        ggtitle("KNN Classification Error Rate")
training_error
ggsave('../report_images/section_2/q2_knn_error_rate.png')



##########  Q2.3  ##########

# Get best k from previous results:
best_k <- accuracies[accuracies$errorType=="Testing Errors",]
best_k <- best_k[which.max(best_k$accuracy),1]

objects <- dataset[sample(nrow(dataset)),]
features = c("nr_pix", "height", "width", "span", "rows_with_5", "cols_with_5", "neigh1", "neigh5")

objects[features] <- lapply(objects[features], scale)

sample_size <- floor(0.8 * nrow(objects))

train_index <- sample(seq_len(nrow(objects)), size=sample_size)

train <- objects[train_index, ]
test <- objects[-train_index, ]

train_control <- trainControl(method="cv", number=5)
knn.fit <- train(label ~ nr_pix + height + width + span + rows_with_5 + cols_with_5 + neigh1 + neigh5,
              data=train,
              trControl=train_control,
              tuneGrid=data.frame(k=best_k),
              method="knn")

knn.pred <- predict(knn.fit, newdata=test)

confusionMatrix(knn.pred, test$label)
table(knn.pred, test$label)
mean(knn.pred == test$label)

t <- melt(table(prediction=knn.pred, result=test$label), value.name="count")
t$tile_count <- ifelse(t$count==0,'',t$count) # Show zeroes?
t$count <- ifelse(t$prediction==t$result,0,t$count)

# Inspiration: https://stackoverflow.com/a/53612391
ggplot(t, aes(x=prediction, y=result, fill=count)) +
        geom_tile(color="grey") +
        theme_bw() +
        coord_equal() +
        scale_fill_gradient("count",low="white", high="red") +
        guides(fill=F) +
        ggtitle("Heatmap of Confusion Matrix of Doodle Prediction Errors") +
        geom_text(aes(label=tile_count), color="black")
ggsave('../report_images/section_2/q3_knn_confusion_matrix_heatmap.png')
