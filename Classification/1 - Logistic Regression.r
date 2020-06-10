library(caret)
library(ggplot2)
library(gridExtra)

# Check working directory
if (basename(getwd()) != "Classification"){
  print("Incorrect working directory, please run from 'Classification' folder")
  quit()
}

set.seed(3060)

# Read in all data from csv
dataset <- read.csv("../Feature Extraction and Analysis/Features/40183333_features.csv", sep='\t')

##########  QUESTION 1.1  ##########
dataset$dummy.living[dataset$label %in% c("banana", "cherry", "flower", "pear")] <- 1
dataset$dummy.living[dataset$label %in% c("envelope", "golfclub", "pencil", "wineglass")] <- 0

# TODO: Consider using Living and Non-Living instead of 1 & 0, then factoring and releveling

training_data <- dataset

# plt <- ggplot(training_data, aes(x=verticalness, fill=as.factor(dummy.living))) +
#   geom_histogram(binwidth=.2, alpha=.5, position='identity')
# plt 

glmfit <- glm(dummy.living ~ verticalness, 
            data = training_data, 
            family = 'binomial') 
png("../report_images/section_1/q1_glm_result_coefficients.png")
grid.table(round(summary(glmfit)$coefficients,4))
dev.off()

x.values <- seq(max(training_data$verticalness),
                min(training_data$verticalness),
                length.out=1000)

predicted.curve <- data.frame(verticalness = x.values)
predicted.curve$dummy.living = predict(glmfit, predicted.curve, type="response")

plt <-ggplot(training_data, aes(x=verticalness, y=dummy.living)) + 
  geom_point(aes(colour = factor(dummy.living,labels=c("Non-Living","Living"))), position="dodge") +
  geom_line(data=predicted.curve, colour="purple", size=1) +
  labs(colour="Image Class") +
  ggtitle("Predicted Curve of the Label~Verticalness Classifier")
plt
ggsave('../report_images/section_1/q1_verticalness_predicted_curve.png',dpi=600)


# "Zoomed out" equivalent:

x.values <- seq(-2,3,length.out=1000) # To see curve better
predicted.curve <- data.frame(verticalness = x.values)
predicted.curve$dummy.living = predict(glmfit, predicted.curve, type="response")
plt <-ggplot(training_data, aes(x=verticalness, y=dummy.living)) + 
  geom_point(aes(colour = factor(dummy.living,labels=c("Non-Living","Living"))), position="dodge") +
  geom_line(data=predicted.curve, colour="purple", size=1) +
  labs(colour="Image Class") +
  ggtitle("Predicted Curve of the Label~Verticalness Classifier (Larger X Range)")
plt
ggsave('../report_images/section_1/q1_verticalness_predicted_curve_zoomout.png',dpi=600)

remove(x.values)



##########  QUESTION 1.2  ##########

 # 0.5 cutoff:
training_data$pred.class = ifelse(predict(glmfit, training_data, type="response")>0.5,1,0)

accuracy <- mean(training_data$pred.class == training_data$dummy.living)

# Draw Tables
png("../report_images/section_1/q2_verticalness_predicted_curve_accuracy.png")
grid.table(data.frame(Accuracy=accuracy,row.names=c("")))
dev.off()


##########  Question 1.3  ##########

dataset_shuffled <- dataset[sample(nrow(dataset)),]

kfolds <- 5
dataset_shuffled$folds <- cut(seq(1,nrow(dataset_shuffled)),breaks=kfolds,labels=FALSE)

accuracy_values <- rep(NA, kfolds)

for(i in 1:kfolds) 
{
  train_this_fold  = dataset_shuffled[dataset_shuffled$folds != i,] 
  test_this_fold = dataset_shuffled[dataset_shuffled$folds == i,]
  
  glmfit <- glm(dummy.living ~ width + cols_with_5 + hollowness, # nr_pix instead of width
            data = train_this_fold, 
            family = 'binomial')

  test_this_fold$pred.val = predict(glmfit, test_this_fold, type="response")
  test_this_fold$pred.class = ifelse(test_this_fold$pred.val > 0.5,1,0)

  accuracy <- mean(test_this_fold$pred.class == test_this_fold$dummy.living)
  accuracy_values[i] <- accuracy
  print(accuracy)
}

mean(accuracy_values)

# Confirm results with caret
train_control <- trainControl(method="cv", number=5)
model <- train(factor(dummy.living) ~ width + cols_with_5 + hollowness,
            data=training_data,
            trControl=train_control,
            method="glm",
            family='binomial')
model$results[2]


# Loop over all 1140 possible permutations - takes a few minutes:

# formulae <- combn(colnames(dataset_shuffled)[3:22],3)
# formulae <- apply(formulae,2, function(x)  paste("dummy.living ~ ", paste(x,collapse=" + "),sep=""))

# training_data$dummy.living <- as.factor(training_data$dummy.living)

# formula_accuracies <- rep(NA, length(formulae))

# for (i in seq_along(formulae))
# {
#     model <- train(formula(formulae[i]),
#             data=training_data,
#             trControl=train_control,
#             method="glm",
#             family='binomial')

#     formula_accuracies[i] <- model$results[,2]
#     print(i)
# }

# # Create df with the accuracy of each formula
# accuracies <- data.frame(formulae, accuracy=formula_accuracies)
# accuracies <- accuracies[order(-accuracies$accuracy),]

# write.csv(accuracies, "../report_images/best_3_features.csv")



##########  Question 1.4  ##########


predictions <- data.frame(samples=c(0:160), probabilityCorrect = pbinom(c(0:160), 160, 0.5, lower.tail=F), model="Random")
predictions <- rbind(predictions, data.frame(samples=c(0:160), probabilityCorrect = pbinom(c(0:160), 160, 0.925, lower.tail=F), model="GLM"))

ggplot(predictions, aes(x=samples, y=probabilityCorrect, color=model, group=model)) + 
      geom_point() +
      geom_line() + 
      xlab("Correct Predictions") +
      ylab ("Probability") +
      ggtitle("Probability of getting X or More Correct Predictions From 160 Samples")
ggsave("../report_images/section_1/q4_random_and_glm.png", dpi=300)


##########  Question 1.5  ##########

dataset$pred.class = ifelse(predict(model, dataset, type="prob")[2] > 0.5, 1, 0)

living_fp <- paste(mean(dataset$pred.class == 0 & dataset$dummy.living == 1)*100, '%')
non_living_fp <- paste(mean(dataset$pred.class == 1 & dataset$dummy.living == 0)*100, '%')

png("../report_images/section_1/q5_false_positive_rate.png")
grid.table(data.frame(group=c("Non-Living","Living"), incorrect=c(non_living_fp,living_fp)))
dev.off()
