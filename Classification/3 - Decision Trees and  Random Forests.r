library(caret)
library(ipred)
library(ggplot2)
library(gridExtra)
library(plyr)
library(ranger)
library(rpart)
library(vip)

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

f <- as.formula(label ~ nr_pix + height + width + span + rows_with_5 + cols_with_5 + neigh1 + neigh5)


#####  Q3.1  #####

number_bags = c(25, 50, 200, 400, 800)

oob_accuracies <- rep(NA, length(number_bags))
cv_accuracies <- rep(NA, length(number_bags))


#Takes 10 mins+
for (i in seq_along(number_bags))
{
    n_bags <- number_bags[i]

    oob_bag_model <- bagging(
    formula = f,
    data = dataset,
    nbagg = n_bags,  
    coob = TRUE,
    control = rpart.control(minsplit = 2, cp = 0)
    )

    cv_bag_model <- train(
    f,
    data = dataset,
    method = "treebag",
    trControl = trainControl(method = "cv", number = 5),
    nbagg = n_bags,  
    control = rpart.control(minsplit = 2, cp = 0)
    )

    oob_accuracies[i] <- 1-oob_bag_model$err
    cv_accuracies[i] <- cv_bag_model$result[,2]
}

bagging_accuracies<-data.frame(bags=number_bags, oob=oob_accuracies, cv=cv_accuracies)

png("../report_images/section_3/q1_bagging_accuracies.png")
grid.table(bagging_accuracies)
dev.off()

bagging_accuracies <- data.frame(
    bags=c(number_bags,number_bags),
    errorEstimate=c(rep("oob",length(oob_accuracies)),rep("cv",length(cv_accuracies))),
    accuracies=c(oob_accuracies,cv_accuracies))

bag_accuracy_plot <- ggplot(bagging_accuracies,aes(x=bags, y=accuracies, colour=errorEstimate)) + 
                        geom_line() + 
                        geom_point(size=3) +
                        ylim(0.81,0.84)
bag_accuracy_plot

ggsave("../report_images/section_3/q1_bagging_accuracies_plot.png")

# vip(cv_bag_model, num_features=8, bar = F) #variable importance plot
# ggsave("../report_images/section_3/q1_bagging_feature_importance_plot.png",dpi=600)


#####  Q3.2  #####

tgrid <- expand.grid(mtry=c(2, 4, 6, 8),
                     splitrule = "gini",#, "extratrees"),
                     min.node.size = 1,
                     trees = seq(25,400,25), #Not passed to caret directly
                     accuracy = NA) #For me to record each accuracy result

for (i in seq_len(nrow((tgrid))))
{
    model_caret <- train(f,
                    data = dataset,
                    method = "ranger",
                    trControl = trainControl(method="cv", number = 5, classProbs = T),
                    tuneGrid = tgrid[i,1:3],
                    num.trees = tgrid$trees[i])

    tgrid[i,"accuracy"] <- model_caret$results[4]
}

ggplot(tgrid[order(tgrid$accuracy),],aes(x=trees, y=accuracy, colour=factor(mtry))) + 
                        geom_line() +
                        scale_y_continuous(breaks = round(seq(min(tgrid$accuracy), max(tgrid$accuracy), by = 0.001),3))

ggsave("../report_images/section_3/q2_random_forest_tuning_plot.png")



#####  Q3  #####

best_params <- tgrid[which.max(tgrid$accuracy),-5]

tgrid <- best_params[,-4]


model <- train(f,
               data = dataset,
               method = "ranger",
               trControl = trainControl(method="repeatedcv", number = 5, repeats=20),
               tuneGrid = tgrid,
               num.trees = best_params$tree)

accuracy <- data.frame(model$results[4], round(model$results[6],5))

png("../report_images/section_3/q3_20_repeat_cv_accuracy.png")
grid.table(accuracy, rows="",cols=c("Accuracy", "SD"))
dev.off()




#####  Q4 #####


# Grid of ideal parameters
# tgrid <- expand.grid(mtry=2,
#                      splitrule = "gini",
#                      min.node.size = 1)

# Get variable importance with Mean Decrease in Impurity
rf_impurity <- train(f,
               data = dataset,
               method = "ranger",
               trControl = trainControl(method="repeatedcv", number = 5, repeats=20),
               tuneGrid = tgrid,
               num.trees = best_params$tree,
               importance = "impurity")
               
# Get variable importance with Mean Decrease in Accuracy
rf_permutation <- train(f,
               data = dataset,
               method = "ranger",
               trControl = trainControl(method="repeatedcv", number = 5, repeats=20),
               tuneGrid = tgrid,
               num.trees = best_params$tree,
               importance = "permutation")

p1 <- ggplot(varImp(rf_impurity)) + ylab("Importance (Impurity)")
p2 <- ggplot(varImp(rf_permutation)) + ylab("Importance (Permutation)")

grid.arrange(p1, p2, nrow = 1)
g <- arrangeGrob(p1, p2, nrow = 1)
ggsave("../report_images/section_3/q4_var_important_plot.png",plot=g,dpi=600)


# Create 7 formulae with different feature missing from each
formulae <- combn(colnames(dataset)[3:10],7)
formulae <- apply(formulae,2, function(x)  paste("label ~ ", paste(x,collapse=" + "),sep=""))

formula_accuracies <- rep(NA, length(formulae))

for (i in seq_along(formulae))
{
    model <- train(as.formula(formulae[i]),
                data = dataset,
                method = "ranger",
                trControl = trainControl(method="repeatedcv", number = 5, repeats=20),
                tuneGrid = tgrid,
                num.trees = best_params$tree)

    formula_accuracies[i] <- model$results[,4]
}

# Create df with the accuracy of each formula
accuracies <- data.frame(missing=rev(colnames(dataset)[3:10]), accuracy=formula_accuracies)
accuracies <- accuracies[order(-accuracies$accuracy),]

png("../report_images/section_3/q4_accuracy_different_formulae.png")
grid.table(accuracies, rows=rep("",length(accuracies[,1])),cols=c("Missing", "Accuracy"))
dev.off()


mod_model <- train(label ~ nr_pix + height + width + rows_with_5 + cols_with_5 + neigh1 + neigh5,
                   data = dataset,
                   method = "ranger",
                   trControl = trainControl(method="repeatedcv", number = 5, repeats=20),
                   tuneGrid = tgrid,
                   num.trees = best_params$tree)
mod_model$results[4]

png("../report_images/section_3/q4_missing_feature_accuracy.png")
grid.table(data.frame(accuracy=mod_model$results[,4]), rows=c())
dev.off()