library(ggplot2)
library(gridExtra)
library(e1071)
library(DescTools)

# Check working directory
if (basename(getwd()) != "Analysis"){
  print("Incorrect working directory")
  quit()
}

set.seed(3060)
  
# Read in all data from csv
all_data <- read.csv("../Features/40183333_features.csv", sep='\t')

# Define living and non_living subsets of all_data dataframe
living_data <- all_data[all_data$label %in% c("banana", "cherry", "flower", "pear"),]
non_living_data <- all_data[all_data$label %in% c("envelope", "golfclub", "pencil", "wineglass"),]


### QUESTION 1 ###
# Number of pixels Histograms
ggplot(all_data,aes(x=nr_pix)) +
  geom_histogram(binwidth = 10,color = "darkblue", fill="lightblue") +
  ggtitle("Number of Black Pixels in All Images")
ggsave("report_images/q1_nr_pix_all.png")

ggplot(living_data,aes(x=nr_pix)) +
  geom_histogram(binwidth = 10, color = "darkblue", fill="lightblue") +
  ggtitle("Number of Black Pixels in Living Images")
ggsave("report_images/q1_nr_pix_living.png")

ggplot(non_living_data,aes(x=nr_pix)) +
  geom_histogram(binwidth = 10, color="darkblue", fill="lightblue") +
  ggtitle("Number of Black Pixels in Non-Living Images")
ggsave("report_images/q1_nr_pix_non_living.png")

# Height Histograms 
ggplot(all_data,aes(x=height)) +
  geom_histogram(binwidth = 1, color="darkblue", fill="lightblue") +
  ggtitle("Height in All Images")
ggsave("report_images/q1_height_all.png")

ggplot(living_data,aes(x=height)) +
  geom_histogram(binwidth = 1, color="darkblue", fill="lightblue") +
  ggtitle("Height in Living Images")
ggsave("report_images/q1_height_living.png")

ggplot(non_living_data,aes(x=height)) +
  geom_histogram(binwidth = 1, color="darkblue", fill="lightblue") +
  ggtitle("Height in Non-Living Images")
ggsave("report_images/q1_height_non_living.png")

# Columns With 5 or More Pixels Histograms 
ggplot(all_data,aes(x=cols_with_5)) +
  geom_histogram(binwidth = 2, color="darkblue", fill="lightblue") +
  ggtitle("Columns With 5 or More Pixels in All Images")
ggsave("report_images/q1_cols_with_5_all.png")

ggplot(living_data,aes(x=cols_with_5)) +
  geom_histogram(binwidth = 2, color="darkblue", fill="lightblue") +
  ggtitle("Columns With 5 or More Pixels in All Living Images")
ggsave("report_images/q1_cols_with_5_living.png")

ggplot(non_living_data,aes(x=cols_with_5)) +
  geom_histogram(binwidth = 1, color="darkblue", fill="lightblue") +
  ggtitle("Columns With 5 or More Pixels in Non-Living Images")
ggsave("report_images/q1_cols_with_5_non_living.png")



### QUESTION 2 ###
# Means
all_data_means <- round(sapply(all_data[3:22], mean), 3)
living_data_means <- round(sapply(living_data[3:22], mean), 3)
non_living_data_means <- round(sapply(non_living_data[3:22], mean), 3)

# Standard Deviations
all_data_sd <- round(sapply(all_data[3:22], sd), 3)
living_data_sd <- round(sapply(living_data[3:22], sd), 3)
non_living_data_sd <- round(sapply(non_living_data[3:22], sd), 3)

# Interquartile Ranges
all_data_IQRs <- round(sapply(all_data[3:22], IQR), 3)
living_data_IQRs <- round(sapply(living_data[3:22], IQR), 3)
non_living_data_IQRs <- round(sapply(non_living_data[3:22], IQR), 3)

# Draw Tables
png("report_images/q2_means.png")
col_names = c("Mean - All","Mean - Living", "Mean - Non Living")
df <- cbind(all_data_means, living_data_means, non_living_data_means)
grid.table(df,cols=col_names)
dev.off()

png("report_images/q2_stdevs.png")
col_names = c("SD - All","SD - Living", "SD - Non Living")
df <- cbind(all_data_sd,living_data_sd, non_living_data_sd)
grid.table(df,cols=col_names)
dev.off()

png("report_images/q2_iqrs.png")
col_names = c("IQR - All","IQR - Living", "IQR - NonLiving")
df <- cbind(all_data_IQRs, living_data_IQRs, non_living_data_IQRs)
grid.table(df,cols=col_names)
dev.off()

# Create copy of all_data but with only "Living" or "Non-Living" labels
modified_data <- all_data
modified_data$label <- as.character(modified_data$label)
modified_data$label[modified_data$label %in% c("banana", "cherry", "flower", "pear")] <- "Living"
modified_data$label[modified_data$label %in% c("envelope", "golfclub", "pencil", "wineglass")] <- "Non-Living"

ggplot(modified_data,aes(y=cols_with_5, x = label, fill = label)) +
  geom_boxplot(show.legend = FALSE) +
  ggtitle("Columns with 5 or more pixels in Living and Non-Living Images")
ggsave("report_images/q2_cols_with_5_box.png")

ggplot(modified_data,aes(x=neigh5, fill = label)) + geom_histogram(color = "black", binwidth = 5) +
  ggtitle("Pixels with 5 or more neighbours or more pixels in Living and Non-Living Images")
ggsave("report_images/q2_neigh5_hist.png")

ggplot(modified_data,aes(y=nr_regions, x = label, fill = label)) +
  geom_boxplot(show.legend = FALSE) +
  ggtitle("Number of Regions in Living and Non-Living Images")
ggsave("report_images/q2_nr_regions_box.png")

rm(modified_data)

#ggplot(all_data,aes(x=label, y=nr_pix,fill = label)) + geom_boxplot() + ggtitle("Number of Black Pixels in All Images")
#ggplot(all_data,aes(x=nr_pix, fill = label)) + geom_histogram(binwidth = 5, color="black") + ggtitle("Number of Black Pixels in All Images")
#ggplot(living_data,aes(x=nr_regions, fill = label)) + geom_histogram(binwidth = 1) + geom_point(aes(y=nr_regions)) + geom_histogram(data=non_living_data, binwidth = 1)


### QUESTION 3 ###
ggplot(all_data,aes(x = nr_pix)) +
  geom_histogram(aes(y=..density.., fill=..count..),
                 binwidth = 10,
                 color='black') +
  xlim(-10,280) +
  stat_function(fun = dnorm, color = 'red', args = list(mean = mean(all_data$nr_pix), 
          sd = sd(all_data$nr_pix)),
          size = 1) +
  ggtitle("Histogram of Number of Black Pixels in All Images","and Theoretical Normal Distribution")
ggsave("report_images/q3.png")


### QUESTION 4 ###
qnorm(0.95, mean(all_data$nr_pix), sd(all_data$nr_pix))

### QUESTION 5 ###
sapply(all_data[3:16],skewness)

features <- c()
old_skews <- c()
new_skews <- c()

#Create before and after transofrmation histograms of skewed features
for (feature in names(all_data[3:16])){
  if (abs(skewness(all_data[,feature])) >= 1){
    old_s <- round(skewness(all_data[,feature]),3)
    new_s <- round(skewness(log(all_data[,feature]+1)),3)
    png(paste("report_images/q5_", feature, "_before.png"))
    hist(all_data[,feature], main=paste("Before transformation - ",feature), xlab=feature, col="orange") 
    dev.off()
    
    png(paste("report_images/q5_", feature, "_after.png"))
    hist(log(all_data[,feature]+1), main=paste("After transformation - ",feature), xlab=paste("log(",feature,"+1)"), col="cyan") 
    dev.off()
  
    features <- append(features, feature)
    old_skews <- append(old_skews, old_s)
    new_skews <- append(new_skews, new_s)
  }
}

# Create table of above results
df <- cbind(features, old_skews, new_skews)
png("report_images/q5_transformation_table.png")
grid.table(df, cols=c("Features", "Skewness of X", "Skewness of log(X+1)"))
dev.off()


#ggplot(all_data,aes(x=neigh5, fill = label)) + geom_histogram(binwidth = 5, color="black") + ggtitle("Number of Black Pixels in All Images")
#ggplot(all_data,aes(x=log(neigh5+1), fill = label)) + geom_histogram(binwidth = .5, color="black") + ggtitle("Number of Black Pixels in All Images")
#ggplot(all_data,aes(x=neigh5^(1/log(max(neigh5))), fill = label)) + geom_histogram(binwidth = .5, color="black") + ggtitle("Number of Black Pixels in All Images")


### QUESTION 6 ###
ggplot(all_data,aes(x=height, y=span)) + geom_point() + geom_smooth(method='lm') + ggtitle("Scatter Plot of Height and Span")
ggsave("report_images/q6.png")
#TODO Hypothesis test?
cor.test(all_data$height,all_data$span, method='pearson')
cor(all_data$height,all_data$span, method='pearson')


### QUESTION 7 ###
#Pre-ANOVA visualisation
ggplot(non_living_data,aes(x=label, y=nr_pix, fill=label)) + 
  geom_dotplot(binaxis = 'y', stackdir = 'center') +
  coord_flip() +
  ggtitle("Dotplot of Number of Black Pixels in Non-Living Data")
ggsave("report_images/q7_dotplot.png")

ggplot(non_living_data, aes(x = label, y = nr_pix, fill = label)) +
  geom_boxplot() +
  geom_jitter() +
  coord_flip() +
  ggtitle("Boxplot of Number of Black Pixels in Non-Living Data")
ggsave("report_images/q7_boxplot.png")

png("report_images/q7_table.png")
col_names = c("label","n", "mean", "sd")
q7_columns <- c("wineglass", "golfclub", "pencil", "envelope")

n <- c()
means <- c()
stdevs <- c()

# Get n, mean & sd of nr_pix for each non-living image group:
for (label in q7_columns) {
  n <- append(n, length(non_living_data[non_living_data$label == label,"nr_pix"]))
  means <- append(means, mean(non_living_data[non_living_data$label == label,"nr_pix"]))
  stdevs <- append(stdevs, sd(non_living_data[non_living_data$label == label,"nr_pix"]))
}

# Append overall n, mean and sd for nr_pix
n <- append(n, sum(sapply(n,sum)))
means <- append(means, mean(sapply(means,sum)))
stdevs <- append(stdevs, sd(non_living_data$nr_pix))

df <- cbind(append(q7_columns, "OVERALL"),n,means, stdevs)
grid.table(df,
           cols=col_names,
           theme=ttheme_default(
             core=list(fg_params=list(fontface=c(rep("plain", 4), "bold")),
                       bg_params=list(fill=c(rep(c("grey95", "grey90"), length.out=4), "#D4D6FC")))))
dev.off()


anova_nr_pix <- aov(nr_pix~label, data = non_living_data)
summary(anova_nr_pix)

pairwise.t.test(non_living_data$nr_pix, non_living_data$label,  data=non_living_data, p.adj = "bonferroni")


### QUESTION 8 ###
anova_hollowness <- aov(hollowness~label, data = non_living_data)
f_stat <- summary(anova_hollowness)[[1]][["F value"]][1]

f_values <- c()
n <- 100000#00
for (i in 1:n){
  shuffled_df = transform(non_living_data,hollowness=sample(hollowness))
  anova_hollowness <- aov(hollowness~label, data = shuffled_df)
  f_values <- append(f_values, summary(anova_hollowness)[[1]][["F value"]][1])
}

df <- data.frame(f_values)
ggplot(df, aes(x=df$f_values)) +
  geom_histogram(binwidth = 0.5,color="black", aes(y = ..density.., fill = ..count..)) +
  scale_fill_gradient("count",low="#eb4034", high="#ffd2c9") +
  ggtitle(paste("Randomsied F Values for ", n, " Iterations")) +
  xlab("F Value") +
  stat_function(fun = function(x) df(x,df1 = 3, df2 = 76), size = 1)
ggsave("report_images/q8.png")
dev.off()


### QUESTION 9 ###

features <- c()
p_values <- c()

for (feature in names(all_data)[3:22]){
  t <- t.test(living_data[,feature],non_living_data[,feature])
  features <- append(features, feature)
  p_values <- append(p_values,t$p.value)
}

png("report_images/q9_ttest.png")
df <- cbind(features,p_values)
grid.table(df,cols=c("Feature","P Value"))
dev.off()


### QUESTION 10 ###
features <- c()
f_values <- c()

for (feature in names(all_data)[4:8]){
  anova_result <- aov(all_data[,feature]~label, data = all_data)
  f_value <-round(summary(anova_result)[[1]][["F value"]][1], 3)
  
  if ((length(f_values) > 0) && (f_value > max(f_values))){
    highest_feature_f <- feature
    p_value <- summary(anova_result)[[1]][["Pr(>F)"]][1]
  }
  
  features <- append(features, feature)
  f_values <- append(f_values, f_value)
}

png("report_images/q10_aov.png")
df <- cbind(features, f_values)
grid.table(df,cols=c("Feature","F Value"))
dev.off()

p_value # of highest f value feature

pairwise.t.test(all_data[,highest_feature_f],all_data$label,  data=all_data, p.adj = "bonferroni")
