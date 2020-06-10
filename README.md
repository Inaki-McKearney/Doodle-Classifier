# Doodle Classifier

A project (based on an assignment) to learn the fundamentals of data analytics and artificial intelligence by developing a basic image classifier from the ground up.

## Work Process

### Feature Extraction

To obtain useful information for discriminating between 8 types of doodles, I produced a feature vector. This involved:

- Drawing a dataset of 160 doodles
- Naming, categorising and converting the doodles with Python
- Using Python to extract 20 unique features for each doodle and export the results to a CSV file

### Analysis

I performed statistical analysis to determine the most informative and discriminating features for my dataset. This included:

- Creating and analysing feature histograms
- Calculating various summary statistics
- Investigated the skewness (and corrective transformations) of the feature values
- Determined and visualised the correlation between various feature variables.
- Performed ANOVA to evaluate the effectiveness of each feature in discriminating between image groups
- Performed permutation tests, t-tests and pairwise t-tests to provide greater understanding of the ANOVA results

### Classification

Fitting discriminative models to create accurate classifiers involved:

- Fitting a **Logistic Regression** models and **Generalised Linear Regression** models using certain features
- Using a larger dataset to create a **k-Nearest Neighbour** classifier
- Investigating the effect _k_ has on the overall (cross-validated) error rate of the classifier on my dataset
- Producing a **confusion matrix** to better visualise the types of error made by the classifier
- Performing classification using **Bootstrap Aggregation** of **Decision Trees**
- Performing classification with **Random Forests** and tuned the appropriate hyper-parameters
- Plotting informative **Variable Importance Plots**

## Results Achieved

The optimum results achieved in this experiment for this particular set of images was **85%** by using **Random Decision Forests** with only **7 of the 20 available features**
