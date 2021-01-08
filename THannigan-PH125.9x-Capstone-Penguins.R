# Install packages and libraries

install.packages("palmerpenguins")
install.packages("ellipse")
install.packages("lsr")
install.packages("explore")

library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(lsr)
library(randomForest)
library(palmerpenguins)
library(explore)
library(dplyr)
library(purrr)
library(rpart)
library(e1071)

# Get data

data(package = 'palmerpenguins')

#review data

summary(penguins)

# There are some NA's in the data.

# Remove entries that include an NA in any of the variable columns
# Remove columns that are not pertinent to simplify dataset

wpenguins <- penguins[!is.na(penguins$bill_length_mm) | !is.na(penguins$bill_depth_mm) | !is.na(penguins$flipper_length_mm) | !is.na(penguins$body_mass_g),]
simplep <- wpenguins %>% select(length=bill_length_mm, depth=bill_depth_mm, flipper=flipper_length_mm, body=body_mass_g, species)

# Confirm all NA's for variables are removed

summary(!is.na(simplep))
head(simplep)

# Create training and test set at 20% partition.

set.seed(68, sample.kind="Rounding") 
trainIndex <- createDataPartition(simplep$species, p=0.8, list=FALSE)
data_train <- simplep[trainIndex,]
data_test <- simplep[-trainIndex,]

summary(data_train)
summary(data_test)

# Graph relationships in training data

x <- data_train[, 1:4]
y <- factor(data_train[,5])

par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(x)[i])
}

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales, auto.key = list(columns = 3))


featurePlot(x, y, plot = "ellipse", auto.key = list(columns = 3))

#Generate several fit models to estimate accuracy

# CART

set.seed(68, sample.kind="Rounding") 
cart_fit <- train(species~., data=data_train, method="rpart")
cart_fit

cart_model <- rpart(species~.,data = data_train)
plot(cart_model, compress = TRUE, branch = 1)
text(cart_model, digits = 3)

cart_acc <- confusionMatrix(predict(cart_fit, data_test), data_test$species)$overall["Accuracy"]
cart_acc

# Random Forest

set.seed(68, sample.kind="Rounding")
rf_fit <- randomForest(species ~ ., data=data_train)
rf_fit

plot(rf_fit)

rf_acc <- confusionMatrix(predict(rf_fit, data_test), data_test$species)$overall["Accuracy"]
rf_acc

# LDA

set.seed(68, sample.kind="Rounding")
lda_fit <- train(species~., data=data_train, method="lda")
lda_fit

lda_acc <- confusionMatrix(predict(lda_fit, data_test), data_test$species)$overall["Accuracy"]
lda_acc

# QDA

set.seed(68, sample.kind="Rounding")
qda_fit <- train(species~., data=data_train, method="qda")
qda_fit

qda_acc <- confusionMatrix(predict(qda_fit, data_test), data_test$species)$overall["Accuracy"]
qda_acc

# SVM

# With radial kernel
set.seed(68, sample.kind="Rounding")
svm_fit_r <- svm(species~., data=data_train, kernel='radial')
svm_fit_r

svm_acc_r <- confusionMatrix(predict(svm_fit_r, data_test), data_test$species)$overall["Accuracy"]
svm_acc_r

# With polynomial kernel
set.seed(68, sample.kind="Rounding")
svm_fit_p <- svm(species~., data=data_train, kernel='polynomial')
svm_fit_p

svm_acc_p <- confusionMatrix(predict(svm_fit_p, data_test), data_test$species)$overall["Accuracy"]
svm_acc_p


# summarize accuracy of models

acc_results <- tibble(method = c("CART", "RF", "LDA", "QDA", "SVM_R", "SVM_P"), accuracy = c(cart_acc, rf_acc, lda_acc, qda_acc, svm_acc_r, svm_acc_p))

acc_results %>% knitr::kable()

acc_results %>% ggplot(aes(method,accuracy)) + 
  geom_col(color = "black", fill = "gold") +
  geom_text(aes(label = format(accuracy, digits = 7)), nudge_y = 0.005) +
  coord_cartesian(ylim=c(0.9, 1.0)) +
  geom_abline(slope=0, intercept=0.98,  col = "red",lty=2) +
  ggtitle("Accuracy by Method") +
  theme(plot.title = element_text(hjust = 0.5))



# Citations

#HarvardX PH125.9x Capstone Course: https://www.edx.org/professional-certificate/harvardx-data-science

#Textbook:  “Introduction to Data Science”, by Rafael A. Irizarry: https://rafalab.github.io/dsbook/

citation("palmerpenguins")
#> 
#> To cite palmerpenguins in publications use:
#> 
#>   Horst AM, Hill AP, Gorman KB (2020). palmerpenguins: Palmer
#>   Archipelago (Antarctica) penguin data. R package version 0.1.0.
#>   https://allisonhorst.github.io/palmerpenguins/. doi:
#>   10.5281/zenodo.3960218.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {palmerpenguins: Palmer Archipelago (Antarctica) penguin data},
#>     author = {Allison Marie Horst and Alison Presmanes Hill and Kristen B Gorman},
#>     year = {2020},
#>     note = {R package version 0.1.0},
#>     doi = {10.5281/zenodo.3960218},
#>     url = {https://allisonhorst.github.io/palmerpenguins/},
#>   }