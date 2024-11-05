rm(list=ls())
library(dplyr)
library(dendextend)
library(RColorBrewer)
library(reshape2)
library(ggthemes)
library(GGally)
library(hdrcde)
library(KernSmooth)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(wordcloud)
library(wordcloud2)
library(wesanderson)
library(RColorBrewer)
library(plotly)
library(kernlab)
library(vscc)
library(caret)
library(stringr)

#read/import the data
kidney_disease <- read.csv("/Users/daisy/Desktop/kidney_disease.csv")
kidney_disease

#Remove the 'id' column
kidney_disease <- kidney_disease %>% dplyr::select(-id)


#Replace the incorrect values in the categorical values
library(stringr)
#list for replacement using 'gsub' functioin
kidney_disease$dm <- gsub(pattern = "\tno", replacement = "no", kidney_disease$dm)
kidney_disease$dm <- gsub(pattern = "\tyes", replacement = "yes", kidney_disease$dm)
kidney_disease$dm <- gsub(pattern = " yes", replacement = "yes", kidney_disease$dm)

kidney_disease$cad <- gsub(pattern = "\tno", replacement = "no", kidney_disease$cad)

kidney_disease$classification <- gsub(pattern = "ckd\t", replacement = "ckd", kidney_disease$classification)
kidney_disease$classification <- gsub(pattern = "notckd", replacement = "not ckd", kidney_disease$classification)

summary(kidney_disease)
str(kidney_disease)

df=kidney_disease
#quick look
head(df)
(df)
str(df)
dim(df)

### EDA
#quick summary
summary(df)
# finds the count of missing values 
sum(is.na(df))

#Basic Data Cleaning

df$pcv <- as.numeric(df$pcv)
df$wc <- as.numeric(df$wc)
df$rc <- as.numeric(df$rc)

df$classification <- as.factor(df$classification)

#ggpairs
ggpairs(df, aes(colour=classification, alpha=0.4))


### bar plot 

# one variable for 'age'

ggplot(df, aes(x=classification, y=age, fill=classification)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_minimal() +
  scale_fill_brewer(palette="Set1", name="Classification") +
  labs(x="", y="Age")

# all variables

# Select the numerical variable columns and exclude the 'id' column 
numeric_vars_cols <- setdiff(names(df)[sapply(df, is.numeric)], 'id')

# melt the data to Long Format
melted_df <- melt(df, id.vars = "classification", measure.vars = numeric_vars_cols)

ggplot(melted_df, aes(x = classification, y = value, fill = classification)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3", name = "Classification") +
  facet_wrap(~variable, scales = 'free_y') +
  labs(x = "", y = "Value") +
  theme(axis.text.x = element_blank())

#pie chart

pie_data <- df %>% 
  count(classification) %>% 
  mutate(per = n / sum(n)) %>% 
  arrange(desc(classification))

# add percent label
pie_data$label <- scales::percent(pie_data$per)

# draw the pie chart
ggplot(data = pie_data) +
  geom_bar(aes(x = "", y = per, fill = classification), stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Set3", name = "Classification") +
  theme_void() +
  geom_text(aes(x = 1.5, y = cumsum(per) - per/2, label = label), size = 5)


###Data preparation
old_columns <- colnames(kidney_disease)
new_columns <- c("age", "blood_pressure", "specific_gravity",
                 "albumin", "sugar", "red_blood_cells", "pus_cell", 
                 "pus_cell_clumps", "bacteria", "blood_glucose_random",
                 "blood_urea", "serum_creatinine", "sodium", "potassium",
                 "haemoglobin", "packed_cell_volume", "white_blood_cell_count",
                 "red_blood_cell_count", "hypertension", "diabetes_mellitus",
                 "coronary_artery_disease", "appetite", "peda_edema",
                 "aanemia","class")
colnames(kidney_disease) <- new_columns
head(kidney_disease)
str(kidney_disease)

##Convert the data type for some columns in the kidney_disease
library(dplyr)
# Define a function to convert the character to numerical 
coerce_to_numeric <- function(x) {
  # Ignore any warnings
  numeric_values <- suppressWarnings(as.numeric(x))
  
  # Check any non-numerical values and replace them to NA
  numeric_values[is.na(numeric_values)] <- NA
  
  return(numeric_values)
}

# execute the function to every column of the data set
kidney_disease <- kidney_disease %>%
  mutate(
    packed_cell_volume = coerce_to_numeric(packed_cell_volume),
    white_blood_cell_count = coerce_to_numeric(white_blood_cell_count),
    red_blood_cell_count = coerce_to_numeric(red_blood_cell_count)
  )
#check the data frame after the converting
str(kidney_disease)

##Extract the categorical column and numerical column
categorical_col <- select_if(kidney_disease, is.character) %>%
  colnames()

numerical_col <- select_if(kidney_disease, is.numeric) %>%
  colnames()

#replace 'class' column to numerical
kidney_disease <- kidney_disease %>%
  mutate(
    class = factor(class, levels = c('ckd', 'not ckd'), labels = c(1, 0))
  )
str(kidney_disease$class)


##For missing values in numerical column, we use mean and median value to replace the missing values
kidney_disease[kidney_disease == ""] <- NA
mean_value_imputation <- function(kidney_disease, feature) {
  if (is.numeric(kidney_disease[[feature]]) && sum(!is.na(kidney_disease[[feature]])) > 0) {
    mean <- mean(kidney_disease[[feature]], na.rm = TRUE) 
    kidney_disease[[feature]][is.na(kidney_disease[[feature]])] <- mean 
  }
  return(kidney_disease)
}

median_value_imputation <- function(kidney_disease, feature) {
  median <- median(kidney_disease[[feature]], na.rm = TRUE)
  kidney_disease[[feature]][is.na(kidney_disease[[feature]])] <- median
  return(kidney_disease)
}

mean_cols <- c("age", "specific_gravity", "haemoglobin", "packed_cell_volume", "red_blood_cell_count")
median_cols <- c("blood_pressure", "albumin", "sugar", "blood_glucose_random", "blood_urea", "serum_creatinine", "sodium", "potassium", "white_blood_cell_count")

for (col in mean_cols) {
  kidney_disease <- mean_value_imputation(kidney_disease, col)
}

for (col in median_cols) {
  kidney_disease <- median_value_imputation(kidney_disease, col)
}

##For missing values in categorical column, we use mode to replace the missing values.
#Convert the "" to NA
impute_mode <- function(kidney_disease, feature) {
  mode <- names(which.max(table(kidney_disease[[feature]]))) 
  kidney_disease[[feature]][is.na(kidney_disease[[feature]])] <- mode 
  return(kidney_disease)
  
}
#insert the mode value into the categorical columns.
for (col in categorical_col) {
  kidney_disease <- impute_mode(kidney_disease, col)
}

#Check if we have the missing values after cleaning
sapply(kidney_disease, function(x) sum(is.na(x)) / length(x) * 100)

#Scale the data
kidney_disease[,sapply(kidney_disease, is.numeric)] <- scale(kidney_disease[,sapply(kidney_disease, is.numeric)])


#Split data train/test
library(caret)
set.seed(2344)
train.index <- createDataPartition(kidney_disease$class, p = .75, list = FALSE)
train <- kidney_disease[train.index, ]
test  <- kidney_disease[-train.index, ]


###Method
library(randomForest)
library(MASS)
library(tree)
library(e1071)
library(class)
library(rpart)
library(rattle )

#KNN Method
library(class)
train_class <- train$class 
test_class <- test$class
train_num <- train[, sapply(train, is.numeric)]
test_num <- test[, sapply(test, is.numeric)]

###KNN Method
## Choosing k via cross-validation
# 10-fold CV
set.seed(123)
kd_cv <- tune.knn(train_num, train_class, k = 1:10, tunecontrol = tune.control(sampling = "cross",cross=10))
summary(kd_cv)
plot(kd_cv)

# kNN for k = 7
knn7 <- knn(train_num, test_num, cl=train_class, k=7, prob=TRUE)
knn7


###Bagging
set.seed(123)
bag.kd=randomForest(class~.,data=train,mtry=24,importance=TRUE,type="class")
bag.kd
#importance(bag.kd)
varImpPlot(bag.kd)

###Random Forest
#First do cross-validate
set.seed(123)
kd_rf = tune.randomForest(class~., data = train, mtry = 1:24,ntree=100*1:5,tunecontrol = tune.control(sampling = "cross",cross=10))
summary(kd_rf)
plot(kd_rf)
set.seed(123)
rf.kd<-randomForest(class~.,data=train,mtry=2,ntree=100,importance=TRUE,type="class")
rf.kd
# importance(rf.kd)
varImpPlot(rf.kd)

#knn
tab_knn=table(knn7,test_class)
tab_knn
#ARI
classAgreement(tab_knn)$crand
#misclassification rate
1-(sum(diag(tab_knn)) / sum(tab_knn))

#bag
kd.pred_bag=predict(bag.kd,test,type="class")
tab_bag<-table(test_class,kd.pred_bag)
tab_bag
#ARI
classAgreement(tab_bag)$crand
#MCR
1-(sum(diag(tab_bag)) / sum(tab_bag))

#RF
kd.pred.rf=predict(rf.kd,test,type="class")
tab_rf=table(test_class,kd.pred.rf)
tab_rf
#ARI
classAgreement(tab_rf)$crand
#MCR
1-(sum(diag(tab_rf)) / sum(tab_rf))

###logistic regression model 
kd_logreg <- glm(class ~ age + blood_pressure+specific_gravity + albumin + sugar + red_blood_cells
                 + pus_cell + pus_cell_clumps + bacteria + blood_glucose_random
                 +blood_urea + serum_creatinine + sodium + potassium
                 +haemoglobin + packed_cell_volume + white_blood_cell_count
                 +red_blood_cell_count + hypertension + diabetes_mellitus
                 +coronary_artery_disease + appetite + peda_edema
                 +aanemia, data=kidney_disease, family=binomial("logit"))
summary(kd_logreg)

#Since there's no significant variables shown, we choose the hight three variables and lowest two
# 
kd_logreg_new <- glm(class ~ specific_gravity + haemoglobin + serum_creatinine
                     + white_blood_cell_count + bacteria + packed_cell_volume
                     + pus_cell_clumps,
                     data=kidney_disease, family=binomial("logit"))
summary(kd_logreg_new)

#Odds ratio
exp(coef(kd_logreg_new))
exp(cbind(OR = coef(kd_logreg_new), confint.default(kd_logreg_new)))
confint.default(kd_logreg_new)
#p-value
G=53.259-7.7000e-08
dof=392-375
pchisq(G,dof,lower.tail = F)

###to get the citation of R
citation()

