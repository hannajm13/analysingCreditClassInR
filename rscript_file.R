
library(DataExplorer)
library(ggplot2)
library(crayon)
library(vcd)
library(broom)
library(dplyr)
library(caTools)
library(caret)
library(effects)
library(VIM)
library(pdp)

fileurl = "" #insert dataset URL here

df = read.csv(fileurl)
df


#Objective 1: To investigate the relationship between years of employment and credit class

#Dodged bar chart of employment by class
#for all values of “employment”, 
#how many corresponding values of “class” were good or bad. 
ggplot(df, aes(x = employment, fill = class)) +
  geom_bar(position = "dodge") +  # make bars side-by-side
  geom_text(stat = 'count', aes(label = after_stat(count)), #use labels to view count
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Dodged Bar Chart of Employment by Class",
       x = "Years of Employment",
       y = "Count",
       fill = "Class") +
  theme_minimal()

# stacked bar chart
ggplot(df, aes(x = class, fill = employment)) + 
  geom_bar(position = "stack") +
  labs(title = "Stacked Bar Chart of Class by Employment ")

#find the strength (instead of the nature) 
#of the relationship between employment and credit class
#no relationship - cramer's v is 0
#perfect relationship - cramer's v is 1
#The higher the Pearson value, the more 
#the null hypothesis (the variables have no relation) can be rejected

contingency_table <- table(df$employment, df$class) #create contingency table
cramer_v <- assocstats(contingency_table) #get cramer's v
cramer_v

#Objective 2:	To investigate how class is affected when taking into account both employment and job.
  
#2.1	Finding out the relationship between job and employment

#MOSAIC PLOT of employment and job___________________________________________________________-
#As both are categorical variables, a mosaic plot is a good tool 
#to plot the values of job and employment against each other. 

color = c( "lightblue", "lavender", "pink")
#create contingency table first
contingency_table2 <- table(df$employment, df$job)
mosaicplot(contingency_table2, main = "Mosaic Plot of Employment and Job", 
           xlab = "Employment", ylab = "Job", color = color)

#Cramer's V of employment and job
contingency_table2 <- table(df$employment, df$job)
cramer_v <- assocstats(contingency_table2) #get cramer's v
cramer_v

#2.2	Finding out the relationship between job and class.

#STACKED BAR CHART JOB AND CLASS
# stacked bar chart
ggplot(df, aes(x = class, fill = job)) + 
  geom_bar(position = "stack") +
  labs(title = "Stacked Bar Chart of Class by Job")


#Cramer's v for class and job
contingency_table3 <- table(df$class, df$job)
cramer_v <- assocstats(contingency_table3) 
cramer_v


#OBJECTIVE 3:	To find any any interactions between job and employment that could affect class.
#Does the impact of job on credit class depend on employment and vice versa?

#LOGISTIC REGRESSION_______________________________________________________

#make class ( the dependent variable) as a factor and relevel it so that the
#reference class is "bad"
df$class <- relevel(as.factor(df$class), ref = "bad")

set.seed(123)

#split dataset in 9:1 ratio
split = sample.split(df$class, SplitRatio = 0.9)

training_set = subset(df, split == T)

#test set is to use the trained model to predict on unseen data
test_set = subset(df, split == F)


#build logistic regression for model 1
classifier1 = glm(class ~ employment + job, training_set, family = binomial)
#model 1 summary
summary(classifier1)

#exponentiate the coefficients to get odds ratios
formatted_findings <- tidy(classifier1) %>%
  mutate(odds_ratio = exp(estimate)) 
formatted_findings

#PREDICTION 
#extract the desired columns
test_set1 <- test_set %>% select(employment, job, class)
test_set2 <- test_set %>% select(employment, job, class)


#predicting on test set using Model 1
pred_prob_test = predict(classifier1, type = "response", test_set1[,-3])
pred_class_test = ifelse(pred_prob_test > 0.5, "good", "bad")

confusionMatrix(table(pred_class_test, test_set1$class))

#add predicted class set to test set so comparison can be made
test_set1$pred_class <- pred_class_test
test_set1$pred_prob <- pred_prob_test
View(test_set1)

#interaction model - Model 2
#fit the model
classifier2 = glm(class ~ employment * job, training_set, family = binomial)
summary(classifier2)

#predict on test set using Model 2
pred_prob_test2 = predict(classifier2, type = "response", test_set2[,-3])

pred_class_test2 = ifelse(pred_prob_test2 > 0.5, "good", "bad")


confusionMatrix(table(pred_class_test2, test_set2$class))

#add predicted class set to test set so comparison can be made
test_set2$pred_class <- pred_class_test
test_set2$pred_prob <- pred_prob_test
View(test_set2)

#COMPARISON BETWEEN MODEL 1 AND MODEL 2 ______________

#anova of model 1 and model 2
anova(classifier1, classifier2)

#chi squared distribution
#47.084 = difference in deviance of model 1 and 2
#degrees of freedom = 8
p_value <- pchisq(47.084, df = 8, lower.tail = FALSE)
p_value

#VISUALISING THE INTERACTION

#plot classifier 2 to view interaction effects
plot(allEffects(classifier2), multiline = TRUE, ci.style = "bands")


#Partial Dependent Plot (PDP)
#The partial function tells us for each given value of job and employment
#what the average marginal effect on the prediction is by 
#replacing the job type/ employment category of all data instances 
#with each possible value and averaging  the predictions

#a flat PDP indicates that the feature 
#is not important, and the more the PDP varies, the more 
#important the feature is.


#show how each employment category/job category influences the
#model's predicted outcome while averaging over the effects 
#of other features.


#turn employment and job to factor so pdp can be plotted
training_set$employment <- factor(training_set$employment)
training_set$job <- factor(training_set$job)
table(training_set$class)

classifier2 = glm(class ~ employment * job, training_set, family = binomial)

#pdp of employment
pdp_emp <- partial(classifier2, pred.var = "employment", grid.resolution = 50)

ggplot(pdp_emp, aes(x = employment, y = yhat)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Partial Dependence Plot for Employment - Model 2",
       x = "Employment",
       y = "Predicted Probability for Class") +
  theme_minimal()

#pdp of job
pdp_job <- partial(classifier2, pred.var = "job", grid.resolution = 50)

ggplot(pdp_job, aes(x = job, y = yhat)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Partial Dependence Plot for Job - Model 2",
       x = "Job",
       y = "Predicted Probability for Class") +
  theme_minimal()

#two-variable PDP shows the dependence of the 
#class on joint values of job and employment
#can help identify possible interactions

pdp_result2 <- partial(classifier2, pred.var = c("job", "employment"))

ggplot(pdp_result2, aes(x = job, y = yhat)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_wrap(~ pdp_result2$employment)
labs(title = "PDP of Logistic Regression Model 2",
     x = "Employment",
     y = "Predicted Probability for Class") +
  theme_minimal()



#Random Forest - Feature Importance

#perform hot deck to replace missing values based on class value 
training_set = hotdeck(training_set, domain_var = "class", imp_var = FALSE)

rf_model <- randomForest(x = training_set[, -21], 
                         y = training_set$class, 
                         ntree = 500, 
                         importance = TRUE)

importance_matrix <- importance(rf_model)
importance_matrix


