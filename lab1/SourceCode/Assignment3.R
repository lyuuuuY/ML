data = read.csv("C:/Users/崔庆轩喜欢摆/Desktop/MachineLearning/Lab/Lab1/LAB1data/pima-indians-diabetes.csv")
colnames(data) = list("pegnant_times", 
                      "glucose_level", 
                      "blood_pressure", 
                      "skin_thickness", 
                      "serum_insulin", 
                      "boby_mass", 
                      "diabetes_pedigree_func", 
                      "age", 
                      "diabetes")

library(lattice)
library(caret)
library(ggplot2)

ggplot(data, aes(age, glucose_level, color = as.factor(diabetes))) + 
  geom_point(size = 2) +
  labs(title = "Scatter Plot between Age and Glucose Level",
       x = "Age", 
       y = "Glucose Level") + 
  scale_color_manual(values = c("0" = "green", "1" = "red")) +
  theme_minimal()

# model 1
model_data = as.data.frame(x = cbind(data$glucose_level, data$age, data$diabetes))
colnames(model_data) = list("x1", "x2", "label")


model = train(as.factor(label) ~ ., 
              data = model_data, 
              method = "glm", 
              family = "binomial")
# summary(model)

output_prob = predict(model, model_data, type = "prob")[,2]
output_label  = ifelse(output_prob >= 0.5, 1, 0)
loss  = mean((as.numeric(output_label) - as.numeric(model_data$label)) ** 2)
cat("The misclassification error is:", loss, "\n")
acc = mean(output_label == model_data$label)
cat("The accuracy can be considered as quality of classification:", acc)
ggplot(cbind(model_data,output_label), aes(x2, x1, color = as.factor(output_label))) + 
  geom_point(size = 2) +
  labs(title = "Scatter Plot between Age and Glucose Level",
       x = "Age", 
       y = "Glucose Level") + 
  scale_color_manual(values = c("0" = "green", "1" = "red")) +
  theme_minimal()

coeff = coef(model$finalModel)
intercept = coeff[1]
param_x1 = coeff[2]
param_x2 = coeff[3]
cat("Boundary Decision：", intercept, "+", param_x1, "* x1 +", param_x2, "* x2 = 0")

# create the gird
x2_range = seq(min(model_data$x2), max(model_data$x2), length.out = dim(model_data))
x1_bound = -(intercept + param_x2 * x2_range) / param_x1

ggplot(cbind(model_data,output_label), aes(x2, x1)) + 
  geom_point(aes(color = as.factor(output_label)),size = 2) +
  geom_line(aes(x = x2_range, y = x1_bound), color = "black", linetype = "dashed")+
  labs(title = "Scatter Plot between Age and Glucose Level",
       x = "Age", 
       y = "Glucose Level") + 
  scale_color_manual(values = c("0" = "green", "1" = "red")) +
  theme_minimal()

r = c(0.2, 0.8)
output_label  = ifelse(output_prob >= r[1], 1, 0)

ggplot(cbind(model_data,output_label), aes(x2, x1, color = as.factor(output_label))) + 
  geom_point(size = 2) +
  labs(title = "Scatter Plot between Age and Glucose Level",
       x = "Age", 
       y = "Glucose Level") + 
  scale_color_manual(values = c("0" = "green", "1" = "red")) +
  theme_minimal()

output_label  = ifelse(output_prob >= r[2], 1, 0)
ggplot(cbind(model_data,output_label), aes(x2, x1, color = as.factor(output_label))) + 
  geom_point(size = 2) +
  labs(title = "Scatter Plot between Age and Glucose Level",
       x = "Age", 
       y = "Glucose Level") + 
  scale_color_manual(values = c("0" = "green", "1" = "red")) +
  theme_minimal()




label = model_data$label
model_data$label = NULL

for (i in 0:4){
  feature_name = paste0("z", as.character(i+1))
  model_data[[feature_name]] = (model_data$x1 ** (4 - i)) * (model_data$x2 ** i)
}
model_data$label = label
head(model_data)

model = train(as.factor(label) ~ ., 
              data = model_data, 
              method = "glm", 
              family = "binomial")
output_prob = predict(model, model_data, type = "prob")[,2]
output_label  = ifelse(output_prob >= 0.5, 1, 0)

loss  = mean((as.numeric(output_label) - as.numeric(model_data$label)) ** 2)
acc = mean(output_label == model_data$label)
cat("The misclassification error is:", loss, "\n")
cat("The accuracy can be considered as quality of classification:", acc)

db_points = model_data[abs(output_prob - 0.5) < 0.05,]

ggplot() +
  geom_point(data = cbind(model_data, output_label), aes(x = x2, y = x1, color = as.factor(output_label))) +
  labs(title = "Scatter Plot between Age and Glucose Level",
       x = "Age", 
       y = "Glucose Level") + 
  scale_color_manual(values = c("0" = "green", "1" = "red")) +
  geom_smooth(data = db_points, aes(x = x2, y = x1), method = "loess",color = "black", linetype = "dashed") +
  theme_minimal()
