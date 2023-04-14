install.packages('DescTools')
install.packages('dplyr')
install.packages("labeling")
install.packages('lmtest')
install.packages('olsrr')
install.packages('nortest')
install.packages('rsq')
install.packages('minqa')
library(DescTools)
library(dplyr)
library(ggplot2)
library(stringr)
library(labeling)
library(lmtest)
library(MASS)
library(car)
library(olsrr)
library(rsq)
#Loading the Data
df = `car.details.v4`
print(df)

#Cleaning the data
is.na(df)
sum(is.na(df))
car_data = na.omit(df)
sum(is.na(car_data))
print(car_data)
print(length(unique(car_data$Make)))
sapply(car_data, typeof)
length(unique(car_data$Transmission))
unique(car_data$Owner)
unique(car_data$Fuel.Type)
table(car_data$Owner)

ggplot(data = car_data, aes(x=reorder(Fuel.Type, Fuel.Type, function(x)-length(x)), fill = Fuel.Type)) +
  geom_bar() + labs(x='Fuel Type') + labs(title = "Bar Graph of Fuel Type")


ggplot(data = car_data, aes(x=reorder(Owner, Owner, function(x)-length(x)), fill = Owner)) +
  geom_bar() + labs(x='Owner') + labs(title = "Bar Graph of Owner") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Most of the cars are owned by first owners.


ggplot(data = car_data, aes(x=reorder(Seating.Capacity, Seating.Capacity, function(x)-length(x)), fill = Seating.Capacity)) +
  geom_bar() + labs(x='Seats') + labs(title = "Bar Graph of Seats") +theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Majority of the seats in the cars are 5 seaters. So compact cars are the most dominant one in the car market.

#Convertin Transmission into binary column <- Changing 'manual' to 0 and 'automatic' to 1
car_data$Transmission <- str_replace(car_data$Transmission, 'Manual', "0")
car_data$Transmission <- str_replace(car_data$Transmission, 'Automatic', "1")

car_data$Fuel.Type <- str_replace(car_data$Fuel.Type, 'Petrol', "0")
car_data$Fuel.Type <- str_replace(car_data$Fuel.Type, 'Diesel', "1")
car_data$Fuel.Type <- str_replace(car_data$Fuel.Type, 'CNG', "2")
car_data$Fuel.Type <- str_replace(car_data$Fuel.Type, 'LPG', "3")
car_data$Fuel.Type <- str_replace(car_data$Fuel.Type, 'Hybrid', "4")

#Converting owner into Ordinal Encoder
car_data$Owner <- str_replace(car_data$Owner, 'First', "0")
car_data$Owner <- str_replace(car_data$Owner, 'Second', "1")
car_data$Owner <- str_replace(car_data$Owner, 'Third', "2")
car_data$Owner <- str_replace(car_data$Owner, 'Fourth', "3")
car_data$Owner <- str_replace(car_data$Owner, 'UnRegistered Car', "4")
car_data$Owner <- str_replace(car_data$Owner, '4 or More', "5")

cols <- c(6, 7, 10, 15)
car_data[, cols] <- apply(car_data[, cols], 2, as.numeric)

#Histogram and Density Plot of Price
ggplot(car_data, aes(x=Price)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="blue")+
  labs(x='Selling Price ') + labs(title = "Histogram Graph of Selling Price") +
  scale_x_continuous(trans='log10')

#Histogram of Kilometers driven
ggplot(car_data, aes(x=Kilometer)) + 
  geom_histogram(color="black", fill="blue", bins = 200)+
  labs(x='Km Driven ') + labs(title = "Histogram Graph of Km Driven") +
  scale_x_continuous(trans='log10')

#Summary of the data
summary(car_data)

#Outlier Detection
boxplot(car_data$Year)
boxplot(car_data$Engine)
boxplot(car_data$Max.Power.bhp.)
boxplot(car_data$Max.Torque.Nm.)
boxplot(car_data$Seating.Capacity)
boxplot(car_data$Fuel.Tank.Capacity)
boxplot(car_data$Price)
boxplot(car_data$Kilometer)
boxplot(car_data$Length)
boxplot(car_data$Width)
boxplot(car_data$Height)

#Assumption 1
plot(car_data$Price~car_data$Year) #scatter plot 
plot(car_data$Price~car_data$Kilometer)
plot(car_data$Price~car_data$Engine)
plot(car_data$Price~car_data$Max.Power.bhp.)
plot(car_data$Price~car_data$Max.Torque.Nm.)
plot(car_data$Price~car_data$Length)
plot(car_data$Price~car_data$Height)
plot(car_data$Price~car_data$Width)
plot(car_data$Price~car_data$Fuel.Tank.Capacity)

correlation <- cor(car_data[,-c(1,2,4,8,9,11,15)]) #correlation is not 0 therefore the is a linear relation ship 
abs(correlation[,1]) !=0


#Assumption 2
correlation > 0.8 #manullay done backward selection
heatmap(correlation)

df.lm= lm(Price~Kilometer+Fuel.Type+Transmission+Owner+Engine+Max.Power.bhp.+Max.Torque.Nm.+Length+Width+
            Height+Seating.Capacity+Fuel.Tank.Capacity,data = car_data)
VIF(df.lm)[VIF(df.lm) > 5]

df.lm= lm(Price~Kilometer+Fuel.Type+Transmission+Owner+Engine+Max.Power.bhp.+Length+Width+
            Height+Seating.Capacity+Fuel.Tank.Capacity,data = car_data)
VIF(df.lm)[VIF(df.lm) > 5]

df.lm= lm(Price~Kilometer+Fuel.Type+Transmission+Owner+Engine+Length+Width+
            Height+Seating.Capacity+Fuel.Tank.Capacity,data = car_data)
VIF(df.lm)[VIF(df.lm) > 5]

df.lm= lm(log(Price)~Kilometer+Fuel.Type+Transmission+Owner+Engine+Width+
            Height+Seating.Capacity+Fuel.Tank.Capacity,data = car_data)
VIF(df.lm)[VIF(df.lm) > 5]

#Assumption 4 
dwtest(df.lm)

#Assumption 5 
ggplot(car_data, aes(x=Price)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="blue")+
  labs(x='Selling Price ') + labs(title = "Histogram Graph of Selling Price") +
  scale_x_continuous(trans='log10')

shapiro.test(resid(df.lm))

plot(cooks.distance(df.lm), pch = 20, main = "Cook's distance plot")
abline(h = 4/nrow(car_data), col = "red")
cutoff <- 4/nrow(car_data)
newdata <- subset(car_data, cooks.distance(df.lm) < cutoff)
car_data = newdata

df.lm= lm(log(Price)~Kilometer+Fuel.Type+Transmission+Owner+Engine+Width+
            Height+Seating.Capacity+Fuel.Tank.Capacity,data = car_data)
VIF(df.lm)[VIF(df.lm) > 5]

shapiro.test(resid(df.lm))

#model

summary(df.lm)

#Homoscedasticity
par(mfrow=c(2,2))
plot(df.lm)
par(mfrow=c(1,1))

#outlier removal
cut <- quantile(car_data$Price, c(0.05, 0.95))
new<- subset(car_data, car_data$Price >= cut[1] & car_data$Price <= cut[2])

df1.lm= lm(Price~Kilometer+Fuel.Type+Transmission+Owner+Engine+Max.Power.bhp.+Max.Torque.Nm.+Length+Width+
            Height+Seating.Capacity+Fuel.Tank.Capacity,data = new)
summary(df1.lm)

df1.lm= lm(Price~Kilometer+Fuel.Type+Transmission+Owner+Engine+Max.Power.bhp.+Max.Torque.Nm.+Width+
             Height+Seating.Capacity+Fuel.Tank.Capacity,data = new)
summary(df1.lm)

df1.lm= lm(Price~Kilometer+Fuel.Type+Transmission+Owner+Engine+Max.Power.bhp.+Max.Torque.Nm.+Width+
             Height+Fuel.Tank.Capacity,data = new)
summary(df1.lm)

df1.lm= lm(Price~Kilometer+Fuel.Type+Transmission+Owner+Engine+Max.Power.bhp.+Max.Torque.Nm.+Width+
             Height,data = new)
summary(df1.lm)

df1.lm= lm(Price~Kilometer+Fuel.Type+Transmission+Owner+Max.Power.bhp.+Max.Torque.Nm.+Width+
             Height,data = new)
summary(df1.lm)
summary(df.lm)

#backward selection
formula <- as.formula(Price~Kilometer+Fuel.Type+Transmission+Owner+Engine+Max.Power.bhp.+Max.Torque.Nm.+Length+Width+
                        Height+Seating.Capacity+Fuel.Tank.Capacity)
df2.lm= lm(formula,data = car_data)
summary(df.lm)
summary(df2.lm)

ols_step_backward <- function(df2.lm, formula, threshold = 5) {
  
  while (TRUE) {
    
    # Check if there are any predictor variables left in the model
    if (length(coefficients(df2.lm)) <= 1) {
      break
    }
    
    # Calculate VIF values for each predictor variable
    vif_values <- vif(df2.lm)
    
    # Find the maximum VIF value
    max_vif <- max(vif_values)
    
    # Check if the maximum VIF value is above the threshold
    if (max_vif > threshold) {
      # Get the name of the predictor variable with the highest VIF
      name_max_vif <- names(vif_values)[which.max(vif_values)]
      
      # Update the formula by removing the predictor variable with the highest VIF
      if (length(formula[[2]]) > 1) {  # check that formula is not empty
        formula <- update(formula, paste("-", name_max_vif, sep = ""))
      }
      
      # Fit a new model with the updated formula
      model <- lm(formula, data = car_data)
      
    } else {
      # Check the p-values for each predictor variable
      p_values <- summary(df2.lm)$coefficients[, 4]
      
      # Find the predictor variable with the highest p-value greater than 0.05
      max_p <- max(p_values[p_values > 0.05], default = -Inf)
      
      # Check if there is any predictor variable to remove based on the p-value threshold
      if (max_p > 0) {
        # Get the name of the predictor variable with the highest p-value above 0.05
        name_max_p <- names(max_p)[which(p_values == max_p)]
        
        # Update the formula by removing the predictor variable with the highest p-value above 0.05
        if (length(formula[[2]]) > 1) {  # check that formula is not empty
          formula <- update(formula, paste("-", name_max_p, sep = ""))
        }
        model <- lm(formula, data = car_data)
        
      } else {
        break
      }
    }
  }
  
  return(model)
}
# Print the final model summary
summary(model)


