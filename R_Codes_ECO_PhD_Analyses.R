# ENES CENGIZOGUZ PHD CANCIDATE

# Loading libraries
install.packages("pacman")
library(pacman)

p_load(tidyverse,apaTables, zoo,lmtest,sandwich,car, readxl, BiocManager, corrplot, Hmisc)

#Setting coorect working directory
setwd("~/Desktop/ENES")

# Imprting data (as a .csv file)
data<- read_excel("DATA_2024.xls")
names(data)

# removing the COVID-19 (2020) period from the data
start <- as.Date("2020-01-01")
finish <- as.Date("2020-12-01")
data <- data[!(data$TIME >= start & data$TIME <= finish), ]

# Check the results
print(data)
View(data)

str(data)
data<- read_excel("DATA_2024(2020yok).xls")


names(data)

# Convert date column to time series format
data$TIME <- as.yearmon(data$TIME, format = "%Y-%m")

names(data)

#Transformation: First-order differencing was applied to make the series stationary.
# Stationary test (ADF)

library(tseries)
adf.test(data$UR)

diff_data <- diff(data$UR)
adf.test(diff_data)

#Adding differenced data to the dataset
data$UR_diff <- c(NA, diff(data$UR))  # İlk satır için NA eklenir çünkü fark alma işlemi bir değer kaybeder

# checking new dataset
head(data)

# Graphing (For time series analysis)
## Before stationary

plot(data$TIME, data$UR, type = "l", col = "blue", main = "Unemployment Rate Over Time",
     xlab = "Time", ylab = "Unemployment Rate")

#After stationary
plot(data$TIME, data$UR_diff, type = "l", col = "blue", main = "Unemployment Rate Over Time",
     xlab = "Time", ylab = "Unemployment Rate")


#1. Creating Correlation Matrix
# Selecting study variamles from the dataset
selected_data <- data[, c("CCI", "BCI", "IPI", "UR_diff", "IR")]

#Creating correlation matrix
cor_matrix <- cor(selected_data, use = "complete.obs")  # NA values ignored
print(cor_matrix)

#2. Correlation Matrix Visualization
# Loading Libraries
library(corrplot)

# Correlation matrix plot
corrplot(cor_matrix, method = "circle", type = "lower", tl.col = "black", tl.srt = 45)

# Correlation and significance testing with Hmisc library
library(Hmisc)
cor_test <- rcorr(as.matrix(selected_data))  # Correlation and p-values
cor_test$r  # correlation coefficients
cor_test$P  # p-values

# Correlation and significance testing
selected_data <- data[, c("CCI", "BCI", "IPI", "UR_diff", "IR")]
cor_test <- rcorr(as.matrix(selected_data))

# Correlation and significance testing
cor_matrix <- cor_test$r  #correlation coefficients
p_matrix <- cor_test$P    # p-values

# Merge the matrix by adding stars
cor_with_stars <- matrix(paste0(round(cor_matrix, 2), stars(p_matrix)), 
                         nrow = nrow(cor_matrix), ncol = ncol(cor_matrix))
rownames(cor_with_stars) <- rownames(cor_matrix)
colnames(cor_with_stars) <- colnames(cor_matrix)

# printing matrix
print(cor_with_stars)


# Define function for stars
stars <- function(p) {
  ifelse(p < 0.001, "***",
         ifelse(p < 0.01, "**",
                ifelse(p < 0.05, "*", "")))
}

# Converting to a matrix containing stars
p_stars <- matrix(stars(p_matrix), nrow = nrow(p_matrix), ncol = ncol(p_matrix))

# Visualizing
corrplot(cor_matrix, method = "number", type = "lower", 
         p.mat = p_matrix, sig.level = 0.05, insig = "blank", 
         addCoef.col = "black", number.cex = 0.7, tl.col = "black",
         tl.srt = 45, cl.cex = 0.7, 
         addgrid.col = "gray")


# Visualization with stars
corrplot(cor_matrix, method = "number", type = "lower", 
         col = colorRampPalette(c("blue", "white", "red"))(200),
         p.mat = p_matrix, sig.level = 0.05, insig = "blank", 
         addCoef.col = "black", number.cex = 0.8, tl.col = "black", 
         tl.srt = 45, cl.cex = 0.8)

# Multiple regression analysis for direvct efeccts
model_0 <- lm(UR_diff ~ CCI + BCI + IPI, data = data)
summary(model_0)
apa.reg.table(model_0,filename="Direct Effects.doc")#Export the findings as a WORD document


# # Interaction product-term test with blocks fro IR as the moderator on the BCI-UN relationship
blk1 <- lm(UR_diff ~ BCI + IR, data=data)
blk2 <- lm(UR_diff ~ BCI + IR + I(BCI * IR), data=data)

apa.reg.table(blk1,blk2,filename="IR-BCI-Interaction1.doc") #Export the findings as a WORD document


# Modelin Anlamodel_0# Modelin Anlamlılık Testleri
library(lmtest)
dwtest(model_0)  # Otokorelasyon Testi
bptest(model_0)  # Heteroskedastiklik Testi


library(forecast)

# ARIMA Modeli Oluşturma
arima_model <- auto.arima(diff_data)
summary(arima_model)

# Tahmin Yapma
forecast_values <- forecast(arima_model, h = 12)  # 12 ay için tahmin
plot(forecast_values)

#Adjusting data for the same number of rows and columns

length(data$TIME)
length(diff_data)
length(fitted(model))

#Comparing predcited and actual vales 
plot(data$TIME[-1], diff_data, type = "l", col = "blue", main = "Real vs Predicted Values")
lines(data$TIME, fitted(model), col = "red")
legend("topright", legend = c("Real Data", "Fitted Model"), col = c("blue", "red"), lty = 1)

# Durbin-Watson Test (Checkinf for autocoreelation problem)
dwtest(model)
model_with_lag <- lm(UR_diff ~ CCI + BCI + IPI + lag(UR_diff, 1), data = data)
summary(model_with_lag)
# Heteroskedasticity Test
bptest(model)
bptest(model_with_lag)
writeLines(capture.output(cat(code)), "my_code.txt")



