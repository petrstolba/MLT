ggplot(telco, aes(x = MonthlyCharges*tenure, TotalCharges)) +
    geom_line() +
geom_smooth(method = "lm")

imput = lm(TotalCharges ~ MonthlyCharges:tenure, telco)

summary(imput)
plot(imput)

## SOLVED

## IMPUTE!
train[is.na(train$TotalCharges),]$TotalCharges = predict(imput, train[is.na(train$TotalCharges),])
test[is.na(test$TotalCharges),]$TotalCharges = predict(imput, test[is.na(test$TotalCharges),])
