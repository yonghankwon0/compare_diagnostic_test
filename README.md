# compare_diagnostic_test

```
train <- read_xlsx("meningioma/data.xlsx", sheet="TRAINING_SET (SEV)")
train$reader1 <- ifelse(train$reader1>=3,1,0)
train$reader2 <- ifelse(train$reader2>=3,1,0)

data <- train
y <- y1 <- "LABEL"
X <- X1 <- c("reader1", "T1C_T2", "T2", "T1C", "reader2")
```
<img width="943" alt="Screen Shot 2022-10-31 at 22 10 05" src="https://user-images.githubusercontent.com/31601961/199015563-870b8a96-b3c4-4d43-8154-42efb8e69b66.png">
