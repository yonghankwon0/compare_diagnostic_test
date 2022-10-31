# compare_diagnostic_test

```
train <- read_xlsx("meningioma/data.xlsx", sheet="TRAINING_SET (SEV)")
train$reader1 <- ifelse(train$reader1>=3,1,0)
train$reader2 <- ifelse(train$reader2>=3,1,0)

data <- train
y <- y1 <- "LABEL"
X <- X1 <- c("reader1", "T1C_T2", "T2", "T1C", "reader2")

automatic_comparison(y = y1, X = X1, data = data)
```
<img width="933" alt="Screen Shot 2022-10-31 at 22 11 02" src="https://user-images.githubusercontent.com/31601961/199015684-5c1dadd0-8e2a-4567-9d12-0da985a82d98.png">
