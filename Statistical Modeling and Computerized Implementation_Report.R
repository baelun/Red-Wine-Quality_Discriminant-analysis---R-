library(MASS)
?lda
?cbind
file_url ="D:/元智/三年級/統計建模/archive/winequality-red.csv"
red <- read.table(file_url, header = TRUE, stringsAsFactors = FALSE, sep = ",")
View(red)
train = sample(1:1599,500)
sort(train)#排列1~大
table(red[train,]$quality)

z = lda(quality~.,data = red, subset = train)
z
est = predict(z, red[-train, ])
ls(est)
est$posterior   #驗後機率
est$x
est$class




library(MASS)
?lda
?cbind
file_url ="C:/Users/tina2/OneDrive - 元智大學/三上/統計建模/數據/報告/winequality-red.csv"
red <- read.table(file_url, header = TRUE, stringsAsFactors = FALSE, sep = ",")
View(red)

red2=red[,sapply(red,is.numeric)]
set.seed(123)
kmeans.result=kmeans(red2,3)
kmeans.result
table(red$quality,kmeans.result$cluster)
plot(red2,col=kmeans.result$cluster)

kmeans.result$centers
centers=kmeans.result$centers[kmeans.result$cluster,]
head(centers)
distances=sqrt(rowSums((red2-centers)^2))
outliers=order(distances,decreasing=T)[1:5]
outliers
red2[outliers,]
plot(red2[c("pH", "alcohol")], col=kmeans.result$cluster)
plot(red2[c("fixed.acidity", "citric.acid")], col=kmeans.result$cluster)
plot(red2[c("free.sulfur.dioxide", "total.sulfur.dioxide")], col=kmeans.result$cluster)
plot(red2[c("citric.acid", "residual.sugar")], col=kmeans.result$cluster)
plot(red2[c("density", "pH")], col=kmeans.result$cluster)
