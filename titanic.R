
setwd('E:/ExtraIncome/20180427arule_class_beiligong/titanic')

## ---------------- titanic classification ---------------------
titanic1 <- read.csv('./train.csv',stringsAsFactors = FALSE)
titanic2 <- titanic1[,-c(1,4,9)] # -c("PassengerId", "Name", "Ticket")
titanic2$Cabin2 <- as.factor(ifelse(nchar(titanic2$Cabin)==0,0,1))
titanic2 <- titanic2[,-8] # -cabin
str(titanic2)
titanic2$Pclass <- as.factor(titanic2$Pclass)


library(rpart)
cart1 <- rpart(Survived~.,data = titanic2,
               method = 'class')
printcp(cart1)

cart2 <- prune(cart1,cp=0.01)
cart2

rpart.plot::rpart.plot(cart2,
                       extra = 100,
                       branch.lty=3,
                       type=2)



## --------------------------- cluster ------------------------

library(dplyr)
library(cluster)
library(Rtsne)
library(ggplot2)
#titanic3 <- titanic2[,-1] # -survived # 也可以考虑将是否存货踢出聚类依据，重新做聚类试试
titanic3 <- titanic2 %>%
  mutate(Sex = factor(Sex),
         Embarked = factor(Embarked),
         Survived = factor(Survived))
str(titanic3)

hist(titanic3$Age)
hist(titanic3$Fare)

gower_dist <- daisy(titanic3, #利用gower算法计算样本间距离
                    metric = "gower",
                    type = list(logratio = 7)) # var[7]需要log转换

summary(gower_dist)

gower_mat <- as.matrix(gower_dist)

# Output most similar pair
titanic3[
   which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
         arr.ind = TRUE)[1, ], ]

# Output most dissimilar pair
titanic3[
   which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
         arr.ind = TRUE)[1, ], ]

# 聚类个数的选择
sil_width <- c(NA)
for(i in 2:15){
  pam_fit <- pam(gower_dist, # 利用pam聚类模型预建模获取轮廓系数
                 diss = TRUE,
                 k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

plot(1:15, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:15, sil_width)
# 利用轮廓系数来确定最佳的聚类个数，轮廓系数是一个用于衡量聚类离散度的内部指标
# 该指标的取值范围是[-1,1]，其数值越大越好
# k=13

## 正式聚类
pam_fit <- pam(gower_dist, diss = TRUE, k = 13)

## 查看每个聚类类别内的汇总信息
pam_results <- titanic3 %>%
   mutate(cluster = pam_fit$clustering) %>%
   group_by(cluster) %>%
   do(the_summary = summary(.))
 
print(pam_results$the_summary)

## 查看每个聚类类别的样本中心点
titanic3[pam_fit$medoids, ]


## 利用t-sne进行聚类可视化
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE) # 利用rrsne将多维数据转化为2维数据,方便后面作图

tsne_data <- tsne_obj$Y %>%
   data.frame() %>%
   setNames(c("X", "Y")) %>%
   mutate(cluster = factor(pam_fit$clustering),
          name = paste0('Obs',1:nrow(titanic3)))

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))



