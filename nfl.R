
## --------- 1. 数据整理和转化
setwd('E:/dateming/NFLPlaybyPlay2009_2017')
library(dplyr)
nfl2017 <- read.csv('./nfl2017.csv',stringsAsFactors = FALSE)
names(nfl2017)[1] <- 'Date'
dim(nfl2017)
str(nfl2017)

nfl201701 <- nfl2017 %>%
  select(-Date,-GameID,-PlayTimeDiff,-desc,-Passer,-Rusher,-Receiver,-(No_Score_Prob:Season)) %>%
  ## 将原始无法直接使用的变量转化成可以直接利用的有效变量
  mutate(sideoffield2=ifelse(SideofField==posteam,'jingong','fangshou'), # 球权归攻方还是守方
         Returner=ifelse(is.na(Returner),0,1), # 是否安排了returner
         BlockingPlayer=ifelse(is.na(BlockingPlayer),0,1), #是否安排了blocking
         Tackler=2-is.na(Tackler1)+is.na(Tackler2), # 安排tracker的数量
         RecFumbTeam=ifelse(RecFumbTeam==posteam,'jingong','fangshou'),
         id1=1:nrow(nfl2017)
         ) %>%
  # 将原始无法直接使用的变量删除
  select(-SideofField,-posteam,-DefensiveTeam, # sideoffield2
         -(yrdln:ydsnet),-Yards.Gained,# FirstDown
         -AirYards,-YardsAfterCatch, # PassLength
         -FieldGoalDistance, #FieldGoalResult
         -RecFumbPlayer #RecFumbteam
         ) %>% data.table::data.table()
dim(nfl201701)

nfl201702 <- data.table::melt(nfl201701,id=c('id1'))[order(id1)] # 数据从
head(nfl201702) # nfl201702的数据样子
# id1  variable value
# 1:   1     Drive     1
# 2:   1       qtr     1
# 3:   1      down    NA
# 4:   1 TimeUnder    15
# 5:   1  GoalToGo     0
# 6:   1 FirstDown    NA
nfl201703 <- na.omit(nfl201702)  # 去掉无效信息1
nfl201703 <- nfl201703[value!='None',] # 去掉无效信息2
nfl201703[,items:=paste(variable,value,sep='=')] # 将剩余有效信息合并成购物篮商品的模式
head(nfl201703)
# id1      variable value           items
# 1:   1         Drive     1         Drive=1
# 2:   1           qtr     1           qtr=1
# 3:   1     TimeUnder    15    TimeUnder=15
# 4:   1      GoalToGo     0      GoalToGo=0
# 5:   1 PlayAttempted     1 PlayAttempted=1
# 6:   1            sp     0            sp=0
nfl201704 <- nfl201703[,.(id1,items)] # 只提取购物篮编号和物品2列
write.csv(nfl201704,'./nfl_item.csv',row.names = FALSE) # 将结果集写入本地文件

library(arules)
nfl_items <- read.transactions('./nfl_item.csv',format = 'single', # 将本地文件重新读取为关联分析格式
                               cols = c('id1','items'),sep=',',skip = 0)

# inspect(head(nfl_items)) # 查看转化后的前6笔交易数据
# 
# rules <- apriori(nfl_items,
#                  parameter = list(minlen=2,
#                                   supp=0.5,
#                                   conf=0.8),
#                  appearance = list(rhs=c('Touchdown=1', 'Touchdown=0'),
#                                    default='lhs'))

rules <- apriori(nfl_items, # 建立关联分析模型
                 parameter = list(minlen=2,
                                  supp=0.5,
                                  conf=0.9))

## ------- 2.	找出频繁项集
summary(nfl_items) # 概述购物篮基本情况
# most frequent items:
#   PlayAttempted=1   BlockingPlayer=0           Safety=0       Onsidekick=0 Challenge.Replay=0            (Other) 
# 48060              48046              48043              47995              47609            1751685 
itemfreq <- itemFrequency(nfl_items,type='absolute')
head(itemfreq[order(-itemfreq)]) # 查看频繁项集top6
# PlayAttempted=1     BlockingPlayer=0             Safety=0         Onsidekick=0   Challenge.Replay=0 
# 48060                48046                48043                47995                47609 
# InterceptionThrown=0 
# 47569 



## -------- 3.	导出关联规则，计算其支持度和置信度
inspect(sort(rules,by=c('confidence'))[1:10]) 
# # 查看前10条规则的support(支持度)和confidence(置信度)
# lhs                                rhs                            support   confidence lift     count
# [1]  {sideoffield2=fangshou}         => {Safety=0}                     0.5082813 1          1.000354 24428
# [2]  {sideoffield2=fangshou}         => {PlayAttempted=1}              0.5082813 1          1.000000 24428
# [3]  {Tackler=2}                     => {PlayAttempted=1}              0.5359134 1          1.000000 25756
# [4]  {PassAttempt=0}                 => {Reception=0}                  0.5893883 1          1.329240 28326
# [5]  {PassAttempt=0}                 => {InterceptionThrown=0}         0.5893883 1          1.010322 28326
# [6]  {PassAttempt=0}                 => {PlayAttempted=1}              0.5893883 1          1.000000 28326
# [7]  {FirstDown=0}                   => {PlayAttempted=1}              0.6635040 1          1.000000 31888
# [8]  {AwayTimeouts_Remaining_Post=3} => {AwayTimeouts_Remaining_Pre=3} 0.6615897 1          1.489678 31796
# [9]  {AwayTimeouts_Remaining_Post=3} => {PlayAttempted=1}              0.6615897 1          1.000000 31796
# [10] {HomeTimeouts_Remaining_Post=3} => {HomeTimeouts_Remaining_Pre=3} 0.6641490 1          1.484112 31919



## ----- 4.	对规则进行评价，可使用Lift，也可以使用教材中所提及的其它指标
inspect(sort(rules,by = c('lift','support'))[1:6]) # 按照lift降序提取前6位规则

# lhs                               rhs                               support confidence     lift count
# [1] {AwayTimeouts_Remaining_Pre=3,                                                                       
# Timeout_Indicator=0}          => {AwayTimeouts_Remaining_Post=3} 0.6518727          1 1.511511 31329
# [2] {AwayTimeouts_Remaining_Pre=3,                                                                       
# PlayAttempted=1,                                                                                    
# Timeout_Indicator=0}          => {AwayTimeouts_Remaining_Post=3} 0.6518727          1 1.511511 31329
# [3] {AwayTimeouts_Remaining_Pre=3,                                                                       
# Safety=0,                                                                                           
# Timeout_Indicator=0}          => {AwayTimeouts_Remaining_Post=3} 0.6516646          1 1.511511 31319
# [4] {AwayTimeouts_Remaining_Pre=3,                                                                       
# BlockingPlayer=0,                                                                                   
# Timeout_Indicator=0}          => {AwayTimeouts_Remaining_Post=3} 0.6516646          1 1.511511 31319
# [5] {AwayTimeouts_Remaining_Pre=3,                                                                       
# PlayAttempted=1,                                                                                    
# Safety=0,                                                                                           
# Timeout_Indicator=0}          => {AwayTimeouts_Remaining_Post=3} 0.6516646          1 1.511511 31319
# [6] {AwayTimeouts_Remaining_Pre=3,                                                                       
# BlockingPlayer=0,                                                                                   
# PlayAttempted=1,                                                                                    
# Timeout_Indicator=0}          => {AwayTimeouts_Remaining_Post=3} 0.6516646          1 1.511511 31319


























