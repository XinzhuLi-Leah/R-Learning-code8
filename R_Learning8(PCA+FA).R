

#----------------------------------------------------------------------------------------------------------------------------------------
#主成分分析 Principal Analysis
#用的例子是美国法官的评价指标 数据

# 安装并加载必要的包
install.packages("FactoMineR")  # 如果未安装
install.packages("ggplot2")      # 如果未安装
install.packages("factoextra") 

library(FactoMineR)
library(factoextra)
library(ggplot2)

#不限制主成分数
pca_result <- PCA(USJudgeRatings[, -1], scale.unit = TRUE, ncp = ncol(USJudgeRatings) - 1, graph = FALSE) 
#用这个，这个很清楚，这个碎石图scree plot, 生成碎石图
fviz_screeplot(pca_result, addlabels = TRUE, ylim = c(0, 50))  
#fviz_screeplot(pca_result, ncp = 11, addlabels = TRUE, ylim = c(0, 50))
# 碎石图可以看出来，这里其实主成分只需要提取一个即可，肘部在2

pca_result <- PCA(USJudgeRatings[, -1], scale.unit = TRUE, ncp = 1, graph = TRUE)
# 查看主成分分析的结果
summary(pca_result)  # 查看主成分的方差解释情况

pca_result$ind$coord  # 1查看样本在主成分上的得分
#•	用途: 这个命令用于获取样本（观察值）在主成分空间中的得分（coordinates）。
#•	每个样本在各个主成分上的得分，形成一个得分矩阵。这个得分表明了样本在新构建的主成分空间中的位置。
#•	可以用来可视化样本的分布，或者进行聚类分析，比如 K-means 或层次聚类，以识别样本之间的相似性和差异。

loadings_pc1 <- pca_result$var$coor # 2查看变量在主成分上的负载
loadings_pc1
#•	用途: 这个命令用于获取变量在主成分空间中的负载（loadings）。
#•	输出内容:
#•	每个变量在各个主成分上的负载值，表示该变量在每个主成分中的贡献程度。负载越高，表示该变量在主成分中的影响力越大。
#•	这些信息帮助你理解哪些原始变量对主成分分析的结果影响最大，可以用来命名主成分（如：如果某个主成分主要由“数学”、“物理”的负载构成，可以将其命名为“理科能力”）。

# 3查看特征值+总方差+累积方差表【主成分分析的特征>1会比较好，同时累积的方差在70%，80%以上也会比较好】
explained_variance <- pca_result$eig
explained_variance
#结合碎石图和上面的特征根和累积方差，我们确实觉得1个主成分就可以的,因为这里的cumulative percentage of variance和eigenvalue，一个主成分就完全可以的

#第一个主成分的特征值：
eig1 <- pca_result$eig[1,1]

#查看变量在主成分上的负载进行标准化【除以特征值】【主成分得分系数矩阵】
standardized_loadings <- loadings_pc1 / sqrt(eig1)

#【假设 loadings 是一个载荷矩阵，每列代表一个主成分
# explained_variance 是每个主成分对应的特征根（解释方差）
#standardized_loadings <- sweep(loadings, 2, sqrt(explained_variance), FUN = "/")】


## 假设原始数据是USJudgeRatings，去掉行名列
scaled_data <- scale(USJudgeRatings[, -1])
# 计算综合得分
composite_scores <- scaled_data %*% standardized_loadings

composite_scores[rev(order(composite_scores)),]

#每一个律师的12个指标和这个标准化后的变量在主成分上的负载 相乘+相加
#所以这个最后的综合得分，在这里也就是显示为各个律师在主成分1的得分，因为只有一个主成分啊！
#在 PCA 的综合得分计算中，%*% 用来将标准化后的载荷矩阵与数据矩阵相乘，以计算每个样本在各个主成分上的得分。
#在更为广泛的例子中，是需要计算每一个律师的主成分1的得分，主成分2 的得分。。。。。主成分4，5，6的得分【如有】，然后在加权计算综合得分




# 假设 composite_scores 是你的综合得分
set.seed(123)  # 为了结果可重复
kmeans_result <- kmeans(composite_scores, centers = 3)  # 假设分成 3 组
print(kmeans_result$cluster)  # 查看每个律师的聚类分配

#----------------------------------------------------------------------------------------------------------------------------------------
#因子分析 factor analysis

library(FactoMineR)
library(psych)
library(ggplot2)
library(GPArotation)

data("swiss")
# 标准化数据集（建议进行标准化，尤其是因子分析涉及不同量纲的变量时）
swiss_scaled <- scale(swiss)


# 可视化碎石图 (Scree plot)
fa.parallel(swiss_scaled, fa = "fa", n.iter = 100, show.legend = TRUE, main = "Parallel Analysis for Swiss Data")
#n.iter = 100 指定了模拟次数，通常设为一个较大值（如 100 或 500）以保证结果的稳定性 ，一般默认就是100
#Parallel analysis suggests that the number of factors =  3  and the number of components =  NA 

#平行分析的结果显示，建议提取 3 个因子。这是基于平行分析生成的随机数据集的特征值，与原始数据特征值进行比较得出的结论。通常情况下，只有那些在实际数据中特征值大于随机数据特征值的因子，才被视为重要因子，因此这里建议提取 3 个因子。
#结果解读
#1.	因子数量：平行分析认为有 3 个因子是显著的，表明数据结构可以合理地被 3 个因子解释。
#2.	组件数量：NA 意味着未能识别出显著的主成分，因此建议关注因子分析而非主成分分析。
#这意味着，提取 3 个因子可能更符合数据的实际结构，而进一步的旋转和分析可以帮助更好地解释这些因子的含义。

# 因子分析
factor_result <- fa(swiss_scaled, nfactors = 3, rotate = "varimax", fm = "ml")  # 使用主成分法 (ml)，并选择 varimax 旋转
print(factor_result)


# 查看因子载荷矩阵
print(factor_result$loadings, cutoff = 0.3)  
# cutoff 参数用于仅显示绝对值超过0.3的载荷值，便于解释，命名因子
# 可以看到教育和考试在第一个因子中值最大，可以命名为文化因素， 然后就是农业还有宗教在第二个因子里面只最大，可以命名为社会因素， 最后 就是生育率和死亡率，在第三个因子中值最大命名为人口因素


# 获取各因子的方差解释
factor_variance <- factor_result$Vaccounted[2, ]  
factor_variance 
#计算权重
total_variance <- sum(factor_variance)
weights <- factor_variance / total_variance
class(weights)
#numeric

# 进一步解读因子得分[已经有啦！！]
factor_scores <- factor_result$scores  # 提取因子得分
factor_scores
class(factor_scores)
#matrix

#再以每个因子的方差解释率为权数进行线性加权平均，最后得到一个综合得分模型：
#计算综合得分（按方差解释率加权） 计算每一个样本的。这里就是每一个城市
composite_scores <- factor_scores %*% weights # 加权求和

class(composite_scores)
#matrix

#转换为数据框
composite_scores <- as.data.frame(composite_scores)
#把列名改一下
colnames(composite_scores) <- "Score" 

#加上一列地区名字 ，虽然有行名，why?
#行名不参与数据操作：行名在排序时不会被视为数据的一部分。
#直接对数据进行排序后，行名并不会随数据移动。通过将行名转为一列，可以确保排序后区域名称与得分保持一致。
composite_scores$Region <- rownames(composite_scores)

composite_scores
ordered_scores <- composite_scores[order(composite_scores$Score, decreasing = TRUE), ]

ordered_scores



#in  conclusion
#PCA 的目标是方差最大化和数据降维：在 PCA 中，分析的重点是提取尽可能多的方差信息，即将原始变量转化为少量的主成分来解释数据的整体变化。通过这种方式，我们可以保留尽可能多的方差，同时简化数据的结构，便于可视化和进一步分析。PCA 的应用场景主要是数据降维和解释数据的总方差结构。
#	FA 的目标是揭示潜在关系或结构：因子分析的假设是数据由少数几个潜在因子控制。FA 通过提取潜在因子，解释变量之间的关系，目的是理解和建模变量之间的关联。因子分析应用于探究数据的潜在结构，而不是简单的降维，适合研究变量之间的共变结构和解释原因。

#所以，PCA 更偏重于描述数据的方差结构，而 FA 则更关注于揭示数据背后的潜在因子或结构。









