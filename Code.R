library(ggcorrplot)
library(FactoMineR)
library(factoextra)
library(plotly)
library(gridExtra)
library(ca)
library(tidyverse)
library(MASS)
library(GGally) 

# -------------------------------------------------------------------------------------------
set.seed(42)

rm(list = ls())
options(digits=3) # print everything with 3 digits

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# About the dataset -  https://archive.ics.uci.edu/dataset/45/heart+disease

data <- read.csv("processed.cleveland.data", header = FALSE)

# Assigning column names 
data_column <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")
colnames(data) = data_column

str(data)
#------------------------------------------Identify Missing values & Removing ------------------------------------------------
missing_values <- sapply(data, function(x) sum(is.na(x) | x == "?"))
missing_values

# Replace "?" with NA
data[data == "?"] <- NA

# Remove rows with any NA values
data <- na.omit(data)

# ----------------------------------------------------------------------------------------------------------------------------

str(data)

data$ca <- as.numeric(data$ca)
data$thal <- as.numeric(data$thal)

# Categrical - fbs, restecg, exang, sex, cp, slope , thal #7
# Numerical - age, trestbps, chol, thalach, oldpeak, ca #6
  
summary(data)
nrow(data)
ncol(data)

# Plot --------------------------------------------------------------------------------
data %>%
  dplyr::select(age, trestbps, chol, thalach, oldpeak, ca, num) %>%
  mutate(num = ifelse(data$num > 0 , 1, 0)) %>%
  mutate(num = as.factor(num)) %>%
  ggpairs(mapping = aes(group=num, color=num), 
          upper = list(continuous = wrap("cor", size = 3.5) ,combo = wrap("box", size=0.2)),
          lower = list(continuous = wrap("points", alpha = 0.7, size=0.3),
                       combo = wrap("facethist", bins=30, size=0.3)),
          diag = list(continuous = wrap("densityDiag", alpha=0.3)),
          title = "Heart Disease - Numeric Values") %>%
  ggsave(filename="data_pairs.svg", width=10, height=10)

# =============================== Principal Component Analysis (PCA) ============================================================

# PCA works only with numeric column, keep only numeric "age, trestbps, chol, thalach, oldpeak," columns.  ca is in character format

selected_data = data[, c("age", "trestbps", "chol", "thalach", "oldpeak", "ca")]

# Scale the selected variables in the dataset. This step ensures all the features are standardized to a same scale
normalized_data = scale(selected_data)

#------------------------
data.pca =  prcomp(normalized_data)

data.pca  
# The Sd of a PC is the square root of its eigenvalues and it indicates the amount of variation captured by that PC, higher values indicate that the component explains more variance in the data
# The values under each PC are eigenvectors. When we multipy by each variable we can get the PC score

summary(data.pca)
#Proportion of variance -  variability of the data explained by given PC, ex. 35.9% of the variance in the data is explained by PC1
#Since the first three principal components (PC1, PC2, and PC3) together explain approximately 75.29% of the variance, 
#you can reduce the dimensionality of your data from five dimensions to three while retaining most of the variance. 


scree_plot <- fviz_eig(data.pca, addlabels = TRUE, ylim = c(0, 40),
                       xlab = "Principal Components", ylab = "Cumulative Percentage",
                       main = "Cumulative Percentage of PCA") +
  theme_classic()

# Display the plot
print(scree_plot)

# Save the plot to a file
ggsave("scree_plot.png", plot = scree_plot, width = 8, height = 6)

cum_plot <- plot(cumsum(data.pca$sdev^2)/sum(data.pca$sdev^2),
                 type = 'b',
                 cex=.75,
                 xlab = "Number of components",
                 ylab = "% variance explained",
                 main = "Cumulative Percentage of PCA")
abline(h = 0.8,col = "red",lty = 2)
abline(v = 4,col = "blue", lty = 2)

ggsave("cumula.png", plot = cum_plot, width = 8, height = 6)


table(data$num)
#----------------------------------- Biplot of the attributes ---------------------

p1 <- fviz_pca_var(data.pca, axes = c(1, 2), geom = c("arrow", "text"),
                   col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), label = "var", repel = TRUE) +
  ggtitle("PC1 vs PC2")
# Plot for PC1 vs PC3
p2 <- fviz_pca_var(data.pca, axes = c(1, 3), geom = c("arrow", "text"),
                   col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), label = "var", repel = TRUE) +
  ggtitle("PC1 vs PC3")


# Plot for PC2 vs PC3
p3 <- fviz_pca_var(data.pca, axes = c(2, 3), geom = c("arrow", "text"),
                   col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),label = "var", repel = TRUE) +
  ggtitle("PC2 vs PC3")


combined_plot <- grid.arrange(p1, p2, p3, ncol = 3)

#------------------------------Contribution of each variables ---------------------
fviz_cos2(data.pca, choice = "var", axes = 1:3)


# ------------------------- Correlation between variables with PC -----------------
r.pc.x <- cor(normalized_data, data.pca$x)
r.pc.x

par(mfrow =c(1,3))
plot(cos((0:360)/180*pi), sin((0:360)/180*pi), type = "l", lty = "dashed",
     col = "red",
     xlab = "cor with PC1", ylab = "cor with PC2",
     main = "PC1 vs PC2",asp = 1)

text(x=r.pc.x[,c(1,2)], labels = colnames(normalized_data), cex = 1, col = "blue")


plot(cos((0:360)/180*pi), sin((0:360)/180*pi), type = "l", lty = "dashed",
           col = "red",
           xlab = "cor with PC1", ylab = "cor with PC3",
           main = "PC1 vs PC23",asp = 1)
text(x=r.pc.x[,c(1,3)], labels = colnames(normalized_data), cex = 1, col = "blue")

plot(cos((0:360)/180*pi), sin((0:360)/180*pi), type = "l", lty = "dashed",
           col = "red",
           xlab = "cor with PC2", ylab = "cor with PC3",
           main = "PC2 vs PC3",asp = 1)
text(x=r.pc.x[,c(2,3)], labels = colnames(normalized_data), cex = 1, col = "blue")

par(mfrow =c(1,1))



# =============================== Factor Analysis (FA) ============================================================

# fa_data <- normalized_data %>%
#   dplyr::select(age, trestbps, chol, thalach, oldpeak, ca)
# # select(where(is.numeric))
# 
# head(fa_data)
# cor(fa_data)

options(digits=3) # print everything with 3 digits

# Function to detect how many factors are reasonable
get_d = function(k, p){
  return(0.5*(k-p)^2 - 0.5*(p+k))
}
x <- as.matrix(normalized_data)
n <- nrow(x)
p <- ncol(x)

d_ <- get_d(k=1:6, p=p)
d_

# ----------------The Principal Component Method------------------

eigen <- eigen(cov(x))
eigen


plot(eigen$values)

# All loadings eigenvectors are:
Q <- eigen$vectors %*% diag(sqrt(eigen$values))
Q

# We only retain the first k = 2 factors:
Q <- Q[,1:2]
Q

# Estimated specific variances are provided by the diagonal elements of the matrix R 
psi <- diag(cov(x) - Q %*% t(Q))
psi

# Estimated loadings after varimax:
rot.vmax <- varimax(Q)
Q.rot <- rot.vmax$loadings
Q.rot

# Ψ after rotation:
psi.rot <- diag(cov(x) - Q.rot %*% t(Q.rot))
psi.rot

# ----------Maximum likelihood method---------------

# Output: Any number of factors in [1, 2, 3] is fine but choose 1 or 2
k <- 2
d <- d_[k]

d

# Estimate the Factor Model using Maximum Likelihood 
fa.data <- factanal(~., 
                    factors = k,
                    rotation = "none",
                    scores = "regression", 
                    data = data.frame(x))
fa.data
#Communality and uniquenesses
fa.data$loadings %*% t(fa.data$loadings) + diag(fa.data$uniquenesses)
cor(fa_data)

# communality
apply(fa.data$loadings^2, 1, sum) 

# uniqueness
1 - apply(fa.data$loadings^2, 1, sum) 

#Likelihood ratio test (Recall: Wilk)
Q <- fa.data$loadings
t_stat <- n * log(det(Q %*% t(Q) + diag(fa.data$uniquenesses)) / det(cor(fa_data)))
t_stat

# Improve the test statistics (Bartlett 1954)
t_stat <- (n-1-(2*p+4*k+5)/6) * log(det(Q %*% t(Q) + diag(fa.data$uniquenesses)) / det(cor(fa_data)))
t_stat
pchisq(t_stat, df = d, lower.tail = FALSE)

# Estimate the Factor Model using Maximum Likelihood
# and a variation roation
fa.data.varimax <- factanal(~., 
                            factors = k,
                            rotation = "varimax",
                            scores = "regression", 
                            data = data.frame(x))
fa.data.varimax$loadings

# Visualization #######
par(mfrow=c(1,2))
# par(mfrow=c(2,2))
plot(cbind(cos((0:360)/180*pi), sin((0:360)/180*pi)),
     type = 'l',
     lty = 'dotted',
     xlab = 'Factor1 ', 
     ylab = 'Factor2',
     main = "No Rotation",
     repel = TRUE)
abline(h=0)
abline(v=0)
text(fa.data$loadings[,1:2],
     labels=colnames(fa_data),
     col="black")

plot(cbind(cos((0:360)/180*pi),sin((0:360)/180*pi)),
     type="l",
     lty="dotted",
     xlab = "Factor 1",
     ylab = "Factor 2",
     main="Varimax",
     repel = TRUE)
abline(h = 0)
abline(v = 0)
text(fa.data.varimax$loadings[,1:2],
     labels=colnames(fa_data),
     col="black")

ggsave("fa.png", plot = fa, width = 15, height = 5)

cbind(fa.data$loadings[,1], fa.data.varimax$loadings[,1])
#-----------------------------------------------------------------------------------------------

# =============================== Correspondence Analysis (CA) ============================================================

# Categrical - fbs, restecg, exang, sex, cp, slope , thal

# 2. Chest Pain (CP)
cp_num <- table(data$cp, data$num)
colnames(cp_num) <- c("No","L1","L2","L3","L4")
rownames(cp_num) <- c("typical angina","atypical angina","non-anginal pain","asymptomatic")

cp.cra <- FactoMineR::CA(cp_num, graph = TRUE)

# 2. Resting Electrocardiographic Results (restecg)
restecg_num <- table(data$restecg, data$num)
colnames(restecg_num) <- c("No","L1","L2","L3","L4")
rownames(restecg_num) <- c("normal","st wave abnormal","l.v.hypertrophy")

restecg.cra <- FactoMineR::CA(restecg_num, graph = TRUE)

# 3. The slope of peak exercise ST segment (slope)
slope_num <- table(data$slope, data$num)
colnames(slope_num) <- c("No","L1","L2","L3","L4")
rownames(slope_num) <- c("upsloping","flat","downsloping")

slope.cra <- FactoMineR::CA(slope_num, graph = TRUE)


# 4. Thallium stress 
thal_num <- table(data$thal, data$num)
colnames(thal_num) <- c("No","L1","L2","L3","L4")
rownames(thal_num) <- c("Normal result","Fixed defect","Reversible defect")

thal_num.cra <- FactoMineR::CA(thal_num, graph = TRUE)


#for Binary

# 1. SEX
sex_num <- table(data$sex, data$Target) # rows are for sex, columns are for num, 
colnames(sex_num) <- c("No","Yes")
rownames(sex_num) <- c("Female","Male")

sex.cra <- FactoMineR::CA(sex_num, graph = TRUE)

sex.cra
sex.cra$row$coord
sex.cra$col$coord

# 2. Fasting blood sugar
fbs_num <- table(data$fbs, data$Target) # rows are for sex, columns are for num, 
colnames(fbs_num) <- c("No","Yes")
rownames(fbs_num) <- c("<120",">120")

fbs.cra <- FactoMineR::CA(fbs_num, graph = TRUE)
fbs.cra$row$coord
fbs.cra$col$coord

# 3. Exercise Induced Agina 
exang_num <- table(data$exang, data$Target) # rows are for sex, columns are for num, 
colnames(exang_num) <- c("No","Yes")
rownames(exang_num) <- c("normal blood","reduced blood")

exang.cra <- FactoMineR::CA(exang_num, graph = TRUE)
exang.cra$row$coord
exang.cra$col$coord

#Chisq test
chisq.test(cp_num)
chisq.test(restecg_num)
chisq.test(slope_num)
chisq.test(ca_num)

chisq.test(thal_num)
chisq.test(sex_num)

chisq.test(fbs_num)
chisq.test(exang_num)

# =============================== Discriminant Analysis (DA) ============================================================

# LDA with original label data 
y.true <- data[, 14]

table(as.factor(y.true))

data.lda <- lda(num ~ . - Target, data=data, prior=c(160, 54, 35, 35, 13)/297)
data.lda.predict <- predict(data.lda)
mean(y.true == data.lda.predict$class)
table(y.true, data.lda.predict$class)
table(y.true == data.lda.predict$class)

# Adjust the labels to use just 0 and 1 ---------------------------------------
data.adjust <- data %>%
  mutate(num = ifelse(num == 0, 0, 1),
         num = as.factor(num)) %>%
  dplyr::select(-Target)

y.true.adjust <- data.adjust[,14]

# LDA 
data.lda <- lda(num ~ ., data=data.adjust, prior = c(0.5, 0.5))
# data.lda <- lda(num ~ ., data=data.adjust)
data.lda.predict <- predict(data.lda)
mean(y.true.adjust == data.lda.predict$class) # portion of correct predicted data
table(y.true.adjust, data.lda.predict$class) # comparison for the predicted labels
table(y.true.adjust == data.lda.predict$class) # number of prediction distribution 

# QDA 
data.qda <- qda(num ~ ., data = data.adjust, prior = c(0.5, 0.5))
# data.qda <- qda(num ~ ., data = data.adjust)
data.qda.predict <- predict(data.qda)
mean(y.true.adjust == data.qda.predict$class)
table(y.true.adjust, data.qda.predict$class)
table(y.true.adjust == data.qda.predict$class)

# # using adjusted data from the fa -------------------------------------

# fa.data.lda <- data.adjust %>%
#   cbind(fa.data.varimax$scores) %>%
#   dplyr::select(-c(age, trestbps, chol, thalach, oldpeak, ca)) 
# 
# # LDA
# # lda.factor.data <- lda(num ~ ., data = fa.data.lda, prior = c(0.5, 0.5))
# lda.factor.data <- lda(num ~ ., data = fa.data.lda)
# lda.factor.data.predict <- predict(lda.factor.data)
# mean(y.true.adjust == lda.factor.data.predict$class)
# table(y.true.adjust, lda.factor.data.predict$class)
# table(y.true.adjust == lda.factor.data.predict$class)
# 
# # QDA 
# # fa.data.qda <- qda(num ~ ., data = fa.data.lda, prior = c(0.5, 0.5))
# fa.data.qda <- qda(num ~ ., data = fa.data.lda)
# fa.data.qda.predict <- predict(fa.data.qda)
# mean(y.true.adjust == fa.data.qda.predict$class)
# table(y.true.adjust, fa.data.qda.predict$class)
# table(y.true.adjust == fa.data.qda.predict$class)

# using adjusted data from PCA -------------------------------------------

pca.data.lda <- data.adjust %>%
  cbind(data.pca$x[,1:4]) %>%
  dplyr::select(-c(age, trestbps, chol, thalach, oldpeak, ca)) 

# LDA
# lda.pca.data <- lda(num ~ ., data = pca.data.lda, prior = c(0.5, 0.5))
lda.pca.data <- lda(num ~ ., data = pca.data.lda)
lda.pca.data.predict <- predict(lda.pca.data)
mean(y.true.adjust == lda.pca.data.predict$class)
table(y.true.adjust, lda.pca.data.predict$class)
table(y.true.adjust == lda.pca.data.predict$class)

# QDA 
# pca.data.qda <- qda(num ~ ., data = pca.data.lda, prior = c(0.5, 0.5))
pca.data.qda <- qda(num ~ ., data = pca.data.lda)
pca.data.qda.predict <- predict(pca.data.qda)
mean(y.true.adjust == pca.data.qda.predict$class)
table(y.true.adjust, pca.data.qda.predict$class)
table(y.true.adjust == pca.data.qda.predict$class)


plot(data.lda, type="both")
plot(lda.factor.data, type="both")
svg("DA_PCA_Density_Plot.svg")
plot(lda.pca.data, type="both", ylab="Density")
dev.off()

# Optimising the training using cross-validation and mlr3 package
library(mlr3)
library(mlr3viz)

tasks <- list(TaskClassif$new(data.adjust, target="num" ,id="heart_disease"),
              # TaskClassif$new(fa.data.lda, target="num" ,id="heart_disease_fa"),
              TaskClassif$new(pca.data.lda, target="num" ,id="heart_disease_pca"))

learners <- list(lrn("classif.lda", label="lda_weighted"),
                 lrn("classif.lda", label="lda_no_weight" , prior=c(0.5, 0.5)),
                 lrn("classif.qda", label="qda_weighted"),
                 lrn("classif.qda", label="qda_no_weight", prior=c(0.5, 0.5)))

resampling <- rsmp("cv", folds = 10)

benchmark_design <- benchmark_grid(tasks, learners, resampling)

bmr <- benchmark(benchmark_design)

bmr$aggregate()

bmr %>% autoplot %>% ggsave(filename="last.svg")


