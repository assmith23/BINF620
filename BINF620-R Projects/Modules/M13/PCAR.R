#https://rforhealthcare.org/principal-component-analyses/
install.packages("factoextra")
install.packages("FactoMineR")

library(factoextra)
library(FactoMineR)

df_num <- data.frame(los=rexp(100, 5),
                     weight=rnorm(100, 78, 12),
                     height=rnorm(100, 165, 10),
                     procedures=rpois(100, 1.5),
                     age=rnorm(100, 45, 10),
                     complexity=rexp(100, 2),
                     severity=rexp(100, 3))
df_num$bmi <- df_num$weight/((df_num$height/100)^2)
head(df_num)
str(df)

df_pca <- prcomp(df_num,center=TRUE,scale.=TRUE)
fviz_screeplot(df_pca,labels=TRUE)

vars <- get_pca_var(df_pca)
vars$contrib

fviz_pca_var(df_pca)
fviz_pca_ind(df_pca)
fviz_pca_biplot(df_pca)
fviz_pca(df_pca)
fviz_contrib(df_pca, choice = "var", axes = 1)
fviz_contrib(df_pca, choice = "var", axes = 2)

#Multiple correspondence analysis
df_cat <- data.frame(admitted=sample(0:1,100,replace=TRUE),
                     admitType=sample(1:4,100,replace=TRUE),
                     firstVisit=sample(0:1,100,replace=TRUE),
                     complex=sample(0:1,100,replace=TRUE),
                     ageGroup=sample(1:6,100,replace=TRUE),
                     diagCat=sample(1:10,100,replace=TRUE),
                     day=sample(1:7,100,replace=TRUE))
head(df_cat)

df_cat <- as.data.frame(lapply(df_cat, factor))
str(df_cat)

require(FactoMineR)

df_mca <- MCA(df_cat,graph=FALSE)
fviz_screeplot(df_mca,labels=TRUE)

vars <- get_mca_var(df_mca)
vars$contrib

fviz_mca_var(df_mca)
fviz_contrib(df_mca, choice = "var", axes = 1)
fviz_contrib(df_mca, choice = "var", axes = 2)

#Factor analysis of mixed data
df_mix <- data.frame(admitType=sample(1:4,100,replace=TRUE),
                     day=sample(1:7,100,replace=TRUE),
                     diagCat=sample(1:10,100,replace=TRUE),
                     complex=sample(0:1,100,replace=TRUE),
                     los=rexp(100, 5),
                     weight=rnorm(100, 78, 12),
                     height=rnorm(100, 165, 10),
                     procedures=rpois(100, 1.5),
                     age=rnorm(100, 45, 10))
head(df_mix)

fac <- c(1:4)
facNames <- names(df_mix[fac])
df_mix[facNames] <- lapply(df_mix[facNames], factor)
str(df_mix)

df_famd <- FAMD(df_mix, graph=FALSE)

eig.val <- df_famd$eig
x <- barplot(eig.val[, 2], 
             names.arg = 1:nrow(eig.val), 
             main = "Variances Explained by Dimensions (%)",
             xlab = "Principal Dimensions",
             ylab = "Percentage of variances",
             col ="steelblue")

lines(x = x, eig.val[, 2], 
      type = "b", pch = 19, col = "red")

vars <- get_famd_var(df_famd)
vars$contrib

fviz_famd_var(df_famd, repel = TRUE)
fviz_contrib(df_famd, "var", axes = 1)
fviz_contrib(df_famd, "var", axes = 2)
