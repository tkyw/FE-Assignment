library("FinTS")
library("tseries")
library("tidyverse")
library("dplyr")
library("plyr")
library("readxl")
library("ggplot2")
library("sjPlot")
library("sjmisc")
library("lubridate")
library("broom")
library("faraway")
library("corrplot")
library("DescTools")
library("stats")
library("lmtest")
library("orcutt")

df <- read_excel("C:\\Users\\USER\\Python Programme\\uni\\fe_assignment\\combine_data.xlsx", sheet = 1)
df$Date = as.Date(df_trans$Date, format="%m/%d/%Y")
df = df %>% column_to_rownames(., var="Date")

model = lm(spot ~ ., data=df)

### Autocorrelation

acf(model$residuals, type="correlation", main="ACF Plot")
## acf(ori_model$residuals, type="correlation", main="ACF Before PCA")


lmtest::dwtest(model)

lmtest::bgtest(model, order=3)

#### Autocorrelation: Remedy

modelco <- cochrane.orcutt(model,convergence = 5, max.iter=1000)
rho<- modelco$rho
rho
clipr::write_clip(df_trans)

df_trans <- read_excel("C:\\Users\\USER\\Python Programme\\uni\\fe_assignment\\pca1.xlsx", sheet=4)
model_trans <- lm(`spot*` ~ ., data=df_trans)
par(mfrow=c(1,1), cex=1.1)
acf(model_trans$residuals, type="correlation", main="ACF After Cochrane Orcutt Transformation")
lmtest::dwtest(model_trans)
lmtest::bgtest(model_trans, order=1)

summary(model)
summary(model_trans)



### multicollienarity -> PCR

df_trans %>% dim
df_trans.descr <- df_trans

df_trans.y <- df_trans.descr$`spot*`
df_trans.descr$`spot*` <- NULL

res <- cor(df_trans.descr, method="pearson")
res
colnames(res)
corrplot::corrplot(res, method= "color", order = "hclust", tl.cex = .7, type="full")

df_trans.norm <- scale(df_trans.descr, center = TRUE, scale = TRUE)
df_trans.y.norm <- scale(df_trans.y, center = TRUE, scale = TRUE)

df_trans.pca1 <- prcomp(df_trans.norm, center=TRUE, scale.=TRUE)

df_trans.pca1
df_trans.pca1$x [,1:6]  

res1 <- cor(df_trans.pca1$x, method="pearson")
res1
png("correlation after PCA.png")                              
corrplot::corrplot(res1, tl.cex=.7, method="square")
dev.off()
?corrplot

summary(df_trans.pca1)

png("percentage_variance_explained.png", width=589, height=384)
par(mfrow=c(1,1))
plot(summary(df_trans.pca1)$importance[2,], ylab="% Variance Explained", xlab="n components", type="o", main="Scree Plot of Eigenvalue")
dev.off()
summary(df_trans.pca1)

pcs <- as.data.frame(df_trans.pca1$x)
ols.data <- cbind(df_trans.y.norm, pcs)
ols.data
lmodel <- lm(df_trans.y.norm ~ ., data = ols.data)
barplot(vif(lmodel), horiz = TRUE, col="steelblue", main="VIF Values After PCA", cex.names = .8, xlim=c(0,4), xlab="VIF")
clipr::write_clip(summary(lmodel) %>% tidy)
summary(lmodel)


beta.Z <- as.matrix(lmodel$coefficients[2:7])
beta.Z
V <- as.matrix(df_trans.pca1$rotation)
beta.X <- V %*% beta.Z
beta.X
