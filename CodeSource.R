library(haven)
library(dplyr)
library(psych)
library(GGally)
library(ggcorrplot) 
library(FactoMineR) 
library(factoextra)   
library(lavaan)
library(tidyverse)
library(GPArotation)

df<-read_dta("Global_Party_Survey.dta")


populism_vars <- df %>% select(V8_Scale, V9, V18, V19, V20, V21)

pop_var_indir <- df %>% select(V18, V19, V20, V21)

##Plotting the data

populism_vars%>%ggplot(aes(y=V8_Scale))+
  geom_bar(width = .1) 






alpha_result <- psych::alpha(populism_vars)
alpha_result

cor_matrix <- cor(populism_vars, use = "complete.obs")
ggcorrplot(cor_matrix, lab = TRUE, title = "Correlation Matrix for Populism Indicators")

##PCA
pca_result <- PCA(populism_vars, scale.unit = TRUE)
fviz_eig(pca_result)
fviz_pca_var(pca_result, col.var = "contrib", repel = TRUE) +
  labs(title = "PCA of Populism Indicators")
summary(pca_result)

##Explorative Component Analysis
nfac<-VSS(populism_vars,n=8,rotate="none",plot =FALSE)
nfac

paf_fit<-fa(populism_vars, nfactors=2, fm = "pa", rotate="varimax")
summary(paf_fit)
paf_fit
print(paf_fit$loadings, cutoff = 0.1) 
fa.diagram(paf_fit, simple = TRUE, main = "EFA Factor Loadings Diagram")


##The PCA (Principal Component Analysis)  and ECA show that we could build two
##separate dimension of Populism




cfa1<-'WillOfPpl=~V18+V19
        AntiElitism=~V20'

cfa(cfa1,data=df)


shapiro_test<- shapiro.test(populism_vars[[var]])

shapiro_test





