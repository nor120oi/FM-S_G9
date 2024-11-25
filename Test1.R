library(sjPlot)
library(tidyverse)
library(haven)
library(vdemdata)


df<-read_dta("Global_Party_Survey.dta")


##PERSONAL BIAS

df1<-df%>%filter(Gender!=9)

plot1<-df1%>%ggplot(aes(x=Ideology,y=Type_Populism))+
  geom_point()+
  geom_smooth(method="lm")
plot1

summary(lm(df1$Type_Populism~df1$Ideology))


plot2<-df1%>%ggplot(aes(x=Difficulty,y=Type_Populism))+
  geom_point()+
  geom_smooth(method="lm")
plot2

summary(lm(df1$Type_Populism~df1$Difficulty))



dfreg1<-df%>%filter(Region==1)
dfreg2<-df%>%filter(Region==2)
dfreg3<-df%>%filter(Region==3)
dfreg4<-df%>%filter(Region==4)
dfreg5<-df%>%filter(Region==5)
dfreg6<-df%>%filter(Region==6)


populism_vars_reg1 <- dfreg1 %>% select(V8_Scale, V9, V18, V19, V20, V21)
populism_vars_reg2 <- dfreg2 %>% select(V8_Scale, V9, V18, V19, V20, V21)
populism_vars_reg3 <- dfreg3 %>% select(V8_Scale, V9, V18, V19, V20, V21)
populism_vars_reg4 <- dfreg4 %>% select(V8_Scale, V9, V18, V19, V20, V21)
populism_vars_reg5 <- dfreg5 %>% select(V8_Scale, V9, V18, V19, V20, V21)
populism_vars_reg6 <- dfreg6 %>% select(V8_Scale, V9, V18, V19, V20, V21)


cor_matrix_reg1 <- cor(populism_vars_reg1, use = "complete.obs")
ggcorrplot(cor_matrix_reg1, lab = TRUE, title = "Correlation Matrix for Populism Indicators")

cor_matrix_reg2 <- cor(populism_vars_reg2, use = "complete.obs")
ggcorrplot(cor_matrix_reg2, lab = TRUE, title = "Correlation Matrix for Populism Indicators")

cor_matrix_reg3 <- cor(populism_vars_reg3, use = "complete.obs")
ggcorrplot(cor_matrix_reg3, lab = TRUE, title = "Correlation Matrix for Populism Indicators")

cor_matrix_reg4 <- cor(populism_vars_reg4, use = "complete.obs")
ggcorrplot(cor_matrix_reg4, lab = TRUE, title = "Correlation Matrix for Populism Indicators")

cor_matrix_reg5 <- cor(populism_vars_reg5, use = "complete.obs")
ggcorrplot(cor_matrix_reg5, lab = TRUE, title = "Correlation Matrix for Populism Indicators")

cor_matrix_reg6 <- cor(populism_vars_reg6, use = "complete.obs")
ggcorrplot(cor_matrix_reg6, lab = TRUE, title = "Correlation Matrix for Populism Indicators")


pca_result_reg1 <- PCA(populism_vars_reg1, scale.unit = TRUE)
fviz_eig(pca_result_reg1)
fviz_pca_var(pca_result_reg1, col.var = "contrib", repel = TRUE) +
  labs(title = "PCA of Populism Indicators In Eastern Europe")


pca_result_reg5 <- PCA(populism_vars_reg5, scale.unit = TRUE)
fviz_eig(pca_result_reg1)
fviz_pca_var(pca_result_reg5, col.var = "contrib", repel = TRUE) +
  labs(title = "PCA of Populism Indicators In Western Europe")


populism_vars_reg5%>%ggplot(aes(x=V18,y=V20))+
  geom_point()+
  geom_smooth(method = "lm",formula = y~x)








### WTF IS GOING ON HERE WHY RED DOTS ????????
###TEST


populism_vars_reg5TestPol <- dfreg5 %>% select(V10,V13,V15,V8_Scale, V9, V18, V19, V20, V21)

cor_matrix_reg5TestPol <- cor(populism_vars_reg5TestPol, use = "complete.obs")
ggcorrplot(cor_matrix_reg5TestPol, lab = TRUE)

nfacPolTest<-VSS(populism_vars_reg5TestPol,n=10,rotate="none",plot =TRUE)
nfacPolTest

paf_fitPolTest<-fa(populism_vars_reg5TestPol, nfactors=2, fm = "pa",rotate="varimax")

summary(paf_fitPolTest)
paf_fitPolTest
print(paf_fitPolTest$loadings, cutoff = 0.1) 
fa.diagram(paf_fitPolTest, simple = TRUE, main = "EFA Factor Loadings")






