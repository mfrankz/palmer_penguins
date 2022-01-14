# Data tidying, visualization, and analysis with the Palmer Penguins Dataset 
This tutorial uses a model comparison approach to analyze a publicly available dataset in R. Tidyverse tools are used for data cleaning, and ggplot2 and the performance library are used for data visualization. 

To begin, we will load packages and import the dataset 

### 1. Import data and packages in RStudio: 
```
#load palmer penguins data
install.packages("palmerpenguins")
library(palmerpenguins)
penguins

#load other libraries
library(car)
library(performance)
library(dplyr)
library(emmeans)
```

### 2. Before analyzing our data, let's develop a research question. I am interested in finding a model that can be used to predict the body mass of penguins. We will first plot the distribution of body mass to get an idea of whether we can use a linear model.
```
#plot distribution of body mass
hist(penguins$body_mass_g)
```
<img src="https://github.com/mfrankz/palmer_penguins/blob/main/mass_dist.png" width="300">

Body mass looks relatively normally distributed, so we will build linear regression models


### 3. Let's consider three "demographic" variables: species, sex, and island of origin. I am going to create linear models with different possible combinations of these variables. 
```
#create models for model comparison
m1<-lm(body_mass_g~species, data=penguins)
m2<-lm(body_mass_g~sex, data=penguins)
m3<-lm(body_mass_g~island, data=penguins)
m4<-lm(body_mass_g~species*sex, data=penguins)
m5<-lm(body_mass_g~species*island, data=penguins)
m6<-lm(body_mass_g~sex*island, data=penguins)
m7<-lm(body_mass_g~species*sex*island, data=penguins)
```

### 4. Instead of assessing the effects of these variables using a null-hypothesis testing approach, we will compare the models using various metrics of model strength (AIC, BIC, R squared, and RMSE). The performance library is used to compare these metrics across models.
```
#create comparison of metrics
comp<-compare_performance(m1,m2,m3,m4,m5,m6,m7,metrics=c("AIC", "BIC",  "R2", "RMSE"), rank=TRUE)

#set plotting theme
my_theme<-theme(
  plot.title = element_text(size=35, face="bold"),
  legend.title = element_text(size = 30, face="bold"),
  legend.text = element_text(size = 25, face="bold"),
  axis.text.x = element_text(size=25, face="bold"),
  axis.text.y = element_text(size=25, face="bold"),
  axis.title.x = element_text(size=25, face="bold", color="black"),
  axis.title.y = element_text(size=25, face="bold", color="black"),
)

#create model comparison plot
plot(comp, size=2)+ 
  ggtitle("Predicting Mass (g) of Palmer Penguins")+ 
  scale_color_brewer(labels = c("Species","Sex", "Island", 
    "Species*Sex", "Species*Island", "Sex*Island",
    "Species*Sex*Island"), 
          palette="Paired")+
  scale_x_discrete(breaks=c("AIC_wt","BIC_wt","R2", "RMSE"),
                   labels=c("AIC", "BIC", expression(R^2), "RMSE"))+
  my_theme
```
<img src="https://github.com/mfrankz/palmer_penguins/blob/main/penguin_comparisons.png" width="500">

We can see from this plot that model4 (species * sex) is the strongest approach for predicting body mass across every metric of model strength. Adding the island variable (model 7) does not substantially improve the R^squared or RMSE, and worsens the AIC and BIC (these metrics are penalized when additional predictors are added to the model).  We will now further inspect this model. 

### 5. Check the linear regression assumptions using the performance library
```
check_model(model4)
```

### 6. Calculate average body mass across species and sex
```
descriptives<-na.omit(penguins)%>% 
  group_by(species,sex) %>% 
  dplyr::summarize(avg_mass=mean(body_mass_g))
```

### 7. Plot the data 
```
ggplot(data=descriptives, aes(x=species, y=avg_mass, fill=species))+
  geom_bar(stat="identity", color="black")+
  facet_wrap(~sex)+
  scale_fill_brewer(palette="Set1")+
  ylab("Mass (g)")+
  xlab("Species")+
  my_theme+
  theme(panel.background = element_rect(fill="azure3", colour="azure3"),  
          panel.border = element_rect(colour = "black", fill=NA, size=2), 
          strip.text.x = element_text(size = 25, face="bold"),
          strip.background = element_rect(color="white", fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none")
```
<img src="https://github.com/mfrankz/palmer_penguins/blob/main/penguin_mass.png" width="500">

### 8. Conduct null-hypothesis testing and view main effects and interaction of species and sex
```
Anova(model4, type="III")
summary(model4)
```
The output here shows that there is a significant main effect of species and sex, and a species * sex interaction. You can use the emmeans() function to better understand the direction/magnitude of these effects. The plot can also help you determine the direction of effects.
```
emmeans(model4, pairwise~species+sex)
```
                      


