# Data tidying, visualization, and analysis with the Palmer Penguins Dataset 
This tutorial demonstrates how to use a model comparison approach with a publicly available dataset in R. Tidyverse tools are used for data cleaning, and ggplot2 and the performance library are used for data visualization. 

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

### 2. Visualize relationships among variables using standard Pearson correlations. We will create a correlation table and plot the results in a heatmap.
```
#create correlation table
cormat <- round(cor(penguins[, c(3:6)], use = "pairwise.complete.obs"),2)
head(cormat)

# Get inverse of lower triangle 
get_lower_tri<-function(cormat){
  cormat[apply(lower.tri(cormat), 1, rev)] <- NA
  return(cormat)
}

#Order data
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
cormat <- reorder_cormat(cormat)
cormat<-cormat[,c(4:1)] #change number 4 to your number of rows in cormat

#select either the lower triangle of the matrix 
lower_tri <- get_lower_tri(cormat)
melted_cormat <- melt(lower_tri, na.rm = TRUE)

## Step 5. Create plot
ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(high = "#0A8819", low = "#C11919", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  guides(fill = guide_colorbar(barwidth = 3, barheight = 8,
                               title.position = "top", title.hjust = 0.5))+
  my_theme
ggsave("heatmap.png", width = 25, height = 25, units = "cm")
```
<img src="https://github.com/mfrankz/palmer_penguins/blob/main/heatmap.png" width="300">

### 3. A model comparison approach can be used as an alternative to classic null-hypothesis testing. Instead of placing emphasis on significance testing, you can create different statistical models (using the same data) and compare those models using a fit metric, such as AIC or BIC. To illustrate this approach with Palmer Penguins, let's develop a research question. I am interested in finding a model that can be used to predict the body mass of penguins. We will first plot the distribution of body mass to get an idea of whether we can use a linear model.
```
#plot distribution of body mass
hist(penguins$body_mass_g)
```
<img src="https://github.com/mfrankz/palmer_penguins/blob/main/mass_dist.png" width="300">

Body mass looks relatively normally distributed, so we will build linear regression models


### 4. Let's consider three variables that might be predictive of body mass: species, sex, and island of origin. I am going to create linear models with different possible combinations of these variables. 
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

### 5. Instead of assessing the effects of these variables using a null-hypothesis testing approach, we will compare the models using various metrics of model strength (AIC, BIC, R squared, and RMSE). The performance library is used to compare these metrics across models by generating a ranked score (i.e., larger values in the plot signify better model strength).
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

We can see from this plot that m4 (species * sex) is the strongest approach for predicting body mass across every metric of model strength. Adding the island variable (model 7) does not substantially improve the R^squared or RMSE, and worsens the AIC and BIC (these metrics control for multiple predictors).  We will now further inspect this model. 

### 6. Check the linear regression assumptions using the performance library
```
check_model(m4)
```

### 7. Calculate average body mass across species and sex
```
descriptives<-na.omit(penguins)%>% 
  group_by(species,sex) %>% 
  dplyr::summarize(avg_mass=mean(body_mass_g))
```

### 8. Plot the data 
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

### 9. Conduct null-hypothesis testing and view main effects and interaction of species and sex
```
Anova(m4, type="III")
summary(m4)
```
The output here shows that there is a significant main effect of species and sex, and a species * sex interaction. You can use the emmeans() function to better understand the direction/magnitude of these effects. The plot can also help you determine the direction of effects.
```
emmeans(m4, pairwise~species+sex)
```
                      


