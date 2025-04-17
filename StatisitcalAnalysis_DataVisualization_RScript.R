## This code corresponds to the statistical analysis conducted for the honours thesis project entitled 
# "Investigating the Relationship Between Immigration Status and Mortality During the 1918 Influenza Pandemic in Cleveland, Ohio". 

## "PI" refers to pneumonia and/or influenza related causes of death
## "COD" means "Cause of Death"
## "DOD" means "Date of Death"
## "Status" refers to immigration status (i.e., Domestic, First-Generation, or Second-Generation)
## "Age" refers to the individuals age at time of death in years (Grouped into the age bins of: 0-19, 20-39, 40-59, and 60+)


# Import necessary packages
library(tidyverse)
library(ggplot2)
library(lubridate)
library(patchwork)
library(grid)
library(dplyr)
library(tidyr)
library(tibble)
library(emmeans)


# Import data set
AllData<-read.csv(file.choose(),header=TRUE) # Choose file "All_Data_Cleaned_Thesis.csv"
summary(AllData) # View the imported data
DData<-subset(AllData,select=-c(Age,COD,X,X.1,X.2,X.3,X.4)) # Removing unnecessary rows
summary(DData) # DData is now the data set to work with
str(DData) # Ensure data is being read properly

## Creating a subset of all data, only for PI-related deaths
DDataPI<-subset(DData, COD_Cleaned=="PI") 


####-------------------------------------------------

### Statistical analysis 

## Ensuring the data follows certain trends of the 1918 Influenza Pandemic
# Visualizing the data, looking at all death counts for each month

## Figure 1:
# Making a data frame with the monthly death counts for the Control and Flu
data <- data.frame(
  Sept = c(17, 14),               
  Oct = c(17, 48),
  Nov = c(15, 46),
  Dec = c(16, 35),
  Jan = c(22,25),
  Feb = c(16, 24),
  Mar = c(19, 29),
  Apr = c(22, 24)
)
rownames(data) <- c("Control", "Flu")  # Making the row names

# Convert row names into a column
data_long <- data %>%
  rownames_to_column(var = "Group") %>%  
  pivot_longer(cols = -Group, names_to = "Month", values_to = "Deaths")

# Ensuring the months read in order
data_long<-data_long%>%    
  mutate(Month = factor(Month, levels = c("Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr")))

# Line graph showing the number of deaths for each month in each respective timeline (Control and Flu)
ggplot(data_long, aes(x = Month, y = Deaths, color = Group, group = Group)) +
  geom_line(size= 1.1) +
  scale_color_manual(values = c("Control" = "black", "Flu" = "azure3")) +  
  labs(x = "Month", y = "Number of Deaths", title = "Monthly Death Counts by Timeline") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 50)) +
  theme(
    axis.text = element_text(size = 18, face = "bold",colour = "black"),       
    axis.title.x = element_text(size = 19, face = "bold", colour = "black"),     
    axis.title.y = element_text(size = 19,face = "bold", colour = "black"),    
    legend.title = element_text(size = 17, face = "bold", colour = "black"),    
    legend.text = element_text(size = 15, face = "bold", colour = "black"),
    plot.title = element_text(size = 18, face = "bold", colour = "black"),
    plot.background = element_rect(fill = "white", color = "white"), 
    axis.ticks = element_line(color = "black"),  
    panel.grid.major = element_line(color = "azure2", linewidth = 0.4),
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white", color = "white")) 

##---------------
## Trend 1: 
# Increase in the number of PI-related deaths between the Control and Flu
# Making a table showing the number of PI-related deaths and all other CODs between the Control and Flu
TCD_CF<-table(DData$Control_Flu, DData$COD_Cleaned)
TCOD_CF<-TCD_CF[, !colnames(TCD_CF) %in% c("Unknown")] # removing the unknown columns
TCOD_CF
TCOD_CF2<-as.data.frame(TCOD_CF) # Making it read as a data frame

## Visualizing the data
# Figure 2:
# Bar graph showing the number of PI deaths and all other CODs in the Control and Flu
F2A<- ggplot(TCOD_CF2, aes(x = Var1, y = Freq, fill = factor(Var2))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste("n=", Freq)), 
            position = position_dodge(width = 0.9),  
            vjust = -0.5, 
            size = 5,      
            color = "black") +
  labs(x = "Control vs Flu", y = "No. of Deaths", fill = "Cause of Death") +
  scale_fill_manual(values = c("black", "grey"), labels = c("Other", "PI")) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 20, face = "bold", colour = "black"),        
    axis.title.x = element_text(size = 22, face = "bold", colour = "black"),    
    axis.title.y = element_text(size = 22,face = "bold", colour = "black"),     
    legend.title = element_text(size = 17,face = "bold", colour = "black"),     
    legend.text = element_text(size = 15,face = "bold", colour = "black"),      
    plot.title = element_text(size = 18,face = "bold", colour = "black")) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(TCOD_CF2$Freq) + 10)) +
  theme(legend.position = "top")+
  labs(tag="A")+
  theme(
    plot.tag = element_text(size = 28, face = "bold"))

# Mosaic plot showing the proportion of PI deaths and all other CODs in the Control and Flu
F2B<- ggplot(TCOD_CF2, aes(x = Var1, y = Freq, fill = factor(Var2))) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  scale_fill_manual(values = c("black", "grey"), labels = c("Other", "PI")) +
  labs(x = "Control vs Flu", y = "Proportion", fill = "Cause of Death") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 19, face = "bold", colour = "black"),        
    axis.title.x = element_text(size = 22, face = "bold", colour = "black"),     
    axis.title.y = element_text(size = 22,face = "bold", colour = "black"),     
    legend.title = element_text(size = 17, face = "bold", colour = "black"),     
    legend.text = element_text(size = 15, face = "bold", colour = "black"),      
    plot.title = element_text(size = 18, face = "bold", colour = "black")) + 
  theme(legend.position = "top")+
  labs(tag="B")+
  theme(
    plot.tag = element_text(size = 28, face = "bold"))
Figure2 <- (F2A + F2B)


##
## Analysis
## Chi squared test assessing the difference in death counts between the Control and Flu for PI deaths and all other CODs
ChiTest_CD_PIvsO<-chisq.test(TCOD_CF)
ChiTest_CD_PIvsO_res<-ChiTest_CD_PIvsO$residuals # Looking at the residuals

##--------------

# Trend 2: Determining if a specific age group had a disproportionate number of deaths during the Flu
# All causes of death

# Making a table showing the number of all deaths for each age group between the Control and Flu
# Removing "unknown" rows from immigration status
DDData <- DData %>%
  filter(!Status %in% c("unknown", "Unknown"))

tage<-table(DDData$Age_Bin, DDData$Control_Flu)
tage2<-as.data.frame(tage) # Making it read as a data frame

## Visualizing the data
# Figure 3
# Bar graph showing the number of deaths for each age group between the Control and Flu
F3A<-ggplot(tage2, aes(x = Var1, y = Freq, fill = factor(Var2))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste("n=", Freq)), 
            position = position_dodge(width = 0.9),  
            vjust = -0.5,  
            size = 5,      
            color = "black") +
  labs(x = "Age", y = "No. of Deaths", fill = "Control vs Flu") +
  scale_fill_manual(values = c("black", "grey"), labels = c("Control", "Flu")) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 19, face = "bold", colour = "black"),        
    axis.title.x = element_text(size = 22, face = "bold", colour = "black"),     
    axis.title.y = element_text(size = 22, face = "bold", colour = "black"),     
    legend.title = element_text(size = 17, face = "bold", colour = "black"),     
    legend.text = element_text(size = 15, face = "bold", colour = "black"),      
    plot.title = element_text(size = 18, face = "bold", colour = "black")) + 
  theme(legend.position = "top")+
  labs(tag="A")+
  theme(
    plot.tag = element_text(size = 28, face = "bold"))

# Mosaic plot showing the proportion of deaths for each age group between the Control and Flu
F3B<-ggplot(tage2, aes(x = Var1, y = Freq, fill = factor(Var2))) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  scale_fill_manual(values = c("black", "grey"), labels = c("Control", "Flu")) +
  labs(x = "Age", y = "Proportion", fill = "Control vs Flu") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 18, face = "bold", colour = "black"),        
    axis.title.x = element_text(size = 20, face = "bold", colour = "black"),     
    axis.title.y = element_text(size = 20, face = "bold", colour = "black"),     
    legend.title = element_text(size = 17, face = "bold", colour = "black"),     
    legend.text = element_text(size = 15, face = "bold", colour = "black"),      
    plot.title = element_text(size = 18, face = "bold", colour = "black")) + 
  theme(legend.position = "top")+
  labs(tag="B")+
  theme(
    plot.tag = element_text(size = 28, face = "bold"))

Figure3<- (F3A + F3B)


## Analysis
# Chi squared test comparing the differences in death counts between the groups
ChiTest_AgeVSCF<-chisq.test(tage)
# Significant p-value produced
# Indicating that at least one age group experienced disproportionate mortality

##---------------------

## Repeating analysis with only PI-related deaths

# Making a table showing the number of PI-related deaths for each age group between the Control and Flu
TAge_CF_PI<-table(DDataPI$Control_Flu, DDataPI$Age_Bin)
TAge_CF_PI2<-as.data.frame(TAge_CF_PI) # Making the data read as a data frame

## Visualizing the data

# Figure 4:
# Bar graph showing the number of PI deaths for each age group between the Control and Flu
F4A<- ggplot(TAge_CF_PI2, aes(x = Var2, y = Freq, fill = factor(Var1))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste("n=", Freq)), 
            position = position_dodge(width = 0.9),  # Adjust text position for dodged bars
            vjust = -0.5,  # Adjust vertical position of text
            size = 5,      # Adjust text size
            color = "black") +
  labs(x = "Age Groups", y = "No. of Deaths", fill = "Control vs Flu") +
  scale_fill_manual(values = c("black", "grey"), labels = c("Control", "Flu")) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(TAge_CF_PI2$Freq) + 10)) +
  theme(
    axis.text = element_text(size = 18, face = "bold", colour = "black"),        
    axis.title.x = element_text(size = 20, face = "bold", colour = "black"),     
    axis.title.y = element_text(size = 20, face = "bold", colour = "black"),     
    legend.title = element_text(size = 17, face = "bold", colour = "black"),    
    legend.text = element_text(size = 15, face = "bold", colour = "black"),    
    plot.title = element_text(size = 18, face = "bold", colour = "black")) + 
  theme(legend.position = "top")+
  labs(tag="A")+
  theme(
    plot.tag = element_text(size = 28, face = "bold"))

# Mosaic plot showing the proportion of PI deaths for each age group between the Control and Flu
F4B<- ggplot(TAge_CF_PI2, aes(x = Var2, y = Freq, fill = factor(Var1))) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  scale_fill_manual(values = c("black", "grey"), labels = c("Control", "Flu")) +
  labs(x = "Age Groups", y = "Proportion", fill = "Control vs Flu") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 19, face = "bold", colour = "black"),       
    axis.title.x = element_text(size = 22, face = "bold", colour = "black"),     
    axis.title.y = element_text(size = 22, face = "bold", colour = "black"),     
    legend.title = element_text(size = 17, face = "bold", colour = "black"),     
    legend.text = element_text(size = 15, face = "bold", colour = "black"),      
    plot.title = element_text(size = 18, face = "bold", colour = "black")) + 
  theme(legend.position = "top")+
  labs(tag="B")+
  theme(
    plot.tag = element_text(size = 28, face = "bold"))

Figure4<- (F4A + F4B)


## Analysis
## Chi squared test comparing the differences in death counts between the groups
ChiTest_AgeVSCF_PI<-chisq.test(TAge_CF_PI)
# Chi squared test approximation may be incorrect given the limited data

## Fisher exact test to check the approximation
Fisher_AgeVSCF_PI<-fisher.test(TAge_CF_PI)
# Significant p-value produced
# Indicating that at least one age group experienced disproportionate mortality


###---------------


### Research Question 1
### Assessing if a specific immigration status experienced disproportional mortality 
## All CODs

## Import new dataset
NewData<-read.csv(file.choose(),header=TRUE)  # Import file named "Data_Categorical_Counts_Thesis.csv"
attach(NewData) # Attach the new file to ensure analysis uses the death counts 
names(NewData)
newdata<-as.data.frame(NewData) # Making new data read as a data frame


## Making a table comparing number of deaths in the Control and Flu for each immigration status
tb<-table(DDData$Status, DDData$Control_Flu)
tb2<-as.data.frame(tb) # Making it read as a data frame

## Visualizing the data

# Ensure the factor levels for Var1 (immigration status) are in the desired order
tb2$Var1 <- factor(tb2$Var1, levels = c("Domestic", "1st_Gen", "2nd_Gen"))

## Figure 5

# Bar plot showing the number of deaths for each immigration status in the Control and Flu
F5A<- ggplot(tb2, aes(x = Var1, y = Freq, fill = factor(Var2))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste("n=", Freq)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,  
            size = 5,      
            color = "black") +
  labs(x = "Status", y = "No. of Deaths", fill = "Control vs Flu") +
  scale_fill_manual(values = c("black", "grey"), labels = c("Control", "Flu")) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(tb2$Freq) + 10)) +
  theme(
    axis.text = element_text(size = 18, face = "bold", colour = "black"),        
    axis.title.x = element_text(size = 20, face = "bold", colour = "black"),    
    axis.title.y = element_text(size = 20, face = "bold", colour = "black"),     
    legend.title = element_text(size = 17, face = "bold", colour = "black"),     
    legend.text = element_text(size = 15, face = "bold", colour = "black"),      
    plot.title = element_text(size = 18, face = "bold", colour = "black")) + 
  theme(legend.position = "top")+
  labs(tag="A")+
  theme(
    plot.tag = element_text(size = 28, face = "bold"))

# Mosaic plot showing the proportion of deaths for each immigration status between the Control and Flu
F5B<-ggplot(tb2, aes(x = Var1, y = Freq, fill = factor(Var2))) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  scale_fill_manual(values = c("black", "grey"), labels = c("Control", "Flu")) +
  labs(x = "Status", y = "Proportion", fill = "Control vs Flu") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 18, face = "bold", colour = "black"),       
    axis.title.x = element_text(size = 20, face = "bold", colour = "black"),     
    axis.title.y = element_text(size = 20, face = "bold", colour = "black"),     
    legend.title = element_text(size = 17, face = "bold", colour = "black"),    
    legend.text = element_text(size = 15, face = "bold", colour = "black"),      
    plot.title = element_text(size = 18, face = "bold", colour = "black")) + 
  theme(legend.position = "top")+
  labs(tag="B")+
  theme(
    plot.tag = element_text(size = 28, face = "bold"))

Figure5<- (F5A + F5B)


## Analysis

## Creating a saturated generalized linear model of the variables, not including age
## for all CODs 
mod<-glm(Count~Control_Flu*Status, poisson, data = newdata)

# Creating a model, removing the interaction between timeline (Control/Flu) and immigration status to see if this interaction significantly impacts the fit of the model
mod2<-update(mod,~.-Control_Flu:Status)

# Analysis of deviance using a Chi squared test evaluating if the interaction significantly impacts the fit of the model
anova(mod,mod2, test="Chi") 
# No significant difference found between the two models
# Indicating that there is no significant difference in the death counts between the immigration statuses, between the Control and Flu


###---------------

### Research Question 2
### Assessing if a specific age and immigration status combination experienced disproportional mortality 
### All CODs

## Visualizing the data

# Making tables showing the number of deaths in each group between the Control and Flu
# For Domestic-Status
ddatadom<-subset(DDData, Status=="Domestic")
tagdom<-table(ddatadom$Age_Bin, ddatadom$Control_Flu)
tagedom<-as.data.frame(tagdom) # Making it read as a data frame

# For First-Generation
ddata1g<-subset(DDData, Status=="1st_Gen")
tagg1<-table(ddata1g$Age_Bin, ddata1g$Control_Flu)
tageg1<-as.data.frame(tagg1) # Making it read as a data frame

# For Second-Generation
ddata2g<-subset(DDData, Status=="2nd_Gen")
tagg2<-table(ddata2g$Age_Bin, ddata2g$Control_Flu)
tageg2<-as.data.frame(tagg2) # Making it read as a data frame


# Figure 7
# Mosaic plots showing the proportion of deaths occurring for each age group between the Control and Flu

# For Domestic-Status
F7A<- ggplot(tagedom, aes(x = Var1, y = Freq, fill = factor(Var2))) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  scale_fill_manual(values = c("black", "grey"), labels = c("Control", "Flu")) +
  labs(x = "Age", y = "Proportion", fill = "Control vs Flu") +
  theme_minimal() +
  ggtitle("Domestic") +
  theme(
    axis.text = element_text(size = 19, face = "bold", colour = "black"),        
    axis.title.x = element_text(size = 22, face = "bold", colour = "black"),     
    axis.title.y = element_text(size = 22, face = "bold", colour = "black"),   
    legend.title = element_text(size = 17, face = "bold", colour = "black"),    
    legend.text = element_text(size = 15, face = "bold", colour = "black"),      
    plot.title = element_text(size = 18, face = "bold", colour = "black")) + 
  theme(legend.position = "top")+
  labs(tag="A")+
  theme(
    plot.tag = element_text(size = 28, face = "bold"))

# For First-Generation
F7B<- ggplot(tageg1, aes(x = Var1, y = Freq, fill = factor(Var2))) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  scale_fill_manual(values = c("black", "grey"), labels = c("Control", "Flu")) +
  labs(x = "Age", y = "Proportion", fill = "Control vs Flu") +
  theme_minimal() +
  ggtitle("1st Generation")+
  theme(
    axis.text = element_text(size = 19, face = "bold", colour = "black"),        
    axis.title.x = element_text(size = 22, face = "bold", colour = "black"),     
    axis.title.y = element_text(size = 22, face = "bold", colour = "black"),     
    legend.title = element_text(size = 17, face = "bold", colour = "black"),     
    legend.text = element_text(size = 15, face = "bold", colour = "black"),      
    plot.title = element_text(size = 18, face = "bold", colour = "black")) + 
  theme(legend.position = "top")+
  labs(tag="B")+
  theme(
    plot.tag = element_text(size = 28, face = "bold"))

# For Second-Generation
F7C<- ggplot(tageg2, aes(x = Var1, y = Freq, fill = factor(Var2))) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  scale_fill_manual(values = c("black", "grey"), labels = c("Control", "Flu")) +
  labs(x = "Age", y = "Proportion", fill = "Control vs Flu") +
  theme_minimal() +
  ggtitle("2nd Generation") +
  theme(
    axis.text = element_text(size = 19, face = "bold", colour = "black"),        
    axis.title.x = element_text(size = 22, face = "bold", colour = "black"),    
    axis.title.y = element_text(size = 22, face = "bold", colour = "black"),     
    legend.title = element_text(size = 17, face = "bold", colour = "black"),     
    legend.text = element_text(size = 15, face = "bold", colour = "black"),     
    plot.title = element_text(size = 18, face = "bold", colour = "black")) + 
  theme(legend.position = "top")+
  labs(tag="C")+
  theme(
    plot.tag = element_text(size = 28, face = "bold"))

Figure7<- (F7A + F7B + F7C)


## Analysis

## Creating a saturated generalized linear model of the variables, including age
## for all causes of deaths 
model<-glm(Count~Control_Flu*Status*Age, poisson, data=newdata)

# Creating a model, removing the three-way interaction between timeline (Control/Flu), immigration status, and age to see if this interaction significantly impacts the model
model2<-update(model,~.-Control_Flu:Status:Age)

# Analysis of deviance using a Chi squared test evaluating if the interaction significantly impacts the fit of the model
anova(model,model2, test="Chi")  
# No significant difference found between the two models
# Indicating that the three-way interaction does not significantly impact the fit of the model
# Allowing to further investigate other interactions

# Creating a model removing the timeline (Control/Flu) and age interaction
# To assess if there are any age-immigration status combinations where the expected counts between the Control and Flu differ from other combinations
model3<-update(model2,~.-Control_Flu:Age)

# Analysis of deviance using a Chi squared test to assess if the interaction impacts the fit of the model
anova(model3,model2,test="Chi")  
# Significant p-value (>0.05) produced
# Indicating that at least one age-status combination differs from the rest

## Calculating the marginal means without the three-way interaction
emm<-emmeans(model, ~Control_Flu*Status*Age)

# Pairwise post-hoc analysis assessing the marginal means of each age-status combination
# Indicates which combinations differ between the Control and Flu
post_hoc<- contrast(emm, method="pairwise", by=c("Status", "Age"), adjust="bonferroni")
summary(post_hoc) 
# Indicated that all status groups at the age range of 20-39 were significantly different

## Second pairwise post-hoc analysis 
# Calculating the marginal means of the immigration statuses at the age of 20-39 to see if one immigration status at that age range had a more significant difference
emm_20_39 <- emmeans(model, ~ Control_Flu * Status | Age, at = list(Age = "20-39")) 

# Perform pairwise comparisons between Status groups (Domestic, Gen1, Gen2) for Control vs Flu within Age = 20-39
emm_status_comparison <- emmeans(model, ~ Control_Flu * Status | Age, at = list(Age = "20-39"))
# Perform contrast between Status groups (Domestic, Gen1, Gen2) for Control vs Flu
status_comparisonA <- contrast(emm_status_comparison, method = "pairwise", by = "Control_Flu", adjust = "bonferroni")
summary(status_comparisonA)
# Significant p-value (>0.05) found between Domestic-Status and First-Generation in the Flu timeline


###-----------------------------

### Repeating the above analysis (for questions 1 and 2) with only PI-related causes of death

###---------------

### Research Question 1
### Assessing if I specific immigration status experienced disproportional mortality 
### Only PI-related deaths

## Import new dataset
NewDataPI<-read.csv(file.choose(),header=TRUE)  # Import file named "PI_Data_Categorical_Counts_Thesis.csv"
attach(NewDataPI) # Attach the new file to ensure analysis uses the death counts 
names(NewDataPI)
newdataPI<-as.data.frame(NewDataPI) # Making new data read as a data frame

# Making a table of the number of PI deaths for each immigration status for the Control and Flu
tbPI<-table(DDDataPI$Status, DDDataPI$Control_Flu)
tbPI2<-as.data.frame(tbPI) # Making it read as a data frame

## Visualizing the data

# Ensure the factor levels for Var1 (immigration status) are in the desired order
tbPI2$Var1 <- factor(tbPI2$Var1, levels = c("Domestic", "1st_Gen", "2nd_Gen"))

# Figure 6

# Bar plot showing the number of PI deaths for each immigration status in the Control and Flu
F6A<- ggplot(tbPI2, aes(x = Var1, y = Freq, fill = factor(Var2))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste("n=", Freq)), 
            position = position_dodge(width = 0.9),  
            vjust = -0.5,  
            size = 5,      
            color = "black") +
  labs(x = "Status", y = "No. of Deaths", fill = "Control vs Flu") +
  scale_fill_manual(values = c("black", "grey"), labels = c("Control", "Flu")) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(tbPI2$Freq) + 10)) +
  theme(
    axis.text = element_text(size = 18, face = "bold", colour = "black"),        
    axis.title.x = element_text(size = 20, face = "bold", colour = "black"),     
    axis.title.y = element_text(size = 20, face = "bold", colour = "black"),     
    legend.title = element_text(size = 17, face = "bold", colour = "black"),     
    legend.text = element_text(size = 15, face = "bold", colour = "black"),    
    plot.title = element_text(size = 18, face = "bold", colour = "black")) + 
  theme(legend.position = "top")+
  labs(tag="A")+
  theme(
    plot.tag = element_text(size = 28, face = "bold"))

# Mosaic plot showing the proportion of PI deaths for each immigration status between the Control and Flu
F6B<- ggplot(tbPI2, aes(x = Var1, y = Freq, fill = factor(Var2))) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  scale_fill_manual(values = c("black", "grey"), labels = c("Control", "Flu")) +
  labs(x = "Status", y = "Proportion", fill = "Control vs Flu") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 18, face = "bold", colour = "black"),        
    axis.title.x = element_text(size = 20, face = "bold", colour = "black"),     
    axis.title.y = element_text(size = 20, face = "bold", colour = "black"),     
    legend.title = element_text(size = 17, face = "bold", colour = "black"),     
    legend.text = element_text(size = 15, face = "bold", colour = "black"),      
    plot.title = element_text(size = 18, face = "bold", colour = "black")) + 
  theme(legend.position = "top")+
  labs(tag="B")+
  theme(
    plot.tag = element_text(size = 28, face = "bold"))

Figure6<- (F6A + F6B)


## Analysis

## Creating a saturated generalized linear model of the variables, not including age
## for PI deaths 
modPI<-glm(Count~Control_Flu*Status, poisson, data = newdataPI)

# Creating a model, removing the interaction between timeline (Control/Flu) and immigration status to see if this interaction significantly impacts the model
modPI2<-update(modPI,~.-Control_Flu:Status)

# Analysis of deviance using a Chi squared test evaluating if the interaction significantly impacts the fit of the model
anova(modPI,modPI2, test="Chi") 
# No significant difference found between the two models
# Indicating that there is no significant difference in the PI death counts between the immigration statuses, between the Control and Flu



###------------------------------
### Research Question 2
### Assessing if I specific age and immigration status combination experienced disproportional mortality 

## Making subsets for each immigration status

# For Domestic-Status
ddataPIdom<-subset(DDDataPI, Status=="Domestic")
tagPIdom<-table(ddataPIdom$B.Age, ddataPIdom$Control_Flu)
tagePIdom<-as.data.frame(tagPIdom) # Making it read as a data frame

# For First-Generation
ddataPI1g<-subset(DDDataPI, Status=="1st_Gen")
tagPIg1<-table(ddataPI1g$B.Age, ddataPI1g$Control_Flu)
tagePIg1<-as.data.frame(tagPIg1) # Making it read as a data frame

# For Second-Generation
ddataPI2g<-subset(DDDataPI, Status=="2nd_Gen")
tagPIg2<-table(ddataPI2g$B.Age, ddataPI2g$Control_Flu)
tagePIg2<-as.data.frame(tagPIg2) # Making it read as a data frame


## Visualizing the data

##Figure 8

# Mosaic plots showing the proportion of PI deaths occurring for each age group between the Control and Flu

# For Domestic-Status
F8A<- ggplot(tagePIdom, aes(x = Var1, y = Freq, fill = factor(Var2))) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  scale_fill_manual(values = c("black", "grey"), labels = c("Control", "Flu")) +
  labs(x = "Age", y = "Proportion", fill = "Control vs Flu") +
  theme_minimal() +
  ggtitle("Domestic")+
  theme(
    axis.text = element_text(size = 19, face = "bold", colour = "black"),     
    axis.title.x = element_text(size = 22, face = "bold", colour = "black"),     
    axis.title.y = element_text(size = 22, face = "bold", colour = "black"),     
    legend.title = element_text(size = 17, face = "bold", colour = "black"),    
    legend.text = element_text(size = 15, face = "bold", colour = "black"),     
    plot.title = element_text(size = 18, face = "bold", colour = "black")) +
  theme(legend.position = "top")+
  labs(tag="A")+
  theme(
    plot.tag = element_text(size = 28, face = "bold"))

# For First-Generation
F8B<- ggplot(tagePIg1, aes(x = Var1, y = Freq, fill = factor(Var2))) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  scale_fill_manual(values = c("black", "grey"), labels = c("Control", "Flu")) +
  labs(x = "Age", y = "Proportion", fill = "Control vs Flu") +
  theme_minimal() +
  ggtitle("1st Generation")+
  theme(
    axis.text = element_text(size = 19, face = "bold", colour = "black"),       
    axis.title.x = element_text(size = 22, face = "bold", colour = "black"),     
    axis.title.y = element_text(size = 22, face = "bold", colour = "black"),     
    legend.title = element_text(size = 17, face = "bold", colour = "black"),     
    legend.text = element_text(size = 15, face = "bold", colour = "black"),     
    plot.title = element_text(size = 18, face = "bold", colour = "black")) + 
  theme(legend.position = "top")+
  labs(tag="B")+
  theme(
    plot.tag = element_text(size = 28, face = "bold"))

# For Second-Generation
F8C<- ggplot(tagePIg2, aes(x = Var1, y = Freq, fill = factor(Var2))) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  scale_fill_manual(values = c("black", "grey"), labels = c("Control", "Flu")) +
  labs(x = "Age", y = "Proportion", fill = "Control vs Flu") +
  theme_minimal() +
  ggtitle("2nd Generation")+
  theme(
    axis.text = element_text(size = 19, face = "bold", colour = "black"),        
    axis.title.x = element_text(size = 22, face = "bold", colour = "black"),     
    axis.title.y = element_text(size = 22, face = "bold", colour = "black"),     
    legend.title = element_text(size = 17, face = "bold", colour = "black"),     
    legend.text = element_text(size = 15, face = "bold", colour = "black"),      
    plot.title = element_text(size = 18, face = "bold", colour = "black")) + 
  theme(legend.position = "top")+
  labs(tag="C")+
  theme(
    plot.tag = element_text(size = 28, face = "bold"))

Figure8<- (F8A + F8B + F8C)



### Analysis

## Creating a saturated generalized linear model of the variables, including age
## for PI deaths
modelPI<-glm(Count~Control_Flu*Status*Age, poisson, data=newdataPI)

# Creating a model, removing the three-way interaction between timeline (Control/Flu), immigration status, and age to see if this interaction significantly impacts the model
modelPI2<-update(modelPI,~.-Control_Flu:Status:Age)

# Analysis of deviance using a Chi squared test evaluating if the interaction significantly impacts the fit of the model
anova(modelPI,modelPI2, test="Chi")  
# No significant difference found between the two models
# Indicating that the three-way interaction does not significantly impact the fit of the model
# Allowing to further investigate other interactions

# Creating a model removing the timeline (Control/Flu) and age interaction
# To assess if there are any age-immigration status combinations where the expected counts between the Control and Flu differ from other combinations
modelPI3<-update(modelPI2,~.-Control_Flu:Age)

# Analysis of deviance using a Chi squared test to assess if the interaction impacts the fit of the model
anova(modelPI3,modelPI2,test="Chi")  #removing the interaction between ages between control/flu
# Significant p-value (>0.05) produced
# Indicating that at least one age-status combination differs from the rest

## Calculating the marginal means without the three-way interaction
emmPI<-emmeans(modelPI, ~Control_Flu*Status*Age)

# Pairwise post-hoc analysis assessing the marginal means of each age-status combination
# Indicates which combinations differ between the Control and Flu
post_hocPI<- contrast(emmPI, method="pairwise", 
                      by=c("Status", "Age"), 
                      adjust="bonferroni")
summary(post_hocPI) 
# Indicated that all status groups at the age range of 20-39 were significantly different

## Second pairwise post-hoc analysis 
# Calculating the marginal means of the immigration statuses at the age of 20-39 to see if one immigration status at that age range had a more significant difference
emmPI_status_comparison <- emmeans(modelPI, ~ Control_Flu * Status | Age, at = list(Age = "20-39"))

# Perform pairwise comparisons between Status groups (Domestic, Gen1, Gen2) for Control vs Flu within Age = 20-39
status_comparison <- contrast(emmPI_status_comparison, method = "pairwise", by = "Control_Flu", adjust = "bonferroni")
summary(status_comparison)
# Significant p-value (>0.05) found between Domestic-Status and First-Generation in the Flu timeline

