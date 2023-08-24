

library(dplyr)
library(forcats)
library(ggplot2)

dta <- read.csv("Data/part1/cleaned_data.csv", header = TRUE,sep = ",",stringsAsFactors = FALSE)
dta <- subset(dta, select = -c(X,form_temp)) #removing index and form column
str(dta)
dta=subset(dta, mgstr>1)

f_cols= c('USA_region','source','bulk_purchase','state') #changing to factor dtype
dta[f_cols] = lapply(dta[f_cols], as.factor) #changing to factor dtype
levels(dta$bulk_purchase)= c("Not Bulk Purchase","Bulk Purchase")

#moved all websites to internet
dta$source_new<- dta$source
dta$source_new <- fct_collapse(dta$source_new, Online = c("Tramadolprices,co", "Streetrx.com","https://addictionresource.com/drugs/tramadol/tramadol-street-prices/","http://silkroadvb5piz3r.onion/silkroad/category/51/225",
                                                          "http://silkroadvb5piz3r.onion/index.php/silkroad/item/36f1908da6",
                                                          "http://silkroadvb5piz3r.onion/index.php/silkroad/item/6e3cd15aa1" ,
                                                          "http://silkroadvb5piz3r.onion/index.php/silkroad/item/740d0d5672",
                                                          "http://silkroadvb5piz3r.onion/index.php/silkroad/item/8bdcb5d15a" ,
                                                          "http://silkroadvb5piz3r.onion/index.php/silkroad/item/ac63e18eb1",
                                                          "http://silkroadvb5piz3r.onion/index.php/silkroad/item/aeddba3b9b", 
                                                          "http://silkroadvb5piz3r.onion/index.php/silkroad/item/b2eb490fb4", 
                                                          "http://silkroadvb5piz3r.onion/index.php/silkroad/item/cc7cf796da" ,
                                                          "http://silkroadvb5piz3r.onion/silkroad/category/240" ,
                                                          "http://silkroadvb5piz3r.onion/silkroad/category/240/100",
                                                          "http://silkroadvb5piz3r.onion/silkroad/category/240/125" ,
                                                          "http://silkroadvb5piz3r.onion/silkroad/category/240/150",  
                                                          "http://silkroadvb5piz3r.onion/silkroad/category/240/50" ,
                                                          "http://silkroadvb5piz3r.onion/silkroad/category/240/75",
                                                          "http://silkroadvb5piz3r.onion/silkroad/category/51/125" ,
                                                          "http://silkroadvb5piz3r.onion/silkroad/category/51/150" ,
                                                          "http://silkroadvb5piz3r.onion/silkroad/category/51/200",
                                                          "Drugs.com","streetrx.com","Blue Gold : World Water Wars (Official Full Lengt???: http://youtu.be/B1a3tjqQiBI","Google"))

levels(dta$source_new)<- c("Others" ,"Internet" , "Heard it","Internet", "Internet Pharmacy", "Personal")
table(dta$source_new)
str(dta)

nulls = sapply(dta, function(col) sum(length(which(is.na(col))))); nulls #checking for nulls
#Null data for price (7 points), we can't use this
#Hoping regression ignores the 7 nulls
table(dta$state) #Have enough data across regions -- the state of USA is being treated as "other"
summary(dta$ppm)#Price has outliers - not removing them though

#########CONSIDER REMOVING OUTLIERS!


##INVESTIGATING OUTCOME OF INTEREST
ggplot(dta,aes(ppm)) +
  geom_histogram(aes(y=..density..),color="black",linetype="dashed",
                 fill=rainbow(15),bins=15) + theme(legend.position="none") +
  geom_density(alpha=.25, fill="lightblue") + scale_fill_brewer(palette="Blues") +
  labs(title="Distribution of Price",y="Drugs") + theme_classic() #Taking transformation to fit normal assumption model better

dta$logppm=log(dta$ppm)

ggplot(dta,aes(logppm)) +
  geom_histogram(aes(y=..density..),color="black",linetype="dashed",
                 fill=rainbow(15),bins=15) + theme(legend.position="none") +
  geom_density(alpha=.25, fill="lightblue") + scale_fill_brewer(palette="Blues") +
  labs(title="Distribution of Price",y="Drugs") + theme_classic() 


#Taking transformation to fit normal assumption model better
#Are there any variations in ppm by state?

#Let's see for a random sample
set.seed(1000)
sample_state <- sample(unique(dta$state),10,replace=F)
ggplot(dta[is.element(dta$state,sample_state),],
       aes(x=state, y=logppm, fill=state)) +geom_boxplot() +
  labs(title="Log price levels by state",
       x="State",y="Log price") + theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90)) #Differences are seen across states

#Are there any variations in ppm by region?

#Let's see for a random sample
set.seed(1000)
#sample_region <- sample(unique(dta$USA_region),10,replace=F)
ggplot(dta,
       aes(x=USA_region, y=logppm, fill=USA_region)) +
  geom_boxplot() +
  labs(title="Log price levels by region",
       x="Region",y="Log price") + theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90)) #Differences are seen across regions


######################### WE'll BE DOING 2 LEVELS - BY STATE & BY REGION #################################

#EDA - excluding state & region to figure out relationships between other variables
colnames(dta) #variables to explore: logppm, source_new, mgstr, bulk_purchase

ggplot(dta) +
  aes(x = "", y = logppm) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal() + coord_flip() 


#numeric:mgstr
ggplot(dta,aes(x=logppm, y=mgstr)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_minimal() +
  labs(title="ppm vs mgstr") #linear, can be used

#factor: source_new
ggplot(dta,aes(x=source_new, y=logppm, fill=source_new)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  # scale_y_continuous(limits = c(0, 0.25))+
  labs(title="logppm vs source",x="source",y=" ppm") + 
  theme_classic() + theme(legend.position="none")+
  coord_flip() #some difference in medians, can be used

#factor: bulk_purchase
ggplot(dta,aes(x=bulk_purchase, y=logppm, fill=bulk_purchase)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  #scale_y_continuous(limits = c(0, 0.25))+
  labs(title="ppm vs bulk_purchase",x="bulk_purchase",y=" ppm") + 
  theme_classic() + theme(legend.position="none")+
  coord_flip() #some difference in medians, can be used

#does anova show differences for source_new & bulk_purchase?
one.way.race <- aov(logppm ~ source_new, data = dta)
summary(one.way.race) #sigf at 10%

one.way.race <- aov(logppm ~ bulk_purchase, data = dta)
summary(one.way.race) #not sigf

#Exploring interactions

#source and bulk_purchase
ggplot(dta,aes(x=bulk_purchase, y=logppm, fill=source_new)) +
  geom_boxplot(outlier.shape=NA) +
  scale_fill_brewer(palette="Blues") + coord_flip()
  #scale_y_continuous(limits = c(0, 0.25))+
  labs(title="Interaction: Bulk Purchase and Source",x="",y="") + 
  theme_classic() + theme(legend.position="left") #see difference in medians as well as change in trend

#source and mgstr
ggplot(dta,aes(x=logppm, y=mgstr)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="ppm vs mgstr") +
  #scale_x_discrete(labels=c("0" = "No","1" = "Yes")) +
  facet_wrap( ~ source_new) #looks similar, maybe some change in slope

#bulk_purchase and mgstr
ggplot(dta,aes(x=logppm, y=mgstr)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="ppm vs mgstr") +
  #scale_x_discrete(labels=c("0" = "No","1" = "Yes")) +
  facet_wrap( ~ bulk_purchase) #looks similar


#BASELINE LINEAR MODEL, TO FIGURE OUT INTERACTIONS

Model1<- lm(logppm~source_new + mgstr + bulk_purchase,  data=dta)
Model1_inter <- lm(logppm~source_new*mgstr + source_new*bulk_purchase+mgstr*bulk_purchase, data=dta)
summary(Model1)
summary(Model1_inter)
Model_stepwise <- step(Model1, scope = formula(Model1_inter),direction="both",trace=0)
Model_stepwise$call
summary(Model_stepwise)
anova(Model1,Model_stepwise) #keep interaction

#MODEL ASSUMPTIONS WILL NOT HOLD. WE ONLY NEED TO CHECK FOR LINEARITY, MULTICOLL & OUTLIERS
plot(Model_stepwise) #aren't supposed to look good so whatevss
vif(Model_stepwise) #is ok

# dta$mgstrcent <- dta$mgstr - mean(dta$mgstr,na.rm=TRUE) 

#this makes no sense - mgstr should not be zero

# dta$mgstrcent2 <- dta$mgstrcent^2
# Model1_trans<- lm(logppm ~ source_new + mgstrcent2 + bulk_purchase + source_new:bulk_purchase, 
#                   data = dta)
# plot(Model1_trans)
# 
# ggplot(dta,aes(x=mgstr, y=Model_stepwise$residual)) + 
#   geom_point(alpha = .7) +  geom_hline(yintercept=0,col="red3") + theme_classic() +
#   labs(title="Residuals vs mgstr",x="mgstr",y="Residuals")

ggplot(dta[complete.cases(dta),],aes(x=mgstr, y=Model_stepwise$residual)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_minimal() +
  labs(title="ppm vs mgstr") #no trend. is ok

#FINAL LINEAR MODEL: Model_stepwise

#On to hierarchical!

#Level 1

#varying intercept linear model.

Model2 <- lmer(logppm ~ mgstr + bulk_purchase*source_new + (1 | state), data = dta)
summary(Model2)
plot(residuals(Model2))
qqnorm(residuals(Model2))

#do we need varying slopes? - bulk purchase

set.seed(1000)
sample_state <- sample(unique(dta$state),8,replace=F)
ggplot(dta[is.element(dta$state,sample_state),],
       aes(x=bulk_purchase, y=logppm, fill=bulk_purchase)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Greens") +
  labs(title="Log price vs bulk purchase by state",
       x="Bulk purchase",y="Log Price") +
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ state,ncol=4)

#can see change in medians

#do we need varying slopes? - source

set.seed(1000)
sample_state <- sample(unique(dta$state),8,replace=F)
ggplot(dta[is.element(dta$state,sample_state),],
       aes(x=source_new, y=logppm, fill=source_new)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Greens") +
  labs(title="Log price vs source by state",
       x="source",y="Log Price") +
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ state,ncol=4)

#can see some change in medians

#varying intercept + varyling slopes linear model.

Model3<- lmer(logppm ~ mgstr + bulk_purchase*source_new + (bulk_purchase | state), data = dta)
Model4<- lmer(logppm ~ mgstr + bulk_purchase*source_new + (source_new | state), data = dta)
Model5<- lmer(logppm ~ mgstr + bulk_purchase*source_new + (bulk_purchase | state) + (source_new | state), data = dta)
summary(Model3)
summary(Model4)
AIC(Model4)
AIC(Model5)
anova(Model3,Model5) #dont need varying slopes for source? maybe
anova(Model4,Model5) #dont need varying slopes for bulk purchase? maybe
anova(Model2,Model3) #confirmed: dont need varying slopes for bulk purchase
anova(Model2,Model4) #confirmed: dont need varying slopes for source
anova(Model2,Model5) #confirmed: dont need varying slopes for both

AIC(Model5)
#SO FAR, VARYING INTERCEPTS MODEL (Model2) IS GOOD.

plot(residuals(Model2)) #not supposed to be good
qqnorm(residuals(Model2)) #not supposed to be good

#DO WE NEED ANOTHER LEVEL - REGION?

ggplot(dta,aes(x=USA_region, y=logppm, fill=USA_region)) +
  geom_boxplot() +
  labs(title="Log price levels by state",
       x="Region",y="Log Price") + theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90)) #Def some change in median


Model6<- lmer(logppm ~ mgstr + bulk_purchase*source_new + (1 | state)+ (1 | USA_region), data = dta)
AIC(Model2)
AIC(Model6)
anova(Model6,Model7)
summary(Model2)
summary(Model6)


anova(Model2,Model6) #No sigf diff. we'll stick with state then
#Model Assessment and Validation

summary(Model2)
dotplot(ranef(Model2,condVar=TRUE))$USA_region
plot(residuals(Model6)) 
qqnorm(residuals(Model2)) 
ggplot(dta[complete.cases(dta),],aes(x=mgstr, y=residuals(Model6))) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_minimal() +
  labs(title="ppm vs mgstr") #no trend as such
dotplot(ranef(Model7,condVar=TRUE))$state

dta$mgstr_f= factor(dta$mgstr)

Model7<- lmer(logppm ~ mgstr_f + bulk_purchase*source_new + (1 | state)+ (1 | USA_region), data = dta)
AIC(Model7)
dotplot(ranef(Model7,condVar=TRUE))$USA_region
plot(residuals(Model7)) 
qqnorm(residuals(Model7)) 
#turn to factor
summary(Model7)
confint(Model7)


exp(0.29)






