
dta <- read.csv("/Users/sarwaridas/team-project-2-streetrx-and-voting-in-nc-silver7/Data/cleaned_data.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE)
dta <- subset( dta, select = -X ) #removing index column
head(dta)
colnames(dta)
f_cols= c('USA_region','source','form_temp','bulk_purchase') #changing to factor dtype
dta[f_cols] = lapply(dta[f_cols], as.factor) #changing to factor dtype
str(dta)
levels(dta$bulk_purchase)= c("Not Bulk Purchase","Bulk Purchase")

nulls = sapply(dta, function(col) sum(length(which(is.na(col))))); nulls #checking for nulls
#Null data for price, we can't use this
#Hoping regression ignores the 7 nulls
dta$price= dta$ppm * dta$mgstr
table(dta$USA_region) #Have enough data across regions
summary(dta$price) #Price has outliers - not removing them though
dta$logprice=log(dta$price)

ggplot(dta,aes(logprice)) +
  geom_histogram(aes(y=..density..),color="black",linetype="dashed",
                 fill=rainbow(15),bins=15) + theme(legend.position="none") +
  geom_density(alpha=.25, fill="lightblue") + scale_fill_brewer(palette="Blues") +
  labs(title="Distribution of Price",y="Drugs") + theme_classic() #Taking transformation to fit normal assumption model better

ggplot(dta,aes(x=USA_region, y=logprice, fill=USA_region)) +
  geom_boxplot() +
  labs(title="Log price levels by region",
       x="Region",y="Log Price") + theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90)) #Difference in medians is seen (slightly)

#These are our indicidual level variables: source, form_temp, bulk_purchase

table(dta$bulk_purchase) 

ggplot(dta,aes(x=bulk_purchase, y=logprice, fill=bulk_purchase)) +
  geom_boxplot() + scale_fill_brewer(palette="Greens") +
  labs(title="Log price vs bulk purchase", x="Bulk Purchase",y="Log Price") +
  theme_classic() + theme(legend.position="none") #No difference seen

table(dta$form_temp) #NOT ENOUGH DATA!

ggplot(dta,aes(x=form_temp, y=logprice, fill=form_temp)) + #NOT ENOUGH DATA!
  geom_boxplot() + scale_fill_brewer(palette="Greens") +
  labs(title="Log price vs form", x="form_temp",y="Log Price") +
  theme_classic() + theme(legend.position="none")

library(dplyr)
library(forcats)
table(dta$source)
dta$source_new<- dta$source
dta$source_new <- fct_collapse(dta$source_new, online = c("Tramadolprices,co", "Streetrx.com","https://addictionresource.com/drugs/tramadol/tramadol-street-prices/","http://silkroadvb5piz3r.onion/silkroad/category/51/225",
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
table(dta$source_new)
levels(dta$source_new)<- c("Others" ,"online" , "Heard it","Internet", "Internet Pharmacy", "Personal")

ggplot(dta,aes(x=source_new, y=logprice, fill=source_new)) + #NOT ENOUGH DATA!
  geom_boxplot() + scale_fill_brewer(palette="Greens") +
  labs(title="Log price vs form", x="form_temp",y="Log Price") +
  theme_classic() + theme(legend.position="none")


Model1 <- lmer(logprice ~ bulk_purchase + source_new+ (bulk_purchase | USA_region )+(source_new | USA_region ), data = dta)
summary(Model1)




