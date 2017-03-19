library(ggplot2)
library(plyr)
getwd()
#setwd("/home/yanyan/job/tamr/datafeeds")
setwd("/home/yanyan/job/tamr/datafeeds2")
list.files()
df = read.csv("segmentaa.csv")
dim(df)
df$vendorname.c = as.character(df$vendorname)  
df$vendordoingasbusinessname.c = as.character(df$vendordoingasbusinessname)  
df$vendorlegalorganizationname.c = as.character(df$vendorlegalorganizationname)
df$vendoralternatename.c = as.character(df$vendoralternatename ) 

sa_match = subset(df, vendorname.c == vendordoingasbusinessname.c | 
                    vendordoingasbusinessname.c ==vendorlegalorganizationname.c   | 
                    vendorlegalorganizationname.c ==vendoralternatename.c |
                    vendorname.c ==vendorlegalorganizationname.c |
                    vendorname.c ==vendoralternatename.c|
                    vendordoingasbusinessname.c == vendoralternatename.c) 

dim(sa_match)

#sa = subset(df, vendorname!="" | vendordoingasbusinessname !="" | vendorlegalorganizationname !="" | vendoralternatename !="") 
flag_col= which(grepl("flag",colnames(sa_match)))
names(sa_match)

flag_col
sa=sa_match[, c(1,3,11,13,44,45,46,47,56,57,58,59,60,65,66,flag_col)]

names(sa)
dim(sa)
write.csv(sa, "sa.csv")

sa=subset(sa, dollarsobligated!=0.00)
dim(sa)

sa_obiligated = ddply(sa, 
                       "dunsnumber", # split the data into pieces by dunsnumber
                       summarize, # the apply function is summarize
                       sum.money = sum(dollarsobligated),
                       number_of_trans = length(dunsnumber),
                       mean.money= mean(dollarsobligated),
                       median.money = median(dollarsobligated),
                       min.money = min(dollarsobligated),
                       max.money =max(dollarsobligated),
                       q25.money = quantile(dollarsobligated,0.25),
                    q75.money = quantile(dollarsobligated,0.75)) # apply function to each pieces

sa$dunsnumber_f=as.factor(sa$dunsnumber)
sa_name = sa[,c("vendorname", "dunsnumber_f")]
unique(sa_name$vendorname)
sa$vendorname[which(unique(sa_name$dunsnumber_f))]

sa_name2= subset(sa_name, !duplicated(dunsnumber_f)) 
dim(sa_name2)
sa_name2$dunsnumber = as.character(sa_name2$dunsnumber_f)


sa_obligated_with_name =merge(sa_name2, sa_obiligated, by = "dunsnumber")

unique(sa_obiligated$dunsnumber)

dim(sa_obiligated)
dim(sa_obligated_with_name)
head(sa_obiligated)
summary(sa_obiligated$sum.money)

sa$dunsnumber_f= as.factor(sa$dunsnumber)
levels(sa$dunsnumber_f)
sa$dunsnumber[1]
summary(sa$dunsnumber)


hist(log10(sa$dollarsobligated))


get_first_letter = function(sentence){
  if (sentence!="" & is.character(sentence)){
  gsub(",","",sentence) 
  v = as.vector(strsplit(sentence, " "))
  word = ""
  for (a in v ){
  word=paste(word,substr(a,1,1),sep="",collapse ="") }
  return (word)}
}
  
get_first_letter("Give Me That")
get_first_letter("GREAT LAKES MUSEUM OF SCIENCE, ENVIRONMENT, AND TECHNOLOGY")

sa1$vendorlegalorganizationname

summary(sa1$vendorlegalorganizationname)


sa$vendorSimplified = sapply(as.character(sa$vendorlegalorganizationname), get_first_letter, simplify = TRUE, USE.NAMES = TRUE)

sa$vendorlegalorganizationname[1:100]
sa$vendorSimplified[1:1000]

vendor_1fun=function(a, b) {
   ifelse(grepl(a, b),1,0)
}


sa12 = subset(sa, vendorSimplified ==vendoralternatename  )
dim(sa12)

names(sa12)

hist(sa12$dollarsobligated)

ggplot(aes(dollarsobligated),data=sa12)+
  geom_histogram()

sa12$dollarsobligated
sa12_subset= subset(sa12, dollarsobligated!=0 )

dim(sa12_subset)

ggplot(sa12_subset, aes(x=fundedbyforeignentity, y=dollarsobligated) )+ 
  geom_boxplot(aes(colour = fundedbyforeignentity),show.legend = FALSE)+
  coord_flip()

dim(sa)
sa_dollars_subset = subset(sa,dollarsobligated!=0.00 )
dim(sa_dollars_subset)
summary(sa$dollarsobligated)

ggplot(sa_dollars_subset , aes(x="", y=dollarsobligated) )+ 
  geom_boxplot()+
  coord_flip()


ggplot(sa, aes(x=fundedbyforeignentity, y=dollarsobligated) )+ 
  geom_boxplot(aes(colour = fundedbyforeignentity),show.legend = FALSE)+
  coord_flip()


sa_dollars_subset1 = subset(sa_dollars_subset,dollarsobligated >0.00)
dim(sa_dollars_subset1)
ggplot(sa_dollars_subset1, aes(x=fundedbyforeignentity, y=dollarsobligated) )+ 
  geom_boxplot(aes(colour = fundedbyforeignentity),show.legend = FALSE)+
  coord_flip()+
  ylim(1, quantile(sa_dollars_subset1$dollarsobligated,0.80))
sa_dollars_subset1$dollarsobligated
summary(sa_dollars_subset1$dollarsobligated)
min(sa_dollars_subset1$dollarsobligated)
sa_dollars_subset1= subset(sa_dollars_subset1, dollarsobligated==0.00)
dim(sa_dollars_subset1)


ggplot(sa_dollars_subset1 , aes(x="", y=dollarsobligated) )+ 
  geom_boxplot()+
  coord_flip()
summary(sa_dollars_subset1$dollarsobligated)

dim(sa_dollars_subset)
sa_dollars_subset2 = subset(sa_dollars_subset,dollarsobligated <0)
dim(sa_dollars_subset2)
ggplot(sa_dollars_subset2, aes(x=fundedbyforeignentity, y=dollarsobligated) )+ 
  geom_boxplot(aes(colour = fundedbyforeignentity),show.legend = FALSE)+
  coord_flip()

hist(0-sa_dollars_subset2$dollarsobligated)
hist(log10(0-sa_dollars_subset2$dollarsobligated))

#funded by foreign entity
funding_foreign = sa[,c("fundedbyforeignentity")]

levels(funding_foreign)

summary(funding_foreign)

about_dunsnumber= sa[,c("dunsnumber")]
about_dunsnumber

sa$educationalinstitutionflag
ed_spending = subset(sa, educationalinstitutionflag="Y")
hist(log10(ed_spending$dollarsobligated))
summary(ed_spending$dollarsobligated) 

sa$effectivedate
  
names(sa)
datesa=sa[,c(15,16)]
datesa

tail(datesa,100)
