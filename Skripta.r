#install.packages("colorspace") 
#install.packages("ggpubr")
#install.packages("gridExtra")
#install.packages("waffle")
#install.packages("ggplot2")
#install.packages("igraph")
#install.packages("dplyr")
#install.packages("magrittr")
#install.packages("tidyr")
#install.packages("car")
#install.packages("plyr")
#install.packages("plotly")

library(ggplot2)
library(waffle)
library(gridExtra)
library(tidyr)
library(igraph)
#Ovaj paket omogucava da se napravi pajp line
library(magrittr)
library("plyr")
library(dplyr)
library(car)
library(ggpubr)
library(plotly)


getwd()

sm<-read.csv("C:/Users/Elvira/Desktop/AIV1/student-mat.csv",
             header = TRUE,
             sep = ',',
             stringsAsFactors = TRUE)
is.data.frame(sm)
head(sm)
tail(sm)

apply(sm, 2, unique)

dim(sm)

sum(apply(sm, 2, is.na))

str(sm)

table(sm$sex)

colors ＜- c("orange","blue")
colors

df <- data.frame(gender_group = c("Male", "Female"),
                        count =c(187,208),
             percentage_value = c(47.34, 52.65))
df


sss<-ggplot(sm, aes(x=sex)) + 
  geom_bar(stat = "count",
                 aes(fill = sex))+
  scale_y_continuous(breaks = seq(0,210,10))+
  ggtitle("Bar chart representing Gender of students(math)")

sss

table(sm$higher)
higbar<-ggplot(sm, aes(x=higher)) + 
  geom_bar(stat = "count",
                 aes(fill = higher))+
  scale_y_continuous(breaks = seq(0,380,20))+
  ggtitle("Bar chart representing desire for higher education of students(mat)")

table(sm$activities)
act<-ggplot(sm, aes(x=activities)) + 
  geom_bar(stat = "count",
                 aes(fill = activities))+
  scale_y_continuous(breaks = seq(0,210,20))+
  ggtitle("Bar chart for extra-educational activities of students(math)")
grid.arrange(higbar,act,nrow=2)

table(sm$address)
adres<-ggplot(sm, aes(x=address)) + 
  geom_bar(stat = "count",
                 aes(fill = address))+
  scale_y_continuous(breaks = seq(0,310,20))+
  ggtitle("Bar chart for Adress of students(math)\
                    Urban/Rural")


table(sm$romantic)
r<-ggplot(sm, aes(x=romantic)) + 
  geom_bar(stat = "count",
                 aes(fill = romantic))+
  scale_y_continuous(breaks = seq(0,265,20))+
  ggtitle("Bar chart for Romantic relationship status of students(math)\
                Yes/No")
grid.arrange(adres,r,nrow=2)


a<-ggplot(sm, aes(x=age)) + 
  geom_histogram(binwidth = 0.5, aes(fill = ..count..))+
  scale_y_continuous(breaks = seq(0,105,5))+
  scale_x_continuous(breaks = seq(0,22,1))+
  ggtitle("Histogram for Age of students(mat)")
a




table(sm$paid)
pcp<-ggplot(sm, aes(x=paid)) + 
  geom_bar(stat = "count",
                 aes(fill = paid))+
  scale_y_continuous(breaks = seq(0,380,10))+
  ggtitle("Bar chart representing Paid clases of math for students(mat)")

table(sm$school)
schp<-ggplot(sm, aes(school)) + 
  geom_bar(stat = "count",
                 aes(fill = school))+
  scale_y_continuous(breaks = seq(0,350,20))+
  ggtitle("Bar chart for School of students(mat)")
grid.arrange(pcp,schp, nrow=2)

table(sm$famsup)
famsg<-ggplot(sm, aes(x=famsup)) + 
  geom_bar(stat = "count",
                 aes(fill = famsup))+
  scale_y_continuous(breaks = seq(0,380,10))+
  ggtitle("Bar chart representing Family support for students(mat)")
famsg

table(sm$schoolsup)
schsug<-ggplot(sm, aes(x=schoolsup)) + 
  geom_bar(stat = "count",
                 aes(fill = schoolsup))+
  scale_y_continuous(breaks = seq(0,380,10))+
  ggtitle("Bar chart representing school support for students(mat)")
schsug

table(sm$famsize)
famsg<-ggplot(sm, aes(x=famsize)) + 
  geom_bar(stat = "count",
                 aes(fill = famsize))+
  scale_y_continuous(breaks = seq(0,380,10))+
  ggtitle("Bar chart representing famsize for students(mat)")
famsg

table(sm$nursery)
nursg<-ggplot(sm, aes(x=nursery)) + 
  geom_bar(stat = "count",
                 aes(fill = nursery))+
  scale_y_continuous(breaks = seq(0,380,10))+
  ggtitle("Bar chart nursery for students(mat)")
nursg

alcohol.d <- as.data.frame(table(sm$Dalc))
alcohol.d
alcohol.d.1 <- as.numeric(alcohol.d$Freq)
alcohol.d.1
names(alcohol.d.1) <- alcohol.d$Var1
alcohol.d.1<- round(alcohol.d.1/10)
alcohol.d.1

table(sm$Walc)
alcohol.w <- as.data.frame(table(sm$Walc))
alcohol.w
alcohol.w.1 <- as.numeric(alcohol.w$Freq)
alcohol.w.1
names(alcohol.w.1) <- alcohol.w$Var1
alcohol.w.1 <- round(alcohol.w.1/10)
alcohol.w.1


waffle.col <- c("#00d27f","#adff00","#f9d62e","#fc913a","#ff4e50")

alc.d <- waffle(alcohol.d.1,
               rows=5,  
               size=2, 
               title = "Workday alcohol consumption among students",
               glyph_size=8,
               xlab="1 square == 10 students",
               colors=waffle.col,
               legend_pos= "top")


alc.w <- waffle(alcohol.w.1, 
                rows=5, 
                size=2, 
                title = "Weekend alcohol consumption among students",
                glyph_size=8,
                xlab="1 square == 10 students",
                colors=waffle.col,
                legend_pos= "top")




grid.arrange(alc.d,alc.w, nrow=2)


sp<-read.csv("C:/Users/Elvira/Desktop/AIV1/student-por.csv",
             header =TRUE,
             sep = ',',
             stringsAsFactors = TRUE)
is.data.frame(sp)
str(sp)
dim(sp)

apply(sp, 2, unique)


sum(apply(sp, 2, is.na))


df1 <- data.frame(gender_group = c("Male", "Female"),
count =c(266,383))
df1

table(sp$sex)

#Primer kako se može korisitit i druga funkcija koja daje pie chart ali pie chart neće biti 
#prikazan jer funkcija plot_ly ne daje tu mogućnost u pdf formatu

hjk＜- plot_ly(data = df1,
              labels = ~df$gender_group,
              values = ~count,
              type = 'pie',
              sort= FALSE,
              marker= list(colors=colors, line = list(color="black", width=1))) %＞%
     layout(title="Pie chart representing Gender of students(Port. class)\
                         (with Plotly)")
hjk
sas<-ggplot(sp, aes(x=sex)) + 
  geom_bar(stat = "count",
                 aes(fill = sex))+
  scale_y_continuous(breaks = seq(0,390,10))+
  ggtitle("Bar chart representing Gender of students(Port. class)")

sas

#?density
density(sm$G3)
#?ggdensity
deg3<-ggdensity(sm$G3, 
          main="Density plot of G3(final grade)",
          xlab="G3",
          ylab="Density",
          col = "red",
          add = c("median"))   


hisg3<-ggplot(sm, aes(x=G3)) + 
  geom_histogram(binwidth = 0.5, aes(fill = ..count..))+
  scale_x_continuous(breaks = seq(1,20,1))+
  scale_y_continuous(breaks = seq(0,80,4))+
  ggtitle("Histogram of the final grades in math")

grid.arrange(deg3,hisg3,nrow=2)


#?density
density(sp$G3)
#?ggdensity
deg31<-ggdensity(sp$G3, 
          main="Density plot of G3(final grade)",
          xlab="G3",
          ylab="Density",
          col = "red",
          add = c("median"))   


hisg31<-ggplot(sp, aes(x=G3)) + 
  geom_histogram(binwidth = 0.5, aes(fill = ..count..))+
  scale_x_continuous(breaks = seq(1,20,1))+
  scale_y_continuous(breaks = seq(0,120,5))+
  ggtitle("Histogram of the final grades in port.")

grid.arrange(deg31,hisg31,nrow=2)


#?geom_bar
fin<-ggplot(data=sm, aes(x=G3, fill=sex))+
  geom_bar(stat="count",
           position="dodge",
           color="NA")+
  scale_x_continuous(breaks = seq(0,20,1))+
  scale_y_continuous(breaks = seq(0,30,5))+
  ggtitle("Bar chart of diferences between final grade (Math)\
          of the boys and the girls ")
#?geom_boxplot
avsf<-ggplot(sm, aes(x = sex, y = G3, fill=sex)) +
  geom_boxplot(col = "black") +
  ggtitle("Final scores averages (Math) plotted by sex")

grid.arrange(avsf,fin,nrow=2)

#?geom_bar
fin1<-ggplot(data=sp, aes(x=G3, fill=sex))+
  geom_bar(stat="count",
           position="dodge",
           color="NA")+
  scale_x_continuous(breaks = seq(0,20,1))+
  scale_y_continuous(breaks = seq(0,60,10))+
  ggtitle("Bar chart of diferences between final grade (Portuguese)\
          of the boys and the girls ")
#?geom_boxplot
avsf1<-ggplot(sp, aes(x = sex, y = G3, fill=sex)) +
  geom_boxplot(col = "black") +
  scale_y_continuous(breaks = seq(0,20,2))+
  ggtitle("Final scores averages (Portuguese) plotted by sex")

grid.arrange(avsf1,fin1,nrow=2)


mean(sm$G3[sm$sex=="F"])
#9.966346
mean(sm$G3[sm$sex=="M"])
#10.91444

9.966346-10.91444
#-0.94

#Diferent method
aggregate(sm$G3,
          by=list(sm$sex),
          FUN="mean",
          na.rm=TRUE)



mean(sp$G3[sp$sex=="F"])
#12.25326
mean(sp$G3[sp$sex=="M"])
#11.406015

12.25326-11.406015
#0.847244

#Diferent method
aggregate(sp$G3,
          by=list(sp$sex),
          FUN="mean",
          na.rm=TRUE)


mean(sp$G3[sp$sex=="F" & sp$G3>0])
#12.48138
mean(sp$G3[sp$sex=="M" & sp$G3>0])
#11.75969

12.48138-11.75969
#0.721689

aggregate(sp$G3[sp$G3>0],
          by=list(sp$sex[sp$G3>0]),
          FUN="mean",
          na.rm=TRUE)
aggregate(sp$G3[sp$G3>0],
          by=list(sp$sex[sp$G3>0]),
          FUN="median",
          na.rm=TRUE)


mean(sm$G3[sm$sex=="F" & sm$G3>0])
#11.20541
mean(sm$G3[sm$sex=="M" & sm$G3>0])
#11.86628

11.20541-11.86628
#-0.66

aggregate(sm$G3[sm$G3>0],
          by=list(sm$sex[sm$G3>0]),
          FUN="mean",
          na.rm=TRUE)
aggregate(sm$G3[sm$G3>0],
          by=list(sm$sex[sm$G3>0]),
          FUN="median",
          na.rm=TRUE)



shapiro.test(sm$G3[sm$G3>0])
shapiro.test(sm$G3[sm$sex=="F" & sm$G3>0])
shapiro.test(sm$G3[sm$sex=="M" & sm$G3>0])



shapiro.test(sp$G3[sp$G3>0])
shapiro.test(sp$G3[sp$sex=="F" & sp$G3>0])
shapiro.test(sp$G3[sp$sex=="M" & sp$G3>0])



#bartlett.test
bartlett.test(sm$G3[sm$G3 > 0] ~ sm$sex[sm$G3 > 0])
#leven
leveneTest(y = sm$G3, group = sm$sex)

#bartlett.test
bartlett.test(sp$G3[sp$G3 > 0] ~ sp$sex[sp$G3 > 0])
#leven
leveneTest(y = sp$G3, group = sp$sex)

wilcox.test(sm$G3[sm$G3 > 0] ~ sm$sex[sm$G3 > 0])

#kruskal.test (sm$G3[sm$G3 > 0] ~ sm$sex[sm$G3 > 0],
            #  data=sm)


wilcox.test(sp$G3[sp$G3 > 0] ~ sp$sex[sp$G3 > 0])

str(sp$Medu)

ch ＜- plot_ly(data = sp, labels = ~sp$Fedu, values = ~sum, 
             type = 'pie', sort= FALSE,
            marker= list(colors=colors, line = list(color="black", width=1))) %＞%
            layout(title="Pie chart for Father education level\
                                    (with Plotly)")
ch

ggplot(sp, aes(x=Fedu)) + 
  geom_bar(stat = "count",
                 aes(fill =..count..))+
  scale_y_continuous(breaks = seq(0,210,10))+
  ggtitle("Bar chart representing Father education level")

gh ＜- plot_ly(data = sp, labels = ~sp$Medu, values = ~sum, 
             type = 'pie', sort= FALSE,
            marker= list(colors=colors, line = list(color="black", width=1))) %＞%
            layout(title="Pie chart for Mother education level\
                                    (with Plotly)")
gh
ggplot(sp, aes(x=Medu)) + 
  geom_bar(stat = "count",
                 aes(fill =..count..))+
  scale_y_continuous(breaks = seq(0,190,10))+
  ggtitle("Bar chart representing  Mother education level")

shapiro.test(sp$G3)
shapiro.test(sp$G3[sp$Medu])
shapiro.test(sp$G3[sp$Fedu])



aggregate(sp[33],
          sp[7],
          median)

mg3<-ggplot(sp, aes(x=G3)) +
        geom_histogram(fill="yellow", colour="black", binwidth = 2) +
        facet_grid(Medu ~ .)+
        ggtitle("Mother's education and Final Grades")+
        geom_vline(data=aggregate(sp[33],
                                  sp[7],
                                  median),
                   mapping=aes(xintercept=G3),
                   color="red")


aggregate(sp[33],
          sp[8],
          median)


fg3<-ggplot(sp, aes(x=G3)) +
      geom_histogram(fill="blue", colour="black", binwidth = 2) +
      facet_grid(Fedu ~ .)+
      ggtitle("Fathers's education and Final Grades")+
      geom_vline(data=aggregate(sp[33],
                                 sp[8],
                                median),
           mapping=aes(xintercept=G3),
           color="red")

grid.arrange(mg3,fg3,ncol=2)

sp$Medu<-factor(sp$Medu,
                     levels=c(0,1,2,3,4),
                     labels=c("none",
                              "primary education ",
                              "5th to 9th grade",
                              "secondary education",
                              "higher education"))
sp$Fedu<-factor(sp$Fedu,
                     levels=c(0,1,2,3,4),
                     labels=c("none",
                              "primary education ",
                              "5th to 9th grade",
                              "secondary education",
                              "higher education"))

table(sp$G3,sp$Medu)
table(sp$G3,sp$Fedu)
kruskal.test(sp$G3~sp$Medu)
kruskal.test(sp$G3~sp$Fedu)

kruskal.test(sp$G3~sp$Medu)
kruskal.test(sp$G3~sp$Fedu)

shapiro.test(sm$G3)
shapiro.test(sm$G1)
shapiro.test(sm$G2)


shapiro.test(sp$G3)
shapiro.test(sp$G1)
shapiro.test(sp$G2)


g1g3<-ggplot(sm, aes(x=G1, y=G3))+
        geom_point()+
        ggtitle("Relationship Between First Period and Final Grade")+
        xlab("First Period Grade")+
        ylab("Final Grade")
g1g2<-ggplot(sm, aes(x=G1, y=G2))+
        geom_point()+
        ggtitle("Relationship Between First Period and Second Period Grade")+
        xlab("First Period Grade")+
        ylab("Second Grade")

g2g3<-ggplot(sm, aes(x=G2, y=G3))+
        geom_point()+
        ggtitle("Relationship Between First Period and Final Grade")+
        xlab("Second Period Grade")+
        ylab("Final Grade")
grid.arrange(g1g3,g1g2,g2g3, nrow=3)

g1g3a<-ggplot(sp, aes(x=G1, y=G3))+
        geom_point()+
        ggtitle("Relationship Between First Period and Final Grade")+
        xlab("First Period Grade")+
        ylab("Final Grade")
g1g2a<-ggplot(sp, aes(x=G1, y=G2))+
        geom_point()+
        ggtitle("Relationship Between First Period and Second Period Grade")+
        xlab("First Period Grade")+
        ylab("Second Grade")

g2g3a<-ggplot(sp, aes(x=G2, y=G3))+
        geom_point()+
        ggtitle("Relationship Between First Period and Final Grade")+
        xlab("Second Period Grade")+
        ylab("Final Grade")
grid.arrange(g1g3a,g1g2a,g2g3a, nrow=3)

cor.test(sm$G1,
         sm$G3,
         method = "spearman")

cor.test(sm$G1,
         sm$G2,
         method = "spearman")


cor.test(sm$G3,
         sm$G2,
         method = "spearman")

cor.test(sp$G1,
         sp$G3,
         method = "spearman")

cor.test(sp$G1,
         sp$G2,
         method = "spearman")


cor.test(sp$G3,
         sp$G2,
         method = "spearman")

hghd<- ggplot(sm,aes(higher,Dalc))+
                geom_violin()+coord_flip()+
                xlab("Wants to take higher education")+
                ylab("Workday Alcohol Consumption")+
                ggtitle(" Distribution of Alcohol Consumption Given Desire for Higher Education")




hghw<- ggplot(sm,aes(higher,Walc))+
                geom_violin()+coord_flip()+
                xlab("Wants to take higher education")+
                ylab("Weekend Alcohol Consumption")+
                ggtitle(" Distribution of Alcohol Consumption Given Desire for Higher Education")

grid.arrange(hghd,hghw,nrow=2)


waffle.col <- c("#00d27f","#adff00","#f9d62e","#fc913a","#ff4e50")

waw<-ggplot(sm, aes(higher,Walc, fill=higher))+ 
        geom_boxplot()+ 
        theme_bw()+ 
        theme(legend.position="none")+ 
        scale_fill_manual(values=waffle.col)+ 
        xlab("Desire For Higher Education")+ 
        ylab("Alcohol Consumption")+        
        ggtitle("Desire For Higher education vs.\nWeekend Alcohol Consumption ") 
daw<-ggplot(sm, aes(higher,Dalc, fill=higher))+ 
            geom_boxplot()+ 
            theme_bw()+ 
            theme(legend.position="none")+ 
            scale_fill_manual(values=waffle.col)+ 
            xlab("Desire for Higher Education")+ 
            ylab("Alcohol Consumption")+ 
            ggtitle("Desire For Higher education vs.\nWeekday Alcohol Consumption") 



grid.arrange(waw,daw,ncol=2) 

table(sm$Walc,sm$higher)
table(sm$Dalc, sm$higher)
chisq.test(table(sm$Walc,sm$higher))
chisq.test(table(sm$Dalc, sm$higher))

studytime.factor<-factor(sm$studytime,
                     levels=c(1,2,3,4),
                     labels=c("1-2","2-5","5-10",">10"))


ggplot(sm, aes(sex,studytime, fill=sex))+ 
        geom_boxplot()+ 
        theme_bw()+ 
        theme(legend.position="none")+ 
        scale_fill_manual(values=waffle.col)+ 
        xlab("Gender")+ 
        ylab("Study time")+        
        ggtitle("Gender vs.\nStudy time ") 


table(studytime.factor, sm$sex)

chisq.test(table(studytime.factor, sm$sex))


ggplot(sm, aes(romantic, studytime, fill=romantic))+ 
        geom_boxplot()+ 
        theme_bw()+ 
        theme(legend.position="none")+ 
        scale_fill_manual(values=waffle.col)+ 
        xlab("Romantic relationship")+ 
        ylab("Study time")+        
        ggtitle("Romantic relationship vs.\nStudy time ") 




table(studytime.factor, sm$romantic)
chisq.test(table(studytime.factor, sm$romantic))


waffle.col <- c("#00d27f","#adff00","#f9d62e","#fc913a","#ff4e50")

ggplot(sm, aes(paid,studytime, fill=paid))+ 
        geom_boxplot()+ 
        theme_bw()+ 
        theme(legend.position="none")+ 
        scale_fill_manual(values=waffle.col)+ 
        xlab("Paid classes in math")+ 
        ylab("Study time")+        
        ggtitle("Paid classes in math vs.\nStudy time ") 


table(studytime.factor, sm$paid)
chisq.test(table(studytime.factor, sm$paid))



ggplot(sm, aes(x=failures)) + 
  geom_histogram(binwidth = 0.5, aes(fill = ..count..))+
  scale_x_continuous(breaks = seq(0,4,1))+
  scale_y_continuous(breaks = seq(0,395,10))+
  ggtitle("Histogram of the failures in math")

ggplot(data=sm, aes(x=failures, fill=sex))+
  geom_bar(stat="count",
           position="dodge",
           color="NA")+
  scale_x_continuous(breaks = seq(0,3,1))+
  scale_y_continuous(breaks = seq(0,170,5))+
  ggtitle("Histogram of diferences between failures\
          of the boys and the girls ")


table(sm$failures, sm$sex)


chisq.test(table(sm$failures, sm$sex))


ggplot(data=sm, aes(x=failures, fill=romantic))+
  geom_bar(stat="count",
           position="dodge",
           color="NA")+
  scale_x_continuous(breaks = seq(0,3,1))+
  scale_y_continuous(breaks = seq(0,220,10))+
  ggtitle("Histogram of diferences between failures\
          pupils in/out of romantic relationship ")


chisq.test(table(sm$failures,sm$romantic))

table(sm$failures, sm$romantic)

ggplot(data=sm, aes(x=higher, fill=sex))+
  geom_bar(stat="count",
           position="dodge",
           color="NA")+
  ggtitle("Histogram of diferences between desire for higher education\
          of the boys and the girls ")




table(sm$higher, sm$sex)
chisq.test(table(sm$higher, sm$sex))

ggplot(data=sm, aes(x=higher, fill=address))+
  geom_bar(stat="count",
           position="dodge",
           color="NA")+
  ggtitle("Histogram of diferences between desire for higher education\
          of the students from urban and rural areas ")



    table(sm$higher,sm$address)
chisq.test(table(sm$higher,sm$addres))

ggplot(sm, aes(x=absences)) + 
  geom_histogram(binwidth = 0.5, aes(fill = ..count..))+
  scale_x_continuous(breaks = seq(0,80,5))+
  scale_y_continuous(breaks = seq(0,120,10))+
  ggtitle("Histogram of the absences")

sm$Dalc <- as.factor(sm$Dalc) 
sm$Dalc <- mapvalues(sm$Dalc, 
                     from = 1:5, 
                     to = c("Very Low", "Low", "Medium", "High", "Very High"))

sm$Walc <- as.factor(sm$Walc)      
sm$Walc <- mapvalues(sm$Walc, 
                    from = 1:5, 
                    to = c("Very Low", "Low", "Medium", "High", "Very High"))

abs<-ggplot(sm, aes(x=Dalc, y=absences, fill=Dalc))+
      geom_violin()+
      scale_fill_manual(values = waffle.col)+
      theme_bw()+
      theme(legend.position="none")+
      ggtitle("Absences distribution per Workday alcohol consumption")+
      xlab("Alcohol consumption")+
      ylab("Number of school absences")
absw<-ggplot(sm, aes(x=Walc, y=absences, fill=Walc))+
      geom_violin()+
      scale_fill_manual(values = waffle.col)+
      theme_bw()+
      theme(legend.position="none")+
      ggtitle("Absences distribution per Weekend alcohol consumption")+
      xlab("Alcohol consumption")+
      ylab("Number of school absences")
grid.arrange(abs,absw, nrow=2)

table(sm$absences,sm$Walc)
table(sm$absences,sm$Dalc)
kruskal.test(sm$absences~sm$Walc)
kruskal.test(sm$absences~sm$Dalc)


 cor.test(sm$failures,
         sm$G3,
         method = "spearman")


 cor.test(sp$failures,
         sp$G3,
         method = "spearman")



