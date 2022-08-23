##### SETUP THE WORKING DIRECTORY ##### setwd("C:/Training") 
#### INSTALLING PACKAGES AND INVOKING LIBRARIES #### library(dplyr) library(tidyverse) library(Hmisc) library(psych) library(corrplot) library(nFactors) library(qgraph) library(faraway) library(GGally) 
#### IMPORTING THE DATASET INTO R STUDIO #### data<-read.csv("Factor-Hair-Revised.csv",header = TRUE) 
#######################################################
################################ 
            # EXPLORATORY DATA ANALYSIS # 
#######################################################
################################ View(data) dim(data) 
# [1] 100  13 
 
str(data)   
## 'data.frame':    100 obs. of  13 variables: 

##  $ ID          : int  1 2 3 4 5 6 7 8 9 10 ... 
##  $ ProdQual    : num  8.5 8.2 9.2 6.4 9 6.5 6.9 6.2 5.8 6.4 ... 
##  $ Ecom        : num  3.9 2.7 3.4 3.3 3.4 2.8 3.7 3. 3 3.6 4.5 ... 
##  $ TechSup     : num  2.5 5.1 5.6 7 5.2 3.1 5 3.9 5. 1 5.1 ... 
##  $ CompRes     : num  5.9 7.2 5.6 3.7 4.6 4.1 2.6 4.
8 6.7 6.1 ... 
##  $ Advertising : num  4.8 3.4 5.4 4.7 2.2 4 2.1 4.6 3.7 4.7 ... 
##  $ ProdLine    : num  4.9 7.9 7.4 4.7 6 4.3 2.3 3.6 
5.9 5.7 ... 
##  $ SalesFImage : num  6 3.1 5.8 4.5 4.5 3.7 5.4 5.1 5.8 5.7 ... 
##  $ ComPricing  : num  6.8 5.3 4.5 8.8 6.8 8.5 8.9 6. 9 9.3 8.4 ... 
##  $ WartyClaim  : num  4.7 5.5 6.2 7 6.1 5.1 4.8 5.4 5.9 5.4 ... 
##  $ OrdBilling  : num  5 3.9 5.4 4.3 4.5 3.6 2.1 4.3 4.4 4.1 ... 
##  $ DelSpeed    : num  3.7 4.9 4.5 3 3.5 3.3 2 3.7 4. 6 4.4 ... 
##  $ Satisfaction: num  8.2 5.7 8.9 4.8 7.1 4.7 5.7 6.
3 7 5.5 ... 
summary(data) 
##        ID            ProdQual           Ecom          TechSup      
##  Min.   :  1.00   Min.   : 5.000   Min.   :2.200   M in.   :1.300   
##  1st Qu.: 25.75   1st Qu.: 6.575   1st Qu.:3.275   1 st Qu.:4.250   

##  Median : 50.50   Median : 8.000   Median :3.600   M
edian :5.400   
##  Mean   : 50.50   Mean   : 7.810   Mean   :3.672   M ean   :5.365   
##  3rd Qu.: 75.25   3rd Qu.: 9.100   3rd Qu.:3.925   3 rd Qu.:6.625   
##  Max.   :100.00   Max.   :10.000   Max.   :5.700   M ax.   :8.500   
##     CompRes       Advertising       ProdLine      Sa
lesFImage    
##  Min.   :2.600   Min.   :1.900   Min.   :2.300   Min
.   :2.900   
##  1st Qu.:4.600   1st Qu.:3.175   1st Qu.:4.700   1st Qu.:4.500   
##  Median :5.450   Median :4.000   Median :5.750   Med ian :4.900   
##  Mean   :5.442   Mean   :4.010   Mean   :5.805   Mea n   :5.123   
##  3rd Qu.:6.325   3rd Qu.:4.800   3rd Qu.:6.800   3rd Qu.:5.800   
##  Max.   :7.800   Max.   :6.500   Max.   :8.400   Max .   :8.200   
##    ComPricing      WartyClaim      OrdBilling       DelSpeed     
##  Min.   :3.700   Min.   :4.100   Min.   :2.000   Min
.   :1.600   
##  1st Qu.:5.875   1st Qu.:5.400   1st Qu.:3.700   1st Qu.:3.400   
##  Median :7.100   Median :6.100   Median :4.400   Med ian :3.900   
##  Mean   :6.974   Mean   :6.043   Mean   :4.278   Mea n   :3.886   

##  3rd Qu.:8.400   3rd Qu.:6.600   3rd Qu.:4.800   3rd 
Qu.:4.425   
##  Max.   :9.900   Max.   :8.100   Max.   :6.700   Max .   :5.500   
##   Satisfaction   ##  Min.   :4.700   ##  1st Qu.:6.000   
##  Median :7.050   
##  Mean   :6.918   
##  3rd Qu.:7.625   
##  Max.   :9.900 
 
names(data) 
##  [1] "ID"           "ProdQual"     "Ecom"         "T echSup"      "CompRes"      
##  [6] "Advertising"  "ProdLine"     "SalesFImage"  "C
omPricing"   "WartyClaim"   
## [11] "OrdBilling"   "DelSpeed"     "Satisfaction" 
 
class(data) 
## [1] "data.frame" 
 head(data) 
##   ID ProdQual Ecom TechSup CompRes Advertising ProdL ine SalesFImage ComPricing 
## 1  1      8.5  3.9     2.5     5.9         4.8      4.9         6.0        6.8 
## 2  2      8.2  2.7     5.1     7.2         3.4      7.9         3.1        5.3 

## 3  3      9.2  3.4     5.6     5.6         5.4      
7.4         5.8        4.5 
## 4  4      6.4  3.3     7.0     3.7         4.7      4.7         4.5        8.8 
## 5  5      9.0  3.4     5.2     4.6         2.2      6.0         4.5        6.8 
## 6  6      6.5  2.8     3.1     4.1         4.0      4.3         3.7        8.5 
##   WartyClaim OrdBilling DelSpeed Satisfaction 
## 1        4.7        5.0      3.7          8.2 
## 2        5.5        3.9      4.9          5.7 
## 3        6.2        5.4      4.5          8.9 
## 4        7.0        4.3      3.0          4.8 ## 5        6.1        4.5      3.5          7.1 ## 6        5.1        3.6      3.3          4.7 
 tail(data) 
##      ID ProdQual Ecom TechSup CompRes Advertising Pr odLine SalesFImage 
## 95   95      9.3  3.8     4.0     4.6         4.7      
6.4         5.5 
## 96   96      8.6  4.8     5.6     5.3         2.3
6.0         5.7 
## 97   97      7.4  3.4     2.6     5.0         4.1      
4.4         4.8 
## 98   98      8.7  3.2     3.3     3.2         3.1      6.1         2.9 
## 99   99      7.8  4.9     5.8     5.3         5.2      5.3         7.1 
## 100 100      7.9  3.0     4.4     5.1         5.9      4.2         4.8 

##     ComPricing WartyClaim OrdBilling DelSpeed Satisf
action 
## 95         7.4        5.3        3.6      3.4          7.7 
## 96         6.7        5.8        4.9      3.6          7.3 
## 97         7.2        4.5        4.2      3.7          6.3 
## 98         5.6        5.0        3.1      2.5          
5.4 
## 99         7.9        6.0        4.3      3.9          
6.4 
## 100        9.7        5.7        3.4      3.5          6.4 
 
describe(data) 
## data  
##  
##  13  Variables      100  Observations 
## ------------------------------------------------------------------------------- 
## ID  
##        n  missing distinct     Info     Mean      Gm d      .05      .10  
##      100        0      100        1     50.5    33.6
7     5.95    10.90  
##      .25      .50      .75      .90      .95  ##    25.75    50.50    75.25    90.10    95.05  
##  
## lowest :   1   2   3   4   5, highest:  96  97  98  99 100 

## ----------------------------------------------------
---------------------------- 
## ProdQual  
##        n  missing distinct     Info     Mean      Gm d      .05      .10  
##      100        0       43    0.999     7.81     1.6
1    5.595    5.790  
##      .25      .50      .75      .90      .95  
##    6.575    8.000    9.100    9.410    9.900  
##  
## lowest :  5.0  5.1  5.2  5.5  5.6, highest:  9.4  9.
5  9.6  9.9 10.0 
## ----------------------------------------------------
---------------------------- 
## Ecom  
##        n  missing distinct     Info     Mean      Gm d      .05      .10  
##      100        0       27    0.996    3.672   0.767
4    2.595    2.800  
##      .25      .50      .75      .90      .95  ##    3.275    3.600    3.925    4.530    5.100  
##  
## lowest : 2.2 2.4 2.5 2.6 2.7, highest: 4.9 5.1 5.5 5 .6 5.7 
## ----------------------------------------------------
---------------------------- 
## TechSup  
##        n  missing distinct     Info     Mean      Gm d      .05      .10  
##      100        0       50    0.999    5.365    1.75 5    2.700    3.280  

##      .25      .50      .75      .90      .95  
##    4.250    5.400    6.625    7.210    7.605  
##  
## lowest : 1.3 2.5 2.6 2.7 3.0, highest: 7.7 7.9 8.0 8 .4 8.5 
## ----------------------------------------------------
---------------------------- 
## CompRes  
##        n  missing distinct     Info     Mean      Gm d      .05      .10  
##      100        0       45    0.999    5.442    1.38
8    3.595    3.900  
##      .25      .50      .75      .90      .95  ##    4.600    5.450    6.325    7.010    7.305  
##  
## lowest : 2.6 3.0 3.2 3.5 3.6, highest: 7.4 7.5 7.6 7
.7 7.8 
## ------------------------------------------------------------------------------- 
## Advertising  
##        n  missing distinct     Info     Mean      Gm d      .05      .10  
##      100        0       41    0.999     4.01    1.30 2    2.200    2.400  
##      .25      .50      .75      .90      .95  
##    3.175    4.000    4.800    5.510    5.800  
##  
## lowest : 1.9 2.1 2.2 2.3 2.4, highest: 5.7 5.8 5.9 6 .3 6.5 

## ----------------------------------------------------
---------------------------- 
## ProdLine  
##        n  missing distinct     Info     Mean      Gm d      .05      .10  
##      100        0       42    0.999    5.805    1.50
9    3.900    4.190  
##      .25      .50      .75      .90      .95  
##    4.700    5.750    6.800    7.600    7.805  
##  
## lowest : 2.3 2.9 3.3 3.6 3.9, highest: 7.7 7.8 7.9 8
.3 8.4 
## ------------------------------------------------------------------------------- 
## SalesFImage  
##        n  missing distinct     Info     Mean      Gm d      .05      .10  
##      100        0       35    0.997    5.123     1.1
9    3.385    3.790  
##      .25      .50      .75      .90      .95  ##    4.500    4.900    5.800    6.610    7.100  
##  
## lowest : 2.9 3.0 3.1 3.4 3.5, highest: 6.8 6.9 7.1 7 .8 8.2 
## ----------------------------------------------------
---------------------------- 
## ComPricing  
##        n  missing distinct     Info     Mean      Gm d      .05      .10  
##      100        0       45    0.998    6.974    1.77 8    4.500    4.800  

##      .25      .50      .75      .90      .95  
##    5.875    7.100    8.400    8.810    9.105  
##  
## lowest : 3.7 3.8 4.4 4.5 4.6, highest: 9.2 9.3 9.6 9 .7 9.9 
## ----------------------------------------------------
---------------------------- 
## WartyClaim  
##        n  missing distinct     Info     Mean      Gm d      .05      .10  
##      100        0       34    0.998    6.043   0.937
2    4.795    5.000  
##      .25      .50      .75      .90      .95  ##    5.400    6.100    6.600    7.200    7.305  
##  
## lowest : 4.1 4.3 4.5 4.7 4.8, highest: 7.3 7.4 7.5 7
.7 8.1 
## ----------------------------------------------------
---------------------------- 
## OrdBilling  
##        n  missing distinct     Info     Mean      Gm d      .05      .10  
##      100        0       37    0.998    4.278    1.03 3    2.595    3.000  
##      .25      .50      .75      .90      .95  
##    3.700    4.400    4.800    5.400    5.605  
##  
## lowest : 2.0 2.1 2.4 2.5 2.6, highest: 5.5 5.6 5.7 6 .5 6.7 

## ----------------------------------------------------
---------------------------- 
## DelSpeed  
##        n  missing distinct     Info     Mean      Gm d      .05      .10  
##      100        0       30    0.997    3.886   0.826
7    2.595    2.990  
##      .25      .50      .75      .90      .95  
##    3.400    3.900    4.425    4.710    4.900  
##  
## lowest : 1.6 2.0 2.4 2.5 2.6, highest: 4.7 4.8 4.9 5
.2 5.5 
## ------------------------------------------------------------------------------- 
## Satisfaction  
##        n  missing distinct     Info     Mean      Gm d      .05      .10  
##      100        0       29    0.997    6.918    1.37
1    5.190    5.400  
##      .25      .50      .75      .90      .95  ##    6.000    7.050    7.625    8.600    8.900  lowest : 4.7 4.8 5.0 5.2 5.4, highest: 8.6 8.7 8.9 9.0 9.9 
## ----------------------------------------------------
---------------------------- 
 
Mydata<-data[,c(2:13)]#### OMITTING THE FIRST COLUMN ##
## view(Mydata) 
 

dim(Mydata) 
## [1] 100  12 
 attach(Mydata) 
 
#### UNIVARIATE ANALYSIS ### #### PRODUCT QUALITY #### hist(ProdQual,col="blue") boxplot(ProdQual,col="blue") 
plot(density(ProdQual,main="PRODUCT QUALITY")) 
#### E-COMMERCE #### hist(Ecom,col ="red" ) boxplot(Ecom,col = "red") 
plot(density(Ecom,main="EFFECT OF E-COMMERCE")) 
#### TECHNICAL SUPPORT #### hist(TechSup,col = "orange") boxplot(TechSup,col = "orange") 
plot(density(TechSup,main="TECHNICAL SUPPORT")) 
#### COMPLAINT RESOLUTION #### hist(CompRes,col="gray") boxplot(CompRes,col="gray") 
plot(density(CompRes,main="COMPLAINT RESOLUTION")) 
#### ADVERTISING #### hist(Advertising,col="green") boxplot(Advertising,col="green") 
plot(density(Advertising,main="ADVERTISING")) 
#### PRODUCT LINE #### 

hist(ProdLine,col="yellow") boxplot(ProdLine,col="yellow") 
plot(density(ProdLine,main="PRODUCT LINE")) 
#### SALESFORCE IMAGE #### hist(SalesFImage,col="coral") boxplot(SalesFImage,col="coral") 
plot(density(SalesFImage,main="SALESFORCE IMAGE")) 
#### COMPETITIVE PRICING #### hist(ComPricing,col = "maroon") boxplot(ComPricing,col="maroon") 
plot(density(ComPricing,main="COMPETITIVE PRICING")) 
#### WARRANTY & CLAIM #### hist(WartyClaim,col="violet") boxplot(WartyClaim,col="violet") 
plot(density(WartyClaim,main="WARRANTY AND CLAIMS")) 
#### ORDERING & BILLING #### hist(OrdBilling,col="purple") boxplot(OrdBilling,col="purple") 
plot(density(OrdBilling,main="ORDER & BILLING")) 
#### DELIVERY SPEED #### hist(DelSpeed,col="Brown") boxplot(DelSpeed,col="Brown") 
plot(density(DelSpeed,main="DELIVERY SPEED")) #### SATISFACTION #### 
hist(Satisfaction,col="royal blue") boxplot(Satisfaction,col="royal blue") 
plot(density(Satisfaction,main="CUSTOMER SATISFACTION")
) 

 
#### BI-VARIATE ANALYSIS #### 
histogram(ProdQual,Satisfaction,xlab = "PRODUCT QUALITY ",ylab="SATISFACTION",main="PRODUCT QUALITY VS SATISFAC TION") 
histogram((Satisfaction~Advertising),col="brown",xlab=" ADVERTISEMENT",ylab = "SATISFACTION",main="PRODUCT ADVE RTISEMENT VS CUSTOMER SATISFACTION") 
histogram(Satisfaction~Ecom,xlab ="E-COMMERCE",ylab = " CUSTOMER SATISFACTION",main="E-COMMERCE VS SATISFACTION
" ) 
 
#### MULTIVARIATE ANALYSIS #### 
qplot(ProdQual,Advertising,color=Satisfaction) qplot(OrdBilling,WartyClaim,color=Satisfaction) qplot(TechSup,CompRes,color=Satisfaction) 
 
#### CHECK FOR NA VALUES #### anyNA(Mydata) 
## [1] FALSE 
 
#### CHECK FOR OUTLIERS #### boxplot.stats(Mydata$Ecom)$out 
## [1] 5.6 5.7 5.1 5.1 5.1 5.5 
boxplot.stats(Mydata$SalesFImage)$out 
## [1] 7.8 7.8 8.2 
boxplot.stats(Mydata$OrdBilling)$out 
## [1] 6.7 6.5 2.0 2.0 
boxplot.stats(Mydata$DelSpeed)$out 

## [1] 1.6 
 
 
####################################################### # CHECK FOR MULTICOLLINEARITY # 
####################################################### 
(cor(Mydata))->corr corr 
##                 ProdQual          Ecom       TechSup    CompRes Advertising 
## ProdQual      1.00000000 -0.1371632174  0.0956004542  
0.1063700 -0.05347313 
## Ecom         -0.13716322  1.0000000000  0.0008667887  0.1401793  0.42989071 
## TechSup       0.09560045  0.0008667887  1.0000000000  0.0966566 -0.06287007 
## CompRes       0.10637000  0.1401792611  0.0966565978  
1.0000000  0.19691685 
## Advertising  -0.05347313  0.4298907110 -0.0628700668  0.1969168  1.00000000 
## ProdLine      0.47749341 -0.0526878383  0.1926254565  0.5614170 -0.01155082 
## SalesFImage  -0.15181287  0.7915437115  0.0169905395  0.2297518  0.54220366 
## ComPricing   -0.40128188  0.2294624014 -0.2707866821 
-0.1279543  0.13421689 
## WartyClaim    0.08831231  0.0518981915  0.7971679258  0.1404083  0.01079207 
## OrdBilling    0.10430307  0.1561473316  0.0801018246  0.7568686  0.18423559 

## DelSpeed      0.02771800  0.1916360683  0.0254406935  
0.8650917  0.27586308 
## Satisfaction  0.48632500  0.2827450147  0.1125971788  
0.6032626  0.30466947 
##                 ProdLine SalesFImage  ComPricing  Wa rtyClaim  OrdBilling 
## ProdQual      0.47749341 -0.15181287 -0.40128188  0.
08831231  0.10430307 
## Ecom         -0.05268784  0.79154371  0.22946240  0.
05189819  0.15614733 
## TechSup       0.19262546  0.01699054 -0.27078668  0.
79716793  0.08010182 
## CompRes       0.56141695  0.22975176 -0.12795425  0. 14040830  0.75686859 
## Advertising  -0.01155082  0.54220366  0.13421689  0. 01079207  0.18423559 
## ProdLine      1.00000000 -0.06131553 -0.49494840  0. 27307753  0.42440825 
## SalesFImage  -0.06131553  1.00000000  0.26459655  0. 10745534  0.19512741 
## ComPricing   -0.49494840  0.26459655  1.00000000 -0. 24498605 -0.11456703 
## WartyClaim    0.27307753  0.10745534 -0.24498605  1. 00000000  0.19706512 
## OrdBilling    0.42440825  0.19512741 -0.11456703  0.
19706512  1.00000000 
## DelSpeed      0.60185021  0.27155126 -0.07287173  0. 10939460  0.75100307 
## Satisfaction  0.55054594  0.50020531 -0.20829569  0. 17754482  0.52173191 
##                 DelSpeed Satisfaction ## ProdQual      0.02771800    0.4863250 

## Ecom          0.19163607    0.2827450 
## TechSup       0.02544069    0.1125972 ## CompRes       0.86509170    0.6032626 ## Advertising   0.27586308    0.3046695 ## ProdLine      0.60185021    0.5505459 ## SalesFImage   0.27155126    0.5002053 ## ComPricing   -0.07287173   -0.2082957 
## WartyClaim    0.10939460    0.1775448 
## OrdBilling    0.75100307    0.5217319 ## DelSpeed      1.00000000    0.5770423 ## Satisfaction  0.57704227    1.0000000 
qgraph(corr) 
chart.Correlation(Mydata,histogram = TRUE,pch="+",metho d = c("pearson","kendall","spearman")) corrplot(corr,method = "number") 
 
#### PAIR-WISE CORRELATION #### 
rcorr(as.matrix(Mydata))->c####with p values c 
##              ProdQual  Ecom TechSup CompRes Advertis ing ProdLine SalesFImage 
## ProdQual         1.00 -0.14    0.10    0.11       -0
.05     0.48       -0.15 
## Ecom            -0.14  1.00    0.00    0.14        0
.43    -0.05        0.79 
## TechSup          0.10  0.00    1.00    0.10       -0 .06     0.19        0.02 
## CompRes          0.11  0.14    0.10    1.00        0 .20     0.56        0.23 

## Advertising     -0.05  0.43   -0.06    0.20        1
.00    -0.01        0.54 
## ProdLine         0.48 -0.05    0.19    0.56       -0 .01     1.00       -0.06 
## SalesFImage     -0.15  0.79    0.02    0.23        0 .54    -0.06        1.00 
## ComPricing      -0.40  0.23   -0.27   -0.13        0 .13    -0.49        0.26 
## WartyClaim       0.09  0.05    0.80    0.14        0
.01     0.27        0.11 
## OrdBilling       0.10  0.16    0.08    0.76        0
.18     0.42        0.20 
## DelSpeed         0.03  0.19    0.03    0.87        0 .28     0.60        0.27 
## Satisfaction     0.49  0.28    0.11    0.60        0 .30     0.55        0.50 
##              ComPricing WartyClaim OrdBilling DelSpe ed Satisfaction 
## ProdQual          -0.40       0.09       0.10     0. 03         0.49 
## Ecom               0.23       0.05       0.16     0. 19         0.28 
## TechSup           -0.27       0.80       0.08     0. 03         0.11 
## CompRes           -0.13       0.14       0.76     0.
87         0.60 
## Advertising        0.13       0.01       0.18     0. 28         0.30 
## ProdLine          -0.49       0.27       0.42     0. 60         0.55 
## SalesFImage        0.26       0.11       0.20     0.
27         0.50 

## ComPricing         1.00      -0.24      -0.11    -0.
07        -0.21 
## WartyClaim        -0.24       1.00       0.20     0. 11         0.18 
## OrdBilling        -0.11       0.20       1.00     0. 75         0.52 
## DelSpeed          -0.07       0.11       0.75     1.
00         0.58 
## Satisfaction      -0.21       0.18       0.52     0.
58         1.00 
##  
## n= 100  
##  
##  
## P 
##              ProdQual Ecom   TechSup CompRes Adverti sing ProdLine SalesFImage 
## ProdQual              0.1736 0.3441  0.2922  0.5972      0.0000   0.1316      
## Ecom         0.1736          0.9932  0.1642  0.0000      0.6026   0.0000      
## TechSup      0.3441   0.9932         0.3387  0.5343      0.0549   0.8668      
## CompRes      0.2922   0.1642 0.3387          0.0496      
0.0000   0.0215      
## Advertising  0.5972   0.0000 0.5343  0.0496              
0.9092   0.0000      
## ProdLine     0.0000   0.6026 0.0549  0.0000  0.9092           
0.5445      
## SalesFImage  0.1316   0.0000 0.8668  0.0215  0.0000      0.5445               

## ComPricing   0.0000   0.0216 0.0064  0.2046  0.1831
0.0000   0.0078      
## WartyClaim   0.3823   0.6081 0.0000  0.1635  0.9151      0.0060   0.2873      
## OrdBilling   0.3017   0.1208 0.4282  0.0000  0.0665      0.0000   0.0517      
## DelSpeed     0.7843   0.0561 0.8016  0.0000  0.0055      0.0000   0.0063      
## Satisfaction 0.0000   0.0044 0.2647  0.0000  0.0021      
0.0000   0.0000      
##              ComPricing WartyClaim OrdBilling DelSpe
ed Satisfaction 
## ProdQual     0.0000     0.3823     0.3017     0.7843   0.0000       
## Ecom         0.0216     0.6081     0.1208     0.0561   0.0044       
## TechSup      0.0064     0.0000     0.4282     0.8016   0.2647       
## CompRes      0.2046     0.1635     0.0000     0.0000   0.0000       
## Advertising  0.1831     0.9151     0.0665     0.0055   0.0021       
## ProdLine     0.0000     0.0060     0.0000     0.0000   0.0000       
## SalesFImage  0.0078     0.2873     0.0517     0.0063   
0.0000       
## ComPricing              0.0140     0.2564     0.4712   0.0376       
## WartyClaim   0.0140                0.0494     0.2786   0.0772       
## OrdBilling   0.2564     0.0494                0.0000   0.0000       

## DelSpeed     0.4712     0.2786     0.0000              
0.0000       
## Satisfaction 0.0376     0.0772     0.0000     0.0000 
c$r 
##                 ProdQual          Ecom       TechSup    CompRes Advertising 
## ProdQual      1.00000000 -0.1371632174  0.0956004542  0.1063700 -0.05347313 
## Ecom         -0.13716322  1.0000000000  0.0008667887  
0.1401793  0.42989071 
## TechSup       0.09560045  0.0008667887  1.0000000000  0.0966566 -0.06287007 
## CompRes       0.10637000  0.1401792611  0.0966565978  
1.0000000  0.19691685 
## Advertising  -0.05347313  0.4298907110 -0.0628700668  0.1969168  1.00000000 
## ProdLine      0.47749341 -0.0526878383  0.1926254565  0.5614170 -0.01155082 
## SalesFImage  -0.15181287  0.7915437115  0.0169905395  
0.2297518  0.54220366 
## ComPricing   -0.40128188  0.2294624014 -0.2707866821 -0.1279543  0.13421689 
## WartyClaim    0.08831231  0.0518981915  0.7971679258  0.1404083  0.01079207 
## OrdBilling    0.10430307  0.1561473316  0.0801018246  
0.7568686  0.18423559 
## DelSpeed      0.02771800  0.1916360683  0.0254406935  0.8650917  0.27586308 
## Satisfaction  0.48632500  0.2827450147  0.1125971788  
0.6032626  0.30466947 
##                 ProdLine SalesFImage  ComPricing  Wa rtyClaim  OrdBilling 

## ProdQual      0.47749341 -0.15181287 -0.40128188  0.
08831231  0.10430307 
## Ecom         -0.05268784  0.79154371  0.22946240  0. 05189819  0.15614733 
## TechSup       0.19262546  0.01699054 -0.27078668  0. 79716793  0.08010182 
## CompRes       0.56141695  0.22975176 -0.12795425  0.
14040830  0.75686859 
## Advertising  -0.01155082  0.54220366  0.13421689  0.
01079207  0.18423559 
## ProdLine      1.00000000 -0.06131553 -0.49494840  0.
27307753  0.42440825 
## SalesFImage  -0.06131553  1.00000000  0.26459655  0. 10745534  0.19512741 
## ComPricing   -0.49494840  0.26459655  1.00000000 -0. 24498605 -0.11456703 
## WartyClaim    0.27307753  0.10745534 -0.24498605  1. 00000000  0.19706512 
## OrdBilling    0.42440825  0.19512741 -0.11456703  0. 19706512  1.00000000 
## DelSpeed      0.60185021  0.27155126 -0.07287173  0. 10939460  0.75100307 
## Satisfaction  0.55054594  0.50020531 -0.20829569  0.
17754482  0.52173191 
##                 DelSpeed Satisfaction 
## ProdQual      0.02771800    0.4863250 
## Ecom          0.19163607    0.2827450 
## TechSup       0.02544069    0.1125972 ## CompRes       0.86509170    0.6032626 ## Advertising   0.27586308    0.3046695 ## ProdLine      0.60185021    0.5505459 

## SalesFImage   0.27155126    0.5002053 
## ComPricing   -0.07287173   -0.2082957 ## WartyClaim    0.10939460    0.1775448 ## OrdBilling    0.75100307    0.5217319 ## DelSpeed      1.00000000    0.5770423 ## Satisfaction  0.57704227    1.0000000 
c$P 
##                  ProdQual         Ecom     TechSup      
CompRes  Advertising 
## ProdQual               NA 1.735678e-01 0.344069774 2 .921951e-01 5.972310e-01 
## Ecom         1.735678e-01           NA 0.993171061 1
.642110e-01 8.057831e-06 
## TechSup      3.440698e-01 9.931711e-01          NA 3 .387408e-01 5.343289e-01 
## CompRes      2.921951e-01 1.642110e-01 0.338740849           
NA 4.956684e-02 
## Advertising  5.972310e-01 8.057831e-06 0.534328881 4 .956684e-02           NA 
## ProdLine     5.079855e-07 6.026343e-01 0.054850413 1 .230039e-09 9.091904e-01 
## SalesFImage  1.316067e-01 0.000000e+00 0.866755288 2 .147747e-02 5.663263e-09 
## ComPricing   3.508603e-05 2.164685e-02 0.006431524 2
.045561e-01 1.830814e-01 
## WartyClaim   3.822660e-01 6.080893e-01 0.000000000 1 .635161e-01 9.151325e-01 
## OrdBilling   3.017269e-01 1.208120e-01 0.428231475 0 .000000e+00 6.651550e-02 
## DelSpeed     7.842785e-01 5.613196e-02 0.801621550 0 .000000e+00 5.468280e-03 

## Satisfaction 2.900993e-07 4.367712e-03 0.264693293 3
.085354e-11 2.056065e-03 
##                  ProdLine  SalesFImage   ComPricing  
WartyClaim   OrdBilling 
## ProdQual     5.079855e-07 1.316067e-01 3.508603e-05 0.382265961 3.017269e-01 
## Ecom         6.026343e-01 0.000000e+00 2.164685e-02 0.608089260 1.208120e-01 
## TechSup      5.485041e-02 8.667553e-01 6.431524e-03 
0.000000000 4.282315e-01 
## CompRes      1.230039e-09 2.147747e-02 2.045561e-01 
0.163516085 0.000000e+00 
## Advertising  9.091904e-01 5.663263e-09 1.830814e-01 0.915132498 6.651550e-02 
## ProdLine               NA 5.445035e-01 1.653082e-07 0.005979643 1.079444e-05 
## SalesFImage  5.445035e-01           NA 7.807337e-03 0.287270577 5.171654e-02 
## ComPricing   1.653082e-07 7.807337e-03           NA 
0.014025148 2.563719e-01 
## WartyClaim   5.979643e-03 2.872706e-01 1.402515e-02 NA 4.939207e-02 
## OrdBilling   1.079444e-05 5.171654e-02 2.563719e-01 0.049392074           NA 
## DelSpeed     3.524825e-11 6.277454e-03 4.712084e-01 
0.278609678 0.000000e+00 
## Satisfaction 2.953080e-09 1.164314e-07 3.755877e-02 
0.077195604 2.601982e-08 
##                  DelSpeed Satisfaction 
## ProdQual     7.842785e-01 2.900993e-07 ## Ecom         5.613196e-02 4.367712e-03 ## TechSup      8.016216e-01 2.646933e-01 

## CompRes      0.000000e+00 3.085354e-11 
## Advertising  5.468280e-03 2.056065e-03 ## ProdLine     3.524825e-11 2.953080e-09 ## SalesFImage  6.277454e-03 1.164314e-07 ## ComPricing   4.712084e-01 3.755877e-02 ## WartyClaim   2.786097e-01 7.719560e-02 ## OrdBilling   0.000000e+00 2.601982e-08 
## DelSpeed               NA 3.300471e-10 
## Satisfaction 3.300471e-10           NA 
 
#### VARIABLE INFLATION FACTOR #### 
vif(mymodel)####IF VIF VALUES ARE > 4,THEN MULTICOLLINE
ARITY EXISTS BETWEEN THE REGRESSORS 
##    ProdQual        Ecom     TechSup     CompRes Adve rtising    ProdLine  
##    1.635797    2.756694    2.976796    4.730448    1
.508933    3.488185  
## SalesFImage  ComPricing  WartyClaim  OrdBilling    D elSpeed  
##    3.439420    1.635000    3.198337    2.902999    6 .516014 
 
mean(vif(mymodel)) 
## [1] 3.162602 
###################################################### 
# SIMPLE LINEAR REGRESSION # 
####################################################### MODEL1=lm(Satisfaction~ProdQual,data = Mydata) summary(MODEL1) 

##  
## Call: 
## lm(formula = Satisfaction ~ ProdQual, data = Mydata) 
##  
## Residuals: 
##      Min       1Q   Median       3Q      Max  ## -1.88746 -0.72711 -0.01577  0.85641  2.25220  
##  
## Coefficients: 
##             Estimate Std. Error t value Pr(>|t|)     ## (Intercept)  3.67593    0.59765   6.151 1.68e-08 *** 
## ProdQual     0.41512    0.07534   5.510 2.90e-07 *** 
## --- 
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 
0.1 ' ' 1 
##  
## Residual standard error: 1.047 on 98 degrees of free
dom 
## Multiple R-squared:  0.2365, Adjusted R-squared:  0. 2287  
## F-statistic: 30.36 on 1 and 98 DF,  p-value: 2.901e-
07 
plot(ProdQual,Satisfaction,col="maroon",abline(lm(Satis faction~ProdQual),col="dark blue")) MODEL2=lm(Satisfaction~Ecom) summary(MODEL2) 
##  
## Call: 
## lm(formula = Satisfaction ~ Ecom) 

##  
## Residuals: 
##      Min       1Q   Median       3Q      Max  ## -2.37200 -0.78971  0.04959  0.68085  2.34580  
##  
## Coefficients: 
##             Estimate Std. Error t value Pr(>|t|)     
## (Intercept)   5.1516     0.6161   8.361 4.28e-13 *** 
## Ecom          0.4811     0.1649   2.918  0.00437 **  
## --- 
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
##  
## Residual standard error: 1.149 on 98 degrees of free dom 
## Multiple R-squared:  0.07994,    Adjusted R-squared:  
0.07056  
## F-statistic: 8.515 on 1 and 98 DF,  p-value: 0.00436 8 
plot(Ecom,Satisfaction,col="blue",abline(lm(Satisfactio n~Ecom),col="red")) 
MODEL3=lm(Satisfaction~TechSup) summary(MODEL3) 
##  
## Call: 
## lm(formula = Satisfaction ~ TechSup) 
##  
## Residuals: 
##      Min       1Q   Median       3Q      Max  
 

## -2.26136 -0.93297  0.04302  0.82501  2.85617  
##  
## Coefficients: 
##             Estimate Std. Error t value Pr(>|t|)     ## (Intercept)  6.44757    0.43592  14.791   <2e-16 *** ## TechSup      0.08768    0.07817   1.122    0.265     
## --- 
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 
0.1 ' ' 1 
##  
## Residual standard error: 1.19 on 98 degrees of freed
om 
## Multiple R-squared:  0.01268,    Adjusted R-squared:  
0.002603  
## F-statistic: 1.258 on 1 and 98 DF,  p-value: 0.2647 
plot(TechSup,Satisfaction,col="black",abline(lm(Satisfa ction~TechSup),col="red")) MODEL4=lm(Satisfaction~CompRes) summary(MODEL4) 
##  
## Call: 
## lm(formula = Satisfaction ~ CompRes) 
##  
## Residuals: 
##      Min       1Q   Median       3Q      Max  
## -2.40450 -0.66164  0.04499  0.63037  2.70949  
##  
## Coefficients: 
##             Estimate Std. Error t value Pr(>|t|)     

## (Intercept)  3.68005    0.44285   8.310 5.51e-13 *** 
## CompRes      0.59499    0.07946   7.488 3.09e-11 *** ## --- 
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 
0.1 ' ' 1 
##  
## Residual standard error: 0.9554 on 98 degrees of fre
edom 
## Multiple R-squared:  0.3639, Adjusted R-squared:  0. 3574  
## F-statistic: 56.07 on 1 and 98 DF,  p-value: 3.085e-
11 
plot(CompRes,Satisfaction,col="red",abline(lm(Satisfact ion~CompRes),col="black")) 
MODEL5=lm(Satisfaction~Advertising) summary(MODEL5) 
##  
## Call: 
## lm(formula = Satisfaction ~ Advertising) 
##  
## Residuals: 
##      Min       1Q   Median       3Q      Max  ## -2.34033 -0.92755  0.05577  0.79773  2.53412  
##  
## Coefficients: 
##             Estimate Std. Error t value Pr(>|t|)     
## (Intercept)   5.6259     0.4237  13.279  < 2e-16 *** ## Advertising   0.3222     0.1018   3.167  0.00206 **  
## --- 

## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 
0.1 ' ' 1 
##  
## Residual standard error: 1.141 on 98 degrees of free dom 
## Multiple R-squared:  0.09282,    Adjusted R-squared:  
0.08357  
## F-statistic: 10.03 on 1 and 98 DF,  p-value: 0.00205
6 
plot(Advertising,Satisfaction,col="black",abline(lm(Sat isfaction~Advertising),col="red")) MODEL6=lm(Satisfaction~ProdLine) summary(MODEL6) 
##  
## Call: 
## lm(formula = Satisfaction ~ ProdLine) 
##  
## Residuals: 
##     Min      1Q  Median      3Q     Max  ## -2.3634 -0.7795  0.1097  0.7604  1.7373  
##  
## Coefficients: 
##             Estimate Std. Error t value Pr(>|t|)     
## (Intercept)  4.02203    0.45471   8.845 3.87e-14 *** 
## ProdLine     0.49887    0.07641   6.529 2.95e-09 *** 
## --- 
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 
0.1 ' ' 1 
##  

## Residual standard error: 1 on 98 degrees of freedom 
## Multiple R-squared:  0.3031, Adjusted R-squared:  0. 296  
## F-statistic: 42.62 on 1 and 98 DF,  p-value: 2.953e-
09 
plot(ProdLine,Satisfaction,col="blue",abline(lm(Satisfa ction~ProdLine),col="red")) 
MODEL7=lm(Satisfaction~SalesFImage) summary(MODEL7) 
##  
## Call: 
## lm(formula = Satisfaction ~ SalesFImage) 
##  
## Residuals: 
##     Min      1Q  Median      3Q     Max  ## -2.2164 -0.5884  0.1838  0.6922  2.0728  
##  
## Coefficients: 
##             Estimate Std. Error t value Pr(>|t|)     ## (Intercept)  4.06983    0.50874   8.000 2.54e-12 *** ## SalesFImage  0.55596    0.09722   5.719 1.16e-07 *** 
## --- 
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 
0.1 ' ' 1 
##  
## Residual standard error: 1.037 on 98 degrees of free dom 
## Multiple R-squared:  0.2502, Adjusted R-squared:  0.
2426  

## F-statistic:  32.7 on 1 and 98 DF,  p-value: 1.164e-
07 
plot(SalesFImage,Satisfaction,col="blue",abline(lm(Sati sfaction~SalesFImage),col="red")) MODEL8=lm(Satisfaction~ComPricing) summary(MODEL8) 
##  
## Call: 
## lm(formula = Satisfaction ~ ComPricing) 
##  
## Residuals: 
##     Min      1Q  Median      3Q     Max  
## -1.9728 -0.9915 -0.1156  0.9111  2.5845  
##  
## Coefficients: 
##             Estimate Std. Error t value Pr(>|t|)     
## (Intercept)  8.03856    0.54427  14.769   <2e-16 *** 
## ComPricing  -0.16068    0.07621  -2.108   0.0376 *   ## --- 
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 
0.1 ' ' 1 
##  
## Residual standard error: 1.172 on 98 degrees of free
dom 
## Multiple R-squared:  0.04339,    Adjusted R-squared:  
0.03363  
## F-statistic: 4.445 on 1 and 98 DF,  p-value: 0.03756 
plot(ComPricing,Satisfaction,col="blue",abline(lm(Satis faction~ComPricing),col="red")) 
 

MODEL9=lm(Satisfaction~WartyClaim) summary(MODEL9) 
##  
## Call: 
## lm(formula = Satisfaction ~ WartyClaim) 
##  
## Residuals: 
##      Min       1Q   Median       3Q      Max  
## -2.36504 -0.90202  0.03019  0.90763  2.88985  
##  
## Coefficients: 
##             Estimate Std. Error t value Pr(>|t|)     
## (Intercept)   5.3581     0.8813   6.079 2.32e-08 *** ## WartyClaim    0.2581     0.1445   1.786   0.0772 .   ## --- 
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 
0.1 ' ' 1 
##  
## Residual standard error: 1.179 on 98 degrees of free dom 
## Multiple R-squared:  0.03152,    Adjusted R-squared:  
0.02164  
## F-statistic:  3.19 on 1 and 98 DF,  p-value: 0.0772 
plot(WartyClaim,Satisfaction,col="blue",abline(lm(Satis faction~WartyClaim),col="red")) MODEL10=lm(Satisfaction~OrdBilling) summary(MODEL10) 
##  
## Call: 

## lm(formula = Satisfaction ~ OrdBilling) 
##  
## Residuals: 
##     Min      1Q  Median      3Q     Max  ## -2.4005 -0.7071 -0.0344  0.7340  2.9673  
##  
## Coefficients: 
##             Estimate Std. Error t value Pr(>|t|)     
## (Intercept)   4.0541     0.4840   8.377 3.96e-13 *** ## OrdBilling    0.6695     0.1106   6.054 2.60e-08 *** ## --- 
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 
0.1 ' ' 1 
##  
## Residual standard error: 1.022 on 98 degrees of free dom 
## Multiple R-squared:  0.2722, Adjusted R-squared:  0.
2648  
## F-statistic: 36.65 on 1 and 98 DF,  p-value: 2.602e-
08 
plot(OrdBilling,Satisfaction,col="blue",abline(lm(Satis faction~OrdBilling),col="red")) MODEL11=lm(Satisfaction~DelSpeed) summary(MODEL11) 
##  
## Call: 
## lm(formula = Satisfaction ~ DelSpeed) 
##  
## Residuals: 

##      Min       1Q   Median       3Q      Max  
## -2.22475 -0.54846  0.08796  0.54462  2.59432  
##  
## Coefficients: 
##             Estimate Std. Error t value Pr(>|t|)     ## (Intercept)   3.2791     0.5294   6.194 1.38e-08 *** ## DelSpeed      0.9364     0.1339   6.994 3.30e-10 *** 
## --- 
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 
0.1 ' ' 1 
##  
## Residual standard error: 0.9783 on 98 degrees of fre
edom 
## Multiple R-squared:  0.333,  Adjusted R-squared:  0. 3262  
## F-statistic: 48.92 on 1 and 98 DF,  p-value: 3.3e-10 
plot(DelSpeed,Satisfaction,col="blue",abline(lm(Satisfa ction~DelSpeed),col="red")) 
#################################################### # PRINCIPAL COMPONENT ANALYSIS AND FACTOR ANALYSIS # #################################################### 
MOD_DATA<-Mydata[,-12] 
View(MOD_DATA) 
 
dim(MOD_DATA) 
## [1] 100  11 
 
class(MOD_DATA) 
## [1] "data.frame" 

 
M<-eigen(cor((MOD_DATA))) 
EIGENVALUES<-M$values FACTORS=c(1:11) 
kappa(cor(MOD_DATA),exact = TRUE) 
## [1] 34.81738 
SCREE<-data.frame(FACTORS,EIGENVALUES) 
SCREE 
##    FACTORS EIGENVALUES ## 1        1  3.42697133 ## 2        2  2.55089671 
## 3        3  1.69097648 
## 4        4  1.08655606 ## 5        5  0.60942409 ## 6        6  0.55188378 ## 7        7  0.40151815 
## 8        8  0.24695154 
## 9        9  0.20355327 ## 10      10  0.13284158 ## 11      11  0.09842702 
 
#### SCREE PLOT USING BASE PLOT #### 
plot(SCREE,main="SCREE PLOT",col="Blue",ylim=c(0,4)) lines(SCREE,col="Red") abline(h=1,col="Green") 
 
#### SCREE PLOT USING GGPLOT2 #### 
 

ggplot(data=SCREE,mapping = aes(x=FACTORS,y=EIGENVALUES ))+geom_point()+geom_line()+scale_y_continuous(name = " EIGEN VALUES",limits = c(0,5))+theme(panel.background = element_blank())+theme(plot.background = element_blank( ))+theme(panel.grid.major.y = element_line(colour = "CO RAL"))+ggtitle("SCREE PLOT") 
 
max(eigen(cor(MOD_DATA))$values)/min(eigen(cor(MOD_DATA ))$values) 
## [1] 34.81738 
 
kappa(cor(MOD_DATA),exact = TRUE) 
## [1] 34.81738 
 
corr1<-cor(MOD_DATA) 
#### HYPOTHESIS TESTING #### 
# H0 = DIMENSIONAL REDUCTION IS POSSIBLE # H1 = DIMENSIONAL REDUCTION IS NOT POSSIBLE library(psych) 
KMO(corr1)####IF MSA VALUE IS > 0.5,FACTOR ANALYSIS CAN 
BE DONE FOR THE INDEPENDENT VARIABLES 
## Kaiser-Meyer-Olkin factor adequacy 
## Call: KMO(r = corr1) 
## Overall MSA =  0.65 
## MSA for each item =  
##    ProdQual        Ecom     TechSup     CompRes Adve rtising    ProdLine  
##        0.51        0.63        0.52        0.79        0.78        0.62  

## SalesFImage  ComPricing  WartyClaim  OrdBilling    D
elSpeed  
##        0.62        0.75        0.51        0.76        0.67 
#### BARLETT's TEST FOR SPHERICITY #### print(cortest.bartlett(corr1,nrow(MOD_DATA))) 
## $chisq 
## [1] 619.2726 
##  
## $p.value 
## [1] 1.79337e-96 
##  
## $df 
## [1] 55 
 
##### USING FACTANAL COMMAND ##### nfactors<-4 
fit<-factanal(MOD_DATA,nfactors,scores = c("regression"
),rotation = "none") print(fit) 
##  
## Call: 
## factanal(x = MOD_DATA, factors = nfactors, scores = 
c("regression"),     rotation = "none") 
##  
## Uniquenesses: 
##    ProdQual        Ecom     TechSup     CompRes Adve rtising    ProdLine  

##       0.682       0.360       0.228       0.178       
0.679       0.005  
## SalesFImage  ComPricing  WartyClaim  OrdBilling    D elSpeed  
##       0.017       0.636       0.163       0.347       
0.076  
##  
## Loadings: 
##             Factor1 Factor2 Factor3 Factor4 
## ProdQual     0.467  -0.148  -0.229  -0.159  ## Ecom                 0.791                  ## TechSup      0.198          -0.482   0.707  
## CompRes      0.589   0.316   0.535   0.300  
## Advertising          0.556   0.110          ## ProdLine     0.997                          ## SalesFImage          0.987                  
## ComPricing  -0.491   0.252   0.238          
## WartyClaim   0.279   0.124  -0.487   0.712  ## OrdBilling   0.452   0.275   0.493   0.360  ## DelSpeed     0.629   0.364   0.582   0.241  
##  
##                Factor1 Factor2 Factor3 Factor4 
## SS loadings      2.522   2.316   1.469   1.322 
## Proportion Var   0.229   0.211   0.134   0.120 ## Cumulative Var   0.229   0.440   0.573   0.694 
##  
## Test of the hypothesis that 4 factors are sufficient
. 

## The chi square statistic is 24.26 on 17 degrees of f
reedom. 
## The p-value is 0.113 
 
##### USING VARIMAX ROTATION ##### 
fit1<-factanal(MOD_DATA,nfactors,scores = c("regression
"),rotation = "varimax") print(fit1) 
##  
## Call: 
## factanal(x = MOD_DATA, factors = nfactors, scores = c("regression"),     rotation = "varimax") 
##  
## Uniquenesses: 
##    ProdQual        Ecom     TechSup     CompRes Adve rtising    ProdLine  
##       0.682       0.360       0.228       0.178       
0.679       0.005  
## SalesFImage  ComPricing  WartyClaim  OrdBilling    D elSpeed  
##       0.017       0.636       0.163       0.347       
0.076  
##  
## Loadings: 
##             Factor1 Factor2 Factor3 Factor4 
## ProdQual                             0.557  ## Ecom                 0.793                  ## TechSup                      0.872   0.102  ## CompRes      0.884   0.142           0.135  

## Advertising  0.190   0.521          -0.110  
## ProdLine     0.502           0.104   0.856  ## SalesFImage  0.119   0.974          -0.130  ## ComPricing           0.225  -0.216  -0.514  ## WartyClaim                   0.894   0.158  ## OrdBilling   0.794   0.101   0.105          ## DelSpeed     0.928   0.189           0.164  
##  
##                Factor1 Factor2 Factor3 Factor4 ## SS loadings      2.592   1.977   1.638   1.423 ## Proportion Var   0.236   0.180   0.149   0.129 
## Cumulative Var   0.236   0.415   0.564   0.694 
##  
## Test of the hypothesis that 4 factors are sufficient . 
## The chi square statistic is 24.26 on 17 degrees of f
reedom. 
## The p-value is 0.113 
 
#### PRINCIPAL AXIS METHOD #### 
fa_unrot<-fa(r=MOD_DATA,nfactors = 4,rotate = "none",fm
="pa") print(fa_unrot) 
## Factor Analysis using method =  pa 
## Call: fa(r = MOD_DATA, nfactors = 4, rotate = "none" , fm = "pa") 
## Standardized loadings (pattern matrix) based upon co rrelation matrix 
##               PA1   PA2   PA3   PA4   h2    u2 com 
 

## ProdQual     0.20 -0.41 -0.06  0.46 0.42 0.576 2.4 
## Ecom         0.29  0.66  0.27  0.22 0.64 0.362 2.0 ## TechSup      0.28 -0.38  0.74 -0.17 0.79 0.205 1.9 ## CompRes      0.86  0.01 -0.26 -0.18 0.84 0.157 1.3 ## Advertising  0.29  0.46  0.08  0.13 0.31 0.686 1.9 ## ProdLine     0.69 -0.45 -0.14  0.31 0.80 0.200 2.3 ## SalesFImage  0.39  0.80  0.35  0.25 0.98 0.021 2.1 
## ComPricing  -0.23  0.55 -0.04 -0.29 0.44 0.557 1.9 
## WartyClaim   0.38 -0.32  0.74 -0.15 0.81 0.186 2.0 ## OrdBilling   0.75  0.02 -0.18 -0.18 0.62 0.378 1.2 ## DelSpeed     0.90  0.10 -0.30 -0.20 0.94 0.058 1.4 
##  
##                        PA1  PA2  PA3  PA4 ## SS loadings           3.21 2.22 1.50 0.68 ## Proportion Var        0.29 0.20 0.14 0.06 ## Cumulative Var        0.29 0.49 0.63 0.69 
## Proportion Explained  0.42 0.29 0.20 0.09 
## Cumulative Proportion 0.42 0.71 0.91 1.00 
##  
## Mean item complexity =  1.9 
## Test of the hypothesis that 4 factors are sufficient
. 
##  
## The degrees of freedom for the null model are  55  a nd the objective function was  6.55 with Chi Square of  619.27 
## The degrees of freedom for the model are 17  and the objective function was  0.33  
##  

## The root mean square of the residuals (RMSR) is  0.0
2  
## The df corrected root mean square of the residuals i s  0.03  
##  
## The harmonic number of observations is  100 with the empirical chi square  3.19  with prob <  1  
## The total number of observations was  100  with Like
lihood Chi Square =  30.27  with prob <  0.024  
##  
## Tucker Lewis Index of factoring reliability =  0.921 
## RMSEA index =  0.088  and the 90 % confidence interv
als are  0.032 0.139 
## BIC =  -48.01 
## Fit based upon off diagonal values = 1 
## Measures of factor score adequacy              
##                                                    P
A1  PA2  PA3  PA4 
## Correlation of (regression) scores with factors   0. 98 0.97 0.95 0.88 
## Multiple R square of scores with factors          0. 96 0.95 0.91 0.78 
## Minimum correlation of possible factor scores     0.
92 0.90 0.82 0.56 
fa.diagram(fa_unrot) fa_unrot$loadings 
##  
## Loadings: 
##             PA1    PA2    PA3    PA4    ## ProdQual     0.201 -0.408         0.463 

## Ecom         0.290  0.659  0.270  0.216 
## TechSup      0.278 -0.381  0.738 -0.166 ## CompRes      0.862        -0.255 -0.184 ## Advertising  0.286  0.457         0.129 ## ProdLine     0.689 -0.453 -0.142  0.315 ## SalesFImage  0.395  0.801  0.346  0.251 ## ComPricing  -0.232  0.553        -0.286 
## WartyClaim   0.379 -0.324  0.735 -0.153 
## OrdBilling   0.747        -0.175 -0.181 ## DelSpeed     0.895        -0.303 -0.198 
##  
##                  PA1   PA2   PA3   PA4 
## SS loadings    3.215 2.223 1.499 0.678 ## Proportion Var 0.292 0.202 0.136 0.062 ## Cumulative Var 0.292 0.494 0.631 0.692 
fa_rot<-fa(r=MOD_DATA,nfactors = 4,rotate = "varimax",f m="pa") print(fa_rot) 
## Factor Analysis using method =  pa 
## Call: fa(r = MOD_DATA, nfactors = 4, rotate = "varim ax", fm = "pa") 
## Standardized loadings (pattern matrix) based upon co
rrelation matrix 
##               PA1   PA2   PA3   PA4   h2    u2 com 
## ProdQual     0.02 -0.07  0.02  0.65 0.42 0.576 1.0 
## Ecom         0.07  0.79  0.03 -0.11 0.64 0.362 1.1 ## TechSup      0.02 -0.03  0.88  0.12 0.79 0.205 1.0 ## CompRes      0.90  0.13  0.05  0.13 0.84 0.157 1.1 

## Advertising  0.17  0.53 -0.04 -0.06 0.31 0.686 1.2 
## ProdLine     0.53 -0.04  0.13  0.71 0.80 0.200 1.9 ## SalesFImage  0.12  0.97  0.06 -0.13 0.98 0.021 1.1 ## ComPricing  -0.08  0.21 -0.21 -0.59 0.44 0.557 1.6 ## WartyClaim   0.10  0.06  0.89  0.13 0.81 0.186 1.1 ## OrdBilling   0.77  0.13  0.09  0.09 0.62 0.378 1.1 ## DelSpeed     0.95  0.19  0.00  0.09 0.94 0.058 1.1 
##  
##                        PA1  PA2  PA3  PA4 ## SS loadings           2.63 1.97 1.64 1.37 ## Proportion Var        0.24 0.18 0.15 0.12 
## Cumulative Var        0.24 0.42 0.57 0.69 
## Proportion Explained  0.35 0.26 0.22 0.18 ## Cumulative Proportion 0.35 0.60 0.82 1.00 
##  
## Mean item complexity =  1.2 
## Test of the hypothesis that 4 factors are sufficient
. 
##  
## The degrees of freedom for the null model are  55  a nd the objective function was  6.55 with Chi Square of  619.27 
## The degrees of freedom for the model are 17  and the 
objective function was  0.33  
##  
## The root mean square of the residuals (RMSR) is  0.0 2  
## The df corrected root mean square of the residuals i s  0.03  
 

##  
## The harmonic number of observations is  100 with the empirical chi square  3.19  with prob <  1  
## The total number of observations was  100  with Like lihood Chi Square =  30.27  with prob <  0.024  
##  
## Tucker Lewis Index of factoring reliability =  0.921 
## RMSEA index =  0.088  and the 90 % confidence interv
als are  0.032 0.139 
## BIC =  -48.01 
## Fit based upon off diagonal values = 1 
## Measures of factor score adequacy              
##                                                    P
A1  PA2  PA3  PA4 
## Correlation of (regression) scores with factors   0.
98 0.99 0.94 0.88 
## Multiple R square of scores with factors          0.
96 0.97 0.88 0.78 
## Minimum correlation of possible factor scores     0.
93 0.94 0.77 0.55 
fa.diagram(fa_rot) fa_rot$loadings 
##  
## Loadings: 
##             PA1    PA2    PA3    PA4    
## ProdQual                          0.647 ## Ecom                0.787        -0.113 ## TechSup                    0.883  0.116 ## CompRes      0.898  0.130         0.132 

## Advertising  0.166  0.530               
## ProdLine     0.525         0.127  0.712 ## SalesFImage  0.115  0.971        -0.135 ## ComPricing          0.213 -0.209 -0.590 ## WartyClaim   0.103         0.885  0.128 ## OrdBilling   0.768  0.127               ## DelSpeed     0.949  0.185               
##  
##                  PA1   PA2   PA3   PA4 ## SS loadings    2.635 1.967 1.641 1.371 ## Proportion Var 0.240 0.179 0.149 0.125 
## Cumulative Var 0.240 0.418 0.568 0.692 
fa_rot$scores 
##                PA1         PA2          PA3         PA4 
##   [1,] -0.13388710  0.91751661 -1.719604873  0.09135
411 
##   [2,]  1.62976040 -2.00900531 -0.596361722  0.65808 192 
##   [3,]  0.36376581  0.83617362  0.002979966  1.37548 765 
##   [4,] -1.22252302 -0.54913358  1.245473305 -0.64421 384 
##   [5,] -0.48542093 -0.42762231 -0.026980304  0.47360 747 
##   [6,] -0.59509240 -1.30353334 -1.183019401 -0.95913 571 
##   [7,] -2.52885363  0.38836877 -0.603275803 -1.29659 025 

##   [8,] -0.11315168 -0.13097631 -0.699238481 -1.36606
005 
##   [9,]  0.95751096  0.34755882 -0.142256076 -0.93477 420 
##  [10,]  0.58135807  0.43427719 -0.481549064 -0.66519 579 
##  [11,] -0.04744554 -0.34677999 -0.477931226  0.62086 386 
##  [12,] -1.22969845  1.22373499  0.307420873 -1.06601
488 
##  [13,]  0.70120038  1.40162126 -0.077278204  0.61198
552 
##  [14,]  0.18944710 -0.12001589  0.341391428  1.43748 733 
##  [15,]  1.59586476  0.51484865 -0.307216912 -0.62265 003 
##  [16,]  1.11215548 -1.25985548 -0.535588676  0.99091 689 
##  [17,]  0.90477581 -0.30392244  0.909413294 -1.04926 552 
##  [18,]  1.35863182  0.09820639  0.147598367 -0.63536 585 
##  [19,]  0.76821232  0.25113902 -0.444327163 -0.84712 501 
##  [20,]  0.61161128  1.74911250 -0.747772366 -0.37770
002 
##  [21,] -0.49662748 -0.40513549  1.413398115 -1.42620 085 
##  [22,] -0.24583333  2.83259042  0.458183224  2.15737 479 
##  [23,] -0.08593028 -0.20647990  0.954813784  1.29099 542 

##  [24,]  1.30419410 -0.65840510 -0.735880788  0.79535
237 
##  [25,]  0.02837015  0.11289267  0.352466288 -0.83695 432 
##  [26,]  0.37516895 -0.08949421  0.586591370 -1.33839 027 
##  [27,]  0.69040218 -1.27676215  0.980470048  0.97700 863 
##  [28,]  0.19330562 -1.09019060  0.410744110 -1.15018
948 
##  [29,]  0.74807174 -1.20388888 -0.169444094  1.11442
401 
##  [30,] -0.53645976 -0.31470400 -1.389463973 -0.62508 147 
##  [31,] -0.98478652 -0.32465411  2.054616799  0.57213 335 
##  [32,] -0.89540824 -1.30592549  1.145669622 -0.17981 750 
##  [33,] -0.61006900 -0.25385457  0.201747129 -0.51103 804 
##  [34,]  0.58139098 -0.57102185  0.160508882 -1.09267 691 
##  [35,] -1.08254233  1.61817367  0.121735970 -0.32924 397 
##  [36,] -1.51860194 -1.82264781  0.280394795  1.08323
735 
##  [37,] -0.54298600 -0.45867623  0.359185746  0.44557 509 
##  [38,]  1.35035690  0.28270522  0.116158567  0.69743 150 
##  [39,]  0.98244317 -0.26293227 -1.015054009 -0.98172 621 

##  [40,] -0.92383910  1.28059000 -1.167921851 -0.79331
056 
##  [41,]  0.09932064  0.09827305 -1.837451060 -0.63276 007 
##  [42,]  0.10869713 -0.04357923 -1.050920110  0.49209 145 
##  [43,]  0.44811474  1.20846081 -0.968824054  0.72015 394 
##  [44,]  0.70660340  2.17850627  1.233358555 -0.84868
686 
##  [45,]  1.38263370 -2.03732511 -0.954180731  0.77953
010 
##  [46,]  0.94936859  0.24232567  0.291172997 -0.56344 261 
##  [47,]  0.08771633 -0.60985289  0.976205747  0.45795 642 
##  [48,]  1.81030056  0.43834059  0.814478797 -1.17962 559 
##  [49,] -0.19224074  1.62888205 -0.733730711  1.33934 942 
##  [50,]  0.22531882  0.78430768 -0.957752366  0.92784 955 
##  [51,] -1.41135829 -0.16930613  0.091870699 -0.12631 052 
##  [52,]  1.75365232 -1.99153570 -1.147542434  0.70646
283 
##  [53,]  0.93209854 -0.51397965 -0.162358377  0.70759 613 
##  [54,] -0.94184081 -0.25288506  0.358406630  0.67268 912 
##  [55,]  0.72242047 -0.54924734 -0.909388995 -1.09941 166 

##  [56,] -0.63244099  0.41450294  0.914064214  0.81720
176 
##  [57,]  1.99193341  1.41977208 -0.077384854 -0.41266 731 
##  [58,]  0.08548402  0.30077469  0.319615332  0.74357 193 
##  [59,] -0.57061091 -0.34885418  0.365475898  0.45601 852 
##  [60,]  0.83067496 -1.65626897  0.641678078  0.23244
952 
##  [61,]  0.86635294 -1.24543164  1.546930711  0.94382
806 
##  [62,] -0.60680134  0.74525730 -0.146586558 -0.25509 333 
##  [63,] -1.00306329 -0.09710517 -1.081705300  0.42111 790 
##  [64,] -1.28310920 -1.59838664  0.403626668 -0.02388 625 
##  [65,] -1.39461685 -0.23395772  0.557826636 -0.26460 336 
##  [66,]  1.60952000  0.55610373 -0.999093684 -1.16665 258 
##  [67,]  1.07257520 -0.39772241  1.815878900 -1.14631 126 
##  [68,]  0.50884788 -0.29395183 -0.416389316 -0.75909
753 
##  [69,] -0.70601563 -0.44707894 -0.973611521 -0.83304 582 
##  [70,]  0.23899120  0.05018369 -1.219545924 -1.27828 253 
##  [71,]  0.48631145  1.74069413  0.854795068 -0.43411 595 

##  [72,] -1.37477720 -0.22770098 -1.265787815  1.00015
839 
##  [73,]  0.76539809  0.81612817 -1.748046205 -0.13852 761 
##  [74,] -0.64249465  1.66245576  1.253902625  1.20305 576 
##  [75,] -0.26909538  0.83656999 -0.210138132 -0.11412 392 
##  [76,] -0.12296694 -0.33391431  1.172210398  0.39677
906 
##  [77,]  0.10371878 -0.33487753  2.082854211 -1.18017
022 
##  [78,]  0.46301479 -0.33956677  1.214737442  0.10357 998 
##  [79,]  0.98918468  0.65624508  0.485336460  1.22733 204 
##  [80,] -1.74919939  0.82890385  0.003384046  0.09231 225 
##  [81,] -0.32137992 -0.20110631  0.470666208  0.60888 606 
##  [82,] -0.03471489 -0.38974940  0.412648407  0.57771 035 
##  [83,] -1.17790878 -0.90045717  0.198122454  0.40011 473 
##  [84,] -2.55956258 -0.25686675  1.554283149 -1.14101
556 
##  [85,]  0.73971919 -0.90696938  0.655846983  0.51129 012 
##  [86,] -0.50161741 -0.56408382 -1.009145207 -0.86106 988 
##  [87,] -1.33057806 -0.01745886 -2.201996451 -0.92403 144 

##  [88,]  0.70358137 -0.91236060  1.299031585  0.56968
494 
##  [89,]  0.10912530 -0.46505791  0.266161987  0.17120 142 
##  [90,]  1.02813129  2.57446865  1.640665437 -0.80151 398 
##  [91,] -1.04672561  0.44260491  1.394021786  0.75011 393 
##  [92,] -1.90712581 -0.46993540 -0.522475751 -1.31422
452 
##  [93,]  0.07279015 -0.02235421 -0.352359525  1.61044
504 
##  [94,]  1.08706436  0.61924340  0.089460676  1.16081 123 
##  [95,] -0.95457978  0.66256003 -0.981787816  1.06717 592 
##  [96,] -0.41931326  0.70755398 -0.077703201  0.52522 023 
##  [97,] -0.12315824 -0.25275815 -1.762967608 -0.63424 275 
##  [98,] -1.79270636 -1.59315365 -1.309147686  1.28219 570 
##  [99,] -0.33991434  1.89138931  0.122487640 -0.17511 674 
## [100,] -0.31758889 -0.42356050 -0.453981729 -1.03250
054 
##################################################### 
# NEW DATA FRAME WITH DEPENDENT VARIABLE AND FACTOR SCO RES # 
################################################### newdata=cbind(Mydata$Satisfaction,fa_rot$scores) newdata 

##                    PA1         PA2          PA3         
PA4 
##   [1,] 8.2 -0.13388710  0.91751661 -1.719604873  0.0 9135411 
##   [2,] 5.7  1.62976040 -2.00900531 -0.596361722  0.6 5808192 
##   [3,] 8.9  0.36376581  0.83617362  0.002979966  1.3 7548765 
##   [4,] 4.8 -1.22252302 -0.54913358  1.245473305 -0.6
4421384 
##   [5,] 7.1 -0.48542093 -0.42762231 -0.026980304  0.4
7360747 
##   [6,] 4.7 -0.59509240 -1.30353334 -1.183019401 -0.9 5913571 
##   [7,] 5.7 -2.52885363  0.38836877 -0.603275803 -1.2 9659025 
##   [8,] 6.3 -0.11315168 -0.13097631 -0.699238481 -1.3 6606005 
##   [9,] 7.0  0.95751096  0.34755882 -0.142256076 -0.9 3477420 
##  [10,] 5.5  0.58135807  0.43427719 -0.481549064 -0.6 6519579 
##  [11,] 7.4 -0.04744554 -0.34677999 -0.477931226  0.6 2086386 
##  [12,] 6.0 -1.22969845  1.22373499  0.307420873 -1.0
6601488 
##  [13,] 8.4  0.70120038  1.40162126 -0.077278204  0.6 1198552 
##  [14,] 7.6  0.18944710 -0.12001589  0.341391428  1.4 3748733 
##  [15,] 8.0  1.59586476  0.51484865 -0.307216912 -0.6 2265003 

##  [16,] 6.6  1.11215548 -1.25985548 -0.535588676  0.9
9091689 
##  [17,] 6.4  0.90477581 -0.30392244  0.909413294 -1.0 4926552 
##  [18,] 7.4  1.35863182  0.09820639  0.147598367 -0.6 3536585 
##  [19,] 6.8  0.76821232  0.25113902 -0.444327163 -0.8 4712501 
##  [20,] 7.6  0.61161128  1.74911250 -0.747772366 -0.3
7770002 
##  [21,] 5.4 -0.49662748 -0.40513549  1.413398115 -1.4
2620085 
##  [22,] 9.9 -0.24583333  2.83259042  0.458183224  2.1 5737479 
##  [23,] 7.0 -0.08593028 -0.20647990  0.954813784  1.2 9099542 
##  [24,] 8.6  1.30419410 -0.65840510 -0.735880788  0.7 9535237 
##  [25,] 4.8  0.02837015  0.11289267  0.352466288 -0.8 3695432 
##  [26,] 6.6  0.37516895 -0.08949421  0.586591370 -1.3 3839027 
##  [27,] 6.3  0.69040218 -1.27676215  0.980470048  0.9 7700863 
##  [28,] 5.4  0.19330562 -1.09019060  0.410744110 -1.1
5018948 
##  [29,] 6.3  0.74807174 -1.20388888 -0.169444094  1.1 1442401 
##  [30,] 5.4 -0.53645976 -0.31470400 -1.389463973 -0.6 2508147 
##  [31,] 6.1 -0.98478652 -0.32465411  2.054616799  0.5 7213335 

##  [32,] 6.4 -0.89540824 -1.30592549  1.145669622 -0.1
7981750 
##  [33,] 5.4 -0.61006900 -0.25385457  0.201747129 -0.5 1103804 
##  [34,] 7.3  0.58139098 -0.57102185  0.160508882 -1.0 9267691 
##  [35,] 6.3 -1.08254233  1.61817367  0.121735970 -0.3 2924397 
##  [36,] 5.4 -1.51860194 -1.82264781  0.280394795  1.0
8323735 
##  [37,] 7.1 -0.54298600 -0.45867623  0.359185746  0.4
4557509 
##  [38,] 8.7  1.35035690  0.28270522  0.116158567  0.6 9743150 
##  [39,] 7.6  0.98244317 -0.26293227 -1.015054009 -0.9 8172621 
##  [40,] 6.0 -0.92383910  1.28059000 -1.167921851 -0.7 9331056 
##  [41,] 7.0  0.09932064  0.09827305 -1.837451060 -0.6 3276007 
##  [42,] 7.6  0.10869713 -0.04357923 -1.050920110  0.4 9209145 
##  [43,] 8.9  0.44811474  1.20846081 -0.968824054  0.7 2015394 
##  [44,] 7.6  0.70660340  2.17850627  1.233358555 -0.8
4868686 
##  [45,] 5.5  1.38263370 -2.03732511 -0.954180731  0.7 7953010 
##  [46,] 7.4  0.94936859  0.24232567  0.291172997 -0.5 6344261 
##  [47,] 7.1  0.08771633 -0.60985289  0.976205747  0.4 5795642 

##  [48,] 7.6  1.81030056  0.43834059  0.814478797 -1.1
7962559 
##  [49,] 8.7 -0.19224074  1.62888205 -0.733730711  1.3 3934942 
##  [50,] 8.6  0.22531882  0.78430768 -0.957752366  0.9 2784955 
##  [51,] 5.4 -1.41135829 -0.16930613  0.091870699 -0.1 2631052 
##  [52,] 5.7  1.75365232 -1.99153570 -1.147542434  0.7
0646283 
##  [53,] 8.7  0.93209854 -0.51397965 -0.162358377  0.7
0759613 
##  [54,] 6.1 -0.94184081 -0.25288506  0.358406630  0.6 7268912 
##  [55,] 7.3  0.72242047 -0.54924734 -0.909388995 -1.0 9941166 
##  [56,] 7.7 -0.63244099  0.41450294  0.914064214  0.8 1720176 
##  [57,] 9.0  1.99193341  1.41977208 -0.077384854 -0.4 1266731 
##  [58,] 8.2  0.08548402  0.30077469  0.319615332  0.7 4357193 
##  [59,] 7.1 -0.57061091 -0.34885418  0.365475898  0.4 5601852 
##  [60,] 7.9  0.83067496 -1.65626897  0.641678078  0.2
3244952 
##  [61,] 6.6  0.86635294 -1.24543164  1.546930711  0.9 4382806 
##  [62,] 8.0 -0.60680134  0.74525730 -0.146586558 -0.2 5509333 
##  [63,] 6.3 -1.00306329 -0.09710517 -1.081705300  0.4 2111790 

##  [64,] 6.0 -1.28310920 -1.59838664  0.403626668 -0.0
2388625 
##  [65,] 5.4 -1.39461685 -0.23395772  0.557826636 -0.2 6460336 
##  [66,] 7.6  1.60952000  0.55610373 -0.999093684 -1.1 6665258 
##  [67,] 6.4  1.07257520 -0.39772241  1.815878900 -1.1 4631126 
##  [68,] 6.1  0.50884788 -0.29395183 -0.416389316 -0.7
5909753 
##  [69,] 5.2 -0.70601563 -0.44707894 -0.973611521 -0.8
3304582 
##  [70,] 6.6  0.23899120  0.05018369 -1.219545924 -1.2 7828253 
##  [71,] 7.6  0.48631145  1.74069413  0.854795068 -0.4 3411595 
##  [72,] 5.8 -1.37477720 -0.22770098 -1.265787815  1.0 0015839 
##  [73,] 7.9  0.76539809  0.81612817 -1.748046205 -0.1 3852761 
##  [74,] 8.6 -0.64249465  1.66245576  1.253902625  1.2 0305576 
##  [75,] 8.2 -0.26909538  0.83656999 -0.210138132 -0.1 1412392 
##  [76,] 7.1 -0.12296694 -0.33391431  1.172210398  0.3
9677906 
##  [77,] 6.4  0.10371878 -0.33487753  2.082854211 -1.1 8017022 
##  [78,] 7.6  0.46301479 -0.33956677  1.214737442  0.1 0357998 
##  [79,] 8.9  0.98918468  0.65624508  0.485336460  1.2 2733204 

##  [80,] 5.7 -1.74919939  0.82890385  0.003384046  0.0
9231225 
##  [81,] 7.1 -0.32137992 -0.20110631  0.470666208  0.6 0888606 
##  [82,] 7.4 -0.03471489 -0.38974940  0.412648407  0.5 7771035 
##  [83,] 6.6 -1.17790878 -0.90045717  0.198122454  0.4 0011473 
##  [84,] 5.0 -2.55956258 -0.25686675  1.554283149 -1.1
4101556 
##  [85,] 8.2  0.73971919 -0.90696938  0.655846983  0.5
1129012 
##  [86,] 5.2 -0.50161741 -0.56408382 -1.009145207 -0.8 6106988 
##  [87,] 5.2 -1.33057806 -0.01745886 -2.201996451 -0.9 2403144 
##  [88,] 8.2  0.70358137 -0.91236060  1.299031585  0.5 6968494 
##  [89,] 7.3  0.10912530 -0.46505791  0.266161987  0.1 7120142 
##  [90,] 8.2  1.02813129  2.57446865  1.640665437 -0.8 0151398 
##  [91,] 7.4 -1.04672561  0.44260491  1.394021786  0.7 5011393 
##  [92,] 4.8 -1.90712581 -0.46993540 -0.522475751 -1.3
1422452 
##  [93,] 7.6  0.07279015 -0.02235421 -0.352359525  1.6 1044504 
##  [94,] 8.9  1.08706436  0.61924340  0.089460676  1.1 6081123 
##  [95,] 7.7 -0.95457978  0.66256003 -0.981787816  1.0 6717592 

##  [96,] 7.3 -0.41931326  0.70755398 -0.077703201  0.5
2522023 
##  [97,] 6.3 -0.12315824 -0.25275815 -1.762967608 -0.6 3424275 
##  [98,] 5.4 -1.79270636 -1.59315365 -1.309147686  1.2 8219570 
##  [99,] 6.4 -0.33991434  1.89138931  0.122487640 -0.1 7511674 
## [100,] 6.4 -0.31758889 -0.42356050 -0.453981729 -1.0
3250054 
 
dim(newdata) 
## [1] 100   5 
view(newdata) 
 
class(newdata) 
## [1] "matrix" 
 
newdata=data.frame(newdata) attach(newdata) 
names(newdata)<-c("CUSTOMER_SATISFACTION","PRODUCT_PURC HASE","MARKETING","POST_PROCUREMENT","BRAND_POSITIONING
") 
names(newdata) 
## [1] "CUSTOMER_SATISFACTION" "PRODUCT_PURCHASE"      
"MARKETING"             
## [4] "POST_PROCUREMENT"      "BRAND_POSITIONING" 
head(newdata) 
##   CUSTOMER_SATISFACTION PRODUCT_PURCHASE  MARKETING 
POST_PROCUREMENT 

## 1                   8.2       -0.1338871  0.9175166
-1.719604873 
## 2                   5.7        1.6297604 -2.0090053 -0.596361722 
## 3                   8.9        0.3637658  0.8361736      
0.002979966 
## 4                   4.8       -1.2225230 -0.5491336 1.245473305 
## 5                   7.1       -0.4854209 -0.4276223
-0.026980304 
## 6                   4.7       -0.5950924 -1.3035333
-1.183019401 
##   BRAND_POSITIONING ## 1        0.09135411 ## 2        0.65808192 ## 3        1.37548765 ## 4       -0.64421384 ## 5        0.47360747 ## 6       -0.95913571 
mean(newdata$CUSTOMER_SATISFACTION) 
## [1] 6.918 
####################################################### # MULTIPLE LINEAR REGRESSION # 
####################################################### MRM=lm(CUSTOMER_SATISFACTION~.,data = newdata) summary(MRM) 
##  
## Call: 

## lm(formula = CUSTOMER_SATISFACTION ~ ., data = newda
ta) ##  
## Residuals: 
##     Min      1Q  Median      3Q     Max  ## -1.7125 -0.4708  0.1024  0.4158  1.3483  
##  
## Coefficients: 
##                   Estimate Std. Error t value Pr(>|t |)     
## (Intercept)        6.91800    0.06696 103.317  < 2e-
16 *** 
## PRODUCT_PURCHASE   0.57963    0.06857   8.453 3.32e-
13	*** 
## MARKETING          0.61978    0.06834   9.070 1.61e-
14	*** 
## POST_PROCUREMENT   0.05692    0.07173   0.794    0.4
29     
## BRAND_POSITIONING  0.61168    0.07656   7.990 3.16e-
12 *** 
## --- 
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 
0.1 ' ' 1 
##  
## Residual standard error: 0.6696 on 95 degrees of fre
edom 
## Multiple R-squared:  0.6971, Adjusted R-squared:  0. 6844  
## F-statistic: 54.66 on 4 and 95 DF,  p-value: < 2.2e-
16 
anova(MRM) 

## Analysis of Variance Table 
##  
## Response: CUSTOMER_SATISFACTION 
##                   Df Sum Sq Mean Sq F value    Pr(>F )     
## PRODUCT_PURCHASE   1 34.712  34.712 77.4219 6.084e-1 4 *** 
## MARKETING          1 34.037  34.037 75.9160 9.277e-1
4 *** 
## POST_PROCUREMENT   1  0.663   0.663  1.4784     0.22 7     
## BRAND_POSITIONING  1 28.622  28.622 63.8381 3.162e-1
2 *** 
## Residuals         95 42.593   0.448                      
## --- 
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
predict(MRM)->PREDICTED_SATISFACTION 
COMPARE_VALUES=data.frame(newdata$CUSTOMER_SATISFACTION ,PREDICTED_SATISFACTION) 
COMPARE_VALUES 
##     newdata.CUSTOMER_SATISFACTION PREDICTED_SATISFAC TION 
## 1                             8.2               7.36
7049 
## 2                             5.7               6.98 6102 
## 3                             8.9               8.48 8620 
## 4                             4.8               5.54 5893 

## 5                             7.1               6.65
9765 
## 6                             4.7               5.11 1139 
## 7                             5.7               4.86 5471 
## 8                             6.3               5.89 5844 
## 9                             7.0               7.10
8530 
## 10                            5.5               7.08
9830 
## 11                            7.4               7.02 8136 
## 12                            6.0               6.32 9119 
## 13                            8.4               8.56 3073 
## 14                            7.6               7.85 2140 
## 15                            8.0               7.76 3751 
## 16                            6.6               7.35 7439 
## 17                            6.4               6.66
4020 
## 18                            7.4               7.38 6129 
## 19                            6.8               6.97 5467 
## 20                            7.6               8.08 2976 

## 21                            5.4               5.58
7122 
## 22                            9.9               9.87 6795 
## 23                            7.0               7.58 4247 
## 24                            8.6               7.71 0493 
## 25                            4.8               6.51
2528 
## 26                            6.6               6.29
4716 
## 27                            6.3               7.18 0292 
## 28                            5.4               5.67 4200 
## 29                            6.3               7.27 7482 
## 30                            5.4               5.95 0564 
## 31                            6.1               6.61 2893 
## 32                            6.4               5.54 4834 
## 33                            5.4               6.10
5945 
## 34                            7.3               6.24 1851 
## 35                            6.3               7.09 8978 
## 36                            5.4               5.58 6690 

## 37                            7.1               6.61
1987 
## 38                            8.7               8.30 9137 
## 39                            7.6               6.66 6209 
## 40                            6.0               6.62 4468 
## 41                            7.0               6.54
4837 
## 42                            7.6               7.19
5175 
## 43                            8.9               8.31 2075 
## 44                            7.6               8.22 8844 
## 45                            5.5               6.87 9227 
## 46                            7.4               7.29 0397 
## 47                            7.1               6.92 6559 
## 48                            7.6               7.56 3785 
## 49                            8.7               8.59
3608 
## 50                            8.6               8.04 7728 
## 51                            5.4               5.92 2973 
## 52                            5.7               7.06 6959 

## 53                            8.7               7.56
3296 
## 54                            6.1               6.64 7221 
## 55                            7.3               6.27 2070 
## 56                            7.7               7.36 0217 
## 57                            9.0               8.69
5702 
## 58                            8.2               7.62
6984 
## 59                            7.1               6.67 0786 
## 60                            7.9               6.55 1670 
## 61                            6.6               7.31 3645 
## 62                            8.0               6.86 3797 
## 63                            6.3               6.47 2428 
## 64                            6.0               5.19 1990 
## 65                            5.4               5.83
4539 
## 66                            7.6               7.42 5096 
## 67                            6.4               6.69 5384 
## 68                            6.1               6.54 2730 

## 69                            5.2               5.66
6705 
## 70                            6.6               6.23 6309 
## 71                            7.6               8.06 1845 
## 72                            5.8               6.51 9740 
## 73                            7.9               7.68
3228 
## 74                            8.6               8.38
3210 
## 75                            8.2               7.19 8745 
## 76                            7.1               6.94 9199 
## 77                            6.4               6.16 7244 
## 78                            7.6               7.10 8424 
## 79                            8.9               8.67 6448 
## 80                            5.7               6.47 4512 
## 81                            7.1               7.00
6312 
## 82                            7.4               7.03 3182 
## 83                            6.6               5.93 3185 
## 84                            5.0               4.66 5743 

## 85                            8.2               7.13
4719 
## 86                            5.2               5.69 3498 
## 87                            5.2               5.44 5384 
## 88                            8.2               7.18 2762 
## 89                            7.3               6.81
2889 
## 90                            8.2               8.71
2660 
## 91                            7.4               7.12 3788 
## 92                            4.8               4.68 7695 
## 93                            7.6               7.91 1356 
## 94                            8.9               8.64 7025 
## 95                            7.7               7.37 2224 
## 96                            7.3               7.43 0326 
## 97                            6.3               6.20
1653 
## 98                            5.4               5.60 1265 
## 99                            6.4               7.79 3079 
## 100                           6.4               5.81 4001 
plot(newdata$CUSTOMER_SATISFACTION,col="red") lines(newdata$CUSTOMER_SATISFACTION,col="red") plot(PREDICTED_SATISFACTION,col="blue") lines(PREDICTED_SATISFACTION,col="blue") vif(MRM) 
##  PRODUCT_PURCHASE         MARKETING  POST_PROCUREMEN
T BRAND_POSITIONING  
##          1.001021          1.002683          1.00298
1          1.005848 
mean(newdata$CUSTOMER_SATISFACTION) 
## [1] 6.918 
 
     
 
 
 
