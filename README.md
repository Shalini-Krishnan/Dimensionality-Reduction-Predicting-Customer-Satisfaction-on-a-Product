# Dimensionality-Reduction-Predicting-Customer-Satisfaction-on-a-Product
Analysing and predicting Customer behaviour on a Hair product and remove correlation between variables using Factor Analysis/Principal Component Analysis.

PROJECT OBJECTIVE : 

The main objective of this report is to explore HAIR dataset in R and build optimum regression model to predict Customer Satisfaction. 

The list of steps involved in the analysis are following : 

•	Importing the dataset 
•	Exploratory data analysis 
•	Multicollinearity check between the variables 
•	Building simple and multiple linear regression models 
•	Performing Principal component analysis/Factor analysis 
•	Output interpretation and significance 

ASSUMPTIONS ( REGRESSION MODELS) :

•	Linearity - The relationship between the dependent and independent variables should be linear. 
•	Homoscedasticity - Constant variance of the errors should be maintained. 
•	Multivariate normality – Multiple Regression assumes that the residuals are normally distributed. 
•	Lack of Multicollinearity – It is assumed that there is little or no multicollinearity in the data. 

EXPLORATORY DATA ANALYSIS : 

The variables are of appropriate datatypes. The 5 point summary is calculated for all the variables (Refer source code). It gives the central value (mean) and dispersed value (standard deviation) for each of the variables.  

Histograms shows the frequency distribution of the data whereas boxplot shows the center, shape and spread of the data. The dataset is comprised of only integer and numeric datatypes. No categorical variables available in the given dataset. 

The variable names are ID, ProdQual, Ecom, TechSup, CompRes, Advertising, Prodline, SalesFImage, ComPricing, WartyClaim, OrdBilling, DelSpeed and Satisfaction. The variables with its  Abbreviated form is shown for easy reference. 

![image](https://user-images.githubusercontent.com/81927278/186153649-42371659-5f94-4cac-8bdb-b5fb81e1f69c.png)

The variable ID is a discrete unique number from 1 to 100.It does not have any explanatory power to explain Satisfaction (dependent variable) in the regression equation. So we can safely drop ID variable from the dataset.

UNIVARIATE ANALYSIS : All the variables are analysed through graphs to understand its nature, distribution and presence of outliers.

BIVARIATE ANALYSIS : 

Product Quality Vs Customer Satisfaction : The association between the variables are learnt.As the Product Quality increases, Customer Satisfaction seems to increase . But at a point where Product Quality is more, Customer Satisfaction is deeply less. It shows that there is an amalgamated effect within the variables. Customers expect something else along with the quality. Refer to the Business Report for the visualisations and relation between the variables.

Product Advertisement Vs Customer Satisfaction : The Customer Satisfaction increases with the good level of Advertisement and suddenly there is a steep decrease in the satisfactory level because there is no use of great advertisement without good quality of the product, brand name, warranty period and smooth delivery options. Here with the above graph we can strongly say that there is an evidence of correlation between the variables. 

Data visualisation shows that Customer Satisfaction highly depends on all of the variables which proves to be a strong evidence of Multicollinearity. 

MULTIVARIATE ANALYSIS : The same analysis is done with multiple independent variables with Satisfaction (dependent variable).All the graphs show that the variables are inter-related and are highly correlated with each other.

MISSING VALUE IDENTIFICATION : The presence of missing values (NA) can be ascertained.  There are no missing values in the given dataset. Hence it doesn’t require any of the missing value treatments. If in case of any NA values present in the data, then we can either ignore them or impute them. 

OUTLIER IDENTIFICATION : Outliers are the values which bounds out extremely from the overall pattern of the distribution. Here outliers are present in Ecom,SalesFimage, OrdBilling, and DelSpeed . However we retain them for our further analysis as not all extreme values are outliers.

MULTICOLLINEARITY CHECK : 

Multicollinearity refers to a situation in which two or more explanatory variables in a multiple regression model are highly linearly related.  From the  correlation matrix we can say that, 

•	Ecom and SalesFImage are highly correlated 
•	TechSup and WartyClaim are highly correlated 
•	CompRes and OrdBilling are highly correlated 
•	CompRes and DelSpeed are highly correlated 
•	OrdBilling and DelSpeed are highly correlated 
•	DelSpeed and ProdLine are moderately correlated 
•	CompRes and Satisfaction are moderately correlated. 

Since we have correlated regressors, the model built will not be fair enough for prediction of unknown data. Hence we reduce the dimensions using Factor Analysis and Principal Component Analysis.Since the dataset deals with only continuous variables, PCA would be the optimum solution.

PRINCIPAL COMPONENT ANALYSIS & FACTOR ANALYSIS :

To check the factorability of all the independent variables, we need to perform the Kaiser-Meyer-Olkin test (KMO) and Bartlett’s test of sphericity .Since both the tests prove to be significant, factor analysis is the appropriate technique to reduce the input variables into components/factors. One way to determine the number of factors in a correlation matrix is to examine the scree plot of the successive Eigen values. 

Eigen values represents the largest variance of the input variables summarized or reduced. 
Kaiser-Guttman normalization rule says that it would be appropriate to choose all the factors whose Eigen values are greater than 1.  
Hence from the graphs, we are good to go with 4 factors whose Eigen values are more than or equal to 1.

We use orthogonal rotation (varimax) where the rotated factors will remain uncorrelated whereas in oblique rotation the resulting factors will be correlated. 

We can effectively reduce dimensionality from 11 variables to 4 while losing only about 31% of the variance. 
Factor 1 – accounts for 29.20% of the variance 
Factor 2 – accounts for 20.20% of the variance 
Factor 3 – accounts for 13.60% of the variance 
Factor 4 – accounts for 6% of the variance 
Together, all the 4 factors explain 69% variance in performance.

![image](https://user-images.githubusercontent.com/81927278/186197350-12b7cf49-f56b-45d1-84dd-0757d3de19c4.png)
![image](https://user-images.githubusercontent.com/81927278/186197624-16ce6c79-5865-47cb-8901-7e5710a5c27a.png)

A new data frame is created with Customer Satisfaction as the dependent variable and the 4 different factors as the independent variables which are uncorrelated and balanced. 

MULTIPLE LINEAR REGRESSION (MLR) : 

Multiple Linear Regression Model is built on the new dataset. Multiple Linear Regression is a statistical technique that uses several explanatory variables to predict the outcome of a response variable. The mathematical equation is given by, 
Y = b0+b1X1+b2X2+b3X3……………………+bnXn+e 
Where Y = dependent variable 
X1, X2, X3……Xn = different independent variables b1, b2, b3.…..….bn = coefficients of x variables ; b0 = intercept ; e = random error component 
The regression equation for the regressand Customer Satisfaction and the 4 regressors – Product purchase, Marketing, Post procurement, Brand positioning is given by, 
Y = 6.918 + 0.579 X1 + 0.619 X2 + 0.056 X3 + 0.611 X4 
Where Y = CUSTOMER_SATISFACTION 
             X1 = PRODUCT_PURCHASE 
             X2 = MARKETING 
             X3 = POST_PROCUREMENT 
             X4 = BRAND_POSITIONING 
         6.918 is the value of the intercept. 
         
INTERPRETATION OF MLR: 
![image](https://user-images.githubusercontent.com/81927278/186199150-e9cba61c-835d-4f22-9115-eef9099adc10.png)

From the above summary we can say that, 
•	The coefficient of determination (R2) is 0.6971, which means 69.71% of variations in Customer Satisfaction can be explained by the predictors in the model. R2 tend to increase with the more number of predictors, hence adjusted R2 comes to the rescue. R2=Regression sum of squares/Total sum of squares

•	Adjusted R2 value is 0.6844, which says that 68.44% of variations in Customer Satisfaction can be efficiently explained by the predictors. 

•	The p value (2.2e-16) of the Fstatistic (54.66) with 4 and 95 degrees of freedom of the overall model is less than 0.05(level of significance), which means the model is significant, with atleast one predictor variable is significantly related to the outcome variable.

•	The residual standard error is 0.6696 on 95 degrees of freedom gives the error rate (unexplained variations). 

Error rate=Residual standard error/mean of the predictand Error rate for our model = 0.0967 

•	Intercept is highly significant  with the estimate 6.918 with t value 103.317 

•	PRODUCT_PURCHASE is highly significant with the estimate 0.0579 with t value 8.453 

•	MARKETING is highly significant with the estimate 0.6197 with t value 9.070 

•	POST_PROCUREMENT is insignificant with estimate 0.0569 ,t value – 0.794, p value – 0.429(>alpha=0.05) 

•	BRAND_POSITIONING is highly significant with estimate 0.6116 , t value – 7.990 

OUTPUT INTERPRETATION : 

•	As per the project objective, we tried to build an optimum regression model to predict Customer Satisfaction. 

•	To avoid multicollinearity between the independent variables we performed PCA/FA, in order to reduce the dimensionality of the regressors. 

•	Now the regression model was built with Satisfaction as dependent variable and 4 different factors as the independent variables. 

•	The output explains that 68.44 % variations of Satisfaction is effectively explained by the 4 factors. 

•	Out of 4 factors considered as predictors, only 3 of them proved to be significant .For better results we can drop the factor Post procurement from the data frame and then run the model. Hopefully this might result in a better efficient model to predict Customer Satisfaction.  

•	There are high chances of getting reliable outputs by including interactive models. It might make closer predictions and increase the model performance measures. 















