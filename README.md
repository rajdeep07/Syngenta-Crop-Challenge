# Syngenta-Crop-Challenge
Corn is one of the world’s most important crops. Each year corn breeders create new products known as experimental hybrids by finding the best combination of genes that result in high yielding hybrids. These experimental hybrids are planted in a diverse set of locations where their performance is measured over several years. The best hybrids are advanced each year to be tested again. However, there is a limit to the number of testing locations, causing uncertainty in choosing the best hybrids. If corn breeders could accurately predict the performance of each individual hybrid in untested environments, they could make better decisions on which hybrids to move forward and provide to growers, increasing productivity to meet the world’s growing food demands.

Method of Operations - 
On the basis of provided Soil, Weather, Performance & Genetics data; and taking exogenous data sources for socio-economic parameters we have generated a master file for running machine learning algorithms. 

Approaches -

We have clustered data using k-means algorithm and then made different model using below mentioned techniques.

1. Linear Regression
2. Random Forest
3. Artificial Neural Network

Result - 
Decision on the best model selection is based upon the parameter called as RMSE. (Formula provided in the Report)

Files attached - 
1. LinearModel&RandomForestPredictionR <- Rcode for data manipulation and linear & random forest algorithms
2. ANN Prediction Code <- Python code for ANN 
3. Linear Model Prediction & Random Forest Prediction <- Predicted value for the year 2017
4. Report <- Compiled file to show modus operandi and tableau visualizaiton

Softwares used - 
RStudio, Python Jupyter Notebook & Tableau
