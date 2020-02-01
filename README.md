# Modelling-Electricity-Demand-using-SARIMA-and-HOLT-WINTERS
Electricity demand prediction in today's world plays a very crucial role as it helps plan load allocations and also helps in making decision beforehand regarding the upgradation of the infrastructure for the generation and transmission of electricity. Moreover, forecasting electricty’s demand greatly helps cut down costs as well, because it helps with better planning and decision making. Generally energy consumption time series predictions are difficult to model because of the presence of complex linear and non-linear patterns. In this project we will be using the Autoregressive Integrated Moving Average (ARIMA), seasonal ARIMA (SARIMA) and Holt-Winters method to forecast the future demand of electricity in Toronto.

The dataset used for this project can be downloaded from the link:
http://reports.ieso.ca/public/DemandZonal/.
The data is the hourly consumption of electricity for a number of zones in Ontario, from the period 2003 to 2019. For this project we are only using the data for Toronto’s consumption.
Units: MW (Mega Watts) 
Dataset metrics: 6045 readings
Time granularity: Hour
Time range: 2003-05-01  –  2019-11-17 

Types of Models
For this project we will be using the ARIMA model mainly, and will be comparing its results with the Holt-Winters model.
