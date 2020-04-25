# San Francisco Bike Sharing Dock Allocation

Using Machine Learning and Optimization to reallocate the docks across 70 bike sharing station in SF Bay Area. 

First, predict the demand for the next month using Optimal Regression Trees (Bertsimas et al., 2017). Then, optimize the dock allocation to minimize the number of days with low stock of free docks. 

The data is cleaning in R. The predictive model as well as the optimization is done in Julia with Gurobi as the solver. 
