# San Francisco Bike Sharing Dock Allocation

Using Machine Learning and Optimization to reallocate the docks across 70 bike sharing station in SF Bay Area. 

**Data**
The dataset for this project is publicly available here: https://www.kaggle.com/benhamner/sf-bay-area-bike-share

**Method**
The analysis is split in two steps:
- Predict the demand for the next month using Optimal Regression Trees (Bertsimas et al., 2017). 
- Optimize the dock allocation to minimize the number of days with low stock of free docks. 

**Languages**
The data is cleaning in done in **R**. The predictive model as well as the optimization is done in **Julia** with Gurobi as the solver.
