## IDEA: train on Feb, test on March. use poisson loss to evaluate
## predictions.

## IDEA: use glmnet poisson regression to predict next 4 times, based
## on features such as: counts in the prev few bins, indicator for
## interval, indicator for hour, spline basis on hours past midnight, 
