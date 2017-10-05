from scipy.stats import norm
import sys
import os
import pandas as pd
import numpy as np
from scipy import optimize

# this is the regularization parameter
l = 2

df = pd.read_csv("data.csv")

cols = [c for c in df.columns if c.startswith('P')]
maxplayerID = df[cols].max().max()

def apm_constr(x):
    return np.mean(x)


def obj(x):
    val_cols = [c for c in df.columns if c.startswith('P')]
    home_pred = df[val_cols[0:5]].apply(lambda i: x[i-1]).sum(axis=1)
    away_pred = df[val_cols[5:10]].apply(lambda i: x[i-1]).sum(axis=1)
    pred_diff = home_pred - away_pred
    regularizer = l*(x**2).sum()
    err = ((df.Result - pred_diff)**2).sum() + regularizer 
    return err

x0 = np.zeros(shape=maxplayerID)

res = optimize.minimize(obj,x0,constraints=[{'type':'eq', 'fun':apm_constr}], method="SLSQP",
                        options={'maxiter':10000,'disp':True})

print("                Player   APM")
for i in range(len(x0)):
    print("{:>20s}    {:.4f}".format("P"+str(i+1), res.x[i]))
