#  you need pandas and numpy for this script to run. 
#  from command line:
#  pip install pandas
#  pip install numpy

import pandas as pd
import numpy as np

#  CHANGE SEPARATOR AND FILE PATHS ACCORDINGLY #

actuals = pd.read_csv("actuals.csv", sep=',')
preds = pd.read_csv("predictions.csv", sep=',')
data = pd.merge(actuals,preds, on='admin_L3_code')
R2 = 1-sum((data.actuals-data.predictions)**2)/sum((data.actuals-np.mean(data.actuals))**2)
print("R squared: ", R2)