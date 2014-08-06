DataProd_Shiny
===================

This app serves to demonstrate the Nadaraya-Watson (NW) kernel regression estimator 
and its tuning parameters using the cars dataset.

The NW estimators are plotted together with data points from the cars data. 
There are 2 tuning parameters for the NW estimator: bandwidth and kernel. 
The user can tweak them to observe how they affect the estimators.
The play button for the bandwidth setting allows the user to
observe how the estimator changes as bandwidth increases.
2 NW estimators are plotted to allow the user to compare
the estimators with different tuning parameters set.

The final input is used to predict the stopping distance of the car
with the given speed. The predictions for both estimators are printed
in the main panel.