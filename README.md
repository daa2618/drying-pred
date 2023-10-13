# Aim
Using different Machine Learning algorithms to predict characteristics during the vacuum drying of fruits and vegetables

# Scope of the project
Drying is one of the most important food processing methods to improve the post-harvest
shelf life which could greatly reduce the global food waste. Pulsed vacuum drying,
alternative food drying strategy, has been proven by researches to be highly energy efficient,
particularly when comparing to continuous vacuum drying of food products and is
particularly suitable for drying of heat sensitive fruits and vegetables. Predicting drying
characteristics using data driven methods are proven to be useful in developing sustainable
and energy efficient drying methods. In addition to this, it could also help in real-time
monitoring and control of drying processes which could result in better quality dried
products.Using machine learning (ML) for predicting drying characteristics has been a
bustling field of research in recent years as it could help developing more sustainable and
energy efficient drying methods that would be a boon for businesses operating in the food
industry. With that in mind, this work has been designed with an aim to use three different
machine learning algorithms, namely linear regression (LR), artificial neural networks
(ANN) and support vector regression (SVR) to predict the drying kinetics. The prediction
of important drying kinetics, which is in this case, the moisture ratio, is performed by
training the machine learning algorithms using the data obtained from experiments. Using
this historical data, the models has been optimised using various techniques to improve their
prediction accuracy.

# Optimising the Machine Learning Algorithms
Cross validation technique has been used to reduce the effect of overfitting of the ML models
with the experimental data. An important contribution of this research is the extensive work
done on the application of artificial neural networks for prediction. The ANN model’s
performance is greatly dependent on the hyperparameters such as the activation and training
functions. Therefore, five different training functions namely, Bayesian regularisation
(brnn), resilient backpropagation with backtracking (rprop+), resilient backpropagation
without backtracking (rprop-), smallest absolute gradient (sag) and smallest learning rate
(slr) were used to train the model. An interesting thing to note is that the Bayesian
regularised neural network resulted in poor predictive accuracy and a huge margin of error.
One hidden layer and linear activation function for the output layer are considered for all the
models based on previous studies.

In addition to this, six activation functions were analysed and the best was chosen based on
the minimum cross validated RMSE and maximum R2. These activation functions include,
Elliot symmetric sigmoid (elliotsig), radial basis (radbas), rectified linear unit (reLu),
logistic sigmoid (sigmoid), softplus, and hyperbolic tangent sigmoid (tansig). With that in
mind and assuming sigmoid activation function for the hidden layer, rprop- and radbas
have been identified as the best training and activation functions, respectively. The
appropriate number of hidden neurons to obtain the best predictive accuracy from the model
has been found with the rprop- training function and radbas activation function. With the
obtained parameters, the ANN model was trained and used for predicting the moisture ratio.

# Validation of results
To validate the claims from literatures that support vector regression (SVR) has better
generalisation ability than ANN, a SVR model was created. Radias basis kernel function
was chosen and to tune the hyperparameters such as cost and sigma, Bayesian optimisation
algorithm was used. With the tuned hyperparameters, the SVR model was trained and the
moisture ratio was predicted.

![Screenshot 2023-10-13 at 21 57 08](https://github.com/daa2618/drying-pred/assets/81092551/5d8ae19f-d078-4b27-b516-66ecc8e1dd77)
