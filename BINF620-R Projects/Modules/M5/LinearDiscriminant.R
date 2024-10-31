library(MASS)
data(iris)
iris_lda = lda(Species ~ ., data = iris_trn)
iris_lda

# Here we see the estimated ^πkπ^k and ^μkμ^k for each class.
is.list(predict(iris_lda, iris_trn))

names(predict(iris_lda, iris_trn))

head(predict(iris_lda, iris_trn)$class, n = 10)
   
## Levels: setosa versicolor virginica
head(predict(iris_lda, iris_trn)$posterior, n = 10)

# As we should come to expect, the predict() function operates in a new way when called on an lda object. By default, it returns an entire list.
# Within that list class stores the classifications and posterior contains the estimated probability for each class.
iris_lda_trn_pred = predict(iris_lda, iris_trn)$class
iris_lda_tst_pred = predict(iris_lda, iris_tst)$class
#We store the predictions made on the train and test sets.
calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}

calc_class_err(predicted = iris_lda_trn_pred, actual = iris_trn$Species)

calc_class_err(predicted = iris_lda_tst_pred, actual = iris_tst$Species)


# As expected, LDA performs well on both the train and test data.
table(predicted = iris_lda_tst_pred, actual = iris_tst$Species)

# Looking at the test set, we see that we are perfectly predicting both setosa and versicolor. The only error is labeling a virginica as a versicolor.
iris_lda_flat = lda(Species ~ ., data = iris_trn, prior = c(1, 1, 1) / 3)
iris_lda_flat

# Instead of learning (estimating) the proportion of the three species from the data, we could instead specify them ourselves. Here we choose a uniform distributions over the possible species. We would call this a “flat” prior.
iris_lda_flat_trn_pred = predict(iris_lda_flat, iris_trn)$class
iris_lda_flat_tst_pred = predict(iris_lda_flat, iris_tst)$class
calc_class_err(predicted = iris_lda_flat_trn_pred, actual = iris_trn$Species)

calc_class_err(predicted = iris_lda_flat_tst_pred, actual = iris_tst$Species)

#This actually gives a better test accuracy!
#  11.2 Quadratic Discriminant Analysis
#QDA also assumes that the predictors are multivariate normal conditioned on the classes.
#X∣Y=k∼N(μk,Σk)X∣Y=k∼N(μk,Σk)
#fk(x)=1(2π)p/2|Σk|1/2exp[−12(x−μk)′Σ−1k(x−μk)]fk(x)=1(2π)p/2|Σk|1/2exp⁡[−12(x−μk)′Σk−1(x−μk)]
#Notice that now ΣkΣk does depend on kk, that is, we are allowing a different ΣkΣk for each class. We only use information from class kk to estimate ΣkΣk.

iris_qda = qda(Species ~ ., data = iris_trn)
iris_qda

#Here the output is similar to LDA, again giving the estimated ^πkπ^k and ^μkμ^k for each class. Like lda(), the qda() function is found in the MASS package.
#Consider trying to fit QDA again, but this time with a smaller training set. (Use the commented line above to obtain a smaller test set.) This will cause an error because there are not enough observations within each class to estimate the large number of parameters in the ΣkΣk matrices. This is less of a problem with LDA, since all observations, no matter the class, are being use to estimate the shared ΣΣ matrix.

iris_qda_trn_pred = predict(iris_qda, iris_trn)$class
iris_qda_tst_pred = predict(iris_qda, iris_tst)$class

#The predict() function operates the same as the predict() function for LDA.

calc_class_err(predicted = iris_qda_trn_pred, actual = iris_trn$Species)

calc_class_err(predicted = iris_qda_tst_pred, actual = iris_tst$Species)

table(predicted = iris_qda_tst_pred, actual = iris_tst$Species)

#Here we find that QDA is not performing as well as LDA. It is misclassifying versicolors. Since QDA is a more complex model than LDA (many more parameters) we would say that QDA is overfitting here.
#Also note that, QDA creates quadratic decision boundaries, while LDA creates linear decision boundaries. We could also add quadratic terms to LDA to allow it to create quadratic decision boundaries.