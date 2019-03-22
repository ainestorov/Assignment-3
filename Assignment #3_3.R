# Alex Nestorov
# Econ 613
# Assignment #3

# Exercise 1

## First I install the relevant package and pull the data out into two separate data frames.
install.packages("bayesm")
library(bayesm)
data(margarine)
choicePrice.df <- margarine[["choicePrice"]]
demos.df <- margarine[["demos"]]
## I provide some descriptive statistics of the data. I have pulled out the average and variance of
## the product's price, and the product's market share (i.e. frequency of each product being chosen). 
desc_stats_prod <- data.frame(cbind(lapply(choicePrice.df[,3:12], mean),
                                    lapply(choicePrice.df[,3:12], var), 
                                    paste(round(100*table(choicePrice.df$choice)/
                                                  nrow(choicePrice.df),digits = 2),"%")))
colnames(desc_stats_prod) <- c("average", "variance", "mkt_share")
print(desc_stats_prod)
## I can see that the average product prices vary quite a bit, but the variances tend to be low. 
## Additionally, the most popular product is "PPk_Stk" followed by "PBB_Stk" and "PHse_Stk" while the
## least popular is "PHse_Tub".

## I add a new column of choices where I then group the 3 products ("PPk", "PFl", "PHse") that have 
## both a tub and stick together so we can look at the market share of brands.
choicePrice.df$brands <- choicePrice.df$choice
choicePrice.df$brands[choicePrice.df$brands == 8] <- 1
choicePrice.df$brands[choicePrice.df$brands == 9] <- 3
choicePrice.df$brands[choicePrice.df$brands == 10] <- 4
brands_share <- data.frame(cbind(gsub("_.*","", colnames(choicePrice.df[,3:9])), 
                                      paste(round(100*table(choicePrice.df$brands)/
                                                    nrow(choicePrice.df),digits = 2),"%")))
colnames(brands_share) <- c("brand", "mkt_share")
print(brands_share)
## I see that the most popular product is "PPk" with almost 45% market share, while the least 
## popular is "PImp" with less than 2% market share.

## Finally, I add a new column of choices looking at just the "Tub" vs. "Stk" types of margarine. 
choicePrice.df$type <- choicePrice.df$choice
choicePrice.df$type[choicePrice.df$type < 7] <- 1
choicePrice.df$type[choicePrice.df$type > 6] <- 2
type_share <- data.frame(cbind(c("Stick","Tub"),paste(round(100*table(choicePrice.df$type)/
                                       nrow(choicePrice.df),digits = 2),"%")))
colnames(type_share) <- c("type", "mkt_share")
print(type_share)
## I see that the tub type of margarine is much more popular than the stick (82.55% vs. 17.45%).

## To map the observed demographics and choices I first need to merge the two data frames together.
## I then create individual tables that have the choices by various demographic attributes so that I
## can determine the market shares by these demographics.
data <- merge(choicePrice.df, demos.df, by.x = "hhid", all.x = TRUE)

income_mkt_share <- as.data.frame(cbind(table(data$Income, data$choice)))
income_mkt_share <- round(income_mkt_share/rowSums(income_mkt_share), digits = 2)
colnames(income_mkt_share) <- colnames(choicePrice.df[,3:12])
print(income_mkt_share)
## I first looked at the market shares by family income. These tend to be relatively consistent,
## though families at higher income levels (55 through 130) tend to pick "PPk_Stk", the most popular
## option, less regularly. 

fam_size_mkt_share <- as.data.frame(cbind(table(data$Fam_Size, data$choice)))
fam_size_mkt_share <- round(fam_size_mkt_share/rowSums(fam_size_mkt_share), digits = 2)
colnames(fam_size_mkt_share) <- colnames(choicePrice.df[,3:12])
print(fam_size_mkt_share)
## I also looked at market shares by family size. These also tended to be pretty consistent, though
## family sizes of 7 overwhelmingly picked "PHse_Stk" with 67% which was an anomaly. Additionally, 
## family sizes of 7 and 8 did not pick any "Tub" products.
## I also ran the above methodology for the binary variables, but found no noticeable differences for
## the choices by binary variable. I thus decided not to include these tables in the code here.

# Exercise 2

## I propose using a Conditional Logit which is appropriate for models where we are looking at
## choice behavior and the characteristics of the alternative choices. The explanatory variable 
## here is the product characteristic, namely price, and its effect on demand for specific 
## choices of margarine products. 

## Write the likelihood and optimize the model
## I first create an input vector of random values (0's here) that will contain the beta on price and 
## 9 alphas. I then create a matrix of the various price combinations (which will serve as my 
## independant variables) as well as a vector of the choice selections (my dependant variable).
alphas <- as.vector(rep(0, 10))
X_prices <- as.matrix(choicePrice.df[,3:12])
choice <- as.matrix(data$choice)

## I next create a function to calculate the maximum likelihood for my conditional logit. This takes
## as inputs the input vector and independant and dependant matrices created above.
clogit_mll <- function(alpha_vector, x, y) {
  ## I pull out the single beta coefficient that will be used to multiply the independant matrix per
  ## the formula for conditional logit.
  beta <- alpha_vector[1]
  ## I create a 4,470 x 9 matrix of alphas that will be added to the x*beta.
  alpha_matrix <- cbind(rep(0, nrow(y)), matrix(alpha_vector[2:10], nrow = nrow(y), 
                                                ncol = length(alpha_vector[2:10]), byrow=TRUE))
  ## I compute the addition as described above to get a 4,470 x 9 matrix which will be used in the log
  ## likelihood calculation.
  epsilon <- x*beta + alpha_matrix
  ## I then create an empty vector that is populated using my epsilon calculation above based on the 
  ## choice made by each individual that will form the first part of the log likelihood formula.
  empty_XB <- rep(0, nrow(y))
  for (i in 1:nrow(y)) {
    empty_XB[i] <- epsilon[i, y[i]]
  }
  ## Finally, I calculate the negative log-likelihood to get the maximum, as we did in the previous
  ## assignment.
  -sum(empty_XB - log(rowSums(exp(epsilon))))
}
## I then run the function I have created and get 10,292.56 for the max likelihood.
clogit_mll(alphas, X_prices, choice)

## Finally, I compute and save the conditional logit beta and alphas using the optimize function and 
## the maximum likelihood function that I just created.
clogit_coeffs <- nlm(clogit_mll, alphas, x=X_prices, y=choice)$estimate

## The coefficient on price (which I pull out of the above) can be interpreted as follows: given it is
## negative, the higher the price, the lower the demand for a particular choice. We cannot say 
## anything about the magnitude here.
print(clogit_coeffs[1])

# Exercise 3

## I propose using a Multinomial Logit which is appropriate for models where we are looking at
## the characteristics of individuals, in this case being the family income's effect on demand 
## for specific choices of margarine products. Here the regression coefficients are interpreted
## as reflecting the effects of characteristics on making a choice. I also include the other 
## demographic variables including dummies for family size, education status, job status, and
## retirement status.

## The process here is similar as above, but we will now have many more betas as we have a beta for 
## each variable and choice combination, in this case 7 variables and 9 choices (choice 1 is the base)
## meaning 63 betas. 9 of these will be intercepts, and the other 54 will be the betas. Thus, I first
## create an input vector of 63 random values (0's here). I then create a matrix of the independent 
## variables that I will be analyzing, with a column of 1's serving to calculate the intercepts.
betas <- as.vector(rep(0, 63))
X_demo <- as.matrix(cbind(rep(1, nrow(data)), data$Income, data$Fs3_4, data$Fs5., data$college, 
                          data$whtcollar, data$retired))
## Then, I create my multinomial logit function using the input vector above and my independent and
## dependent variable matrices as inputs.
mlogit_mll <- function(beta_vector, x, y) {
  ## I need to create a matrix that matches the dimensions of the independent variable matrix that I
  ## will be inputing, which will be a 4,470 x 7 matrix, so I create a matrix that is 7 x 10 with a 
  ## column of 0's to ensure that choice 1 is the base version.
  beta_matrix <- cbind(0, matrix(beta_vector, nrow=7, ncol=9, byrow = TRUE))
  ## I calculate x*beta.
  epsilon <- x %*% beta_matrix
  ## As before, I create an empty vector that will be populated with the epsilons above based on the
  ## choice made by each individual.
  empty_XB <- rep(0, nrow(y))
  for (i in 1:nrow(y)) {
      empty_XB[i] <- epsilon[i, y[i]]
  }
  ## Finally, as before, I calculate the negative log likelihood to optimize.
  -sum(empty_XB - log(rowSums(exp(epsilon))))
}
## I run the function I have created and again get 10,292.56 for my maximum likelihood.
mlogit_mll(betas, X_demo, choice)

## I save my multinomial logit coefficients and pull them into a matrix so it is easy to read for 
## which variable and which choice which betas apply.
mlogit_coeffs <- nlm(mlogit_mll, betas, x=X_demo, y=choice)$estimate
mlogit_coeffs_matrix <- matrix(mlogit_coeffs, nrow=7, ncol=9, byrow=TRUE)
rownames(mlogit_coeffs_matrix) <- c("intercept", "Income", "Fs3_4", "Fs5.", "College", "whtcollar",
                                    "retired")
colnames(mlogit_coeffs_matrix) <- colnames(data[,4:12])

## In terms of the family income, we can see that income has different effects on different choices.
## For PBB_Stk, PGen_Stk, PSS_Tub, PHse_Tub the betas are negative, meaning that a higher income 
## makes someone less likely to choose these margarines compared to the base case PPk_Stk. For the 
## others the betas are positive, meaning that a higher income makes someone more likely to choose 
## these margarines compared to the base case PPk_Stk. Again, one cannot interpret anything about the
## magnitude here.
print(mlogit_coeffs_matrix)

# Exercise 4
## I was unfortunately not able to translate the formulas provided in class to the calculation of 
## the marginal effects. We were shown that the marginal effects of the conditional logit is found by
## multiplying p(ij)*(delta(ijk) - p(ik))*beta and the marginal effects of the multinomial logit is 
## found by p(ij)*(beta(j)-sum(p(il)*beta(l))). Computing these will allow us to speak to more than 
## just the directional effect of the betas, as we did above. However, I was particular confused 
## about the meaning behind the "k" and "l" subscripts for the conditional and multinomial logit 
## calculations, respectively, and was not able to receive any clarity on this from class materials 
## or other sources. Generally, marginal effects can be interpreted as the probability effect. They
## will not change signs from the raw coefficients we receive in the outputs above, but will tell
## us much more specifically about what the model is saying and the impact of the variable on a 
## probability scale rather than odds scale as we have done above. 

# Exercise 5
## I turn to estimating the mixed logit maximum likelihood. As before, I start with an input vector,
## which has 64 rows (set at 0) to represent the 63 betas for the 7 variables by 9 choices of the
## demographic variables and a row for the price.
all_data <- as.vector(rep(0, 64))
## Then, I create the mixed logit function. This is a combination of the functions created for the 
## conditional logit and multinomial logit above. I add one input variable so we can use the two
## independent variable matrices for demographic variables and prices. I also add an input variable
## for the number of betas we will be creating given we will be looking at a restricted model below.
mxdlogit_mll <- function(vector, x, w, y, n) {
  beta <- vector[1]
  beta_matrix <- cbind(0, matrix(vector[2:n], nrow=7, ncol=((n-1)/7), byrow = TRUE))
  ## The major difference here will be the calculation of the epsilons, which is now basically a 
  ## summation of the two previous epsilons. Since we already have constants in the multinomial
  ## model, there is no need to add the alpha matrix as we had done before.
  epsilon <- x*beta + w %*% beta_matrix
  empty_XB <- rep(0, nrow(y))
  for (i in 1:nrow(y)) {
    empty_XB[i] <- epsilon[i, y[i]]
  }
  ## We calculate the negative log likelihood as before.
  -sum(empty_XB - log(rowSums(exp(epsilon))))
}
## I run the function I have created and again get 10,292.56 for my maximum likelihood.
mxdlogit_mll(all_data, X_prices, X_demo, choice, 64)

## I then run the optimization and save all of the coefficients as beta_f. If I wanted to view these
## separately then I could pull out the separate betas for the separate groups of variables: the 
## price (the first entry in beta_f) and the betas for the demographics (entries 2-64) in a well-
## organized matrix as before. For space and because the assignment does not ask for these, I have 
## not done so in this step.
beta_f <- nlm(mxdlogit_mll, all_data, X_prices, X_demo, y=choice, 64)$estimate

## I then amend the data to remove choice 10 as an alternative specification. I remove both the rows
## that have chosen choice 10 (n=33) and the 10th choice column (PHse_Tub). I create new independent
## and dependent variable matrices as well.
remove_data <- data[data$choice!= 10,]
remove_data$PHse_Tub <- NULL
X_prices_new <- as.matrix(remove_data[,3:11])
X_demo_new <- as.matrix(cbind(rep(1, nrow(remove_data)), remove_data$Income, remove_data$Fs3_4, 
                              remove_data$Fs5., remove_data$college, remove_data$whtcollar, 
                              remove_data$retired))
choice_new <- as.matrix(remove_data$choice)

## I create a new input vector that has 7 fewer rows because there are no longer 7 betas for the 
## demographic variables for choice 10. I run my mixed logit function using this smaller dataset.
all_data_new <- as.vector(rep(0, 57))
mxdlogit_mll(all_data_new, X_prices_new, X_demo_new, choice_new, 57)
## This time, I get 9,749.085 for the maximum likelihood.
## I then run the optimization and save all of the coefficients as beta_r. If I wanted to view these
## separately then I could pull out the separate betas for the separate groups of variables as I 
## outlined above. For space and because this assignment doesn't ask for this, I don't do so.
beta_r <- nlm(mxdlogit_mll, all_data_new, X_prices_new, X_demo_new, y=choice_new, 57)$estimate

## Now, I need to calculate the likelihood of the unrestricted model, which means I must remove the
## rows from the unrestricted model that correspond to the betas in choice 10 which I removed. 
beta_f_new <- beta_f[-c(10, 19, 28, 37, 46, 55, 64)]

## I calculate the MTT test statistic as defined in the assignment. It equals -1.75.
mtt <- -2*(mxdlogit_mll(beta_f_new, X_prices_new, X_demo_new, choice_new, 57)-
            mxdlogit_mll(beta_r, X_prices_new, X_demo_new, choice_new, 57))
print(mtt)

## Finally, I test the p-value for the MTT which is a chi-squared distribution with 57 degrees of 
## freedom, representing the 57 betas we found. We find that the p-value is extremely small at 
## 5.5x10e-33, which makes sense given that removing choice 10 only removes 33 rows. Thus, we 
## cannot assume that IIA is violated. Although this is contrary to what we typically find, we can 
## explain that the likelihoods are not very different because removing choice 10 only removes 33 
## data points. If we removed choice 1 or choice 2 the difference in likelihoods would be much 
## larger given the many more times that these choices are selected, and it is much more likely we 
## would see IIA to be violated, as is the typical case in these models.
pchisq(abs(mtt), 57)

## END.
