head(Cleaned22)
str(Cleaned22)
#Train-Test Split
#h2o has a convenient function to perform train test splits. To get h2orunning, we’ll need to load and initialize our library:
library(h2o)
h2o.init()
#Using h2o.splitFrame , we can conveniently create a random training-test split of our data but before that, we need to convert our dataframe into a special object that h2o can recognize
Cleaned22.h2o <- as.h2o(Cleaned22)

Cleaned22_split <- h2o.splitFrame(data = Cleaned22.h2o, ratios = 0.8, seed = 1234)
training_data <- Cleaned22_split[[1]]
test_data <- Cleaned22_split[[2]]

#Training a Model
predictors <- c("B030","Cu030","K030","Mn030","N030","Na030","P030","Ptot030","altop","albottom",
                "bdr","ctottop","ctotbottom","catop","cabottom","claytotpsatop","claytotpsabottom",
                "dbodtop","dbodbottom","ececftop","ececfbottom","fetop","febottom","ktop",
                "kbottom","mgtop","mgbottom","ntotncstop","ntotncsbottom","octop","ocbottom","ptop",
                "pbottom","phh2otop","phh2obottom","stop","sbottom","sandtotpsatop","silttotpsatop",
                "silttotpsabottom","wpg2top","wpg2bottom","zntop","znbottom","SOMtop","SOMbottom","PWPtop","PWPbottom",
                "FCtop","FCbottom","SWStop","SWSbottom","DEM","slope","TPI","TRI","tr","di","nrd","tmean","tmin","tmax","Nrate","Prate")
response <- "yield"

Cleaned22_model <- h2o.glm(x = predictors,
                             y = response,
                             training_frame = training_data)

test_predict <- h2o.predict(object = Cleaned22_model, 
                            newdata = test_data)
predictions_x_real <- cbind(
  as.data.frame(test_data$yield),
  as.data.frame(test_predict)
)

Cleaned22_model_regularized <- h2o.glm(x = predictors,
                                         y = response,
                                         training_frame = training_data,
                                         alpha = 1)

#Evaluating our Models
Cleaned22_model <- h2o.glm(x = predictors,
                             y = response,
                             training_frame = training_data,
                             validation_frame = test_data)

h2o.rmse(Cleaned22_model, train=TRUE, valid=TRUE)

h2o.r2(Cleaned22_model, train=TRUE, valid=TRUE)

#It’s a bit expected that our linear regression isn’t performing that well — we haven’t performed any feature engineering and we are probably violating too many linear regression assumptions.
#But, if we can train simple linear regressions, we can probably train other types of models in h2o ,
#Let’s see that in the next section

#More Model Examples
#if we change the h2o function associated with the training process, we will fit other types of models. 
#Let’s train a random forest by calling h2o.randomForest :
Cleaned22_rf <- h2o.randomForest(x = predictors,
                                   y = response,
                                   ntrees = 25,
                                   max_depth = 5,
                                   training_frame = training_data,
                                   validation_frame = test_data)

#I’m setting two hyperparameters for my Random Forest on the function call:
#1-ntrees that sets the number of trees in the forest.
#2-maxdepth that sets the maximum deepness of each tree.

h2o.rmse(Cleaned22_rf, train=TRUE, valid=TRUE)
h2o.r2(Cleaned22_rf, train=TRUE, valid=TRUE)

#Notice that our code practically didn’t change. 
#The only thing that was modified was the model we fed into the first argument. 
#This makes these metric functions highly adaptable to new models, as long as they are trained inside the h2o framework

###let’s fit a Neural Network, using h2o.deeplearning:
nn_model <- h2o.deeplearning(x = predictors,
                             y = response,
                             hidden = c(7,7,4,5),
                             epochs = 2000,
                             train_samples_per_iteration = -1,
                             reproducible = TRUE,
                             activation = "Rectifier",
                             seed = 23123,
                             training_frame = training_data,
                             validation_frame = test_data)
h2o.r2(nn_model, train=TRUE, valid=TRUE)
# lets try another activation function:

nn_model <- h2o.deeplearning(x = predictors,
                             y = response,
                             hidden = c(7,7,4,5),
                             epochs = 2000,
                             train_samples_per_iteration = -1,
                             reproducible = TRUE,
                             activation = "Maxout",
                             seed = 23123,
                             training_frame = training_data,
                             validation_frame = test_data)
h2o.r2(nn_model, train=TRUE, valid=TRUE)

#Maxout activation function is much better in our case
#Another cool feature of h2o is that we can do hyperparameter tuning really smoothly — let’s see how, next.
#HyperParameter Tuning
#In the grid example, we’ll do a search on both parameters plus min_rows . We can do that by using the h2o.grid function:
# Grid Search 
rf_params <- list(ntrees = c(2, 5, 10, 15),
                  max_depth = c(3, 5, 9),
                  min_rows = c(5, 10, 100))
# Train and validate a grid of randomForests
rf_grid <- h2o.grid("randomForest", 
                    x = predictors, 
                    y = response,
                    grid_id = "rf_grid",
                    training_frame = training_data,
                    validation_frame = test_data,
                    seed = 1,
                    hyper_params = rf_params)
h2o.getGrid(grid_id = "rf_grid",
            sort_by = "r2",
            decreasing = TRUE)
#Our best model was the one that had a max_depth of 9, a min_rows of 5 and 15 ntrees — this model achieved an r2 of 0.7669.
#######################################################
#AutoML Features
#If you need a quick and raw way to look at the way different models perform on your dataset, h2o also has an interesting automl routine:
aml <- h2o.automl(x = predictors, 
                  y = response,
                  training_frame = training_data,
                  validation_frame = test_data,
                  max_models = 15,
                  seed = 1)
#We can access the top models of our routine by checking the aml@leaderboard
aml@leaderboard
#By the table above, we can see that a Stacked Ensemble model was the winner (at least, in terms of rmse). We can also get more information on the best model by calling:
h2o.get_best_model(aml)
#The result of h2o.get_best_model(aml) returns more information about the model that achieved the best score on our automlroutine. 
#By the snippet above, we know that our ensemble aggregates the result of:
#8GBM, 2 RF, 4DL,1 GLM
#Depending on your use case, the automl routine can be a quick, dirty way, to understand how ensembles and single models behave on your data, giving you hints on where to go next. 
#For example, helping you on the decision if more complex models are the way to go or if you will probably need to acquire more training data / features.
#Explainability
#Finally, let’s take a look into some of h2o’s explainability modules. In this example, we’ll use the random forest model we’ve trained above:
Cleaned22_rf <- h2o.randomForest(x = predictors,
                                   y = response,
                                   ntrees = 25,
                                   max_depth = 5,
                                   training_frame = training_data,
                                   validation_frame = test_data)
h2o.r2(Cleaned22_rf, train=TRUE, valid=TRUE)
#Just like most machine learning libraries, we can grab the variable importance plot directly with the h2o interface:
h2o.varimp_plot(Cleaned22_rf)


#By the importance plot, we notice that Nrate and Prate are the most important variables for our trained random forest. Calling varimp_plot immediately shows the importance plot for a specific model, without the need to configure anything else.

#Single variable importances are not the only available explainability models in h2o — we can also check shapvalues quickly:
h2o.shap_summary_plot(Cleaned22_rf, test_data)
#Voilà! By the shap_summary_plot, we understand the direction of the relationship between our features and target. For instance:

#higher Nrate explain more yield.
#lower CEC also explain more yield.


