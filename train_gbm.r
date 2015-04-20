    rm(list = ls(all = TRUE))
    library(gbm)

predict_gbm <- function(model, dat, best_iter)
{
    pred = rep(0, nrow(dat))

    step = 10000
    print(length(pred))

    s = seq(1, nrow(dat), step)
    for (i in s)
    {
        start_step = i 
        end_step = i + step
        pred[start_step:end_step] = predict.gbm(model, dat[start_step:end_step,], best_iter, type="response")
    }
    
    for (i in s[length(s)]:nrow(dat))
    {
        pred[i] = predict.gbm(model, dat[i,], best_iter, type="response")
    }

    print(s)
    print(length(pred))
    return(pred[1:nrow(dat)])
}
    
    se <- function (actual, predicted) (actual-predicted)^2
    mse <- function (actual, predicted) mean(se(actual, predicted))
    rmse <- function (actual, predicted) sqrt(mse(actual, predicted))    

    setwd("c:/project/musichackaton/")

    trainF = read.csv("train_features.csv", header=TRUE)
    #for (i in 1:ncol(trainF))
    #{
    #    trainF[,i][trainF[,i] == -1] = NA
    #}    

    trainT = read.csv("train_target.csv", header=TRUE)
    testF = read.csv("test_features.csv", header=TRUE)
    #for (i in 1:ncol(testF))
    #{
    #    testF[,i][testF[,i] == -1] = NA
    #}

    gbm_model <- gbm.fit(x=trainF, y=trainT[["rating"]],        # dataset
                         var.monotone=NULL,
                         distribution="gaussian",
                         n.trees=700,                # number of trees
                         shrinkage=0.01,             # shrinkage or learning rate,0.001 to 0.1 usually work
                         interaction.depth=8,         # 1: additive model, 2: two-way interactions, etc.
                         bag.fraction = 0.6,          # subsampling fraction, 0.5 is probably best
                         train.fraction = 0.8,        # fraction of data for training, first train.fraction*N used for training
                         n.minobsinnode = 10,         # minimum total weight needed in each node
                         keep.data=FALSE,              # keep a copy of the dataset with the object
                         verbose=TRUE)                # print out progress

    best_iter <- gbm.perf(gbm_model, method="test")
    print(best_iter)

    # fixme, save trainP for stacking
    trainP = predict_gbm(gbm_model, trainF, best_iter)
    score = rmse(trainT[["rating"]], trainP)
    print(score)

    testP = predict_gbm(gbm_model, testF, best_iter)
    write.csv(testP, file = "predict_gbm.csv", row.names = F)
