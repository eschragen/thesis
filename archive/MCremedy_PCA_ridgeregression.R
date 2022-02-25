
# #perform PCA####
# df_topics = df_new3 %>% select(topic1,topic2, topic3,topic4)
# pc = prcomp(df_topics,center = TRUE,scale. = TRUE)
# 
# # summary(pc)
# # pc$rotation
# 
# #PCA1 and PCA2 are opposite signs from what we computated earlier. Recall that by default, eigenvectors in R point in the negative direction. We can adjust this with a simple change.
# pc$x = - pc$x
# 
# # #visualize
# # plot_pca = biplot(pc, scale = 0)
# 
# # library(ggbiplot)
# # company_df = df_new5[, 2]
# # g = ggbiplot(pc, obs.scale = 1, var.scale = 1, 
# #              groups = company_df, ellipse = TRUE, 
# #              circle = TRUE)
# # g = g + scale_color_discrete(name = '')
# # g = g + theme(legend.direction = 'horizontal', 
# #               legend.position = 'top')
# # print(g)
# 
# #extract PCA loadings for new dataframe
# pca_loadings = as.data.frame(pc$x[,1:2])
# colnames(pca_loadings) = c("PCA1","PCA2")
# df_new4 = cbind(df_new3, pca_loadings) 
# 
# model7 = lm(vice_sum_100_log ~ 
#               industry_brown + revenues_norm + vice_virality_norm + fairness_foundation +
#               PCA1 + PCA2 + 
#               industry_brown*env_claim_made+
#               followers_count_norm + following_count_norm + 
#               tweet_count_norm, data = df_new4)


# #performe ridge regression####
# library(glmnet)
# library(tidyverse)
# df_new5 = df_new4 %>% drop_na()
# 
# dat = data.frame(y=df_new5$vice_sum_100_log, 
#                  x1=df_new5$industry_brown, x2=df_new5$revenues_norm, x3=df_new5$vice_virality_norm,
#                  x4=df_new5$fairness_foundation, x5=df_new5$topic1_norm, x6=df_new5$topic2_norm,
#                  x7=df_new5$topic3_norm, x8=df_new5$topic4_norm, x9=df_new5$env_claim_made,
#                  x10=df_new5$followers_count_norm, x11=df_new5$following_count_norm, x12=df_new5$tweet_count_norm)
# 
# f = as.formula(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x4*x9)
# 
# x = model.matrix(f,dat)
# y = as.matrix(dat$y, ncol = 1)
# 
# model_ridge = glmnet(x,y, alpha = 0)
# 
# #perform k-fold cross-validation to find optimal lambda value
# cv_model = cv.glmnet(x, y, alpha = 0)
# 
# #find optimal lambda value that minimizes test MSE
# best_lambda = cv_model$lambda.min
# best_lambda
# 
# #produce plot of test MSE by lambda value
# plot(cv_model)
# 
# #find coefficients of best model
# best_model = glmnet(x, y, alpha = 0, lambda = best_lambda)
# 
# coef(best_model)
# 
# #use fitted best model to make predictions
# y_predicted = predict(model_ridge, s = best_lambda, newx = x)
# 
# #find SST and SSE
# sst = sum((y - mean(y))^2)
# sse = sum((y_predicted - y)^2)
# 
# #find R-Squared
# rsq = 1 - sse/sst
# rsq
# 
# 
