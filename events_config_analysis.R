pw <- {
  "s0.Much.Data"
}
#setwd("/Users/ashimdatta/Enterprise/Events activity info- Tathya")
getwd()
library("DBI")
library("RPostgreSQL")
library("sqldf")
library("ggplot2")
library("gridExtra")
library("plyr")
library("dplyr")
library(randomForest)


drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "analytics",
                 host = "10.223.192.6", port = 5432,
                 user = "etl", password = pw)



all_data<- dbGetQuery(con,"select * from ben.tathya_svd_variables a
join rterry.tathyaeventdata7 b
on lower(a.applicationid)=lower(b.applicationid)")


all_data_selected_columns<- dbGetQuery(con,"select a.*,b.name, totalagendasessions,importedbookmarks,usersactive,directmessaging,privatemsging,topicchannel,exhibitorreqinfo,exhibitormsg,socialnetworks,ratingson,
nativesessionnotes,sessionrecommendations,promotedposts,session_notifications,survey_on,polls_on,linkedinimportsetting,digest_email,
speaker_or_custom_items,sectioncount,promoted_posts_per_day,push_notifications_per_day,survey_count,poll_count,user_badges_on,map,
leaderboardv2,bookmarkssectionv2,photofeedv2,attendeeslistv2,qrcodev2,peoplerecommendations,agenda_only_organizer_bookmarks

from ben.tathya_svd_variables a
join rterry.tathyaeventdata7 b
on lower(a.applicationid)=lower(b.applicationid)")


event_config_action_data_svd_social<-all_data_selected_columns[,c(2,5:37)]


hist(event_config_action_data_svd_social$svd_social,xlab=" ",main="Social metric ", col="skyblue")

hist(log(event_config_action_data_svd_social$svd_social),xlab=" ",main="Log of Social metric ", col="skyblue")


set.seed(123)
split <- sample(seq_len(nrow(event_config_action_data_svd_social)), size = floor(0.95 * nrow(event_config_action_data_svd_social)))
trainData <- event_config_action_data_svd_social[split, ]
testData <- event_config_action_data_svd_social[-split, ]

## base model

best.guess <- mean(trainData$svd_social) 

# Evaluate RMSE and MAE on the testing data
RMSE.baseline <- sqrt(mean((best.guess-testData$svd_social)^2,na.rm=TRUE))
RMSE.baseline

MAE.baseline <- mean(abs(best.guess-testData$svd_social),na.rm=TRUE)
MAE.baseline




predictionModel <- lm(svd_social ~ totalagendasessions+importedbookmarks+usersactive+directmessaging+privatemsging+topicchannel+exhibitorreqinfo+exhibitormsg+socialnetworks+ratingson+
                      nativesessionnotes+sessionrecommendations+promotedposts+session_notifications+survey_on+polls_on+linkedinimportsetting+digest_email+
                      speaker_or_custom_items+sectioncount+promoted_posts_per_day+push_notifications_per_day+survey_count+poll_count+user_badges_on+map+
                      leaderboardv2+bookmarkssectionv2+photofeedv2+attendeeslistv2+qrcodev2+peoplerecommendations+agenda_only_organizer_bookmarks,
                      data = trainData)


summary(predictionModel)

test.pred.lin <- predict(predictionModel,testData)

RMSE.lin.reg <- sqrt(mean((test.pred.lin-testData$svd_social)^2,na.rm = TRUE))
RMSE.lin.reg

MAE.lin.reg <- mean(abs(test.pred.lin-testData$svd_social),na.rm = TRUE)
MAE.lin.reg



fit <- randomForest(svd_social ~ totalagendasessions+importedbookmarks+usersactive+directmessaging+privatemsging+topicchannel+exhibitorreqinfo+exhibitormsg+socialnetworks+ratingson+
                    nativesessionnotes+sessionrecommendations+promotedposts+session_notifications+survey_on+polls_on+linkedinimportsetting+digest_email+
                      speaker_or_custom_items+sectioncount+promoted_posts_per_day+push_notifications_per_day+survey_count+poll_count+user_badges_on+map+
                      leaderboardv2+bookmarkssectionv2+photofeedv2+attendeeslistv2+qrcodev2+peoplerecommendations+agenda_only_organizer_bookmarks,
                    data = trainData, 
                    importance=TRUE, na.action=na.roughfix,
                    ntree=500)

varImpPlot(fit)


Prediction <- predict(fit, testData)


RMSE.rf.reg <- sqrt(mean((Prediction-testData$svd_social)^2,na.rm = TRUE))
RMSE.rf.reg

MAE.rf.reg <- mean(abs(Prediction-testData$svd_social),na.rm = TRUE)
MAE.rf.reg


### content_metric

event_config_action_data_svd_content<-all_data_selected_columns[,c(3,5:37)]


hist(event_config_action_data_svd_content$svd_content,xlab=" ",main="Content Metric ", col="skyblue")

hist(log(event_config_action_data_svd_content$svd_content),xlab=" ",main="Content Metric ", col="skyblue")


set.seed(123)
split <- sample(seq_len(nrow(event_config_action_data_svd_content)), size = floor(0.95 * nrow(event_config_action_data_svd_content)))
trainData <- event_config_action_data_svd_content[split, ]
testData <- event_config_action_data_svd_content[-split, ]

## base model

best.guess <- mean(trainData$svd_content) 

# Evaluate RMSE and MAE on the testing data
RMSE.baseline <- sqrt(mean((best.guess-testData$svd_content)^2,na.rm=TRUE))
RMSE.baseline

MAE.baseline <- mean(abs(best.guess-testData$svd_content),na.rm=TRUE)
MAE.baseline




predictionModel <- lm(log(svd_content+5) ~ totalagendasessions+importedbookmarks+usersactive+directmessaging+privatemsging+topicchannel+exhibitorreqinfo+exhibitormsg+socialnetworks+ratingson+
                        nativesessionnotes+sessionrecommendations+promotedposts+session_notifications+survey_on+polls_on+linkedinimportsetting+digest_email+
                        speaker_or_custom_items+sectioncount+promoted_posts_per_day+push_notifications_per_day+survey_count+poll_count+user_badges_on+map+
                        leaderboardv2+bookmarkssectionv2+photofeedv2+attendeeslistv2+qrcodev2+peoplerecommendations+agenda_only_organizer_bookmarks,
                      data = trainData)


summary(predictionModel)

test.pred.lin <- exp(predict(predictionModel,testData))-5

RMSE.lin.reg <- sqrt(mean((test.pred.lin-testData$svd_content)^2,na.rm = TRUE))
RMSE.lin.reg

MAE.lin.reg <- mean(abs(test.pred.lin-testData$svd_content),na.rm = TRUE)
MAE.lin.reg



fit <- randomForest(log(svd_content+5) ~ totalagendasessions+importedbookmarks+usersactive+directmessaging+privatemsging+topicchannel+exhibitorreqinfo+exhibitormsg+socialnetworks+ratingson+
                      nativesessionnotes+sessionrecommendations+promotedposts+session_notifications+survey_on+polls_on+linkedinimportsetting+digest_email+
                      speaker_or_custom_items+sectioncount+promoted_posts_per_day+push_notifications_per_day+survey_count+poll_count+user_badges_on+map+
                      leaderboardv2+bookmarkssectionv2+photofeedv2+attendeeslistv2+qrcodev2+peoplerecommendations+agenda_only_organizer_bookmarks,
                    data = trainData, 
                    importance=TRUE, na.action=na.roughfix,
                    ntree=500)

varImpPlot(fit)


Prediction <- exp(predict(fit, testData))-5


RMSE.rf.reg <- sqrt(mean((Prediction-testData$svd_content)^2,na.rm = TRUE))
RMSE.rf.reg

MAE.rf.reg <- mean(abs(Prediction-testData$svd_content),na.rm = TRUE)
MAE.rf.reg
