library( lattice )
library( rms )
library( caret )
library( parallel )
library( doParallel )
library( neuralnet )
library( kernlab )

load( "nhanes.dat" )

nhanes <- predict( preProcess( nhanes, "range" ), nhanes )

dd <- datadist( nhanes )
options( datadist = "dd" )

nhanesformula <- as.formula( paste0( "BFP ~", paste( names( nhanes )[ 1:40 ], collapse = "+" ) ) )

fit.ols <- ols( nhanesformula, data = nhanes, x = TRUE, y = TRUE )
cairo_pdf( "LinRegrModel.pdf", height = 10 )
plot( summary( fit.ols ) )
dev.off()

xyplot( nhanes$BFP ~ predict( fit.ols ), abline = c( 0, 1 ), xlab = "Predicted BFP", ylab = "Actual BFP",
        xlim = c( 0, 1 ), ylim = c( 0, 1 ) )
sqrt( mean( ( nhanes$BFP-predict( fit.ols ) )^2 ) )

fit.nn <- neuralnet( nhanesformula, data = nhanes, hidden = c( 5, 3 ) )
xyplot( nhanes$BFP ~ compute( fit.nn, nhanes[ , 1:40 ] )$net.result, abline = c( 0, 1 ), xlab = "Predicted BFP", ylab = "Actual BFP",
        xlim = c( 0, 1 ), ylim = c( 0, 1 ) )
sqrt( mean( ( nhanes$BFP-compute( fit.nn, nhanes[ , 1:40 ] )$net.result )^2 ) )

fit.svm <- svm( nhanesformula, data = nhanes )
xyplot( nhanes$BFP ~ predict( fit.svm ), abline = c( 0, 1 ), xlab = "Predicted BFP", ylab = "Actual BFP",
        xlim = c( 0, 1 ), ylim = c( 0, 1 ) )
sqrt( mean( ( nhanes$BFP-predict( fit.svm ) )^2 ) )

cluster <- makeCluster( detectCores()-1 )
registerDoParallel( cluster )

set.seed(1)
fit.ols.tuned <- train( nhanesformula, data = nhanes, method = "lm", trControl = trainControl( number = 100 ) )
xyplot( nhanes$BFP ~ predict( fit.ols.tuned ), abline = c( 0, 1 ), xlab = "Predicted BFP", ylab = "Actual BFP",
        xlim = c( 0, 1 ), ylim = c( 0, 1 ) )
sqrt( mean( ( nhanes$BFP-predict( fit.ols.tuned ) )^2 ) )

set.seed(1)
fit.nn.tuned <- train( nhanesformula, data = nhanes, method = "neuralnet", trControl = trainControl( number = 100 ),
                       tuneGrid = expand.grid( layer1 = c( 2, 3, 5, 10 ), layer2 = c( 2, 3, 5, 10 ), layer3 = 0 ) )
cairo_pdf( "NNsearch.pdf" )
plot( fit.nn.tuned )
dev.off()
cairo_pdf( "NNbest.pdf" )
xyplot( nhanes$BFP ~ predict( fit.nn.tuned ), abline = c( 0, 1 ), xlab = "Predicted BFP", ylab = "Actual BFP",
        xlim = c( 0, 1 ), ylim = c( 0, 1 ) )
dev.off()
sqrt( mean( ( nhanes$BFP-predict( fit.nn.tuned ) )^2 ) )

set.seed(1)
fit.svm.tuned <- train( nhanesformula, data = nhanes, method = "svmRadial", trControl = trainControl( number = 100 ),
                        tuneGrid = expand.grid( C = c( seq( 0.1, 1, 0.1 ), 2:30 ), sigma = 10^(-4:1) ) )
cairo_pdf( "SVMsearch.pdf" )
plot( fit.svm.tuned )
dev.off()
cairo_pdf( "SVMbest.pdf" )
xyplot( nhanes$BFP ~ predict( fit.svm.tuned ), abline = c( 0, 1 ), xlab = "Predicted BFP", ylab = "Actual BFP",
        xlim = c( 0, 1 ), ylim = c( 0, 1 ) )
dev.off()
sqrt( mean( ( nhanes$BFP-predict( fit.svm.tuned ) )^2 ) )

stopCluster( cluster )
registerDoSEQ()

resamps <- resamples( list( OLS = fit.ols.tuned, NN = fit.nn.tuned, SVM = fit.svm.tuned ) )
bwplot( resamps, scales = list( relation = "free" ) )
cairo_pdf( "OverallRes.pdf" )
densityplot( resamps, scales = list( relation = "free" ), auto.key = list( columns = 3 ) )
dev.off()

apply( fit.ols.tuned$resample[ , 1:2 ], 2, function( x ) c( mean( x ), sd( x ) ) )
apply( fit.nn.tuned$resample[ , 1:2 ], 2, function( x ) c( mean( x ), sd( x ) ) )
apply( fit.svm.tuned$resample[ , 1:2 ], 2, function( x ) c( mean( x ), sd( x ) ) )

summary( resamps )

resampdiff <- diff( resamps )
summary( resampdiff )
cairo_pdf( "DiffRMSE.pdf" )
dotplot( resampdiff, metric = "RMSE" )
dev.off()
cairo_pdf( "DiffR2.pdf" )
dotplot( resampdiff, metric = "Rsquared" )
dev.off()