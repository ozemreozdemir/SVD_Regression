#################################################################################################
## OZKAN EMRE OZDEMIR                                                                           #
## HOMEWORK 6 : Perform SVD regression on Crime-community data (Lecture 6)                      #
## 05/13/16                                                                                     #
## Class:  Methods for Data Analysis                                                            #
#################################################################################################
## Clear objects from Memory :
rm(list=ls())
##Clear Console:
cat("\014")

## Get the libraries
library(logging)
library(png)
library(raster)
library(glmnet)
library(pls)
library(zoo)
library(MASS)

# Get the log file name that has a date-time in the name
get_log_filename = function(){
        log_file_name = format(Sys.time(), format="HW6_log_%Y_%m_%d_%H%M%S.log")
        return(log_file_name)
}

# Unit test to check that log file name doesn't exist
test_log_file_name_uniqueness = function(log_file_name){
        all_files = list.files()
        stopifnot(!log_file_name%in%all_files)
}


if (interactive()){
        # Get logger file name
        log_file_name = get_log_filename()
        basicConfig()
        addHandler(writeToFile, file=log_file_name, level='INFO')
    
        # Test for uniqueness
        test_log_file_name_uniqueness(log_file_name)
        
        # Setup working directory
        setwd('~/DataAnalysis/6_Regression_FeatureSelection')
        
        # SVD on features then regression on those features for crime prediction
        
        crime_data = read.table('communities.data', sep=",", header=FALSE, na.strings = c("NA","?"))
        crime_headers = read.table('crime_headers.txt', sep=",")
        names(crime_data) = crime_headers$V1
        
        #drop features that are missing a majority of observations:
        crime_data = crime_data[colSums(is.na(crime_data)) < 100]
        
        # Now remove missing rows
        crime_data = crime_data[complete.cases(crime_data),]
        
        # Disregard 'state' and 'communityname'
        crime_data$state= NULL
        crime_data$communityname = NULL
        
        
        # Consider 'ViolentCrimesPerPop' as the y-dependent variable
        log_data = log(crime_data$ViolentCrimesPerPop)
        log_data[which(!is.finite(log_data))]<-0
        
        # Full linear regression:
        ViolentCrimesPerPop_all_model= lm(log_data~ ., data = crime_data)
        summary(ViolentCrimesPerPop_all_model)
        #R-squared is not that high, 0.806
        
        ## Let's first create our data matrix :
        data_matrix = model.matrix(log_data ~. , data = crime_data)        
       
         ## Then, compute the principle components of the data :
        pc_data_matrix = prcomp(data_matrix)
        
        ## Plot the magnitude of the principal components:
        plot(pc_data_matrix$sdev,pch=16,
             main="The Magnitude of the Principal Components",
             xlab="Principal Component", ylab="Component Magnitude")
        grid()
        
        # Perform linear regression on components:
        pc_all_components = lm(log_data ~ pc_data_matrix$x[,1:10])
        summary(pc_all_components)
        
        # It looks like we will need more than 10  SVD features
        # How many SVD features do we need to get a good enough fit?

        for (i in 1:100){
                pc_all_components = lm(log_data ~ pc_data_matrix$x[,1:i])
                if (summary(pc_all_components)$r.squared >= 0.8)
                        break
        }
        
        
        # Compare AIC
        AIC(ViolentCrimesPerPop_all_model)
        AIC(pc_all_components)
        
        # How many components?  Let's see the AIC by # of components
        aic_by_num_pc = sapply(2:85, function(x){
                formula_rhs_temp = paste(paste0('pc_data_matrix$x[,',1:x,']'), collapse = ' + ')
                formula_temp = paste('log_data ~',formula_rhs_temp)
                pc_all_components_temp = lm(eval(parse(text=formula_temp)))
                return(AIC(pc_all_components_temp))
        })
        plot(aic_by_num_pc, type='l', lwd=2,
             main='AIC of P.C. Linear Reg with X components',
             xlab="# of components", ylab='AIC')
        # add a horizontal line of where the all variable AIC is at
        abline(h=AIC(ViolentCrimesPerPop_all_model), lwd=2, col='red')
        principle_comp = which.min(aic_by_num_pc) # 84 principal components!

        # Log Results
        loginfo(paste('# OZKAN EMRE OZDEMIR # HOMEWORK 6 # \n','How many SVD features do we need to get a good enough fit? \n',
                      'The full linear regression of Violent Crimes per Population has the Adjusted R-squared value of',
                      summary(ViolentCrimesPerPop_all_model)$adj.r.squared,
                      'which is relatively low',
                      '\n From "The Magnitude of the Principal Components" graph',
                      'it can be seen that the after the first 60th feature, we are not adding any predictive power\n',
                      'Results show that at least ',i,
                      ' number of SVD features are needed to get a good enough fit (R-square > 0.8)\n',
                      'which gives the Adjusted R-squared value of',summary(pc_all_components)$adj.r.squared,'\n',
                      'Based on the AIC test ', principle_comp, ' number of principle components are needed \n',
                      'This value indicates that if we cannot reformulate our linear regression model in a way to get rid of dependence on the independent variables, \n',
                      'with SVD we are guarenteed to have every feature independent if we have', principle_comp, ' number of principle components \n',
                      '# END #'))
        
}
#############################           End             #############################  
