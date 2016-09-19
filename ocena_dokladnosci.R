# Image date acquisition settings  <<<<<<<------------------------------------
# Set up "12_06_2010_28_08_2009", "12_06_2010" or "28_08_2009"
Date = "12_06_2010"

#Choice of classification/regression variants
# Set up from 1 to 8
ClassificationVariant = 1
RegressionVariant = 1
  
#---------------------------------------------------------------------------------------------------------------------------------
#Setting paths
GlobalDir = "C:\\Landsat_C50_Cubist\\Pradnik\\"
OutputDir = paste("C:\\Landsat_C50_Cubist\\Pradnik\\", Date, "\\wyniki_testowe", 
                  sep = "")

#---------------------------------------------------------------------------------------------------------------------------------
#Readning data
PracticalClassificationResultDir = paste(GlobalDir, Date, "\\wyniki_testowe\\wyniki_testowe_klas_", toString(ClassificationVariant), ".txt",
                                      sep = "")
PracticalRegressionResultDir = paste(GlobalDir, Date, "\\wyniki_testowe\\wyniki_testowe_proc_", toString(ClassificationVariant), ".txt",
                                  sep = "")
TheorethicalClassificationResultDir = paste(GlobalDir,"\\landsat_klas.test", 
                                            sep = "")
TheorethicalRegressionResultDir = paste(GlobalDir, "\\landsat_proc.test", 
                                            sep = "")

PracticalClassificationResult = read.table(PracticalClassificationResultDir,
                                           header = FALSE,
                                           sep = ",",
                                           dec = ".",
                                           check.names = FALSE)
PracticalRegressionResut = read.table(PracticalRegressionResultDir,
                                      header = FALSE,
                                      sep = ",",
                                      dec = ".",
                                      check.names = FALSE)
TheorethicalClassificationResult = read.table(TheorethicalClassificationResultDir,
                                              header = TRUE,
                                              sep = ",",
                                              dec = ".",
                                              check.names = FALSE)
TheorethicalRegressionResult = read.table(TheorethicalRegressionResultDir,
                                          header = TRUE,
                                          sep = ",",
                                          dec = ".",
                                          check.names = FALSE)

#---------------------------------------------------------------------------------------------------------------------------------
#FUNCTIONS
#---------------------------------------------------------------------------------------------------------------------------------

#General classification accuracy assesment 
#PracticalValue - practical values of classification results (modeling values)
#TheoreticalValue - theoretical values of classification results (reference data)
GeneralClassificationError = function(PracticalValue, TheoreticalValue)
  {
  PracticalValue = as.data.frame(PracticalValue)
  TheoreticalValue = as.data.frame(TheoreticalValue)
  ClassificationError = (1 - (sum(PracticalValue==TheoreticalValue)/dim(TheoreticalValue)[1]))*100
  
  return(ClassificationError)
}
  
#General regression accuracy assesment
#PracticalValue - practical values of classification results (modeling values)
#TheoreticalValue - theoretical values of classification results (reference data)
GeneralRegressionError = function(PracticalValue, TheoreticalValue) 
{
  PracticalValue = as.vector(PracticalValue)
  TheoreticalValue = as.vector(TheoreticalValue)
  Difference = PracticalValue - TheoreticalValue
  
  RMSE = (sqrt(sum(Difference^2)/length(TheoreticalValue)))*100
  AverageError = (sum(Difference)/length(TheoreticalValue))*100
  AbsoluteError = (sum(abs(Difference))/length(TheoreticalValue))*100
  
  return(list(RMSE = RMSE, AverageError = AverageError, AbsoluteError = AbsoluteError))
}


#General two-stage (classification/regression) process accuracy assesment

GeneralTwoStageProcessError = function(PracticalClassificationResult, PracticalRegressionResult, TheoreticalClassificationResult, TheorethicalRegressionResult)
{
  #Combining classification and regression results into one dataset
  PracticalClassificationResult = as.data.frame(PracticalClassificationResult)
  PracticalRegressionResult = as.data.frame(PracticalRegressionResult)
  TheoreticalClassificationResult = as.data.frame(TheoreticalClassificationResult)
  TheorethicalRegressionResult = as.data.frame(TheorethicalRegressionResult)
  RowNumber = dim(PracticalClassificationResult)[1]
  for ( i in 1:RowNumber)
  {
    if (PracticalClassificationResult[i, 1]=="przepuszczalne")
    {
      PracticalRegressionResult[i,1] =0
    }
    
    if (TheoreticalClassificationResult[i, 1]=="przepuszczalne")
    {
      TheorethicalRegressionResult[i,1] =0
    }
  }
  Difference = PracticalRegressionResult - TheorethicalRegressionResult
  RMSE = (sqrt(sum(Difference^2)/RowNumber))*100
  AverageError = (sum(Difference)/RowNumber)*100
  AbsoluteError = (sum(abs(Difference))/RowNumber)*100
  
  return(list(RMSE = RMSE, AverageError = AverageError, AbsoluteError = AbsoluteError))
}




#---------------------------------------------------------------------------------------------------------------------------------
#ERRORS CALCULATION FOR TEST DATA SET
#---------------------------------------------------------------------------------------------------------------------------------

#Calculations for whole test data set
#Classification accuracy
XClassification = PracticalClassificationResult
YClassification = TheorethicalClassificationResult$nieprz_proc
AverageClassificationError = GeneralClassificationError(XClassification,YClassification)

#Regression accuracy
XRegression = PracticalRegressionResut
YRegression = TheorethicalRegressionResult$nieprz_proc
RegressionErrors = GeneralRegressionError(XRegression,YRegression)
RMSERrror = RegressionErrors$RMSE
AverageERrror = RegressionErrors$AverageError
AbsoluteError = RegressionErrors$AbsoluteError

#Two-stage process accuracy
MapErros = GeneralTwoStageProcessError(XClassification, XRegression, YClassification, YRegression )
MapRMSERrror = MapErros$RMSE
MapAverageERrror = MapErros$AverageError
MapAbsoluteError = MapErros$AbsoluteError




AverageClassificationError
RMSERrror
AverageERrror
AbsoulteError

MapRMSERrror 
MapAverageERrror
MapAbsoluteError


