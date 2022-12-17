## Some code

#varsToUse is the array of variablesselectd in the UI
# data is the dataset name
creatextab<-function(factorsToUse, data)
{
 
        newform<-as.formula(paste("Freq ~", paste(factorsToUse, collapse="+"), sep=""))
       xtabs(formula= newform, drop.unused.levels = TRUE, data=data)
        
        
        
}

