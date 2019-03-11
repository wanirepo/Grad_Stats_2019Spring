# create variable containing responses to migraine question
everHadMigraine <- c('Yes','No','Yes','No','No','No','Yes','No','No','No') 
everHadMigraine

# create truth values from everHadMigraine variable
everHadMigraineTF <- everHadMigraine == 'Yes' 
everHadMigraineTF

# evaluate truth of a set of assertions
# 1 is equal to TRUE - should return TRUE
TRUE == 1
# 0 is equal to FALSE - should return TRUE
FALSE == 0
# 0 is equal to true - should return FALSE
TRUE == 0

# create integer values from truth values using as.integer()
everHadMigraineBinary <- as.integer(everHadMigraineTF) 
everHadMigraineBinary