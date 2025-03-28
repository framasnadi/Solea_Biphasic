#script per plottare le curve dei due modelli (3par vs 5par) contro i dati reali

# 5 par population pred
TL <- saemix.vb.fit0@results@ypred   #a vector giving the population predictions obtained with the MAP estimates
Biph_i <- as.data.frame(TL); Biph_i$Age <- data_sol$Age 
Biph_i$id <- data_sol$id
Biph_i$source <- "Biphasic_VB"
# 3 par population pred
TL <- saemix.vb.fit3@results@ypred    #a vector giving the population predictions obtained with the MAP estimates
norm_i <- as.data.frame(TL); norm_i$Age <- data_sol$Age  
norm_i$id <- data_sol$id
norm_i$source <- "Classic_VB"
# data original
TL <- data_sol$TL
orig_i <- as.data.frame(TL); orig_i$Age <- data_sol$Age  
orig_i$id <- data_sol$id
orig_i$source <- "Original_data"

db_fin <- rbind(orig_i,norm_i,Biph_i)
write.csv(db_fin,"db_fin_pop_allcor7plus.csv")



# 5 par indivdual pred
TL <- saemix.vb.fit0@results@ipred    #a vector giving the individual predictions obtained with the MAP estimates
Biph_i <- as.data.frame(TL); Biph_i$Age <- data_sol$Age   
Biph_i$id <- data_sol$id
Biph_i$source <- "Biphasic_VB"
# 3 par indivdual pred
TL <- saemix.vb.fit3@results@ipred    #a vector giving the individual predictions obtained with the MAP estimates
norm_i <- as.data.frame(TL); norm_i$Age <- data_sol$Age  
norm_i$id <- data_sol$id
norm_i$source <- "Classic_VB"
# data original
TL <- data_sol$TL
orig_i <- as.data.frame(TL); orig_i$Age <- data_sol$Age   
orig_i$id <- data_sol$id
orig_i$source <- "Original_data"

db_fin_ind <- rbind(orig_i,norm_i,Biph_i)
write.csv(db_fin_ind,"db_fin_ind_allcor7plus.csv")







