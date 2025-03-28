library(RFishBC)
library(dplyr)
library(FSA)
library(readxl)
library(ggplot2)
library(readr)
setwd("C:/Users/f.masnadi/Desktop/Stock Assessment/SS3/SOLEA_SS3/dati/VBGP/Paper_bifasic/prova_RfishBC/Fatte 2007-2011")

# to digitize the image and measure directly in R.

listOto <- listFiles("tif",other="Sez")
#listOto <- c("ssSez_425.tif","ssSez_443.tif","ssSez_651.tif","ssSez_854.tif","ssSez_878.tif","ssSez_881.tif","ssSez_1039.tif","ssSez_1046.tif","ssSez_1106.tif","ssSez_1120.tif","ssSez_1335.tif","ssSez_1346.tif","ssSez_1349.tif","ssSez_1362.tif")  # lista delle foto 

ids <- getID(listOto) # va solo se c'è "_" prima del numero
ids <- listOto   # se no usare questo che prende direttamente il nome della foto

digitizeRadii(listOto,id=ids,reading="Lettore1",edgeIsAnnulus=F,scaleBar=TRUE,scaleBarLength=1,scaleBarUnits="mm",windowSize=11)  # seguire le istruzioni: prima set la ref scale, poi il transect, poi gli annuli
# f to confirm the action
# z to start over the processing of any image
# q to abort or skip processing an image and move on to the next one
# k will abort the current image and NOT move on to any other images.

#per richiamare le letture nella working directory
list1 <- listFiles("rds")
oto_prova <- combineData(list1)
oto_prova$id <- as.character(parse_number(oto_prova$id))


######################################à
#Visualize One Set of Annuli
# You can review the selected annuli on a structure with showDigitizedImage(), which requires only  the name of an R data file created from digitizeRadii()
fns <- listFiles("rds",other="ssSez")
for (i in 1:length(fns)) {
showDigitizedImage(fns[i])
  dir.new <- paste0(getwd(),"/foto con puntini")
  dir.create(path=dir.new, showWarnings = T, recursive = T)
  dev.copy(png, file=paste0(dir.new, "/puntini_",fns[i],".png"))
   dev.off()
}


# esempio per vedere una lettura gia fatta e salvata 
#showDigitizedImage("Oto_1_Fortu.rds",pch.show="+",col.show="red",col.connect="white",col.ann="red",cex.ann=1)

#> [1] "Scale_1_DHO.rds" "Scale_1_ODH.rds" "Scale_1_OHD.rds"
###############################
# Data for Back-Calculation
# merge con db con le lunghezze alla cattura
info_oto <- read_excel("info_oto.xlsx")%>%mutate(id=as.character(id))
fishdat <- info_oto %>%
  inner_join(oto_prova,by="id")

BCprova <- backCalc(fishdat,TL,BCM = 1,inFormat="long",outFormat="long",digits=1);BCprova# using  the Dahl-Lea model.
write.csv(BCprova,file="BCprova.csv",quote=FALSE,row.names=FALSE)

#plot by id
ggplot(BCprova, aes(ann, bclen, colour=id)) + geom_line() + geom_point() +facet_wrap(~  id)

# calculate mean back-calculated length-at-age 
tmp <- BCprova %>%
  group_by(ann) %>%
  summarize(n=validn(bclen),
            mn=round(mean(bclen),0),
            sd=round(sd(bclen),1)) %>%
  as.data.frame()
tmp$up <- tmp$mn + tmp$sd
tmp$dw <- tmp$mn - tmp$sd
# plot mean back-calculated TL by age and SD
ggplot(tmp, aes(ann, mn)) + geom_line() + geom_line(aes(ann, up),linetype = 2) + geom_line(aes(ann, dw), linetype = 2) + geom_point()