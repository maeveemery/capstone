##Imputing data
wsdata<-read.csv("MasterfileWindspeed 1.csv")
##Filling Error Desc with informational reading
wsdata$ErrorDesc[is.na(wsdata$ErrorDesc)]<-"Informational Reading"
##Fill Error Type with informational if it is not specified
wsdata$ErrorType[is.na(wsdata$ErrorType)]<-"Informational"
##Error Importance
wsdata$ErrorDescImportant <- ifelse(tolower(wsdata$ErrorDesc) %in% tolower(c('Gear Oil Temperature High',
                                                                     'Gear Oil Pressure Too High/Low',
                                                                     'Hs-Gen Gearbearing Superheated',
                                                                     'Gearoil Level Too Low',
                                                                     'Hs-Rot Gearbearing Superheated', 
                                                                     'Ims-Gen Gearbearing Temp Too High',
                                                                     'Lms-Rot Gearbearing Temp Too High',
                                                                     'Converter Tripped, Auto Start',
                                                                     'Mainbreaker Cut Out',
                                                                     'Grid Filter Current Overload',
                                                                     'Ups-Failure',
                                                                     'Ups Bypass Error',
                                                                     'Osc. In Gen Speed, Cons. Lim',
                                                                     'Slip Ring Error',
                                                                     'Genrpm/Srsg Speed Error')), wsdata$ErrorDesc, NA)
##Making Balance of Plant binary
wsdata$BoPbin <- rep(0, dim(wsdata)[1])

wsdata$BoPbin <- ifelse(wsdata$ErrorType %in% 'Balance of Plant', 1, 0)

#Imputing Ambient Temp
naAT<-na.omit(wsdata$AmbTemp)
wsdata$AmbTemp[is.na(wsdata$AmbTemp)]<- median(naAT)
#Imputing BearingIMS
naBI<-na.omit(wsdata$BearingIMS2)
wsdata$BearingIMS2[is.na(wsdata$BearingIMS2)]<- median(naBI)
#Imputing HP
naHP<-na.omit(wsdata$HP)
wsdata$HP[is.na(wsdata$HP)]<- median(naHP)
#Imputing Gen RPM
naRPM<-na.omit(wsdata$GenRPM)
wsdata$GenRPM[is.na(wsdata$GenRPM)]<- median(naRPM)
#Imputing BearingHS
naB<-na.omit(wsdata$BearingHS)
wsdata$BearingHS[is.na(wsdata$BearingHS)]<- median(naB)
#Imputing Oiltemp
naOT<-na.omit(wsdata$OilTemp)
wsdata$OilTemp[is.na(wsdata$OilTemp)]<- median(naOT)
#Imputing Active Power
naAP<-na.omit(wsdata$ActivePower)
wsdata$ActivePower[is.na(wsdata$ActivePower)]<- median(naAP)

