 # set the working directory
setwd("P:/DAACS/Archaeological Sites/Dominica/Morne Patate/Chronology/AllContexts/WithBlockC")

# wareTypeCAandMCD.R
# Establish a DBI connection to DAACS PostgreSQL database and submnit SQL queries
# Created by:  FDN  8.5.2014
# Last update: FDN 8.5.2014  
# Edited by:   LAB 1.17.2017 for Morne Patate
# Edited by:  LC 12.4.2017 hermitage phases
# Edited by:   FDN 12.21.2017 for tidyR


#load the library
require(RPostgreSQL)
library(dplyr)
require(tidyr)

# tell DBI which driver to use
pgSQL <- dbDriver("PostgreSQL")
# establish the connection
DRCcon<-dbConnect(pgSQL, host='drc.iath.virginia.edu', port='5432',
          dbname='daacs-production',
          user='drcquery', password='!queryacct!')


# get the table with the ware type date ranges
MCDTypeTable<- dbGetQuery(DRCcon,'
SELECT * 
FROM "tblCeramicWare"
     ')


# submit a SQL query: note the use of \ as an escape sequence
wareTypeData<-dbGetQuery(DRCcon,'
SELECT
"public"."tblCeramic"."Quantity",
"public"."tblCeramicWare"."Ware",
"public"."tblCeramicWare"."BeginDate",
"public"."tblCeramicWare"."EndDate",
"public"."tblCeramicCEWType"."CeramicCEWType",
"public"."tblProjectName"."ProjectName",
"public"."tblContext"."ProjectID",
"public"."tblContext"."Context",
"public"."tblContext"."DAACSStratigraphicGroup",
"public"."tblContext"."MasterContextNumber",
"public"."tblContext"."FeatureNumber",
"public"."tblContext"."QuadratID",
"public"."tblContext"."DAACSPhase",
"public"."tblContext"."MasterContextInterpretation"

FROM
"public"."tblProjectName"
INNER JOIN "public"."tblProject" ON "public"."tblProject"."ProjectNameID" = "public"."tblProjectName"."ProjectNameID"
INNER JOIN "public"."tblContext" ON "public"."tblContext"."ProjectID" = "public"."tblProject"."ProjectID"
INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
INNER JOIN "public"."tblCeramic" ON "public"."tblCeramic"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
INNER JOIN "public"."tblCeramicWare" ON "public"."tblCeramic"."WareID" = "public"."tblCeramicWare"."WareID"
LEFT JOIN "public"."tblCeramicCEWType" ON "public"."tblCeramic"."CeramicCEWTypeID" = "public"."tblCeramicCEWType"."CeramicCEWTypeID"

WHERE                     
                        "public"."tblContext"."ProjectID" = \'1243\'
                          ')             


#summary1<-ddply(wareTypeData, .(DAACSPhase), summarise, Count=sum(Quantity))
#FDN do a summary using tidyR
summary1 <- wareTypeData %>% group_by(ProjectName,ProjectID) %>% summarise(count = sum(Quantity))
summary1


##Section 1: Customize the Manufacturing Dates ######################

# Change dates for French CEW to 1675-1900
MCDTypeTable <- MCDTypeTable %>% 
   mutate(BeginDate = replace(BeginDate, Ware %in% c('French Coarse Earthenware','Vallauris',
                                                     'Saintonge', 'de lHuveaune'), 1675),
          EndDate = replace(EndDate, Ware %in% c('French Coarse Earthenware','Vallauris',
                                                 'Saintonge','de lHuveaune'), 1900))
MCDTypeTable <- MCDTypeTable %>% 
  mutate(BeginDate = ifelse(BeginDate < 1675, 1675, BeginDate)) 


#Replace 'Caribbean CEW' ware type with applicable CEW Type
wareTypeData <-
  mutate(wareTypeData, Ware=ifelse((Ware %in% c('Caribbean Coarse Earthenware, unid.',
                      'Caribbean Coarse Earthenware, wheel thrown',
                      'Caribbean Coarse Earthenware, hand built')), CeramicCEWType, Ware))

#Replace Unid. Carib CEW type with 'Caribbean CEW Unid.'
wareTypeData <-
  mutate(wareTypeData, Ware=ifelse(Ware == 'Unidentifiable', 'Carribean CEW Unid.', Ware))

#Add rows to type data for MP CEW types
MPCEW <- data.frame(WareID=NA, Ware=c('Morne Patate Type 1', 'Morne Patate Type 2', 'Morne Patate Type 2a', 'Morne Patate Type 2b',
'Morne Patate Type 3'), ObjectTypeID=NA, BeginDate=NA, EndDate=NA, CeramicMaterialID=NA, id=NA)
MCDTypeTable <- bind_rows(MCDTypeTable, MPCEW)

#Replace 'Caribbean CEW' ware type with applicable CEW Type
MCDTypeTable <-
  mutate(MCDTypeTable, Ware=ifelse(Ware == 'Unidentifiable', 'Carribean CEW Unid.', Ware))



# Compute new numeric variables from original ones, which we will need to compute the MCDs
# Non-tidy version
# MCDTypeTable<-within(MCDTypeTable, {     # Notice that multiple vars can be changed
#   midPoint <- (EndDate+BeginDate)/2
#   span <- EndDate - BeginDate
#   inverseVar <- 1/(span/6)##2 
# })


# FDN the tidyR version
MCDTypeTable <- MCDTypeTable %>% 
  mutate(midPoint = (EndDate+BeginDate)/2,
         span = (EndDate - BeginDate),
         inverseVar = 1/(span/6)^2 
         )

# Section 2:Create the UNIT Variable ######################
# This is the level at which assemblages are aggregated
# in the analysis

# delete contexts that have no phase assignment
# for MP we need to double check previous phases entered in DB
# wareTypeData <- filter( wareTypeData, !(DAACSPhase %in% c('',' ')) )

#sum(is.na(wareTypeData$DAACSPhase))

# create the Unit Field
# wareTypeData1 <-  mutate(wareTypeData, unit = paste(ProjectName,DAACSPhase))

wareTypeData$DAACSStratigraphicGroup[is.na(wareTypeData$DAACSStratigraphicGroup)] <- ''
wareTypeData$FeatureNumber[is.na(wareTypeData$FeatureNumber)] <- ''
wareTypeData$QuadratID[is.na(wareTypeData$QuadratID)] <- ''

wareTypeData1 <-
  mutate(wareTypeData, unit=ifelse((QuadratID == '' & FeatureNumber == '' & DAACSStratigraphicGroup == ''),
                                    paste(Context),
                                    ifelse((QuadratID != '' & FeatureNumber == '' & DAACSStratigraphicGroup == ''),
                                           paste(QuadratID,Context),
                                           ifelse((QuadratID == '' & FeatureNumber != '' & DAACSStratigraphicGroup == ''),
                                                  paste(Context,'F',FeatureNumber),
                                                  ifelse((QuadratID != '' & FeatureNumber == '' & DAACSStratigraphicGroup != ''),
                                                         paste(QuadratID,DAACSStratigraphicGroup),
                                                         ifelse((QuadratID == '' & FeatureNumber != '' & DAACSStratigraphicGroup != ''),
                                                                paste(FeatureNumber,DAACSStratigraphicGroup),
                                                                paste(Context)
                                                         ))))))

 
# FDN tidyR total type counts
summary2 <-wareTypeData1 %>% group_by(Ware) %>%  summarise(Count=sum(Quantity))
#print(summary2, n=100)
#write.csv(summary2, "waresummary.csv")

# Drop House Area C - data incomplete
#wareTypeData1 <- filter(wareTypeData1, MasterContextNumber != 'BlockC')

# Add area designation for STPs -- not necessary for this code unless you want to see Wares by Block
# STPblocks <- read.csv('MP_Cxt_QuadID.csv', stringsAsFactors=F, header=T)
# wareTypeData1 <- left_join(wareTypeData1, STPblocks, by='Context')
# wareTypeData1$Block <- wareTypeData1$MasterContextNumber
# wareTypeData1 <- mutate(wareTypeData1, Block=ifelse(Block =='STP', STPBlock, Block))

# Remove ware types with counts of 1 or 2
wareTypeData1 <- subset(wareTypeData1, ! wareTypeData1$Ware  %in%  c('Nottingham', 'Refined Earthenware, modern',
                                                                     'American Stoneware',
                                                                     'Refined Stoneware, unidentifiable',
                                                                     'Fulham Type',
                                                                     'Saintonge',
                                                                     'Astbury Type'))


## Section 3:Transpose the Data ######################

# Create new data frame with contexts as rows and type as cols, with the
# entries as counts
WareByUnit <- wareTypeData1 %>% group_by(Ware,unit) %>% summarise(count = sum(Quantity))

# now we transpose the data so that we end up with a context (rows) x type 
# (cols) data matrix; unit ~ ware formula syntax, left side = row, right side = column, to fill in
# body of table with the counts, fill rest with zeros
require(reshape2)
WareByUnitT <- dcast(WareByUnit, unit ~ Ware, value.var='count', fill=0 )

# lets compute the totals for each context i.e. row
# Note the use of column numbers as index values to get the type counts, which are
# assumed to start iin col 2.
WareByUnitTTotals<- rowSums(WareByUnitT[,2:ncol(WareByUnitT)])

# OK now let's get rid of all the rows where totals are <= 5
WareByUnitT1 <-WareByUnitT[WareByUnitTTotals>5,]  


##Section 4: Define an MCD function and Function to Remove Types w/o Dates ######################

# we build a function that remove types with no dates
# Two arguments 1. unitData: dataframe with counts of ware types in units
# the left variable IDs the units, while the rest of the varaibles are types
# 2. typeData: a dataframe with at least two variables named 'midPoint' and 'inversevar'
# containing the manufacturing midpoints and inverse variances for the types.
# returns a list comprise of two dataframes:
# unitDataWithDates has units with types with dates
# typeDataWithDates has the types with dates

RemoveTypesNoDates <- function(unitData,typeData){
  
  #unitData<- WareByUnitT1
  #typeData <-MCDTypeTable
  typesWithNoDates <- typeData$Ware[(is.na(typeData$midPoint))]
  unitDataWithDates <- unitData[, ! colnames(unitData) %in%  typesWithNoDates]
  typeDataWithDates <- typeData[! typeData$Ware %in%  typesWithNoDates, ]
  unitDataWithDates <- filter(unitDataWithDates, rowSums(unitDataWithDates[,2:ncol(unitDataWithDates)]) > 0)
  return(list(unitDataWithDates=unitDataWithDates, typeDataWithDates=typeDataWithDates))
}
WareandTypeDatawithDates <- RemoveTypesNoDates(WareByUnitT1, MCDTypeTable)

# now we build a function that computes MCDs
# two arguments: 1. unitData: a dataframe with the counts of ware types in units. We assume
# the left variable IDs the units, while the rest of the varaibles are types
# 2. typeData: a dataframe with at least two variables named 'midPoint' and 'inversevar'
# containing the manufacturing midpoints and inverse variances for the types.
# returns a list comprise of two dataframes: 
#     MCDs has units and the vanilla and BLUE MCDs
#     midPoints has the types and manufacturing midpoints, in the order they appeaed in the input
#     unitData dataframe.  

EstimateMCD<- function(unitData,typeData){
 #for debugging
 #unitData<- WareandTypeDatawithDates$unitDataWithDates
 #typeData <-WareandTypeDatawithDates$typeDataWithDates
 countMatrix<- as.matrix(unitData[,2:ncol(unitData)])
 unitNames <- (unitData[,1])
 nUnits <- nrow(unitData)   
 nTypes<- nrow(typeData)
 nTypesFnd <-ncol(countMatrix)
 typeNames<- colnames(countMatrix)
 
 # create two col vectors to hold inverse variances and midpoints
 # _in the order in which the type variables occur in the data_.
 invVar<-matrix(data=0,nrow=nTypesFnd, ncol=1)
 mPoint <- matrix(data=0,nrow=nTypesFnd, ncol=1)
  for (i in (1:nTypes)){
   for (j in (1:nTypesFnd)){
      if (typeData$Ware[i]==typeNames[j]) {
        invVar[j,]<-typeData$inverseVar[i] 
        mPoint[j,] <-typeData$midPoint[i]
      }
     }
   }

 # compute the blue MCDs
 # get a unit by type matrix of inverse variances
 invVarMat<-matrix(t(invVar),nUnits,nTypesFnd, byrow=T)
 # a matrix of weights
 blueWtMat<- countMatrix * invVarMat
 # sums of the weight
 sumBlueWts <- rowSums(blueWtMat)
 # the BLUE MCDs
 blueMCD<-(blueWtMat %*% mPoint) / sumBlueWts
 # compute the vanilla MCDs
 sumWts <- rowSums(countMatrix)
 # the vanilla MCDs
 MCD<-(countMatrix %*% mPoint) / sumWts
 # Finally we assemble th results in to a list
 MCDs<-data.frame(unitNames,MCD,blueMCD,sumWts)
 colnames(MCDs)<- c('unit','MCD','blueMCD', 'Count' )
 midPoints <- data.frame(typeNames,mPoint)
 MCDs <- list(MCDs=MCDs,midPoints=midPoints)
 return(MCDs)
} 

#end of function EstimateMCD
 
# apply the function
MCDByUnit<-EstimateMCD(WareandTypeDatawithDates$unitDataWithDates,WareandTypeDatawithDates$typeDataWithDates)

# let's see what it looks like
MCDByUnit

# a function to sort the rows and cols of a matrix based on the
# orders from two arguments (e.g. MCDs and midpoints)
# arguments:  the name of the variable that contains the unit scores (e.g. MCDs)
#             the name of the variable that contains the type score (e.g. the midpoints)
#             the name of the dataframe that contains the counts of ware types in units
# returns:    the sorted dataframe 
sortData<- function(unitScores,typeScores,unitData){
  #unitScores<-U3MCDByUnit$MCDs$blueMCD
  #typeScores<-U3MCDByUnit$midPoints$mPoint
  #unitData<- U3WareByUnitT1
  sortedData<-unitData[order(unitScores),]
  sortedData<-sortedData[,c(1,order(typeScores)+1)]
  return(sortedData)
}


# apply the function
#WareByUnitT5Sorted<-sortData(MCDByUnit$MCDs$blueMCD,
 #                             MCDByUnit$midPoints$mPoint,
  #                           WareByUnitT2)

WareByUnitT2Sorted<-sortData(MCDByUnit$MCDs$blueMCD,
                            MCDByUnit$midPoints$mPoint,
                            WareandTypeDatawithDates$unitDataWithDates)

# now we prep the sorted dataframe to make a Ford-style battleship plot
# convert to a matrix, whose cols are the counts
# make the unit name a 'rowname" of the matrix
Mat<-as.matrix(WareByUnitT2Sorted[,2:ncol(WareByUnitT2Sorted)])
rownames(Mat)<-WareByUnitT2Sorted$unit
rSums<- matrix (rowSums(Mat),nrow(Mat),ncol(Mat), byrow=F)
MatProp<-Mat/rSums


#(package for seriation)
library(plotrix) 
battleship.plot(MatProp,
                     mar=c(2,5,5,1),
                     #main = 'Seriation by Blue MCD',
                     xlab='Ware Type',
                     ylab= 'Context',
                     col='grey')

# dump out a CSV for a seriation plot using Lipo's Excel macro 
#write.csv(WareByUnitT2Sorted, file='DRC/WareByFeatureT1Sorted.csv')

##Section 5: Run the CA ######################

# Remove context outliers for CA -- AFTER first run of CA
 WareByUnitT1 <- filter(WareByUnitT1, unit != '5364 F 062')

# Remove columns (ware types) where totals < 0
 WareByUnitT1<-WareByUnitT1[,colSums(WareByUnitT1 != 0) > 0]

# now let's try some Correspondence Analysis
MatX<-as.matrix(WareByUnitT1[,2:ncol(WareByUnitT1)]) 
rownames(MatX)<-WareByUnitT1$unit

# This is the second function that we are now using as of 12/15/2016 
require(ca)
ca3<-ca(MatX)

# Default plot
plot(ca3)

# Summary(ca3) - inertia plot
plot(1:(length(ca3$sv)), ca3$sv^2 / sum(ca3$sv^2))

# Create dataframe of unit/context dimension 1 and 2 scores for ggplot
rowscores <- data.frame(ca3$rowcoord[,1], ca3$rowcoord[,2])
colnames(rowscores) <- c("Dim1", "Dim2")

# Create dataframe of ware type dimension 1 and 2 scores for ggplot
colscores <- data.frame(ca3$colcoord[,1], ca3$colcoord[,2])
colnames(colscores) <- c("Dim1", "Dim2")

#### Create plot: Dim 1 and Dim 2 context scores
require(ggplot2)
library(ggrepel)
p1 <- ggplot(rowscores, aes(x=rowscores$Dim1,y=rowscores$Dim2))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  geom_text(aes(label=rownames(rowscores)),vjust=-.6, hjust=-.1, cex=5)+
  #  xlim(-4,4)+
  # geom_text_repel(aes(label=rownames(rowscores)), cex=5, segment.alpha=0.2) +
  theme_classic()+
  labs(title="Morne Patate", x="Dimension 1", y="Dimension 2")+
  theme(plot.title=element_text(size=rel(2), hjust=0.5),axis.title=element_text(size=rel(1.75)),
        axis.text=element_text(size=rel(1.5)))
p1
#ggsave("MP_AllPhases_CADim1Dim2_labels2018.png", p1, width=10, height=7.5, dpi=300)
#p1 + geom_vline(xintercept=c(-0.7), colour="black")


#### Create plot: Dim 1 and Dim 2 ware type scores
p2 <- ggplot(colscores, aes(x=colscores$Dim1,y=colscores$Dim2))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
 # geom_text(aes(label=rownames(colscores)),vjust=-.6, cex=5)+
  geom_text_repel(aes(label=rownames(colscores)), cex=5, segment.alpha=0.2) +
  theme_classic()+
  labs(title="Morne Patate", x="Dimension 1", y="Dimension 2")+
  theme(plot.title=element_text(size=rel(2.25), hjust=0.5),axis.title=element_text(size=rel(1.75)),
        axis.text=element_text(size=rel(1.5)))
p2
#ggsave("MP_AllPhases_CADim1Dim2wares_labels2018.png", p2, width=10, height=7.5, dpi=300)
#p2 + geom_vline(xintercept=c(-0.7), colour="black")

# plot the col scores on dim1 and dim2, which types are important in which regions of the plot
# plot(ca3$colcoord[,1],ca3$colcoord[,2],pch=21, bg="black",cex=1.25,
#      xlab="Dimension 1", ylab="Dimension 2", asp=1 )
# text(ca3$colcoord[,1],ca3$colcoord[,2],rownames(ca3$colcoord),
#      pos=4 ,cex=.75, col="black")

#For ggplot histogram you need to have the context and date information in a dataframe

#Get units from summary of CA scores
rowscores$unit <- rownames(rowscores)

# Make MCD info into its own dataframe, change data type to character
blueMCD <- MCDByUnit$MCDs
blueMCD$unit <- as.character(blueMCD$unit)

# create new dataframe that contains Dim 1 scores and MCDs
scoreDates0  <- inner_join(rowscores, blueMCD, by='unit')
scoreDates <- filter(scoreDates0, MCD > 1)
#write.csv(scoreDates, 'scoreDates.csv')

p3 <- ggplot(scoreDates, aes(x=scoreDates$Dim1,y=scoreDates$blueMCD))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  geom_text(aes(label=scoreDates$unit),vjust=-.6, hjust=-.1, cex=5)+
  #geom_text_repel(aes(label=rownames(rowscores)), cex=6, segment.alpha=0.2) +
  theme_classic()+
  scale_y_continuous(breaks=seq(1700, 1930, 10))+
  labs(title="Morne Patate", x="Dimension 1", y="BLUE MCD")+
  theme(plot.title=element_text(size=rel(2.25), hjust=0.5),axis.title=element_text(size=rel(1.75)),
        axis.text=element_text(size=rel(1.5)))
p3 
ggsave("MP_AllPhases_CADim1blueMCD_labels2018.png", p3, width=10, height=7.5, dpi=300)
#p3 + geom_vline(xintercept=c(-0.7), colour="black")

# Dim 2
p3A <- ggplot(scoreDates, aes(x=scoreDates$Dim2,y=scoreDates$blueMCD))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  #geom_text(aes(label=rownames(rowscores)),vjust=-.6, hjust=-.1, cex=5)+
  #geom_text_repel(aes(label=scoreDates$unit), cex=6, segment.alpha=0.2) +
  theme_classic()+
  scale_y_continuous(breaks=seq(1790, 1910, 10))+
  labs(title="Morne Patate", x="Dimension 2", y="BLUE MCD")+
  theme(plot.title=element_text(size=rel(2.25), hjust=0.5),axis.title=element_text(size=rel(1.75)),
        axis.text=element_text(size=rel(1.5)))
p3A
#ggsave("CAbySitePhase_AllCabin_Dim2BlueMCD.png", p3A, width=10, height=7.5, dpi=300)

####Section 6: Histogram and Phasing #######################

# Create Plot histogram Dim 1 scores weighted
p5 <- ggplot(scoreDates, aes(x=scoreDates$Dim1, weight=scoreDates$Count/sum(scoreDates$Count)))+
  geom_histogram(aes(y=..density..), colour="gray", fill="tan", binwidth=0.1, boundary=0.5)+
  #stat_function(fun = dnorm, colour = "blue")+
  scale_x_continuous(breaks=seq(-8, 1, 0.5))+
  theme_classic()+
  labs(title="Morne Patate", x="Dimension 1", y="Density")+
  theme(plot.title=element_text(size=rel(2.25), hjust=0.5),axis.title=element_text(size=rel(1.75)),
        axis.text=element_text(size=rel(1.5)))
p5A <- p5 + geom_density(fill=NA) + geom_vline(xintercept=c(-2.3), colour="black")
p5A
ggsave("MP_AllPhases_histogram2018.png", p5A, width=10, height=7.5, dpi=300)


# create a vector for the phases with as many entries as assemblages
scoreDates$Phase <- NA

# do the phase assigments
scoreDates$Phase[(scoreDates[,1] <= -2.3)] <- 'P01'

scoreDatesP01 <- filter(scoreDates, !is.na(Phase))

#scoreDatesP01x <- inner_join(scoreDatesP01, wareTypeData1, by='unit')
#write.csv(scoreDatesP01x, 'scoreDates_EarlyTail.csv')


####Section 7: Rerun CA without tail #######################

# Remove contexts assigned to P01
WareByUnitTY <- anti_join(WareByUnitT1, scoreDatesP01, by='unit')

# Remove context outliers for CA -- AFTER first run of CA
#WareByUnitTX <- filter(WareByUnitTY, ! unit  %in% c('N4585E6490 6179', 'N4580E6635 5266'))

# Remove columns (ware types) where totals < 0
#WareByUnitTX<-WareByUnitTX[,colSums(WareByUnitTX != 0) > 0]
#WareByUnitTX<-WareByUnitTY[,colSums(WareByUnitTY != 0) > 0]

# now let's try some Correspondence Analysis
#MatX<-as.matrix(WareByUnitTX[,2:ncol(WareByUnitTX)]) 
#rownames(MatX)<-WareByUnitTX$unit
MatX <-as.matrix(WareByUnitTY[,2:ncol(WareByUnitTY)]) 
rownames(MatX)<-WareByUnitTY$unit

# Run the CA a second time without the early tail (Phase 01)  
require(ca)
ca3x<-ca(MatX)

# Default plot
plot(ca3x)

# Summary(ca3) - inertia plot
plot(1:(length(ca3x$sv)), ca3x$sv^2 / sum(ca3x$sv^2))

# Create dataframe of unit/context dimension 1 and 2 scores for ggplot
rowscoresX <- data.frame(ca3x$rowcoord[,1], ca3x$rowcoord[,2])
colnames(rowscoresX) <- c("Dim1", "Dim2")
rowscoresX$unit <- rownames(rowscoresX)

# Create dataframe of ware type dimension 1 and 2 scores for ggplot
colscoresX <- data.frame(ca3x$colcoord[,1], ca3x$colcoord[,2])
colnames(colscoresX) <- c("Dim1", "Dim2")

#### Create plot: Dim 1 and Dim 2 context scores
require(ggplot2)
library(ggrepel)
p1x <- ggplot(rowscoresX, aes(x=rowscoresX$Dim1,y=rowscoresX$Dim2))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  #geom_text_repel(aes(label=rowscoresX$unit), cex=5, segment.alpha=0.2) +
  #geom_text(aes(label=rowscoresX$unit),vjust=-.6, hjust=-.1, cex=5)+
  #  xlim(-4,4)+
  #geom_text_repel(aes(label=rownames(rowscores)), cex=5, segment.alpha=0.2) +
  theme_classic()+
  labs(title="Morne Patate", x="Dimension 1", y="Dimension 2")+
  theme(plot.title=element_text(size=rel(2), hjust=0.5),axis.title=element_text(size=rel(1.75)),
        axis.text=element_text(size=rel(1.5)))
p1x
#ggsave("MP_WithoutP01_CADim1Dim2.png", p1x, width=10, height=7.5, dpi=300)
#ggsave("MP_WithoutP01_CADim1Dim2_labels.png", p1x, width=10, height=7.5, dpi=300)
#p1 + geom_vline(xintercept=c(-0.7), colour="black")

#wareTypeDataX <- select(wareTypeData1,DAACSStratigraphicGroup, MasterContextNumber, MasterContextInterpretation, Block, unit)
#wareTypeDataY <- unique(wareTypeDataX)
#rowscoresY <- inner_join(rowscoresX, wareTypeDataY, by='unit')

#p1xa <- ggplot(rowscoresY, aes(x=rowscoresY$Dim1,y=rowscoresY$Dim2))+
 # geom_point(aes(fill=rowscoresY$MasterContextNumber), shape=21, size=5, alpha=0.5) +
 #  geom_point(aes(fill=rowscoresY$MasterContextInterpretation), shape=21, size=5, alpha=0.5) +
 #   geom_point(aes(fill=rowscoresY$Block), shape=21, size=5, alpha=0.7) +
      #geom_text(aes(label=rownames(rowscoresX)),vjust=-.6, hjust=-.1, cex=5)+
  #  xlim(-4,4)+
  #geom_text_repel(aes(label=rownames(rowscores)), cex=5, segment.alpha=0.2) +
  #theme_classic()+
  #labs(title="Morne Patate", x="Dimension 1", y="Dimension 2")+
  #theme(plot.title=element_text(size=rel(2), hjust=0.5),axis.title=element_text(size=rel(1.75)),
   #     axis.text=element_text(size=rel(1.5)))+
  #scale_fill_manual(name='Unit', values=c("blue", "green", "yellow", "orange", "lightblue", "pink", "black", "red", "darkgreen", "purple"))
#p1xa
#ggsave("MP_CADim1Dim2_NoEarlyTail_ColorsbyBlock.png", p1xa, width=10, height=7.5, dpi=300)

#scoreDatesminus2A <- filter(rowscoresY, Dim1 < -2)
#scoreDatesminus2 <- inner_join(scoreDatesminus2A, wareTypeData1, by='unit')
#write.csv(scoreDatesminus2, 'scoreDates_EarlierMinus2.csv')



#### Create plot: Dim 1 and Dim 2 ware type scores
p2x <- ggplot(colscoresX, aes(x=colscoresX$Dim1,y=colscoresX$Dim2))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  # geom_text(aes(label=rownames(colscoresX)),vjust=-.6, cex=5)+
  geom_text_repel(aes(label=rownames(colscoresX)), cex=5, segment.alpha=0.2) +
  theme_classic()+
  labs(title="Morne Patate", x="Dimension 1", y="Dimension 2")+
  theme(plot.title=element_text(size=rel(2.25), hjust=0.5),axis.title=element_text(size=rel(1.75)),
        axis.text=element_text(size=rel(1.5)))
p2x
#ggsave("MP_WithoutP01_CADim1Dim2wares_labels.png", p2x, width=10, height=7.5, dpi=300)
#p2x + geom_vline(xintercept=c(-0.7), colour="black")

# plot the col scores on dim1 and dim2, which types are important in which regions of the plot
# plot(ca3$colcoord[,1],ca3$colcoord[,2],pch=21, bg="black",cex=1.25,
#      xlab="Dimension 1", ylab="Dimension 2", asp=1 )
# text(ca3$colcoord[,1],ca3$colcoord[,2],rownames(ca3$colcoord),
#      pos=4 ,cex=.75, col="black")

scoreDatesX0 <- inner_join(rowscoresX, blueMCD, by='unit')
scoreDatesX <- filter(scoreDatesX0, MCD > 1)

#write.csv(scoreDates, 'scoreDates.csv')

p3x <- ggplot(scoreDatesX, aes(x=scoreDatesX$Dim1,y=scoreDatesX$blueMCD))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  #geom_text_repel(aes(label=scoreDatesX$unit), cex=6, segment.alpha=0.2) +
  geom_text(aes(label=scoreDatesX$unit),vjust=-.6, hjust=-.1, cex=5)+
  theme_classic()+
  scale_y_continuous(breaks=seq(1700, 1930, 10))+
  labs(title="Morne Patate", x="Dimension 1", y="BLUE MCD")+
  theme(plot.title=element_text(size=rel(2.25), hjust=0.5),axis.title=element_text(size=rel(1.75)),
        axis.text=element_text(size=rel(1.5)))
p3x 
#ggsave("MP_WithoutP01_CADim1blueMCD_labels.png", p3x, width=10, height=7.5, dpi=300)
#p3 + geom_vline(xintercept=c(-0.7), colour="black")

p5x <- ggplot(scoreDatesX, aes(x=scoreDatesX$Dim1, weight=scoreDatesX$Count/sum(scoreDatesX$Count)))+
  geom_histogram(aes(y=..density..), colour="gray", fill="tan", binwidth=0.1, boundary=0.5)+
  #xlim(-3.5,7)+
  #stat_function(fun = dnorm, colour = "blue")+
  scale_x_continuous(breaks=seq(-9, 3.5, 0.5))+
  theme_classic()+
  labs(title="Morne Patate", x="Dimension 1", y="Density")+
  theme(plot.title=element_text(size=rel(2.25), hjust=0.5),axis.title=element_text(size=rel(1.75)),
        axis.text=element_text(size=rel(1.5)))
p5Ax <- p5x + geom_density(fill=NA, bw="SJ") + geom_vline(xintercept=c(-1.4, 0.7), colour="red")
p5bx <- p5Ax + geom_vline(xintercept=c(-1.2, -.1, 0.5), colour="black")
p5bx
ggsave("MP_WithoutP01_Histogram_BlockC.png", p5bx, width=10, height=7.5, dpi=300)



# create a vector for the phases with as many entries as assemblages
scoreDatesX$Phase <- NA

# do the phase assigments
scoreDatesX$Phase[(scoreDatesX[,1] <= -1.2)] <- 'P05'
scoreDatesX$Phase[(scoreDatesX[,1] > -1.2) & (scoreDatesX[,1] < -0.1)] <- 'P04'
scoreDatesX$Phase[(scoreDatesX[,1] > -0.1) & (scoreDatesX[,1] < 0.5)] <- 'P03'
scoreDatesX$Phase[(scoreDatesX[,1] > 0.5)] <- 'P02'


Phases <- rbind(scoreDatesX, scoreDatesP01)

#Order by dim1 score
CA_MCD_Phase <- Phases[order(Phases$Dim1),]


#weighted mean
#tapply function = applies whatever function you give it, x is object on which you calculate the function
#W is numerical weighted vector
tapply(CA_MCD_Phase$blueMCD, CA_MCD_Phase$Phase, weighted.mean)

#Once phases are assigned we need to calculate MCDs and TPQs by phase 
#Add phase assignments to ware counts by unit
CA_MCD_Phase2 <- left_join(WareByUnitT2Sorted, CA_MCD_Phase, by='unit')
#get rid of dimscores and MCD columns
CA_MCD_Phase2 <- CA_MCD_Phase2[-c(1, 24:28)]

#aggregate counts for ware type by phase
WareByPhase <- ddply(CA_MCD_Phase2, "Phase", numcolwise(sum))

#Check ware by phase
WareByPhase2 <- filter(WareByPhase, !is.na(Phase)) 
rowSums(WareByPhase2[c(2:23)])

#alter EstimateMCDandTPQ function to have phaseData as input for unitData
EstimateMCDandTPQ<- function(phaseData,typeData){
  #for debugging
  #phaseData<- WareByPhase
  #typeData <- MCDTypeTable     
  countMatrix<- as.matrix(phaseData[,2:ncol(phaseData)])
  phaseNames <- (phaseData[,1])
  nPhases <- nrow(phaseData)   
  nTypes<- nrow(typeData)
  nTypesFnd <-ncol(countMatrix)
  typeNames<- colnames(countMatrix)
  # create two col vectors to hold inverse variances and midpoints
  # _in the order in which the type variables occur in the data_.
  invVar<-matrix(data=0,nrow=nTypesFnd, ncol=1)
  mPoint <- matrix(data=0,nrow=nTypesFnd, ncol=1)
  for (i in (1:nTypes)){
    for (j in (1:nTypesFnd)){
      if (typeData$Ware[i]==typeNames[j]) {
        invVar[j,]<-typeData$inverseVar[i] 
        mPoint[j,] <-typeData$midPoint[i]
      }
    }
  }
  # replace NAs for types with no dates with 0s -- so they do not count
  # compute the blue MCDs
  # get a unit by type matrix of inverse variances
  invVarMat<-matrix(t(invVar),nPhases,nTypesFnd, byrow=T)
  # a matrix of weights
  blueWtMat<- countMatrix * invVarMat
  # sums of the weight
  sumBlueWts <- rowSums(blueWtMat)
  # the BLUE MCDs
  blueMCD<-(blueWtMat %*% mPoint) / sumBlueWts
  # compute the vanilla MCDs
  sumWts <- rowSums(countMatrix)
  # the vanilla MCDs
  MCD<-(countMatrix %*% mPoint) / sumWts
  # now for the TPQs
  meltedPhaseData<- melt(phaseData, id.vars='Phase',  variable.name = 'Ware', value.name='count')
  meltedPhaseData1 <- subset(meltedPhaseData, count > 0) 
  mergedPhaseData <- merge(x = meltedPhaseData1, y = typeData,  by.x='Ware', by.y='Ware')
  # the trick is that to figure out the tpq it's best to have each record (row) represent an individual sherd
  # but in its current state, each record has a count that is likely more than 1 so it's necessary to break them up
  # use rep and rownames - rowname is a unique number for each row, kind of link an index
  # rep goes through dataframe mergedUnitData and replicates based on the count column, i.e. if count is
  # 5 it will create 5 records or rows and only replicates columns 2 and 6 (2 is unit name and 6 is begin date)
  repPhaseData <- mergedPhaseData[rep(rownames(mergedPhaseData),mergedPhaseData$count),c(2,6)]
  #once all the rows have a count of one, then can run the quantile function
  TPQ <- tapply(repPhaseData$BeginDate,repPhaseData$Phase, 
                function(x) quantile(x, probs =1.0, type=3 ))              
  TPQp95 <- tapply(repPhaseData$BeginDate,repPhaseData$Phase, 
                   function(x) quantile(x, probs = .95 , type=3 ))                 
  TPQp90 <- tapply(repPhaseData$BeginDate,repPhaseData$Phase, 
                   function(x) quantile(x, probs = .90,  type=3 ))   
  # Finally we assemble the results in to a list
  MCDs<-data.frame(phaseNames,MCD,blueMCD, TPQ, TPQp95, TPQp90, sumWts )
  colnames(MCDs)<- c('Phase','MCD','blueMCD', 'TPQ', 'TPQp95', 'TPQp90', 'Count')
  midPoints <- data.frame(typeNames,mPoint)
  MCDs <- list('MCDs'=MCDs,'midPoints'=midPoints)
  return(MCDs)
} 

#end of function EstimateMCD

# apply the function
MCDByPhase<-EstimateMCDandTPQ(WareByPhase2,MCDTypeTable)
# 
# # let's see what it looks like
MCDByPhase$MCDs

# #check sums of counts for phases
# ddply(CA_MCD_Phase2, .(Phase), summarise, Count=sum(count))
# 
# #weighted mean
# #tapply function = applies whatever function you give it, x is object on which you calculate the function
# #W is numerical weighted vector
# tapply(CA_MCD_Phase2$blueMCD, CA_MCD_Phase2$Phase, weighted.mean)
# 
# 

#####Section 6: Context Phases #####################

#Once phases are assigned we need to have a list of Phases by context to update the database 
#Creat context-level dataframe for the project including Context, Feature Number, and SGs
#ContextListwareTypeDataX<-dbGetQuery(DRCcon,'
ContextList<-dbGetQuery(DRCcon,'
SELECT
             "public"."tblCeramic"."Quantity",
             "public"."tblCeramicWare"."Ware",
             "public"."tblCeramicWare"."BeginDate",
             "public"."tblCeramicWare"."EndDate",
             "public"."tblCeramicCEWType"."CeramicCEWType",
             "public"."tblProjectName"."ProjectName",
             "public"."tblContext"."ProjectID",
             "public"."tblContext"."Context",
             "public"."tblContext"."DAACSStratigraphicGroup",
             "public"."tblContext"."MasterContextNumber",
             "public"."tblContext"."FeatureNumber",
             "public"."tblContext"."QuadratID",
             "public"."tblContext"."DAACSPhase",
             "public"."tblContext"."MasterContextInterpretation"
             
             FROM
             "public"."tblProjectName"
             INNER JOIN "public"."tblProject" ON "public"."tblProject"."ProjectNameID" = "public"."tblProjectName"."ProjectNameID"
             INNER JOIN "public"."tblContext" ON "public"."tblContext"."ProjectID" = "public"."tblProject"."ProjectID"
             INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
             INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
             INNER JOIN "public"."tblCeramic" ON "public"."tblCeramic"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
             INNER JOIN "public"."tblCeramicWare" ON "public"."tblCeramic"."WareID" = "public"."tblCeramicWare"."WareID"
             LEFT JOIN "public"."tblCeramicCEWType" ON "public"."tblCeramic"."CeramicCEWTypeID" = "public"."tblCeramicCEWType"."CeramicCEWTypeID"
             
             WHERE                     
             "public"."tblContext"."ProjectID" = \'1243\'
             ')             


ContextList$DAACSStratigraphicGroup[is.na(ContextList$DAACSStratigraphicGroup)] <- ''
ContextList$FeatureNumber[is.na(ContextList$FeatureNumber)] <- ''
ContextList$QuadratID[is.na(ContextList$QuadratID)] <- ''

ContextList1 <-
  mutate(ContextList, unit=ifelse((QuadratID == '' & FeatureNumber == '' & DAACSStratigraphicGroup == ''),
                                   paste(Context),
                                   ifelse((QuadratID != '' & FeatureNumber == '' & DAACSStratigraphicGroup == ''),
                                          paste(QuadratID,Context),
                                          ifelse((QuadratID == '' & FeatureNumber != '' & DAACSStratigraphicGroup == ''),
                                                 paste(Context,FeatureNumber),
                                                 ifelse((QuadratID != '' & FeatureNumber == '' & DAACSStratigraphicGroup != ''),
                                                        paste(QuadratID,DAACSStratigraphicGroup),
                                                        ifelse((QuadratID == '' & FeatureNumber != '' & DAACSStratigraphicGroup != ''),
                                                               paste(FeatureNumber,DAACSStratigraphicGroup),
                                                               paste(Context)
                                                        ))))))

ContextList1 <- filter(ContextList1, MasterContextNumber != 'BlockC')



#Merge ContextList and CA_MCD_Phase by unit field
ContextPhases <- left_join(ContextList1, CA_MCD_Phase, by="unit")

#Remove unnecessary columns
ContextPhases2 <- ContextPhases[, c(8,21)]

ContextPhases3<- unique(ContextPhases2)

ContextPhases4 <- filter(ContextPhases3, !is.na(ContextPhases3$Phase))

#Create csv list of contexts and phase assignments, label by site name
write.csv(ContextPhases4, file='ContextPhases_MPplantation.csv')



