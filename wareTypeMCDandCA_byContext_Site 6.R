# wareTypeCAandMCD.R
# Establish a DBI connection to DAACS PostgreSQL database and submnit SQL queries
# Created by:  FDN  8.5.2014
# Previous update: EAB 3.24.2015 To add MCDs and TPQs by Phase  
# Last update: LAB 9.5.2017 to add List of Contexts with Phase Assignments for database updates
# Updated to just Site 6 CLP 11.16.2018
# Tidy verion by FDN 11.26.2018
# Adjustment to weighted part of the KDE plot and adding MCDs and tpqs by context by FDN 11.28.2018


# load the libraries
library(RPostgreSQL)
library(dplyr)
library(reshape2)
library (ca)
library (plotrix)
library(ggplot2)
library(viridis)

#Link to file with database password
source("credentials.R")

####1. get the table with the ware type date ranges####
MCDTypeTable<- dbGetQuery(DRCcon,'
                          SELECT * 
                          FROM "tblCeramicWare"
                          ')


#### 2. submit a SQL query: note the use of \ as an escape sequence####
# note the LEFT JOIN on the Feature table retains non-feature contexts
# Fill in your ProjectID



wareTypeData<-dbGetQuery(DRCcon,'
                         SELECT
                         "public"."tblCeramic"."Quantity",
                         "public"."tblCeramicWare"."Ware",
                         "public"."tblCeramicGenre"."CeramicGenre",
                         "public"."tblCeramicCEWType"."CeramicCEWType",
                         "public"."tblProjectName"."ProjectName",
                         "public"."tblContext"."ProjectID",
                         "public"."tblContext"."Context",
                         "public"."tblContextDepositType"."DepositType",
                         "public"."tblContext"."DAACSStratigraphicGroup",
                         "public"."tblContext"."MasterContextNumber",
                         "public"."tblContext"."FeatureNumber",
                         "public"."tblContextFeatureType"."FeatureType",
                         "public"."tblContext"."QuadratID",
                         "public"."tblContext"."DAACSPhase"
                         FROM
                         "public"."tblProjectName"
                         INNER JOIN "public"."tblProject" ON "public"."tblProject"."ProjectNameID" = "public"."tblProjectName"."ProjectNameID"
                         INNER JOIN "public"."tblContext" ON "public"."tblContext"."ProjectID" = "public"."tblProject"."ProjectID"
                         LEFT JOIN "public"."tblContextDepositType" ON "public"."tblContext"."DepositTypeID" = "public"."tblContextDepositType"."DepositTypeID"
                         LEFT JOIN "public"."tblContextFeatureType" ON "public"."tblContext"."FeatureTypeID" = "public"."tblContextFeatureType"."FeatureTypeID" 
                         INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
                         INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
                         INNER JOIN "public"."tblCeramic" ON "public"."tblCeramic"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
                         INNER JOIN "public"."tblCeramicWare" ON "public"."tblCeramic"."WareID" = "public"."tblCeramicWare"."WareID"
                         LEFT JOIN "public"."tblCeramicGenre" ON "public"."tblCeramic"."CeramicGenreID" = "public"."tblCeramicGenre"."CeramicGenreID"
                         LEFT JOIN "public"."tblCeramicCEWType" ON "public"."tblCeramic"."CeramicCEWTypeID" = "public"."tblCeramicCEWType"."CeramicCEWTypeID"
                         WHERE "public"."tblContext"."ProjectID" = \'1413\'
                         ')

#### 3. Any customizations to the Ware Type dates should go here....####
#Change end dates!!!!!!!!!!!
#MCDTypeTable$BeginDate[MCDTypeTable$Ware == 'Porcelain, Chinese']<-1700
#MCDTypeTable$EndDate[MCDTypeTable$Ware == 'Whiteware']<-1930
#MCDTypeTable$EndDate[MCDTypeTable$Ware == 'Ironstone/White Granite']<-1930
#MCDTypeTable$EndDate[MCDTypeTable$Ware == 'Porcelain, French']<-1930


#### 4. Compute new numeric date variables from original ones, which we will need to compute the MCDs####
MCDTypeTable <- MCDTypeTable %>% 
  mutate(midPoint = (EndDate+BeginDate)/2,
         span = (EndDate - BeginDate),
         inverseVar = 1/(span/6)^2 
  )

#### 5. Remove contexts with deposit type cleanup and surface collection ####
wareTypeData <- subset(wareTypeData, ! wareTypeData$DepositType  %in%  c('Clean-Up/Out-of-Stratigraphic Context',
                                                               'Surface Collection'))


#### 6. Do a quick summary of the ware type totals ####
summary1 <- wareTypeData %>% group_by(ProjectName,ProjectID,Ware) %>% 
  summarise(count = sum(Quantity))
summary1


#### 7. Create the UNIT Variable ####
# The UNIT variable contains the level at which assemblages are aggregated in the analysis.
# some housekeeping so we do not stumble on the confusion between R's NA (SQL NULLs) and blanks. 
#wareTypeData$DAACSStratigraphicGroup[is.na(wareTypeData$DAACSStratigraphicGroup)] <- ''
#wareTypeData$FeatureNumber[is.na(wareTypeData$FeatureNumber)] <- ''
#wareTypeData$QuadratID[is.na(wareTypeData$QuadratID)] <- ''
# Use case_when to cycle through QuadID, Feature, SG, and Context to assign aggregration unit
#wareTypeData_Unit <- wareTypeData %>%  
#mutate( unit = case_when(
#QuadratID == "" & FeatureNumber == "" & DAACSStratigraphicGroup == "" 
#~ paste(Context),
#QuadratID == "" & FeatureNumber == "" & DAACSStratigraphicGroup != "" 
#~ paste(DAACSStratigraphicGroup),
#QuadratID != "" & FeatureNumber == "" & DAACSStratigraphicGroup == "" 
#~ paste(QuadratID,Context),
#QuadratID != "" & FeatureNumber == "" & DAACSStratigraphicGroup != "" 
#~ paste(QuadratID,DAACSStratigraphicGroup),
#FeatureNumber != "" & DAACSStratigraphicGroup == ""
#~ paste(FeatureNumber,Context),
#FeatureNumber != "" & DAACSStratigraphicGroup != "" 
#~ paste(FeatureNumber,DAACSStratigraphicGroup)
#))

# Check on the content of the unit variable
#table(wareTypeData_Unit$unit)

  
#### 8. Remove specific contexts, groups of contexts, and ware types as needed ... ####
#Note that if you used Step 7 to create an aggregation unit you may need to change the dataframe name that is used in this step to the dataframe name created in that step, e.g. wareTypeData_Unit
# The Quad that was dug into a backflled quad
wareTypeData1 <- filter(wareTypeData, ! wareTypeData$QuadratID   %in% 
                         c('082%'))

# Remove specific contexts
#wareTypeData2 <- filter (wareTypeData1, !unit  
#%in% c('1030E120A SG04', '1030E130BC 96-01-021','1020E140B SG04','1020E140D SG04',
#'1020E130B SG03', '1030E130A SG04','1020E130B SG04', '1030E120B SG04', '1030E130BC SG04'))

# Remove spcific types
# wareTypeData1 <-  filter(wareTypeData1, ! wareTypeData1$Ware  %in%  
#                    c('Porcellaneous/English Hard Paste',
#                     'Sans Souci Type A',
#                     'Sans Souci Type G',
#                     'White Salt Glaze'))

#### 9. Tranpose the data for the MCD and CA ####
#Note that depending on what you removed in step 8 you may need to change the dataframe name that is used
#for the group_by and sum from wareTypeData1 to wareTypeData2
WareByUnit <- wareTypeData1 %>% group_by(Ware,unit) %>% 
  summarise(count = sum(Quantity))

# now we transpose the data so that we end up with a context (rows) x type 
# (cols) data matrix; unit ~ ware formula syntax, left side = row, right side
#  = column, to fill in body of table with the counts, fill rest with zeros
WareByUnitT <- dcast(WareByUnit, unit ~ Ware, fun.aggregate=sum, 
                     value.var='count', fill=0 )


#### 10. Enforce the sample size cut off -- typically > 5 ####
# lets compute the totals for each unit i.e. row
# Note the use of column numbers as index values to get the type counts, which 
# are assumed to start in col 2.
WareByUnitTTotals<- rowSums(WareByUnitT[,-1])
table(WareByUnitTTotals)

WareByUnitT1 <-WareByUnitT[WareByUnitTTotals > 5,]
# And get rid of any types that do not occur in the subsety of assemblages that 
# DO  meet the sample size cutoff
WareByUnitT1 <- WareByUnitT1 [,colSums(WareByUnitT1[,-1]) > 0]


#### 11. Define a function to Remove Types w/o Dates and a function to compute MCDs, ####
# etc. 
# 11.1 We build a function that removes types with no dates, either because they 
# have no dates in the MCD Ware Type Table or because they are not MCD Ware 
# Types (e.g. they are CEW Types). This approach is useful because it returns a 
# dataframe that contains ONLY types that went into the MCDs, which you may 
# want to analyze using using other methods.
#
# Two arguments 1. unitData: dataframe with counts of ware types in units
# the left variable IDs the units, while the rest of the varaibles are types
# 2. typeData: a dataframe with at least two variables named 'midPoint' and 
# 'inversevar' containing the manufacturing midpoints and inverse variances 
# for the types.
# returns a list comprised of two dataframes:
# unitDataWithDates has units with types with dates
# typeDataWithDates has the types with dates
RemoveTypesNoDates <- function(unitData,typeData){
  #unitData<- WareByUnitT1
  #typeData <-MCDTypeTabl
  typesWithNoDates <- typeData$Ware[(is.na(typeData$midPoint))] 
  # types in the MCD table with no dates.
  moreTypesWithNoDates <- colnames(unitData)[-1][! colnames(unitData)[-1] %in% 
                                                   typeData$Ware] 
  # types in the data that are NOT in the MCD Type table.
  typesWithNoDates <- c(typesWithNoDates, moreTypesWithNoDates)
  unitDataWithDates <- unitData[, ! colnames(unitData) %in%  typesWithNoDates]
  typeDataWithDates <- typeData[! typeData$Ware %in%  typesWithNoDates, ]
  unitDataWithDates <- filter(unitDataWithDates, 
                              rowSums(unitDataWithDates[,2:ncol(unitDataWithDates)])>0)
  return(list(unitData = unitDataWithDates, 
              typeData = typeDataWithDates))
}

dataWithDates <- RemoveTypesNoDates(WareByUnitT1, MCDTypeTable)
dataWithDates


# Define a function that computes MCDs
# Two arguments: 
# 1. unitData: a dataframe with the counts of ware types in units. 
# We assume the first column IDs the units, while the rest of the columns 
# are counts of types.
# 2. typeData: a dataframe with at least two variables named 'midPoint' and 
# 'inversevar' containing the manufacturing midpoints and inverse variances 
# for the types.
# Returns a list comprise of two dataframes: 
#     MCDs has units and the vanilla and BLUE MCDs
#     midPoints has the types and manufacturing midpoints, in the order they 
#     appeared in the input unitData dataframe.  
EstimateMCD<- function(unitData,typeData){
  #unitData <- dataWithDates$unitData
  #typeData <- dataWithDates$typeData
  countMatrix<- as.matrix(unitData[,2:ncol(unitData)])
  originalUnitName <-  colnames(unitData)[1]
  colnames(unitData)[1] <- 'unitID'
  unitID <- (unitData[,1])
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
  # now for the TPQs
  meltedUnitData<- melt(unitData, id.vars = 'unitID',  
                        variable.name = 'Ware', value.name='count')
  meltedUnitData <- subset(meltedUnitData, count > 0) 
  mergedUnitData <- merge(x = meltedUnitData, y = typeData,  
                          by.x='Ware', by.y='Ware')
  # the trick is that to figure out the tpq. it's best to have each record (row) 
  # represent an individual sherd  but in its current state, each record has 
  # a count c:(c > 1). We must generate c records for each original record.
  # Use rep and rownames - rowname is a unique number for each row, kind of 
  # like an index. rep() goes through dataframe mergedUnitData and replicates 
  # based on the count column, i.e. if count is 5 it will create 5 records or 
  # rows and for columns 2 and 6 (2 is unit name and 6 is begin date)
  repUnitData <- mergedUnitData[rep(rownames(mergedUnitData),mergedUnitData$count),c(2,6)]
  # once all the rows have a count of one, then can run the quantile function
  TPQ <- tapply(repUnitData$BeginDate,repUnitData$unit, 
                function(x) quantile(x, probs =1.0, type=3 ))              
  TPQp95 <- tapply(repUnitData$BeginDate,repUnitData$unit, 
                   function(x) quantile(x, probs = .95 , type=3 ))                 
  TPQp90 <- tapply(repUnitData$BeginDate,repUnitData$unit, 
                   function(x) quantile(x, probs = .90,  type=3 ))   
  # Finally we assemble the results in to a list
  MCDs<-data.frame(unitID, MCD, blueMCD, TPQ, TPQp95, TPQp90, sumWts )
  colnames(MCDs)<- c(originalUnitName,'MCD','blueMCD', 'TPQ', 'TPQp95', 'TPQp90', 'Count')
  midPoints <- data.frame(typeNames,mPoint)
  MCDs <- list('MCDs'=MCDs,'midPoints'=midPoints)
  return(MCDs)
} 
#end of function EstimateMCD


# apply the function
MCDByUnit<-EstimateMCD(dataWithDates$unitData, dataWithDates$typeData)

# let's see what it looks like
MCDByUnit


# a function to sort the rows and cols of a matrix based on the
# orders from two arguments (e.g. MCDs and midpoints)
# arguments:  the name of the variable that contains the unit scores (e.g. MCDs)
#             the name of the variable that contains the type score (e.g. the midpoints)
#             the name of the dataframe that contains the counts of ware types in units
# returns:    the sorted dataframe 
sortData<- function(unitScores,typeScores,unitData){
  sortedData<-unitData[order(unitScores),]
  sortedData<-sortedData[,c(1,order(typeScores)+1)]
  return(sortedData)
}


# apply the function
WareByUnitT2Sorted<-sortData(MCDByUnit$MCDs$blueMCD,
                             MCDByUnit$midPoints$mPoint,
                             dataWithDates$unitData)

#### 12.  Make a Ford-style battleship plot  ####
# convert to a matrix, whose cols are the counts
# make the unit name a 'rowname" of the matrix
Mat<-as.matrix(WareByUnitT2Sorted[,2:ncol(WareByUnitT2Sorted)])
rownames(Mat)<-WareByUnitT2Sorted$unit
rSums<- matrix (rowSums(Mat),nrow(Mat),ncol(Mat), byrow=F)
MatProp<-Mat/rSums
# Do the plot
battleship.plot(MatProp,
                mar=c(2,6,10,1),
                main = 'Seriation by Blue MCD',
                xlab='Ware Type',
                ylab= 'Context',
                col='grey')

WareByUnitT2 <- dataWithDates$unitData


#### 13. Now let's try some Correspondence Analysis ####
# You need to decide if you want to use the same data then wn into the MCDs (WareByUnitT2) 
# or the full ware type dataset -- including types with no dates


Matx<-as.matrix(WareByUnitT2[,2:ncol(WareByUnitT2)]) 
rownames(Matx)<-WareByUnitT2$unit


ca1<-ca(Matx)
# Summary(ca1) - inertia plot with broken stick

# put the result in dataframes
inertia <- data.frame('Inertia' = prop.table(ca1$sv^2))
rowScores <- data.frame(ca1$rowcoord, rownames=ca1$rownames)
colScores <- data.frame(ca1$colcoord, rownames=ca1$colnames)


# Compute the broken stick model inertia
broken.stick <- function(p)
  # Compute the expected values of the broken-stick distribution for 'p' pieces.
  # Example: broken.stick.out.20 = broken.stick(20)
  #             Pierre Legendre, April 2007
{
  result = matrix(0,p,2)
  colnames(result) = c("Dim","Expected.Inertia")
  for(j in 1:p) {
    E = 0
    for(x in j:p) E = E+(1/x)
    result[j,1] = j
    result[j,2] = E/p
  }
  result <- result
  return(data.frame(result))
}

bs <- broken.stick(nrow(inertia))



# plot the proportion of inertia
theme_set(theme_classic(base_size = 20))

p <- ggplot(data=inertia , aes(x= 1:length(Inertia), y=Inertia)) +
  # geom_bar(stat="identity", fill="grey") +
  geom_line(col= "cornflower blue", size=1) +
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue") +
  xlab ('Dimension') + 
  ylab( "Proportion of Inertia") +
  ggtitle('CA Scree Plot') +
  geom_line(aes(y = bs[,2], x= bs[,1]), color = "black", linetype = "dashed", 
            size=1)
p


# ggplot version of row scores dim 1 and dim 2
library(ggrepel)
set.seed(42)
p1 <- ggplot(rowScores, aes(x=Dim1,y=Dim2))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  #geom_text(aes(label= rownames(rowscores)),vjust=-.6, cex=5) +
  geom_text_repel(aes(label=rownames(rowScores)), cex = 4) +
  labs(title="Site 6", x="Dimension 1", y="Dimension 2")

p1
#save the plot for website chronology page/presentations
#ggsave("Site 6_Figure1Dim1Dim2_2018cxt.png", p1, width=10, height=7.5, dpi=300)

#ggplot version of col scores dim 1 and dim 2
p2 <- ggplot(colScores, aes(x = Dim1,y = Dim2))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  #geom_text(aes(label=CA_MCD_Phase1$unit),vjust=-.6, cex=5)+
  geom_text_repel(aes(label=rownames(colScores)), cex= 6) +
  labs(title="Site 6", x="Dimension 1", y="Dimension 2") 

p2
#save the plot for website chronology page/presentations
#ggsave("Site 6_Figure2WareTypes_2018cxt.png", p2, width=10, height=7.5, dpi=300)


# sort the data matrix that went into the CA on the dim1 scores and do a 
# battleship plot with the sorted data.
WareByUnitT2Sorted  <- sortData(rowScores$Dim1, colScores$Dim1, WareByUnitT2)
Mat<-as.matrix(WareByUnitT2Sorted[,2:ncol(WareByUnitT2Sorted)])
rownames(Mat)<-WareByUnitT2Sorted$unit
rSums<- matrix (rowSums(Mat),nrow(Mat),ncol(Mat), byrow=F)
MatProp<-Mat/rSums
# Do the plot
battleship.plot(MatProp,
                mar=c(2,4,8,4),
                cex.labels=.8,
                main = 'Seriation by CA Dimension 1',
                xlab='Ware Type',
                ylab= 'Context',
                col='grey')


# finally let's see what the relationship is between MCDs and CA scores

# CA Dim 1 vs. MCDs

#ggplot version of CA Dim 1 vs. MCDs
p3 <- ggplot(rowScores, aes(x=rowScores$Dim1,y=MCDByUnit$MCDs$blueMCD))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  #geom_text(aes(label=CA_MCD_Phase1$unit),vjust=-.6, cex=5)+
  #geom_text_repel(aes(label=rownames(rowscores)), cex=6) +
  labs(title="Site 6", x="Dimension 1", y="BLUE MCD") 
p3 

# save the plot for website chronology page/presentations
# ggsave("Site 6_Dim1BLUEMCD_2018cxt.png", p3, width=10, height=7.5, dpi=300)


p4 <- ggplot(rowScores, aes(x=Dim2,y=MCDByUnit$MCDs$blueMCD))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  #geom_text(aes(label=CA_MCD_Phase1$unit),vjust=-.6, cex=5)+
  # geom_text_repel(aes(label=rownames(rowscores)), cex=6) +
  labs(title="Site 6", x="Dimension 2", y="BLUE MCD") 
p4 
#ggsave("Site 6_Dim2BLUEMCD.png", p4, width=10, height=7.5, dpi=300)


#create table of contexts, counts, and mcds
unit <- MCDByUnit$MCDs$unit
dim1Scores <- ca1$rowcoord[,1]
dim2Scores <- ca1$rowcoord[,2]
MCD<- MCDByUnit$MCDs$MCD
blueMCD <-MCDByUnit$MCDs$blueMCD
count<- MCDByUnit$MCDs$Count

CA_MCD<-data.frame(unit, dim1Scores,dim2Scores,MCD,blueMCD, count) 

# Dim 1 Scores Weighted Histogram, you may need to change scale
dim1ForHist<- data.frame(dim1 = rep(CA_MCD$dim1Scores, CA_MCD$count))
p5 <- ggplot(dim1ForHist, aes(x = dim1)) +
  geom_histogram(aes(y=..density..), colour="black", fill="tan", binwidth=0.1, 
                 boundary= .1) +
   xlim(-9,3)+
  #stat_function(fun = dnorm, colour = "blue")+
  # scale_x_continuous(breaks=seq(-4, 2, 0.5), limits=c(-3.5,3))+
  labs(title="Site 6", x="Dimension 1", y="Density")+
  geom_density(fill=NA)
p5


#Add lines for phase breaks
p5a <- p5 + geom_vline(xintercept=c(-4.1, -2), colour = "gray", linetype = "dashed",
                       size=1)
p5a

# save the plot for website chronology page/presentations
# ggsave("Site6_Histogram.png", p5a, width=10, height=7.5, dpi=300)
# 


# create a vector for the phases with as many entries as assemblages
Phase <- rep(NA, length(rowScores[,1])) 
# do the phase assigments
Phase[rowScores[,1] <= -.4] <- 'P01'  
#Phase[(rowScores[,1] > -.4) & (rowScores[,1]) <= -1.2] <- 'P02'
Phase[rowScores[,1] >  -.4] <- 'P03'

# assemble the results in a dataframe
CA_MCD_Phase <-  cbind(MCDByUnit$MCDs,rowScores[,1:2],Phase)
CA_MCD_Phase


# Compute the MCDS and TPQs for the Phases 
# join the pghases to the ware by unit data
UnitAndPhase <- data.frame(unit, Phase, stringsAsFactors=F )
WareByPhase<- inner_join (WareByUnit, UnitAndPhase, by = 'unit')

# now we transpose the data so that we end up with a context (rows) x type 
# (cols) data matrix; unit ~ ware formula syntax, left side = row, 
# right side = column, to fill in body of table with the counts, 
# fill rest with zeros

WareByPhaseT <- dcast(WareByPhase, Phase ~ Ware,  fun.aggregate=sum, 
                      value.var='count', fill=0 )
dataWithDates <- RemoveTypesNoDates(WareByPhaseT, MCDTypeTable)

# apply the Estimate MCD function
MCDByPhase<-EstimateMCD(dataWithDates$unitData,
                        dataWithDates$typeData)

# let's see what it looks like
MCDByPhase
#Export data
#write.csv(CA_MCD_Phase, file='CA_MCD_Phase_SouthPavilion.csv')

# BlueMCDByDim1 plot
library(viridis)
p6 <- ggplot(CA_MCD_Phase,aes(x = dim1Scores,y = blueMCD, fill= factor(Phase))) +
  scale_y_continuous(limits=c(1760, 1920)) +
  geom_point(shape=21,  alpha = .75, size= 6)  + 
  scale_fill_viridis(discrete= T, name="DAACS Phase",
                     labels=c("P01", "P02", "P03")) + 
  geom_text_repel(aes(label= unit), cex=4) +
  labs(title="Site 6", x="Dimension 1", y="BLUE MCD")
p6


#save the plot for website chronology page/presentations
#ggsave("Site6ByContext.png", p6, width=10, height=7.5, dpi=300)