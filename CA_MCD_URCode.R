# wareTypeCAandMCD.R
# Created by:  FDN  8.5.2014
# Last update: FDN 8.5.2014  
# Edited by:   LAB 1.17.2017 for Morne Patate
# Edited by:   LC 12.4.2017 Hermitage phases
# Edited by:   FDN 12.21.2018 more tidy; fixed MCD function to handle phases.
# Edited by:   CP and EB 3.18.2018 Comments and Section editing for UR code

# load the libraries
library(RPostgreSQL)
library(dplyr)
library(tidyr)
library(reshape2)
library (ca)
library (plotrix)
library(ggplot2)
library(viridis)

# Establish a DBI connection to DAACS PostgreSQL database and submit SQL queries

#Link to file with database password
source("credentials.R")


#### 1. get the table with the ware type date ranges ####
# get the table with the ware type date ranges
MCDTypeTable<- dbGetQuery(DRCcon,'
                          SELECT * 
                          FROM "tblCeramicWare"
                          ')

#### 2. submit a SQL query: note the use of \ as an escape sequence ####
# submit a SQL query: note the use of \ as an escape sequence
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
                         INNER JOIN  "public"."tblProject" ON "public"."tblProject"."ProjectNameID" = 
                         "public"."tblProjectName"."ProjectNameID"
                         INNER JOIN  "public"."tblContext" ON "public"."tblContext"."ProjectID" = 
                         "public"."tblProject"."ProjectID"
                         LEFT JOIN "public"."tblContextDepositType" ON "public"."tblContext"."DepositTypeID" = 
                         "public"."tblContextDepositType"."DepositTypeID"
                         LEFT JOIN "public"."tblContextFeatureType" ON "public"."tblContext"."FeatureTypeID" = 
                         "public"."tblContextFeatureType"."FeatureTypeID" 
                         INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" =
                         "public"."tblContext"."ContextAutoID"
                         INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" =
                         "public"."tblGenerateContextArtifactID"."ContextSampleID"
                         INNER JOIN "public"."tblCeramic" ON "public"."tblCeramic"."GenerateContextArtifactID" =
                         "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
                         INNER JOIN "public"."tblCeramicWare" ON "public"."tblCeramic"."WareID" =
                         "public"."tblCeramicWare"."WareID"
                         LEFT JOIN "public"."tblCeramicGenre" ON "public"."tblCeramic"."CeramicGenreID" =
                         "public"."tblCeramicGenre"."CeramicGenreID"
                         LEFT JOIN "public"."tblCeramicCEWType" ON "public"."tblCeramic"."CeramicCEWTypeID" =
                         "public"."tblCeramicCEWType"."CeramicCEWTypeID"
                         WHERE                     
                         "public"."tblContext"."ProjectID" = \'1243\'
                         ')             


# do a summary
summary1 <- wareTypeData %>%
  group_by(ProjectName,ProjectID,Ware) %>% 
  summarise(count = sum(Quantity))
options(tibble.print_min=100)
summary1


#### 3. Customizations to the Ware Type dates or names####
#3.1 For example, change beginning and end dates for French CEW to 1675-1900
#MCDTypeTable <- MCDTypeTable %>% 
#  mutate(BeginDate = replace(BeginDate, Ware %in% c('French Coarse Earthenware',
#                                                    'Vallauris',
#                                                    'Saintonge',
#                                                    'Huveaune'), 1675),
#         EndDate = replace(EndDate, Ware %in% c('French Coarse Earthenware',
#                                                'Vallauris',
#                                                'Saintonge',
#                                                'Huveaune'),1900))

# Set a minimal start date for all types - this may bite you later if new start 
# dates are >= end dates
#MCDTypeTable <- MCDTypeTable %>% 
#  mutate(BeginDate = ifelse(BeginDate < 1675, 1675, BeginDate)) 

#3.2 Replace ware types with CEWs Types if applicable
#For example, replace 'Caribbean CEW' ware type with applicable CEW Type
#wareTypeData <- mutate(wareTypeData, Ware= ifelse((Ware %in% 
#                                                     c('Caribbean Coarse Earthenware, unid.',
#                                                       'Caribbean Coarse Earthenware, wheel thrown',
#                                                       'Caribbean Coarse Earthenware, hand built')), CeramicCEWType, 
#                                                  Ware))

# Replace Unid. Carib CEW ware type with 'Caribbean CEW Unid.'
#wareTypeData <- mutate(wareTypeData, Ware=ifelse(Ware == 'Unidentifiable', 
#                                                 'Carribean CEW Unid.', Ware))

#3.3. Do a quick summary of the new ware type totals if you run either of the sections above.
#summary2 <- wareTypeData %>% 
#  group_by(ProjectName, ProjectID, Ware) %>% 
#  summarise(count = sum(Quantity))
#summary2


#### 4. Compute new numeric date variables from original ones #### 
# Needed  to compute the MCDs.
MCDTypeTable <- MCDTypeTable %>% 
  mutate(midPoint = (EndDate+BeginDate)/2,
         span = (EndDate - BeginDate),
         inverseVar = 1/(span/6)^2 
  )

#### 5. Here you have the option to remove contexts with deposit type Cleanup and Surface Collection ####
wareTypeData <- subset(wareTypeData, ! wareTypeData$DepositType  %in%  c('Clean-Up/Out-of-Stratigraphic Context',
                                                                         'Surface Collection'))


#### 6. Create the UNIT Variable ####
# The UNIT variable contains the level at which assemblages are aggregated in 
# the analysis. You will need to cutomize this logic for YOUR site. Note that we create a new dataframe: wareTypeData_Unit.
# First some housekeeping so we do not stumble on the confusion between R's NA 
# (SQL NULLs) and blanks. 
wareTypeData$DAACSStratigraphicGroup[is.na(wareTypeData$DAACSStratigraphicGroup)] <- ''
wareTypeData$FeatureNumber[is.na(wareTypeData$FeatureNumber)] <- ''
wareTypeData$QuadratID[is.na(wareTypeData$QuadratID)] <- '' 

# 6.1 
# Use case_when to cycle through QuadID, Feature, SG, and Context to assign 
# aggregration unit. 
wareTypeData_Unit <- wareTypeData %>%  
  mutate( unit = case_when(
    FeatureNumber == "" & DAACSStratigraphicGroup == "" 
    ~ paste(Context),
    FeatureNumber == "" & DAACSStratigraphicGroup != "" 
    ~ paste(DAACSStratigraphicGroup),
    FeatureNumber != "" & DAACSStratigraphicGroup == ""
    ~ paste(FeatureNumber, Context, sep= '.'),
    FeatureNumber != "" & DAACSStratigraphicGroup != "" 
    ~ paste(FeatureNumber,DAACSStratigraphicGroup, sep='.')
  )) 

## 6.2 Use this to assign ContextID to the unit. 
#wareTypeData_Unit <- wareTypeData %>%  
# mutate(unit = wareTypeData$Context)

## 6.3 Use this to assign Quadrat to the unit. 
#wareTypeData_Unit <- wareTypeData %>%  
#mutate( unit = case_when(
#QuadratID == "" ~ paste(Context)
##QuadratID != "" ~ paste(QuadratID)

# Check on the content of the unit variable to make sure all is cool.
table(wareTypeData_Unit$unit)


#### 7. Transpose the data for the MCD and CA ####
wareByUnitT <- wareTypeData_Unit %>% group_by(Ware,unit) %>% 
  summarise(count = sum(Quantity)) %>%
  spread(Ware, value=count , fill=0 )


#### 8. Remove specific ware types (if you must) and set sample size cut off  ####
# 8.1 It is possible at the point to drop types you do not want in the MCD computations
# But it is not clear why one would want to do this. Here we name the types we do NOT
# want included:
wareByUnitT1 <- wareByUnitT %>% dplyr::select(- 'Nottingham', 
                                              - 'Refined Earthenware, modern',
                                              - 'American Stoneware',
                                              - 'Refined Stoneware, unidentifiable',
                                              - 'Fulham Type',
                                              - 'Saintonge',
                                              - 'Astbury Type', 
                                              - 'White Salt Glaze',
                                              - 'Delftware, Dutch/British')

#another way of doing this -- needed?
#badVars <- c('Nottingham', 
#             'Refined Earthenware, modern',
#             'American Stoneware',
#             'Refined Stoneware, unidentifiable',
#             'Fulham Type',
#             'Saintonge',
#             'Astbury Type', 
#             'White Salt Glaze')

#goodVars <- !names(wareByUnitT) %in% badVars
#wareByUnitT1 <- wareByUnitT[,goodVars] 




# 8.2  We may also want to enforce a sample size cut off on the MCD analysis.
# MCDs and TPQs are more reliable with larger samples, but may be in 
# useful in small ones. DAACS standard is N > 5.
# Note the use of column numbers as index values to get the type counts, which 
# are assumed to start in col 2.
wareByUnitTTotals<- rowSums(wareByUnitT1[,-1])
table(wareByUnitTTotals)
wareByUnitT1 <-wareByUnitT1[wareByUnitTTotals > 5,]
# And get rid of any types that do not occur in the subset of assemblages that 
# DO  meet the sample size cutoff
wareByUnitT2 <- wareByUnitT1[,c(T,colSums(wareByUnitT1[,-1])
                                > 0)]




#### 9. Define functions to Remove Types w/o Dates and then compute MCDs ####
# 9.1 We build a function that removes types with no dates, either because they 
# have no dates in the MCDTypeTable or because they are not MCD Ware 
# Types (e.g. they are CEW Types). This approach is useful because it returns a 
# dataframe that contains ONLY types that went into the MCDs, which you may 
# want to analyze using using other methods (e.g. CA).
# Two arguments: 
#   unitData: dataframe with counts of ware types in units
#   the left variable IDs the units, while the rest of the varaibles are types
#   typeData: a dataframe with at least three variables named 'Ware', 'midPoint'
#   and 'inversevar' containing the manufacturing midpoints and inverse 
#   variances for the types.
# Returns a list comprised of two dataframes:
#   unitDataWithDates has units with types with dates
#   typeDataWithDates has the types with dates
RemoveTypesNoDates <- function(unitData,typeData){
  #unitData<- WareByUnitT1
  #typeData <-MCDTypeTable
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

# run the function
dataForMCD <- RemoveTypesNoDates(wareByUnitT2 , MCDTypeTable)


# Define a function that computes MCDs
# Two arguments: 
#   unitData: a dataframe with the counts of ware types in units. 
#   We assume the first column IDs the units, while the rest of the columns 
#   are counts of types.
#   typeData: a dataframe with at least two variables named 'midPoint' and 
#   'inversevar' containing the manufacturing midpoints and inverse variances 
#   for the types.
# Returns a list comprise of two dataframes: 
#     MCDs has units and the vanilla and BLUE MCDs
#     midPoints has the types and manufacturing midpoints, in the order they 
#     appeared in the input unitData dataframe.  
EstimateMCD<- function(unitData,typeData){
  countMatrix<- as.matrix(unitData[,2:ncol(unitData)])
  originalUnitName <-  colnames(unitData)[1]
  colnames(unitData)[1] <- 'unitID'
  unitID <- (unitData[,1])
  unitID[is.na(unitID)] <-'Unassigned'
  unitData[,1] <- unitID
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
  meltedUnitData <- gather(unitData, key = Ware, value=count,- unitID)
  meltedUnitData <- filter(meltedUnitData, count > 0) 
  mergedUnitData <- inner_join(meltedUnitData, typeData, by='Ware')
  # the trick is that to figure out the tpq. it's best to have each record (row) 
  # represent an individual sherd  but in its current state, each record has 
  # a count c:(c > 1). We must generate c records for each original record.
  # Use rep and rownames - rowname is a unique number for each row, kind of 
  # like an index. rep() goes through dataframe mergedUnitData and replicates 
  # based on the count column, i.e. if count is 5 it will create 5 records or 
  # rows and for columns 1 and 6 (col 1 is unit name and 6 is begin date.
  repUnitData <- mergedUnitData[rep(rownames(mergedUnitData ),mergedUnitData$count),c(1,6)]
  # once all the rows have a count of one, we run the quantile function
  TPQ <- tapply(repUnitData$BeginDate,repUnitData$unitID, 
                function(x) quantile(x, probs =1.0, type=3 ))              
  TPQp95 <- tapply(repUnitData$BeginDate,repUnitData$unitID, 
                   function(x) quantile(x, probs = .95 , type=3 ))                 
  TPQp90 <- tapply(repUnitData$BeginDate,repUnitData$unitID, 
                   function(x) quantile(x, probs = .90,  type=3 ))   
  # Finally we assemble the results in to a list
  MCDs<-data.frame(unitID, MCD, blueMCD, TPQ, TPQp95, TPQp90, sumWts )
  colnames(MCDs)<- c(originalUnitName,'MCD','blueMCD', 'TPQ', 'TPQp95', 'TPQp90', 'Count')
  midPoints <- data.frame(typeNames,mPoint)
  MCDs <- list('MCDs'=MCDs,'midPoints'=midPoints)
  return(MCDs)
} 
# end of function EstimateMCD

# apply the function
MCDByUnit<-EstimateMCD(dataForMCD$unitData, dataForMCD$typeData)

# let's see what it looks like
MCDByUnit

#### 10. Seriation diagram based on MCDs ####
# First define a  function to sort the rows and cols of a matrix based on the
# orders from two arguments (e.g. MCDs and midpoints)
# arguments:  the name of the variable that contains the unit scores (e.g. MCDs)
#             the name of the variable that contains the type score (e.g. 
#               the midpoints) 
#             the name of the dataframe that contains the counts of ware types 
#               in units
# returns:    the sorted dataframe 
sortData<- function(unitScores,typeScores,unitData){
  sortedData<-unitData[order(unitScores, decreasing=T),]
  sortedData<-sortedData[,c(1,order(typeScores)+1)]
  return(sortedData)
}

# apply the function
unitDataSorted <-sortData(MCDByUnit$MCDs$blueMCD,
                          MCDByUnit$midPoints$mPoint,
                          dataForMCD$unitData)



#### 11.  Ford-style battleship plot  ####
# convert to a matrix, whose cols are the counts
# make the unit name a 'rowname' of the matrix
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
# You need to decide if you want to use exactly the same data that
# went into the MCD analysis (dataForMCD$unitData), or the 
# data with all the ware types. To chose, commment out one of these two lines:
wareByUnitT_forCA <- wareByUnitT1 # use all the data
# wareByUnitT_forCA <- dataForMCD$unitData # use ONLY the data used for MCDs

# 13.1 USE THIS SECTION AFTER AN INITIAL CA RUN TO remove types and units from the analysis that are outliers

# Remove types
# wareByUnitT_forCA <- wareByUnitT_forCA %>% select( 
#     - 'Nottingham', 
#     - 'Refined Earthenware, modern',
#     - 'American Stoneware',
#     - 'Refined Stoneware, unidentifiable',
#     - 'Fulham Type',
#     - 'Saintonge',
#     - 'Astbury Type', 
#     - 'White Salt Glaze'
# )

# Remove units
# wareByUnitT_forCA <- wareByUnitT_forCA %>% filter(
#  unit != '062.5364')

#Run the CA
matX <- as.matrix(wareByUnitT_forCA[,-1]) 
rownames(matX) <- wareByUnitT_forCA$unit

ca1<-ca(matX)
# Summary(ca1) - inertia plot with broken stick

# put the result in dataframes
inertia <- data.frame('Inertia' = prop.table(ca1$sv^2))
############ fix rownames?? ################

rowScores <- data.frame(ca1$rowcoord[,1:5], unit =ca1$rownames)
colScores <- data.frame(ca1$colcoord[,1:5], type =ca1$colnames)


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
  labs( title="Morne Patate Village", x="Dimension", y='Porportion of Inertia' ) +
  geom_line(aes(y = bs[,2], x= bs[,1]), color = "black", linetype = "dashed", 
            size=1)
p


# ggplot version of row scores dim 1 and dim 2
library(ggrepel)
set.seed(42)
p1 <- ggplot(rowScores, aes(x=Dim1,y=Dim2))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  # geom_text(aes(label= unit,vjust=-.6, cex=5) +
  geom_text_repel(aes(label= unit), cex = 4) +
  labs(title="Morne Patate Village", 
       x = paste ("Dimension 1",":  ", round(inertia[1,]*100),'%', sep=''), 
       y= paste ("Dimension 2",":  ", round(inertia[2,]*100),'%', sep='')
  )
p1
#save the plot for website chronology page/presentations
#ggsave("Site 6_Figure1Dim1Dim2_2018cxt.png", p1, width=10, height=7.5, dpi=300)

#ggplot version of col scores dim 1 and dim 2
p2 <- ggplot(colScores, aes(x = Dim1,y = Dim2))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  #geom_text(aes(label= type),vjust=-.6, cex=5)+
  geom_text_repel(aes(label=type), cex= 3) +
  labs(title="Morne Patate Village", 
       x = paste ("Dimension 1",":  ", round(inertia[1,]*100),'%', sep=''), 
       y= paste ("Dimension 2",":  ", round(inertia[2,]*100),'%', sep='')
  )  
p2
#save the plot for website chronology page/presentations
#ggsave("Site 6_Figure2WareTypes_2018cxt.png", p2, width=10, height=7.5, dpi=300)


# sort the data matrix that went into the CA on the dim1 scores and do a 
# battleship plot with the sorted data.
wareByUnitT_Sorted  <- sortData(rowScores$Dim1, colScores$Dim1, wareByUnitT_forCA)
Mat<-as.matrix(wareByUnitT_Sorted[,-1])
rownames(Mat)<-wareByUnitT_Sorted$unit
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

#### 14. Compare MCD and CA dim scores
# create a data frame of units, counts, and mcds
CA_MCD <- inner_join(MCDByUnit$MCDs, rowScores, by='unit' )

# Plot CA Dim 1 vs. MCDs
#ggplot version of CA Dim 1 vs. MCDs
p3 <- ggplot(CA_MCD, aes(x=Dim1,y=blueMCD))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  #geom_text(aes(label=unit),vjust=-.6, cex=5)+
  geom_text_repel(aes(label=unit), cex=6) +
  labs(title="Morne Patate Village", 
       x="Dimension 1", 
       y="BLUE MCD") 
p3 
# save the plot for website chronology page/presentations
# ggsave("Site 6_Dim1BLUEMCD_2018cxt.png", p3, width=10, height=7.5, dpi=300)

p4 <- ggplot(CA_MCD, aes(x = Dim2,y = blueMCD))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  #geom_text(aes(label=unit),vjust=-.6, cex=5)+
  geom_text_repel(aes(label=unit), cex=6) +
  labs(title="Morne Patate Village", 
       x="Dimension 2", 
       y="BLUE MCD") 
p4 
#ggsave("Site 6_Dim2BLUEMCD.png", p4, width=10, height=7.5, dpi=300)

#### 15. Histogram of Dim 1 scores for Phasing
# Dim 1 Scores Weighted Histogram, you may need to change scale
dim1ForHist<- data.frame(dim1 = rep(CA_MCD$Dim1, CA_MCD$Count))
p5 <- ggplot(dim1ForHist, aes(x = dim1)) +
  geom_histogram(aes(y=..density..), colour="black", fill="tan", binwidth=0.2, 
                 boundary= .1) +
  scale_x_continuous(breaks=seq(- 8 , 4, 2))+
  labs(title="Morne Patate Village", x="Dimension 1", y="Density") +
  geom_density(fill=NA)
p5


# Add lines for phase breaks
p5a <- p5 + geom_vline(xintercept=c(- 3, 0 ), colour = "gray", linetype = "dashed",
                       size=1)
p5a

#### 16.  Do the Dim 1 -  MCD scatterplot with Phase assignments  
# Do the Phase assigments, based on the Dim1 scores
CA_MCD_Phase <- CA_MCD %>% mutate( Phase = case_when (Dim1 <= -3 ~ 'P01',
                                                      (Dim1 > -3) & 
                                                        (Dim1 <= 0) ~ 'P02',
                                                      Dim1 > 0 ~ 'P03'
))

# BlueMCD By Dim1 plot by Phase
# This one uses DAACS Website colors 

p6 <- ggplot(CA_MCD_Phase,aes(x = Dim1, y = blueMCD, 
                              fill= Phase)) +
  #scale_y_continuous(limits=c(1750, 1950)) +
  geom_point(shape=21,  alpha = .75, size= 6)  + 
  scale_fill_manual(name="DAACS Phase",
                    labels=c("P01", "P02", "P03"),
                    values=c("skyblue", "blue", "darkblue")) + 
  geom_text_repel(aes(label= unit), cex=4) +
  labs(title="Morne Patate Village", x="Dimension 1", y="BLUE MCD")
p6

# And here we use viridis colors (for color blind)
p6 <- ggplot(CA_MCD_Phase,aes(x = Dim1,y = blueMCD, fill= factor(Phase))) +
  #scale_y_continuous(limits=c(1760, 1920)) +
  geom_point(shape=21,  alpha = .75, size= 6)  + 
  scale_fill_viridis(discrete= T, name="DAACS Phase",
                     labels=c("P01", "P02", "P03")) + 
  #geom_text_repel(aes(label= unit), cex=4) +
  labs(title="Morne Patate Village", x="Dimension 1", y="BLUE MCD")
p6

# ggsave("MPvillage_Dim1MCDcolor_2018.png", p6, width=10, height=7.5, dpi=300)

##### 17. Compute the MCDs and TPQs for the phases

# join the Phases to the ware by unit data
unitPhase <- select(CA_MCD_Phase, unit, Phase) 

wareByUnit_Phase<- left_join (wareTypeData_Unit, unitPhase, by = 'unit') %>%
  mutate(Phase = ifelse(is.na(Phase),'',Phase))



# Compare assigned DAACS phases to the ones we came up with above
# looks lilke there are diffrences!!!!
with(wareByUnit_Phase, table(DAACSPhase, Phase))


# Transpose the data for the MCD and CA ####
wareByPhaseT <- wareByUnit_Phase %>% group_by(Ware, Phase) %>% 
  summarise(count = sum(Quantity)) %>%
  spread(Ware, value=count , fill=0 )


dataForMCD_Phase <- RemoveTypesNoDates(wareByPhaseT, MCDTypeTable)

# apply the Estimate MCD function
MCDByPhase<-EstimateMCD(dataForMCD_Phase$unitData,
                        dataForMCD_Phase$typeData)

# let's see what it looks like
MCDByPhase

#### 18. Write out .csv contexts and phase assignments for DAACS database #####

phaseAssignments <- select(wareByUnit_Phase, ProjectName, ProjectID, Context, Phase)
phaseAssignments <- unique(phaseAssignments)


write.csv(phaseAssignments, file='ContextPhases_SITENAME.csv')

