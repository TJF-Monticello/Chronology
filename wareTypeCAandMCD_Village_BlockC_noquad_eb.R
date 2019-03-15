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
library(tidyr)
library(reshape2)
library (ca)
library (plotrix)
library(ggplot2)
library(viridis)

#Link to file with database password
pgSQL <- dbDriver("PostgreSQL")
# establish the connection
DRCcon<-dbConnect(pgSQL, host='drc.iath.virginia.edu', port='5432',
                  dbname='daacs-production',
                  user='drcquery', password='!queryacct!')


#### 1. get the table with the ware type date ranges ####
# get the table with the ware type date ranges
MCDTypeTable<- dbGetQuery(DRCcon,'
                          SELECT * 
                          FROM "tblCeramicWare"
                          ')

#### 2. submit a SQL query: note the use of \ as an escape sequence####
# submit a SQL query: note the use of \ as an escape sequence
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
                         WHERE 
                         "public"."tblContext"."ProjectID" = \'106\'
                         ')


# do a summary
summary1 <- wareTypeData %>%
  group_by(ProjectName,ProjectID,Ware) %>% 
  summarise(count = sum(Quantity))
options(tibble.print_min=100)
summary1


#### 3. Any customizations to the Ware Type dates should go here.... ####
# This is typically used when a ceramic type has a long date range, but you know the site
# was occupied starting at a certain date. 
# Change dates for French CEW to 1675-1900
#MCDTypeTable <- MCDTypeTable %>% 
#  mutate(BeginDate = replace(BeginDate, Ware %in% c('French Coarse Earthenware',
#                                                    'Vallauris',
#                                                    'Saintonge',
#                                                    'Huveaune'), 1675),
# EndDate = replace(EndDate, Ware %in% c('French Coarse Earthenware',
#                                                'Vallauris',
#                                                'Saintonge',
#                                                'Huveaune'),1900))

# Set a minimal start date for all types - this may bite you later if new start 
# dates are >= end dates
# MCDTypeTable <- MCDTypeTable %>% 
# mutate(BeginDate = ifelse(BeginDate < 1675, 1675, BeginDate)) 


# Replace 'Caribbean CEW' ware type with applicable CEW Type
#wareTypeData <- mutate(wareTypeData, Ware= ifelse((Ware %in% 
#                                                     c('Caribbean Coarse Earthenware, unid.',
#                                                       'Caribbean Coarse Earthenware, wheelthrown',
#                                                       'Caribbean Coarse Earthenware, hand built')), CeramicCEWType, 
#                                                  Ware))

# Replace Unid. Carib CEW ware type with 'Caribbean CEW Unid.'
#wareTypeData <- mutate(wareTypeData, Ware=ifelse(Ware == 'Unidentifiable', 
#                                                 'Carribean CEW Unid.', Ware))

#### #### 4. Compute new numeric date variables from original ones #### 
# Needed  to compute the MCDs.
MCDTypeTable <- MCDTypeTable %>% 
  mutate(midPoint = (EndDate+BeginDate)/2,
         span = (EndDate - BeginDate),
         inverseVar = 1/(span/6)^2 
  )

#### 5. Here you have the option to remove contexts with deposit type Cleanup and Surface Collection ####
wareTypeData <- subset(wareTypeData, ! wareTypeData$DepositType  %in%  c('Clean-Up/Out-of-Stratigraphic Context',
                                                                         'Surface Collection'))


#Do a quick summary of the new ware type totals #
summary2 <- wareTypeData %>% 
  group_by(ProjectName, ProjectID, Ware) %>% 
  summarise(count = sum(Quantity))



#### 6. Create the UNIT Variable ####
# The UNIT variable contains the level at which assemblages are aggregated in the analysis.
# some housekeeping so we do not stumble on the confusion between R's NA (SQL NULLs) and blanks. 
# 6.1 Use this section to assign SG and Feature to the unit. 
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

## 6.2 Use this section to assign Context to the unit. 
wareTypeData_Unit <- wareTypeData %>%  
  mutate(unit = wareTypeData$Context)

## 6.3 Use this section to assign Quadrat to the unit. 
#wareTypeData_Unit <- wareTypeData %>%  
#mutate( unit = case_when(
#QuadratID == "" ~ paste(Context)
##QuadratID != "" ~ paste(QuadratID)

# Check on the content of the unit variable
#table(wareTypeData_Unit$unit)


#### 7. Remove specific contexts, groups of contexts, and ware types as needed ... ####
#Note that if you used Step 7 to create an aggregation unit you may need to change the dataframe name that is used in this step to the dataframe name created in that step, e.g. wareTypeData_Unit
# Remove contexts and quadrats
#wareTypeData1 <- filter(wareTypeData, ! wareTypeData$unit   %in% 
#                          c('quadratnumberhere', 'contextnumberhere'))
#### 9. Tranpose the data for the MCD and CA ####
#Note that depending on what you removed in step 8 you may need to change the dataframe name that is used
#for the group_by and sum from wareTypeData1 to wareTypeData2. 
#now we transpose the data so that we end up with a context (rows) x type 
#(cols) data matrix; unit ~ ware formula syntax, left side = row, right side
# = column, to fill in body of table with the counts, fill rest with zeros
wareByUnitT <- wareTypeData_Unit %>% group_by(Ware,unit) %>% 
  summarise(count = sum(Quantity)) %>%
  spread(Ware, value=count , fill=0)

# 10  We may also want to enforce a sample size cut off on the MCD analysis.
# MCDs and TPQs are more reliable with larger samples, but may be in 
# useful in small ones. DAACS standard is N > 5.
# Note the use of column numbers as index values to get the type counts, which 
# are assumed to start in col 2.
wareByUnitTTotals<- rowSums(wareByUnitT[,-1])
table(wareByUnitTTotals)
wareByUnitT1 <-wareByUnitT[wareByUnitTTotals > 5,]
# And get rid of any types that do not occur in the subset of assemblages that 
# DO  meet the sample size cutoff
wareByUnitT2 <- wareByUnitT1[,c(T,colSums(wareByUnitT1[,-1])
                                > 0)]


#### 10. Define functions to Remove Types w/o Dates and then compute MCDs ####
# 10.1 We build a function that removes types with no dates, either because they 
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
dataForMCD <- RemoveTypesNoDates(wareByUnitT2, MCDTypeTable)


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
