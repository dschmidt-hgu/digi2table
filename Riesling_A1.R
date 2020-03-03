#setwd("")

##############################################################################
# Source Base Code
##############################################################################
# Set pre-processing code for sourcing
baseCodeFile <- "digi2table_Riesling.R"

# Estimate lines of code to source -- until 'READSOURCEDATABREAK'
#lineN <- as.numeric(system(paste0('grep -n "READSOURCEDATABREAK" ',baseCodeFile,' | grep -o [0-9]*.*[0-9]'),intern=TRUE))
#lineN <- as.numeric(system(paste0('findstr \n "READSOURCEDATABREAK" ',baseCodeFile),intern=TRUE))  #' | grep -o [0-9]*.*[0-9]
# 

# # function to source only n-lines
# source2 <- function(file, start, end, ...) {
#     file.lines <- scan(file, what=character(), skip=start-1, nlines=end-start+1, sep='\n')
#     file.lines.collapsed <- paste(file.lines, collapse='\n')
#     source(textConnection(file.lines.collapsed), ...)
# }
# # Source
# source2(baseCodeFile,1,lineN)

source(baseCodeFile)

##############################################################################
# Load additional packages
##############################################################################
	
usePackage("cowplot")	
usePackage("ggthemes")	
usePackage("geomorph") 
usePackage("pracma")



##############################################################################
# Global Variables
##############################################################################
	
# Colors 
reds <- brewer.pal(9,"Reds")
blues <- brewer.pal(9,"Blues")
	
AEcol <- c(blues[7],reds[6])


##############################################################################
# Modify Plant list [flust]
##############################################################################
	
#flist <- flist[9:10]


##############################################################################
# Plot Plants 2D
##############################################################################
	


dfPlot2d <- rbindlist(lapply(flist,f.dPlantCoords),fill=TRUE)
dfPlot2d$DateXn <- as.numeric(factor(as.numeric(dfPlot2d$Date)))
# dfPlot2d[DateXn%in%1:2, DateX:="1"]  
# dfPlot2d[DateXn%in%3, DateX:="2"]  
# dfPlot2d[DateXn%in%4:6, DateX:="3"] 

#Transformed and aligned x-y-z
#Pointcloud+Path
#Side
pSide <- ggplot(dfPlot2d[oType!="Leaf" & oType!="Flower"], aes(x=x,y=z))+
	geom_point(aes(color=oType))+
	geom_path(aes(color=oType,group=Grp))+
	coord_fixed()+
	facet_grid(Plant~DateXn,labeller = label_both)

pSide

#Back
pBack <- ggplot(dfPlot2d[oType!="Leaf" & oType!="Flower"], aes(x=y,y=z))+
	geom_point(aes(color=oType))+
	geom_path(aes(color=oType,group=Grp))+
	coord_fixed()+
	facet_grid(Plant~DateXn)
	
pBack

# Density 2d
#Side
ggplot(dfPlot2d[oType!="Leaf" & oType!="Flower"], aes(x=x,y=z))+
	geom_density2d(aes(color=Plant))+
	facet_grid(~DateXn)+
	coord_fixed()

#Back
ggplot(dfPlot2d[oType!="Leaf" & oType!="Flower"], aes(x=y,y=z))+
	geom_density2d(aes(color=Plant))+
	facet_grid(~DateXn)+
	coord_fixed()




########################################################################################
# Plot Only Shoots 2d Normalize to Shoot first Node == 000
########################################################################################

dfShoot2d_1 <- dfPlot2d[oType=="sub.Node"]

dfShoot2d_1[,"ShootID" := paste(Plant,Date,Grp,sep="_")]


f.dshoot2d <- function(sID)
{

	ds2d <- dfShoot2d_1[ShootID==sID]

	xyz.norm <- as.data.table(t(t(as.matrix(ds2d[,1:3]))-as.numeric(ds2d[1,1:3])))
	colnames(xyz.norm) <- c("x.Sn","y.Sn","z.Sn")

	ds2d <- cbind(ds2d,xyz.norm)

	return(ds2d)

}

dfPlotShoot2d <- rbindlist(lapply(unique(dfShoot2d_1$ShootID),f.dshoot2d),fill=TRUE)

# Factor for plotting
dfPlotShoot2d[,"Grp":=factor(Grp)]

pSide.S <- ggplot(dfPlotShoot2d, aes(x=x.Sn,y=z.Sn))+
	geom_path(aes(color=Grp,group=Grp),size=1)+
	coord_fixed()+
	facet_grid(Plant~DateXn)

pSide.S

pBack.S <-  ggplot(dfPlotShoot2d, aes(x=y.Sn,y=z.Sn))+
	geom_path(aes(color=Grp,group=Grp),size=1)+
	coord_fixed()+
	facet_grid(Plant~DateXn)


pBack.S






##############################################
# Internode length [Only of primary shoots]
##############################################

# f.ilSubAll <- function(flistID) {

# 	#Get Plant Coordinates
# 	dPlantCoords <- f.dPlantCoords(flistID)

# 	#Function to estimate Internode length from a numerical matrix of 3D coordinates | per "Shoot"
# 	f.ilSub <-  function(grpI) 
# 			{
# 				#Extract data
# 				m <- dPlantCoords[oType=="sub.Node" & Grp==grpI][,1:3]
# 				#Create Matrix
# 				m <- as.matrix(m)
# 				# 
# 				if(nrow(m)==1) #Only one point 
# 					{
# 						out <- data.table(Grp=grpI, IL = 0, Rank = 0)

# 				}else if(nrow(m)==2) #ingle internode Shoots | 2 Points
# 					{
# 						out <- data.table(Grp=grpI, IL = as.numeric(dist(m)))					#dist = euclidean distance
# 						out$Rank <- 1:nrow(out)													#Rank = Number of Internode

# 				}else{ #More than 2 Points ...
# 						out <- data.table(Grp=grpI, IL = diag(as.matrix(dist(m))[-1,-nrow(m)]))  #extract diagonal after removing 1st row and last column
# 						out$Rank <- 1:nrow(out) 												 #Rank = Number of Internode	

# 					}

# 				return(out)
# 			}



# 	#gather data for all shoots
# 	dfILSub <- rbindlist(lapply(unique(dPlantCoords[oType=="sub.Node"]$Grp),f.ilSub))

# 	#Add experimental information
# 	dfILSub <- cbind(dfILSub,dPlantCoords[1,c("Date","Plant","Ring","Elevated")])

# return(dfILSub)
# }



# # Get Internode Data Frame for all Plants of "Primary Shoot" Internodes
# dfILSubAll <-  rbindlist(lapply(flist,f.ilSubAll),fill=TRUE)


# pIL <- ggplot(dfILSubAll, aes(x=Rank, y=IL,color=Plant))+
# 		geom_point()+
# 		geom_line(aes(linetype=Elevated))+
# 		facet_grid(Grp~Date)

# #Mean / Summe

# pIL.box <- ggplot(dfILSubAll, aes(x=Ring, y=IL, fill=Elevated))+
# 				geom_boxplot(position="dodge")+
# 				   facet_grid(.~Date,scales="free_x")
				



##############################################
# Internode length [all levels]
##############################################

f.ilSubAll <- function(flistID) {


	#Get Plant Coordinates
	dPlantCoords <- f.dPlantCoords(flistID)

	#Function to estimate Internode length from a numerical matrix of 3D coordinates
	f.ilSub <-  function(grpI) 
		{
			#Extract data
			m <- dPlantCoords[oType=="sub.Node" & Grp==grpI | oType=="subsub.Node" & Grp==grpI | oType=="subsubsub.Node" & Grp==grpI ][,1:3]
			#Creat Matrix
			m <- as.matrix(m)
			
			#Check if Sub-(Sub)-Node | #wird garnicht weiter verwendet? # todo remove?
			if(length(grep(grpI,pattern = "_",value = TRUE))!=0) #if subsub.node
				{
				 shoot <-  stri_split_regex(grpI,pattern="_",simplify=TRUE)[,1]
				 subNode <- as.numeric(stri_split_regex(grpI,pattern="_",simplify=TRUE)[,2]) 
				 if(ncol(stri_split_regex(grpI,pattern="_",simplify=TRUE))>2){
				 subsubNode <- as.numeric(stri_split_regex(grpI,pattern="_",simplify=TRUE)[,3])
				 }				 
				}
				
			if(nrow(m)==1) #Only one point 
				{
					out <- data.table(Shoot=grpI, IL = 0, Rank = 0)

			}else if(nrow(m)==2) #ingle internode Shoots | 2 Points
				{
					out <- data.table(Shoot=grpI, IL = as.numeric(dist(m)))					#dist = euclidean distance
					out$Rank <- 1:nrow(out)													#Rank = Number of Internode

			}else{ #More than 2 Points ...
					out <- data.table(Shoot=grpI, IL = diag(as.matrix(dist(m))[-1,-nrow(m)]))  #extract diagonal after removing 1st row and last column
					out$Rank <- 1:nrow(out) 												 #Rank = Number of Internode	

				}

			return(out)
		}

	# Apply function to All SubNode and SubSubNode IDs
	# dfILSub <- rbindlist(lapply(unique(dPlantCoords[oType=="sub.Node" | oType=="subsub.Node"]$Grp),f.ilSub))
		dfILSub <- rbindlist(lapply(unique(dPlantCoords[oType=="sub.Node" | oType=="subsub.Node" | oType=="subsubsub.Node"]$Grp),f.ilSub))

	#Internod-Length on Main Shoot
	dfILSub.MAINSHOOT <- data.table(	
										Shoot=0,
										IL=diag(as.matrix(dist(dPlantCoords[!is.na(Order)][,1:3]))[-1,-nrow(dPlantCoords[!is.na(Order)])])[-1], 		#Internodes on Main Shoot
										Rank = dPlantCoords[!is.na(Order)]$Order[-c(1:2)]		#RANK == Shoot
									)

	#Combine Cane + Shoots
	dfILSub <- rbind(dfILSub,dfILSub.MAINSHOOT)


	#Add experimental information
	dfILSub <- cbind(dfILSub,dPlantCoords[1,c("Date","Plant","Ring","Elevated")])


	#Add Parent Shoot
	dfILSub[,"Main":= stri_split_regex(Shoot,pattern="_",simplify=TRUE)[,1]]

	#Add Parent Sub.Shoot (if necessary, else = 0)
	if(ncol(stri_split_regex(dfILSub$Shoot,pattern="_",simplify=TRUE))>1)
		{
			#Add Sub-Main
			dfILSub[,"Sub":=stri_split_regex(Shoot,pattern="_",simplify=TRUE)[,2] ]
			dfILSub[Sub=="", "Sub":= "0"] #fill otheres with 0

			if(ncol(stri_split_regex(dfILSub$Shoot,pattern="_",simplify=TRUE))>2)
			{
				dfILSub[,"SubSub":=stri_split_regex(Shoot,pattern="_",simplify=TRUE)[,3] ]			
				dfILSub[SubSub=="", "SubSub":= "0"] #fill otheres with 0
			}else{
				dfILSub[,"SubSub":= "0"]
			}
		}else{
			dfILSub[,"Sub":= "0"]
			dfILSub[,"SubSub":= "0"]
		}


return(dfILSub)
}

##############################################################################
# OUTPUT 
##############################################################################
	#RANK = Rank at the shoot
	#Shoot_ID  = including sub shoot info (Main_Sub)
	#Main = Main Shoot
	#Sub = Sub Shoot Number at Main Shoot

dfILSubAll <-  rbindlist(lapply(flist,f.ilSubAll)) #,fill=TRUE
dfILSubAll <- dfILSubAll[IL!=0]


# ggplot(dfILSubAll, aes(x=Rank, y=IL,color=Elevated))+
# 		geom_point()+
# 		facet_grid(Plant~Shoot,labeller = label_both)+
# 		scale_colour_manual(values=AEcol)







############################################################################
# Leaf Area Estimation 
# by sum of secondary leaf veins: D_1_4 + D_1_5
# Plots
############################################################################
# usePackage("fs")
# file_show(path("./resources/Riesling_05pt_Paper.pdf"))
#library("rstanarm")
#load(file="./resources/m3.Rdata")
# coef(m3.stan.0)
##        SL 
## 0.6869294 

predictAfromSL <- function(SL) { 
									sqrtA <- 0.6869294*SL
				
								 	A <- sqrtA^2

								 	return(A)
								}

############################################################################

#Function to pre-check Plants for existence of Leaves
check.f.LforA <- function(flistID)
{
	dPlantCoords <- f.dPlantCoords(flistID)

	dL <- dPlantCoords[oType=="Leaf"]

	if(nrow(dL)!=0)
		{
			return(flistID)
		}
		else
		{
			return(NULL)
		}
}

#flist of Plants with leaves
flistLA <- unlist(lapply(X=flist,FUN=check.f.LforA))




#Function to estimate SL's from Plants
f.LforA <- function(flistID)
{
	#Get Plant coordinates
	dPlantCoords <- f.dPlantCoords(flistID)

	#Subset to only leaves
	dL <- dPlantCoords[oType=="Leaf"]

	#Create data frame holding the number of points per digitized leaf
	dtpts <- as.data.table(table(dL$organ_N))
	dtpts[,"ord":= order(unique(dL$organ_N))]	
	dtpts <- dtpts[order(ord)]				#Reorder to match data order in dL

	#create 'Looping' Vector 					
	pts <- dtpts$N 

	#Create PointID sequence
	#ptsSeq <- unlist(lapply(pts, function(i) {c(0:(pts[i]-1))}))
	 ptsSeq <- unlist(lapply(pts, function(i) {c(0:(i-1))}))

	#Assign to data frame
	dL[,"point":= ptsSeq]
	dL[,"pts":= rep(pts,pts)]

	#Extract Point IDs for different leaf complexities that match the SL Points
	L6 <- dL[pts==6 & point%in%c(1,4,5)]
	L10 <- dL[pts==10 & point%in%c(1,4,7)]
	L10[point==7,  "point":= 5]
	L18 <- dL[pts==18 & point%in%c(1,6,13)]
	L18[point==6,  "point":= 4]
	L18[point==13, "point":= 5]

	#Merge all extraction into 1 data table
	LforA <- rbind(L6,L10,L18)

	#measure distances
	mD <- as.matrix(dist(as.matrix(LforA[,1:3],method="euclidean")))

	#Extract every "third" row --> Distances to "1"
	mD1er <- mD[,seq(1,ncol(mD),by=3)]

	#Creat Empty Matrix
	tmp  <- matrix(vector(),ncol=2,ncol(mD1er))

	#Extract for each leaf [col in mD1er]
		# the corresponding rows
		# 1+(i-1)*3+(1:2) : 
			#i=1 >> 2+3
			#i=2 >> 5+6
	for(i in 1:ncol(mD1er))
	{
		tmp[i,] = mD1er[1+(i-1)*3+(1:2),i]
	}


	#Remove duplicated Rows
	LforA <- LforA[seq(1,nrow(LforA),by=3)]

	#Add Distances to Data Frame of Leaves
	LforA <- cbind(LforA,tmp)
	colnames(LforA)[(ncol(LforA)-1):ncol(LforA)] <- c("LL","LR")   #Leaft secondary vein, right secondary vein

	#Create Sum of Secondary Leaf Veins
	LforA[,"SL":=LL+LR]  

	return(LforA)
}

#only perform on plants with leaves
dLforA <- rbindlist(lapply(flistLA,f.LforA))




# #M3
#dLforA$A.mean <- as.numeric(colMeans(posterior_predict(m3.stan.0,newdata = data.frame(SL=dLforA$SL)))^2)
dLforA[,"A.mean":= predictAfromSL(SL)]


dLforA[,"Shoot":= as.numeric(stri_split_regex(organ_N, pattern="[_]",simplify = TRUE)[,2])    ]
dLforA[,"Rank":= as.numeric(stri_split_regex(organ_N, pattern="[_]",simplify = TRUE)[,3])	  ]
dLforA[,"SubShoot":= as.numeric(stri_split_regex(organ_N, pattern="[_]",simplify = TRUE)[,4]) ] #warning if not existent => NA

pAL <- ggplot(dLforA[is.na(SubShoot)], aes(x=Rank, y=A.mean,color=Plant))+
	#geom_ribbon(aes(ymin=A.HPDI95.low,ymax=A.HPDI95.high),alpha=0.5, color="grey")+
	geom_point()+
	geom_line(aes(linetype=Elevated))+
	facet_grid(Shoot~Date)



#Mean / Summe

pAL.box <- ggplot(dLforA[is.na(SubShoot)], aes(x=Ring, y=A.mean, fill=Elevated))+
				geom_boxplot(position="dodge")+
				 facet_grid(.~Date,scales="free_x")





#TODO Go parallel | only for plants with leaves
# allPlant <- rbindlist(lapply(flistLA,f.dPlantCoords),fill=TRUE)

# leaf.Coords <- allPlant[oType=="Leaf"]
# leaf.Coords[,"LID":=paste(Date,Plant,organ_N,sep="_")]
