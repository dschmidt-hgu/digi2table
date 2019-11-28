#setwd("")

#TODO Rename FILES TO MATCH THIS FORMAT! of 2 digits after the -
#TODO #IDValsTable$Sub_0 = leaf = for flistID[5] >> ERROR LEAF wrongly assigned to a Main Node 


##############################################################################
# Function: Automatic installation of R-Packages
##############################################################################

usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE, repos="https://cloud.r-project.org/", lib=.libPaths()[1])
  require(p, character.only = TRUE)
}

'%!in%' <- function(x,y)!('%in%'(x,y))

##############################################################################
# Packages
##############################################################################


usePackage("data.table")
usePackage("rgl")		
usePackage("rglwidget")
usePackage("ggplot2")		
usePackage("cowplot")	
usePackage("ggthemes")	
usePackage("stringi")
usePackage("RColorBrewer")
usePackage("GGally") #devtools::install_github("ggobi/ggally#266")
usePackage("geomorph") 
usePackage("pracma") 

##############################################################################


#Identify Digitization txt files in DATE Folders
	#store full path not only filenames
	#could be "simplified" to a single command if no other/invalid "txt" are located 'close by'
	#Filename must begin with G or ... KLBDE and end with .txt

flist <- list.files("./20190517/", pattern="^[GKLBDE].*.txt$",full.names=TRUE) 

#Add more folders
# flist <- c(flist,list.files("./20190604/", pattern="^[GKLBDE].*.txt$",full.names=TRUE)) 




##################################
#Exclude  files - if necessary --- [-2,_2,_3]
##################################
excludeflist <- grep(x = flist,pattern="-2.txt")
excludeflist2 <- grep(x = flist,pattern="_2.txt")
excludeflist3 <- grep(x = flist,pattern="_3.txt")

if(sum(excludeflist,excludeflist2,excludeflist3)>0)
	{
	#remove unnecessary files
	flist <- flist[-c(excludeflist,excludeflist2,excludeflist3)]
	}else{
	#do nothing
	}

##########################################################################################################################
# Triangle Definitions | for Leaves
##########################################################################################################################
# Noted as
# Columns = Leaf half
# rows/triples = point
#################################
# Ends up in a 3 column matrix 

#Leaf with 17pts
Tri17 <- 
			matrix(c(
			1,9,10,		1,16,17,
			1,9,8,		1,15,16,
			1,8,6,		1,15,13,
			6,7,8,		13,14,15,
			1,5,6,		1,12,13,
			1,2,5,		1,2,12,
			2,5,4,		2,12,11,
			2,3,4,		2,3,11
			),ncol=3,nrow=8*2,byrow=TRUE)
	
#Leaf with 9pts
Tri9 <- 
			matrix(c(
			1,5,6,		1,8,9,
			1,4,5,		1,7,8,
			1,2,4,		1,2,7,
			2,3,4,		2,3,7
			
			),ncol=3,nrow=4*2,byrow=TRUE)

#Leaf with 5pts
Tri5 <- 
			matrix(c(
			1,2,5,		1,2,4,
			2,3,5,		2,3,4
			
			),ncol=3,nrow=2*2,byrow=TRUE)
	
###############################################################################################################################################
# Colors for 3D Plants
###############################################################################################################################################
#Shades of green
cols <- brewer.pal(9, "Greens")
col.leaf <- cols[6]
col.leaf.small <- cols[5]
col.stem <- cols[9]
col.pet <- cols[8]
col.fru <- cols[2]



#Viewpoint | only for output of 3d plots
uMat <- matrix(c(	0,1,0,0,
					0,0,-1,0,
					-1,0,0,0,
					0,0,0,1),byrow=TRUE,nrow=4)


##############################################################################
# Read Data | FUNCTION 
# Modified: https://stackoverflow.com/questions/12626637/reading-a-text-file-in-r-line-by-line
# Read File and paste into data.table line by line
# Each line becomes a string in a row of the data.table
##############################################################################
#Function to read a text file into a single column data table
processFile = function(filepath) {

#get number of lines 	
  linenumber <- as.numeric(stri_split_fixed(system(paste0("wc -l ",filepath),intern = TRUE), pattern=" ",simplify=TRUE)[,1])
#create empty data table with lines mathching line number
  d <- data.table(f=rep("",times=linenumber))
#Set iterator to 0  
  i=0
#create "connection to file"  
  con = file(filepath, "r")
# Read file line by line and paste into data.table  
  while ( TRUE ) {
    i=i+1
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    d$f[i]=line
  }

#close file connection
  close(con)
#return data table  
  return(d)
}




##############################################################################
# Construct Data table with coordinates and organ typed / levels 
##############################################################################

f.dPlantCoords <- function(flistID){

	# Special treatment for "non normal" cases	
	if(flistID=="./20180608//B39-32.txt")
	{
	#dPlantCoords <- fread("./2018060708_B-parts/dPlantCoords_B39-32-20180607-08.csv")
	}else{

	# txt to data.table strings
	d <- processFile(flistID) 



	# Mark Leaves and Internodes
	# Identify lines including the "Digitization ID" 
	IDLines <- (which(substr(d$f,1,2) == "ID"))		# first 2 letter match == ID

	#Check Levels of ID for different treatments []
	if(ncol(stri_split_regex(unlist(d[IDLines]), pattern="[.:_]",simplify=TRUE))>5) #>5 == includes SubSub Level
	{
		#Create Data Table of all IDs
		IDValsTable <- as.data.table(stri_split_regex(unlist(d[IDLines]), pattern="[.:_]", 
			simplify=TRUE))[,c(2:4,6)]

		#Assign Data table colnames
		colnames(IDValsTable) = c("Main","Sub_0","Sub","SubSub")	

		# String to numerical values
		IDValsTable[,"Main":=as.numeric(IDValsTable$Main)]
		IDValsTable[,"Sub":=as.numeric(IDValsTable$Sub)]
		IDValsTable[,"Sub_0":=as.numeric(IDValsTable$Sub_0)]
		IDValsTable[,"SubSub":=as.numeric(IDValsTable$SubSub)]

		# Get Type == 1 line above ID
		IDValsTable[,"Type":=d$f[IDLines-1]]
		# Save Line Number of Type in data.table
		IDValsTable[,"Type.Line":=IDLines-1]	

	}else{
		IDValsTable <- as.data.table(stri_split_regex(unlist(d[IDLines]), pattern="[.:_]", 
			simplify=TRUE))[,c(2:4)]
		colnames(IDValsTable) = c("Main","Sub_0","Sub")	#not including SubSubs Level
		IDValsTable[,"Main":=as.numeric(IDValsTable$Main)]
		IDValsTable[,"Sub":=as.numeric(IDValsTable$Sub)]
		IDValsTable[,"Sub_0":=as.numeric(IDValsTable$Sub_0)] 
		IDValsTable[,"SubSub":=as.numeric(NA)]
		IDValsTable[,"Type":=d$f[IDLines-1]]
		IDValsTable[,"Type.Line":=IDLines-1]	
	}





#Assign information to "Node"-Lines | where to extract node information | 3 lines after Type Specification
	IDValsTable[is.na(Sub) & Type=="//Node","main.nodeLines":=   Type.Line + 3]
	IDValsTable[!is.na(Sub) & is.na(SubSub) & Type=="//Node","sub.nodeLines":=   Type.Line + 3]
	IDValsTable[!is.na(Sub) & !is.na(SubSub) & Type=="//Node","subsub.nodeLines":=   Type.Line + 3]


	#Identify Leaf Lines [3 lines after "//Leaf]
	leafLinesStart <-  (which(d$f == "//Leaf")) + 3

	#Identify Module End Lines [1 lines before "//EndModule]
	modulEndLines <- (which(d$f == "//EndModule"))-1

	#If leaves are present ...
	if(length(leafLinesStart)>0)
	{
	# create emtpy data table | lStart line - lEndLine - Punkte ID
	dfL <- data.table(lS=leafLinesStart,lE=0, p=0)

	#Loop over all leaves
		# for(i in 1:nrow(dfL))
		# {
		# Identify corresponding End Lines | first End Line-Number > than Starting Line
		#  dfL$lE[i] <- modulEndLines[which(modulEndLines > dfL$lS[i])[1]] 		
		# }
	#Function to Identify corresponding End Lines | first End Line-Number > than Starting Line
	getEndLines <- Vectorize(function(i)	{modulEndLines[which(modulEndLines > dfL$lS[i])[1]]}) #double << to assing to global environment)

	#Fill leaf-End Column
	dfL[,"lE":= getEndLines(1:nrow(dfL))]

	# Fill Point Data by estimating the difference between start and end line +1
	dfL[,"p":=(lE - lS)+1]

	#Create vector of all lines (start:end)
	leafLines <- unlist(lapply(1:nrow(dfL), function(x) {c(dfL$lS[x]:dfL$lE[x])}))

	}


	#Coordinate Data Frames [coordinates are in columns 2:4]
		#Main Stem - Skip NA Cols [Leaf etc.]
		main.node.Coords <- as.data.table(stri_split_fixed(d$f[na.omit(IDValsTable$main.nodeLines)], pattern=";", simplify=TRUE)[,2:4])
		colnames(main.node.Coords) <- c("x","y","z")
		main.node.Coords[,"x":= as.numeric(x)]
		main.node.Coords[,"y":= as.numeric(y)]
		main.node.Coords[,"z":= as.numeric(z)]

		#Reorder - if wrong digitization order
		main.node.Coords[,"Order":= IDValsTable[!is.na(main.nodeLines)]$Main]
		main.node.Coords <- main.node.Coords[order(main.node.Coords$Order)]

		#Optional Check Duplicates in IDs
		#sum(duplicated(with(IDValsTable,paste0(Main,Sub,SubSub,Type))))

		#Sub Stems
		sub.node.Coords <- as.data.table(stri_split_fixed(d$f[na.omit(IDValsTable$sub.nodeLines)], pattern=";", simplify=TRUE)[,2:4])
		colnames(sub.node.Coords) <- c("x","y","z")
		sub.node.Coords[,"x":= as.numeric(x)]
		sub.node.Coords[,"y":= as.numeric(y)]
		sub.node.Coords[,"z":= as.numeric(z)]
		sub.node.Coords[,"Grp":=IDValsTable[!is.na(sub.nodeLines)]$Main] # Grp = parent MAIN 

		#SubSub Stems
		if(sum(!is.na(IDValsTable$subsub.nodeLines))!=0) #check if subsub exists
		{
		subsub.node.Coords <- as.data.table(stri_split_fixed(d$f[na.omit(IDValsTable$subsub.nodeLines)], pattern=";", simplify=TRUE)[,2:4])
		colnames(subsub.node.Coords) <- c("x","y","z")
		subsub.node.Coords[,"x":= as.numeric(x)]
		subsub.node.Coords[,"y":= as.numeric(y)]
		subsub.node.Coords[,"z":= as.numeric(z)]		
		subsub.node.Coords[,"Grp" := paste0(IDValsTable[!is.na(subsub.nodeLines)]$Main,"_",IDValsTable[!is.na(subsub.nodeLines)]$Sub)] #Grp = Main_Sub == parent Sub
		}


	#Leaf Coordinates
	if(length(leafLinesStart)>0) #only if leaves are present
	{
		#Leaves
		leaf.Coords <- as.data.table(stri_split_fixed(d$f[leafLines], pattern=";", simplify=TRUE)[,2:4])

		colnames(leaf.Coords) <- c("x","y","z")
		leaf.Coords[,"x":=as.numeric(x)]
		leaf.Coords[,"y":=as.numeric(y)]
		leaf.Coords[,"z":=as.numeric(z)]

		#Create unique Organ Identifier [for grouping of coordinates]
		leaf.Coords[,"organ_N":= rep(paste0("Leaf_",IDValsTable[Type=="//Leaf"]$Main,"_",IDValsTable[Type=="//Leaf"]$Sub,"_",IDValsTable[Type=="//Leaf"]$SubSub),dfL$p)] #Repeat Id x Number of Points
	


		#Flowers 
		flowerLinesStart <-  (which(d$f == "//Flower")) + 3
		if(length(flowerLinesStart)>0) #check for flowers
		{
			flowerLines <- c(flowerLinesStart,flowerLinesStart+1,flowerLinesStart+2)
			flowerLines <- flowerLines[order(flowerLines)]

			flower.Coords <- as.data.table(stri_split_fixed(d$f[flowerLines], pattern=";", simplify=TRUE)[,2:4])
			colnames(flower.Coords) <- c("x","y","z")
			flower.Coords[,"x":=as.numeric(x)]
			flower.Coords[,"y":=as.numeric(y)]
			flower.Coords[,"z":=as.numeric(z)]

			#Create Organ Identifier [for grouping of coordinates] -- #repeat 3 times as flower has 3 points
			flower.Coords[,"organ_N":= rep(paste0("Flower_",IDValsTable[Type=="//Flower"]$Main,"_",IDValsTable[Type=="//Flower"]$Sub,"_",IDValsTable[Type=="//Flower"]$SubSub),each=3)] 

			#Correct IDs for multiple Flowers at the same node
			singleFlowers <-  unique(flower.Coords$organ_N)[order(unique(flower.Coords$organ_N))][as.vector(table(flower.Coords$organ_N)==3)]
			multipleFlowers <- unique(flower.Coords$organ_N)[order(unique(flower.Coords$organ_N))][as.vector(table(flower.Coords$organ_N)!=3)]
			NFormulitpleFlowers <- as.vector(table(flower.Coords$organ_N))[as.vector(table(flower.Coords$organ_N)!=3)]/3

			#Create unique Organ Identifier
			flower.Coords[,"organ_N_unique":=organ_N]

			#add 
			if(length(multipleFlowers)>0)
			{
				for(i in 1:length(multipleFlowers))
				{
					n <- NFormulitpleFlowers[i]
					flower.Coords[organ_N%in%multipleFlowers[i], "organ_N_unique":= rep(paste0(unique(organ_N),"_",1:n),each=3) ]
				}
			}
			
		}
	} #if no leaves then no flowers


	#Add "Organ Type" to Coordinate Data Frames
	main.node.Coords[,"oType":="main.Node"]
	sub.node.Coords[,"oType":="sub.Node"]
	if(exists("subsub.node.Coords")) {	subsub.node.Coords[,"oType":="subsub.Node"] }
	if(exists("leaf.Coords")){			leaf.Coords[,"oType":="Leaf"] }
	if(exists("flower.Coords")){		flower.Coords[,"oType":="Flower"] }

	#Get list of existing Coords Data Frame
	#rm(dPlantCoords)
	#rm(listOfCoordsDF)
	listOfCoordsDF <- as.list(ls()[grep(ls(),pattern=".Coords")])

	#Merge Data Frames into a single Plant Coords Data Frame
	dPlantCoords <- rbindlist(lapply(listOfCoordsDF,get),fill=TRUE)

	# if(sum(!is.na(IDValsTable$subsub.nodeLines))!=0)
	# {
	# 	if(length(leafLinesStart)==0){
	# 	dPlantCoords <- rbindlist(list(main.node.Coords,sub.node.Coords,subsub.node.Coords),fill = TRUE)
	# 	}else{
	# 	dPlantCoords <- rbindlist(list(leaf.Coords,flower.Coords,main.node.Coords,sub.node.Coords,subsub.node.Coords),fill = TRUE)
	# 	}
	# }else{
	# 	if(length(leafLinesStart)==0){
	# 	dPlantCoords <- rbindlist(list(main.node.Coords,sub.node.Coords),fill = TRUE)
	# 	}else{
	# 	dPlantCoords <- rbindlist(list(leaf.Coords,flower.Coords,main.node.Coords,sub.node.Coords),fill = TRUE)
	# 	}
	# }

	#Convert To Cm for 1 CASE
	#"./20180517//E58-03.txt"
	if(flistID == "./20180517//E58-03.txt")
	{
	dPlantCoords[,1:3] <- dPlantCoords[,1:3]*2.54
	}



	# #FIX ONE INTERNODE TO MUCH
	# #"./20180504//K72-33.txt.txt"
	# if(flistID %in% c("./20180504//K72-33.txt","./20180503//D10-32.txt"))
	# 	{
	# 	## 1 NODE TOO much at the beginning
	# 	tmp <- stri_split_regex(dPlantCoords[!is.na(organ_N)]$organ_N,pattern="_",simplify=TRUE)
	# 	tmp[,2] <- as.numeric(tmp[,2])-1
	# 	dPlantCoords[!is.na(organ_N)]$organ_N <-  apply( tmp[ , 1:4 ] , 1 , paste , collapse = "_" )
	# 	dPlantCoords <- dPlantCoords[Order!=1 | is.na(Order) ]
	# 	dPlantCoords[oType=="main.Node" & Order>0]$Order <- dPlantCoords[oType=="main.Node" & Order>0]$Order-1
	# 	dPlantCoords$Grp <- dPlantCoords$Grp - 1
	# 	}

	# #FIX ONE INTERNODE Missing
	# #"./20180504//K72-33.txt.txt"
	# if(flistID == "./20180503//L88-01.txt")
	# 	{
	# 	## 1 Missing --- "Average of Existing ones
	# 		#L88-01 am ersten Datum  
	# 		#6 - 12 -- 13
	# 		#c1 <- f.dPlantCoords(flist[6])
	# 		#c2 <- f.dPlantCoords(flist[12])
	# 		#c3 <- f.dPlantCoords(flist[13])
	# 		#matrix(c1[oType=="main.Node"][,1:3])-c1[oType=="main.Node"][1,1:3]
	# 		#((matrix(c2[oType=="main.Node"][,1:3])-c2[oType=="main.Node"][1,1:3])+(matrix(c3[oType=="main.Node"][,1:3])-c3[oType=="main.Node"][1,1:3]))/2
	# 		#missing node from Startpoint  "-2.360  -1.820 -62.365"
	# 	tmp <- stri_split_regex(dPlantCoords[!is.na(organ_N)]$organ_N,pattern="_",simplify=TRUE)
	# 	tmp[,2] <- as.numeric(tmp[,2])+1
	# 	dPlantCoords[!is.na(organ_N)]$organ_N <-  apply( tmp[ , 1:4 ] , 1 , paste , collapse = "_" )
	# 	dPlantCoords[oType=="main.Node" & Order>0]$Order <- dPlantCoords[oType=="main.Node" & Order>0]$Order+1
	# 	dPlantCoords$Grp <- dPlantCoords$Grp + 1
		
	# 	dPlantCoords.Add <- dPlantCoords[oType=="main.Node" & Order==0]
	# 	dPlantCoords.Add$Order = 1
	# 	dPlantCoords.Add$x = dPlantCoords.Add$x + -2.360
	# 	dPlantCoords.Add$y = dPlantCoords.Add$y + -1.820
	# 	dPlantCoords.Add$z = dPlantCoords.Add$z + -62.365
		
	# 	dPlantCoords <- rbind(dPlantCoords,dPlantCoords.Add)
		
	# 	}



	}#End special case fread("./2018060708_B-parts/dPlantCoords_B39-32-20180607-08.csv")



		
	#FLIP z-COORDINATE
	dPlantCoords[,"z":=z*-1]
	#######################################################################################
	# TODO DIGITIZER ORIENTATION?
	# #and? XY to reconstruct LR SIDE of row 
	# dPlantCoords[,"x":=x*-1]
	# dPlantCoords[,"y":=y*-1]
	#######################################################################################


	#IF PLANT == "L88-01" OR "E58-03" --- Rotate 180° around z-axis 
	#TODO digitized from the other side of the row ?
	if(stri_split_regex(flistID,pattern="[/.]",simplify=TRUE)[,5] %in% c( "L88-01", "E58-03" ))
	{
		#cat("L88/E58-flip \n")
		dPlantCoords$x = dPlantCoords$x*-1
		dPlantCoords$y = dPlantCoords$y*-1
	}
	#######################################################################################


	# Rotate Plants to Best Fitting Vertical Plane through Main nodes
	#Rotate 90 Degree By Chainging x + y | X-AXIS = Cordon [along plant]
	#Move First Node (not at the ground to 0 0 0)(Order==1) [maybe not necessary]
	#Projection to XY_Plane
	#LM Fit

	#Transformation of Coordinates
	xyz.trans <- as.data.table(t(t(as.matrix(dPlantCoords[,1:3]))-as.numeric(dPlantCoords[Order==1][,1:3])))

	#Rename Columns #CHANGED x + y to Rotate 90 Degrees
	colnames(xyz.trans) <- c("y.n","x.n","z.n") 

	#Add transformed coodinates to data.frame
	dPlantCoords <- cbind(dPlantCoords,xyz.trans)

	#Adjust DF Order [sanity check, there was a problem in L88-01]
	dPlantCoords.0 <- dPlantCoords[!is.na(Order)] 					#STEM
	dPlantCoords.0  <- dPlantCoords.0[order(dPlantCoords.0$Order)]	#reorder Stem
	dPlantCoords.1 <- dPlantCoords[is.na(Order)]	#not STREM
	dPlantCoords <- rbind(dPlantCoords.0,dPlantCoords.1)	#PASTE STEM to the front of the data set

	#Get "cane" 
	main <- dPlantCoords[oType=="main.Node" & Order > 0]

	# Fit "cane to axis"
	fit.main <- lm(y.n ~ x.n, main)
	# with(main, plot(y.n~x.n,asp=1))
	# lines(predict(fit.main)~main$x.n)



	#Rotate + Check
	main.new <- main

	##Rotate | get angle
	alph <- atan(as.numeric(coef(fit.main)[2])/1)
	#rad2deg(alph)

	#Rotate 2D
	main.new[, "x.n1":= x.n* cos(alph) - (y.n-coef(fit.main)[2])*sin(alph)]
	main.new[, "y.n1":= x.n* sin(alph) + (y.n-coef(fit.main)[2])*cos(alph)]

	#Second Fit for Checking rotational direction 
	fit.main.new <- lm(y.n1 ~ x.n1, main.new)
	#Check Direction 
	if(abs(coef(fit.main.new)[2]) > abs(coef(fit.main)[2]))	#if fit2 coef larger fit1 coef == slope | wrong direction
	{
		#cat("Check Direction \n")
		main.new <- main
		alph <- -alph
		main.new[, "x.n1":= x.n* cos(alph) - (y.n-coef(fit.main)[2])*sin(alph)]
		main.new[, "y.n1":= x.n* sin(alph) + (y.n-coef(fit.main)[2])*cos(alph)]

		#fit.main.new <- lm(y.n1 ~ x.n1, main.new)

			# if(abs(coef(fit.main.new)[2]) < abs(coef(fit.main)[2])) {
			# #	cat("OK \n")
			# }
	}



	#Perform Rotation on entire Plant
	dPlantCoords[,"x.n":= x.n* cos(alph) - (y.n-coef(fit.main)[2])*sin(alph)]
	dPlantCoords[,"y.n":= x.n* sin(alph) + (y.n-coef(fit.main)[2])*cos(alph)]

	#Overwrite XYZ
	dPlantCoords[,"x":= x.n]
	dPlantCoords[,"y":= y.n]
	dPlantCoords[,"z":= z.n]

	#Remove duplicated coloumns
	dPlantCoords <- dPlantCoords[,-c("x.n","y.n","z.n")]

	#Add columns to identify plant
	dPlantCoords[,"Date":= stri_split_fixed(flistID,pattern="/",simplify=TRUE)[,2] ]	#Date from folder name
	dPlantCoords[,"Plant" := gsub(stri_split_regex(flistID,pattern="[/.]",simplify=TRUE)[,5],pattern = "-", replacement = "") ] #Plant from file name
	dPlantCoords[,"Ring" := substr(Plant,1,1)] 				#From Plant Name
	dPlantCoords[,"Elevated" := Ring%in%c("B","D","E")]		#From Ring Name


	return(dPlantCoords)
}



#READSOURCEDATABREAK

#Remove problematic FILE
#flist <- flist[-5]

allPlant <- rbindlist(lapply(flist,f.dPlantCoords),fill=TRUE)




##############################################
# Create plotting function
#TODO weiter hier
##############################################


plotRGL <- function(flistID){
#rgl.close()

dPlantCoords <- f.dPlantCoords(flistID)
#dPlantCoords <- f.dPlantCoords.filled(flistID)


#Splitback for Plotting
leaf.Coords <- dPlantCoords[oType=="Leaf"]
flower.Coords <- dPlantCoords[oType=="Flower"]
main.node.Coords <- dPlantCoords[oType=="main.Node"]
sub.node.Coords <- dPlantCoords[oType=="sub.Node"]
subsub.node.Coords <- dPlantCoords[oType=="subsub.Node"]



#PREPARE LEAF MATRIX
# Blätter
if(nrow(leaf.Coords)!=0)
{
#Idenfitfy Numbers of Points
fullL <-  unique(leaf.Coords$organ_N)[order(unique(leaf.Coords$organ_N))][as.vector(table(leaf.Coords$organ_N)==18)]
medL <-  unique(leaf.Coords$organ_N)[order(unique(leaf.Coords$organ_N))][as.vector(table(leaf.Coords$organ_N)==9)]
smallL <-  unique(leaf.Coords$organ_N)[order(unique(leaf.Coords$organ_N))][as.vector(table(leaf.Coords$organ_N)==6)]

#Preare Triangle MAtrix
for(i in smallL)
	{
	# Init 
	TriDF <- Tri5
	np <- nrow(leaf.Coords[organ_N==i])
	listL <- smallL
	###
	
	
	Pet  <- leaf.Coords[organ_N==i][1:2,1:3]
	Leaf <-  leaf.Coords[organ_N==i][2:np,1:3]			#2:Number of points

		for(j in 1:nrow(TriDF))							#Triangulation
			{
				tri.Points <- TriDF[j,]
				
				if(j == 1 & i == listL[1]){				#First in SmallL		
						mTriX <-	matrix(	
								c(	Leaf$x[tri.Points],
									Leaf$y[tri.Points],
									Leaf$z[tri.Points])
								,byrow=FALSE,ncol=3)
				}else{
					mTriX <-	rbind(mTriX,matrix(	
								c(	Leaf$x[tri.Points],
									Leaf$y[tri.Points],
									Leaf$z[tri.Points])
								,byrow=FALSE,ncol=3)	
								)
					}	
			}


		#Petioles
		if(i == listL[1]){ 	mPetX <- Pet[,1:3]  				#First in SmallL		
		}else{ mPetX <- rbind(mPetX,Pet[,1:3])	}
		#segments3d(Pet[,1:3],		col=col.pet,		lwd=4)

	}
##########################
for(i in medL)
	{
	# Init 
	TriDF <- Tri9
	np <- nrow(leaf.Coords[organ_N==i])
	listL <- medL
	###
	
	
	Pet  <- leaf.Coords[organ_N==i][1:2,1:3]
	Leaf <-  leaf.Coords[organ_N==i][2:np,1:3]			#2:Number of points

		for(j in 1:nrow(TriDF))							#Triangulation
			{
				tri.Points <- TriDF[j,]
				
#				if(j == 1 & i == listL[1]){				#First in SmallL		
#						mTriX <-	matrix(	
#								c(	Leaf$x[tri.Points],
#									Leaf$y[tri.Points],
#									Leaf$z[tri.Points])
#								,byrow=FALSE,ncol=3)
#				}else{
					mTriX <-	rbind(mTriX,matrix(	
								c(	Leaf$x[tri.Points],
									Leaf$y[tri.Points],
									Leaf$z[tri.Points])
								,byrow=FALSE,ncol=3)	
								)
#					}	
			}


		#Petioles
		#if(i == listL[1]){ 	mPetX <- Pet[,1:3]  				#First in SmallL		
		#}else{ 
		mPetX <- rbind(mPetX,Pet[,1:3])	
		#}
		#segments3d(Pet[,1:3],		col=col.pet,		lwd=4)

	}
	
#FULL
for(i in fullL)
	{
	# Init 
	TriDF <- Tri17
	np <- nrow(leaf.Coords[organ_N==i])
	listL <- fullL
	###
	
	
	Pet  <- leaf.Coords[organ_N==i][1:2,1:3]
	Leaf <-  leaf.Coords[organ_N==i][2:np,1:3]			#2:Number of points

		for(j in 1:nrow(TriDF))							#Triangulation
			{
				tri.Points <- TriDF[j,]
				
					if(exists("mTriX")==FALSE){				#CheckExistance		
							mTriX <-	matrix(	
									c(	Leaf$x[tri.Points],
										Leaf$y[tri.Points],
										Leaf$z[tri.Points])
									,byrow=FALSE,ncol=3)
					}else{			
							mTriX <-	rbind(mTriX,matrix(	
										c(	Leaf$x[tri.Points],
											Leaf$y[tri.Points],
											Leaf$z[tri.Points])
										,byrow=FALSE,ncol=3)	
										)
					}	
			}


		#Petioles
		if(exists("mPetX")==FALSE){ 
		mPetX <- Pet[,1:3]  				#First in SmallL		
		}else{ 
		mPetX <- rbind(mPetX,Pet[,1:3])	
		}
		#segments3d(Pet[,1:3],		col=col.pet,		lwd=4)

	}
}


###############################################

open3d()
###############################################
#Knoten/Stamm
###############################################
lines3d(main.node.Coords[,1:3],lwd=6,col="#7f2704")
for(i in unique(sub.node.Coords$Grp))
{
#TODO Create Lines or multiple segments | multi segs sind schneller geplotted aber vllt irrelevant
	lines3d(sub.node.Coords[Grp==i,1:3],lwd=3,col="#7f2704",add=TRUE)
}


for(i in unique(subsub.node.Coords$Grp))
{
#TODO Create Lines or multiple segments | multi segs sind schneller geplotted aber vllt irrelevant
	lines3d(subsub.node.Coords[Grp==i,1:3],lwd=3,col="#7f2704",add=TRUE)
}

for(i in unique(flower.Coords$organ_N_unique))
{
#TODO Create Lines or multiple segments | multi segs sind schneller geplotted aber vllt irrelevant
	lines3d(flower.Coords[organ_N_unique==i,1:3],lwd=2,col="orange",add=TRUE)
}



###############################################
#Blätter
###############################################
if(nrow(leaf.Coords)!=0)
{
	triangles3d(mTriX,col=col.leaf)	
	segments3d(mPetX,col=col.pet,lwd=4)
}

uMat <- matrix(c(	1,0,0,0,
					0,0,1,0,
					0,1,0,0,
					0,0,0,1),byrow=TRUE,nrow=4)

aspect3d("iso")
title3d(flistID)
#view3d(theta=180, phi=-60)
#par3d(userMatrix=uMat)
#uMat <- matrix(c(	1,0,0,0,
#					0,0,1,0,
#					0,1,0,0,
#					0,0,0,1),byrow=TRUE,nrow=4)

scene1 <- scene3d()
return(scene1)
}


#SOURCEBREAK



# TODO Leaves mit full points 
# vergleichen mit denen die weniger haben
# Estimate leaf area from "seitenlöngen"
# Fehler abschätzung 




#Internode length
#Sub Nodes



####################################################################################################################################################################################################################################
########################################################################################################################################################
# TODO WEITER HIER für 2019
########################################################################################################################################################
#dfILSub <- rbindlist(lapply(unique( dPlantCoords[oType=="sub.Node"]$Grp),f.ilSub))

f.ilSubAll <- function(flistID) {

dPlantCoords <- f.dPlantCoords(flistID)

	f.ilSub <-  function(grpI) {

		m <- dPlantCoords[oType=="sub.Node" & Grp==grpI][,1:3]
		m <- as.matrix(m)
			if(nrow(m)==1)
			{
			out <- data.table(Grp=grpI, IL = 0, Rank = 0)
			}else if(nrow(m)==2) #Added 2019 for single internode stems
			{
				out <- data.table(Grp=grpI, IL = as.numeric(dist(m)))
				out$Rank <- 1:nrow(out)
			}else{
				out <- data.table(Grp=grpI, IL = diag(as.matrix(dist(m))[-1,- nrow(m)]))
				out$Rank <- 1:nrow(out)
			}

		return(out)
		}




dfILSub <- rbindlist(lapply(unique(dPlantCoords[oType=="sub.Node"]$Grp),f.ilSub))



dfILSub$Date <- stri_split_regex(flistID, pattern="[./]",simplify = TRUE)[,3]
dfILSub$Plant <- gsub(stri_split_regex(flistID,pattern="[/.]",simplify=TRUE)[,5],pattern = "-", replacement = "") #stri_split_regex(flistID,pattern="[/.]",simplify=TRUE)[,5]
dfILSub$Ring <- stri_split_regex(flistID, pattern="[./0-9-]",simplify = TRUE)[,13]
dfILSub$Elevated <- dfILSub$Ring%in%c("B","D","E")
return(dfILSub)
}





dfILSubAll <-  rbindlist(lapply(flist,f.ilSubAll),fill=TRUE)


pIL <- ggplot(dfILSubAll, aes(x=Rank, y=IL,color=Plant))+
	geom_point()+
	geom_line(aes(linetype=Elevated))+
	facet_grid(Grp~Date)

#Mean / Summe

pIL.box <- ggplot(dfILSubAll, aes(x=Ring, y=IL, fill=Elevated))+
				geom_boxplot(position="dodge")+
				   facet_grid(.~Date,scales="free_x")
				



############################################################################
##Area Estimation D_1_4 + D_1_5
# Plots
############################################################################
# WITH DATA FROM  JOHANNA DÖRING
# MAIL "WG: Blätter und Blattmodelle"
# M1 = SL = Summe 
# M2 = log(Summe)

#exp(coef(m2)[2])
#(coef(m2)[1])

library(rstanarm)

#save(m3.stan.0, file="m3.Rdata")
load(file="m3.Rdata")

############################################################################

#Distances: 

#flistID <- flist[1]


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

flistLA <- unlist(lapply(X=flist,FUN=check.f.LforA))





f.LforA <- function(flistID)
{
dPlantCoords <- f.dPlantCoords(flistID)


dL <- dPlantCoords[oType=="Leaf"]


dtpts <- as.data.table(table(dL$organ_N))
dtpts$ord <- order(unique(dL$organ_N))
dtpts <- dtpts[order(ord)]


pts <- dtpts$N #as.vector(table(dL$organ_N)) #

ptsSeq <- vector()
for(i in 1:length(pts))
{
ptsSeq <- c(ptsSeq,c(0:(pts[i]-1)))
}

dL$point = ptsSeq
dL$pts <- rep(pts,pts)

L6 <- dL[pts==6 & point%in%c(1,4,5)]
L10 <- dL[pts==10 & point%in%c(1,4,7)]
L10[point==7]$point = 51
L18 <- dL[pts==18 & point%in%c(1,6,13)]
L18[point==6]$point = 4
L18[point==13]$point = 5

LforA <- rbind(L6,L10,L18)

mD <- as.matrix(dist(as.matrix(LforA[,1:3],method="euclidean")))
mD1er <- mD[,seq(1,ncol(mD),by=3)]


tmp  <- matrix(vector(),ncol=2,ncol(mD1er))

for(i in 1:ncol(mD1er))
{
tmp[i,] = mD1er[1+(i-1)*3+(1:2),i]
}


#Remove duplicated Rows
LforA <- LforA[seq(1,nrow(LforA),by=3)]

LforA <- cbind(LforA,tmp)
colnames(LforA)[(ncol(LforA)-1):ncol(LforA)] <- c("LL","LR")
LforA[,"SL.org":=LL+LR]  #ORG
LforA[,"SL":=log(SL.org)]  	#LOG


LforA$Date <- stri_split_regex(flistID, pattern="[./]",simplify = TRUE)[,3]
LforA$Plant <- gsub(stri_split_regex(flistID,pattern="[/.]",simplify=TRUE)[,5],pattern = "-", replacement = "") #stri_split_regex(flistID,pattern="[/.]",simplify=TRUE)[,5]
LforA$Ring <- stri_split_regex(flistID, pattern="[./0-9-]",simplify = TRUE)[,13]
LforA$Elevated <- LforA$Ring%in%c("B","D","E")



return(LforA)
}

#only perform on plants with leaves
dLforA <- rbindlist(lapply(flistLA,f.LforA))




# #M3
 dLforA$A.mean <- as.numeric(colMeans(posterior_predict(m3.stan.0,newdata = data.frame(SL=dLforA$SL.org)))^2)


dLforA$Shoot <- as.numeric(stri_split_regex(dLforA$organ_N, pattern="[_]",simplify = TRUE)[,2])
dLforA$Rank <- as.numeric(stri_split_regex(dLforA$organ_N, pattern="[_]",simplify = TRUE)[,3])
dLforA$SubShoot <- as.numeric(stri_split_regex(dLforA$organ_N, pattern="[_]",simplify = TRUE)[,4])

pAL <- ggplot(dLforA[is.na(SubShoot)], aes(x=Rank, y=A.mean,color=Plant))+
	#geom_ribbon(aes(ymin=A.HPDI95.low,ymax=A.HPDI95.high),alpha=0.5, color="grey")+
	geom_point()+
	geom_line(aes(linetype=Elevated))+
	facet_grid(Shoot~Date)



#Mean / Summe

pAL.box <- ggplot(dLforA[is.na(SubShoot)], aes(x=Ring, y=A.mean, fill=Elevated))+
				geom_boxplot(position="dodge")+
				 facet_grid(.~Date,scales="free_x")



#TODO Check Leaf Area Predictions vs Procrustes Predictions


#TODO Go parallel | only for plants with leaves
allPlant <- rbindlist(lapply(flistLA,f.dPlantCoords),fill=TRUE)

leaf.Coords <- allPlant[oType=="Leaf"]
leaf.Coords[,"LID":=paste(Date,Plant,organ_N,sep="_")]


# #TODO PROBLEM DOPPELTES LEAF
# #also erstmal alle auf 18 Pt erhöhen ...
# #bzw. wenn Doppelt noch etwas dranhängen? Oder ist das schon? s. Gurke
# #Area From Triangulation
# fullLeafIDS <- as.data.table(as.matrix(table(leaf.Coords$LID)==18),keep.rownames = TRUE)[V1==TRUE]$rn

# dfLeafAreaTri <- data.table(LID=fullLeafIDS, A=0)

# 	#Full Leaves 
# 	for(i in fullLeafIDS)
# 		{
# 		Leaf <-   leaf.Coords[LID==i]
# 		Leaf$point=0:17

# 			dfLeafAreaTri[LID==i, "A":=sum(vTriA3D(a=Tri17[,1],b=Tri17[,2],c=Tri17[,3],	
# 								DF=replicate(Leaf,n=nrow(Tri17),simplify = FALSE)))]

		
# 		}




# dfLeafAreaTri
# #dfLeafArea[LID%in%fullLeafIDS]


# dLforA[,"LID":=paste(Date,Plant,organ_N,sep="_")]

# 	nrow(dLforA)
# 	nrow(dfLeafAreaTri)

# length(fullLeafIDS)

# dLforA[dLforA$LID%!in%dfLeafAreaTri$LID]	#3x 10 pts measured
# dfLeafAreaTri[dfLeafAreaTri$LID%!in%dLforA$LID]	


# mergedDLAs <- merge(dLforA,dfLeafAreaTri,by="LID")

# #Vergleich Leaf Area for Procrustes "Rescaling" gegen "Model Leaf Area"
# #plot(mergedDLAs$A.mean~mergedDLAs$A)
# #abline(a=0,b=1)
# #sqrt(sum((mergedDLAs$A.mean-mergedDLAs$A)^2))


# pAProc.AMod <- ggplot(mergedDLAs, aes(x=A.mean,y=A))+
# 					geom_point()+
# 					xlab("A.model")+ylab("A.procrustes")+
# 					geom_abline(intercept=0,slope=1, color="red")+
# 					coord_fixed()

# #WELCHEN FEHLER Macht Procrustes im Vergleich zur 2d Fläche ?
# 	#Fehlerquellen:
# 		#Wölbung
# 		#UND Abschnitte Außen durch rundungen !!!
# ggplot(mergedDLAs, aes(x=A.mean.m3,y=A))+
# 					geom_point()+
# 					xlab("A.model3")+ylab("A.procrustes")+
# 					geom_abline(intercept=0,slope=1, color="red")+
# 					coord_fixed()


