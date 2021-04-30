library(readxl)
library(tidyr)
library(dplyr)

# 1. read excel data and match excel sheet with title---- 
#Example: LakeCampaignList[["Bretaye"]] #Call Bretaye in LakeCampaignList

LakeCampaignList=list(
  Bretaye=list( # include filename and desired title for each lake campaign
    list(filename="N2_Bretaye_2018_06_18", plottitle="Bretaye June 2018"),
    list(filename="N2_Bretaye_2018_09_03", plottitle="Bretaye Sept 2018"),
    list(filename="N2_Bretaye_2019_07_20", plottitle="Bretaye July 2019")
  ), # from here icnlude other lakes,
  Noir=list( # include filename and desired title for each lake campaign
    list(filename="N2_Noir_2018_06_20", plottitle="Noir June 2018"), 
    list(filename="N2_Noir_2018_09_04", plottitle="Noir Sept 2018"),
    list(filename="N2_Noir_2019_07_24", plottitle="Noir July 2019")
  ),  
  Chavonnes=list( # include filename and desired title for each lake campaign
    list(filename="N2_Chav_2018_06_18", plottitle="Chavonnes June 2018"),
    list(filename="N2_Chav_2018_09_05", plottitle="Chavonnes Sept 2018"),
    list(filename="N2_Chav_2019_07_23", plottitle="Chavonnes July 2019")
  ),
  Lioson=list( # include filename and desired title for each lake campaign
    list(filename="N2_Lioson_2018_06_24", plottitle="Lioson June 2018"),
    list(filename="N2_Lioson_2018_08_30", plottitle="Lioson Sept 2018"),
    list(filename="N2_Lioson_2019_07_17", plottitle="Lioson July 2019")
  )
)


#setwd("/Home/desktop/MUSE/Mémoire Muse/Excel sheets")




#2. Create new function to read excel data ----
readCampaign <- function(campaign) { # { } open and closes function (closes in line 61)

# Read first line only (later use first line to match column name)
LakeCampaignColnames <- readxl::read_xlsx(   # les 2 points appelle la fonction dans le package
    path = "Final_VaudLake_CTD_25.04.xlsx", 
    sheet = campaign, 
    n_max = 1,  #max quantity of lines to read
    col_names = F
    ) %>% #same as header = F ; %>% pipe qui appelle la fonction= prend tout ce qui il y a a gauche pour le mettre dans la fonction à droite
  array()  #vector, comme une liste

# les donnes sans headers
LakeCampaignDF <- readxl::read_xlsx(
  path = "Final_VaudLake_CTD_25.04.xlsx", 
  sheet = campaign, 
  skip = 3,  #Skip the first 3 lines
  col_names = F)

# apply colnames to dataframe LakeCampaign
colnames(LakeCampaignDF) <- LakeCampaignColnames
return(LakeCampaignDF)  # Do some processing and return back to the result

} #End of new function

# use these for testing function
testing=LakeCampaignList[[1]][[2]]
plottitle=testing[["plottitle"]]
filename=testing[["filename"]]
LakeCampaign <- readCampaign(filename)
PlotTitle=plottitle

#3.
plotCampaign <- function(LakeCampaign, PlotTitle) {
  
# PLOT: x <- depth, y1 <- N^2, y2 <- Kd, y3 <- CH4, y4 <- T°
# 3. Set parameters ----

 
# x axis'
x1 <- LakeCampaign[["Depth corr"]]
x2 <- LakeCampaign[["Depth"]]


# list(LakeCampaign)

# y axis'
y1 <- LakeCampaign[["Smoothed Y1"]] # N2
y2 <- LakeCampaign[["Specc"]] # Conductivity
y3 <- LakeCampaign[["Tv290C"]] # T°
y4 <- LakeCampaign[["CH4"]] # CH4 measured by Chou Team, use x2


# colors
y1color="hotpink"
y2color="turquoise"
y3color="navy"
y4color="purple"

# axis titles
y1text=expression(paste("N"^2," (","s"^-2,")"))
y1legend=expression(paste("N"^2))

y2text=expression(paste("K"[25]," (µS cm"^-1,")"))
y2legend=expression(paste("K"[25]))

y3text="Temp (°C)"
y3legend="Temp  "

y4text=expression(paste("CH"[4], " (µmol L"^-1,")"))
y4legend=expression(paste("CH"[4]))

LegendList <- list(   #List of axis title and color for each variables
  legends = c(y1legend, y2legend, y3legend, y4legend), 
  legendcols = c(y1color, y2color, y3color, y4color)
  ) #Legendlist for the legend

# list must have: x values, x axis title, y values, y value title, line color, line type, point or no points, 
LoopList <- list(
  list(xval=x1, yval=y1, log='x', ytext=y1text, ylegend=y1legend, linecolor=y1color, linetypelty=1, axisside=1, axispostion=1, makepoints=F),  # N2
  list(xval=x1, yval=y2, log='', ytext=y2text, ylegend=y2legend, linecolor=y2color, linetypelty=1, axisside=1, axispostion=5, makepoints=F),  # condu
  list(xval=x1, yval=y3, log='', ytext=y3text, ylegend=y3legend, linecolor=y3color, linetypelty=1, axisside=3, axispostion=1, makepoints=F),   # T°
  list(xval=x2, yval=y4, log='', ytext=y4text, ylegend=y4legend, linecolor=y4color, linetypelty=2, axisside=3, axispostion=5, makepoints=T)  # CH4
) #Looplist using for graph

#scale_y_log10()

# 4. Making graphing loop ----
# Making a looping a list of variables instead of this for each variables:
# y1: Plot the first series (N2)
# par(mar=c(4, 10, 2, 10) + 0.1) # b,l, t, r
# plot(x1, y1, axes = F, ylim=c(miny1, maxy1), xlab="", ylab="",type="l", col=y1color, lty=1, main="",xlim=c(minx1, maxx1) )
# #points(x1,y1, pch=20, col=y1color)
# axis(2, ylim=c(miny1, maxy1), lwd=axislwd, line=1) # axis 2 (to the left)
# mtext(2, text=y1text, line=3)



# define X axis range and graph parameters

# General Parameters for each lake and campaign
axislwd=1.3 #axis line width
plotlwd=2 #line plot width
mtextcex=0.8 #text size in mtexts

par(mar=c(10, 4, 12, 2) + 0.1) # b,l, t, r (margins)

# parameters=LoopList[[2]] # used for testing each parameters in Looplist
# Run all the parameters (on assigne les parametres à des nouvelles variales -> VectorX,VectorY, etc) for each variables (Loopslit):
for (parameters in LoopList) {  

vectorX=parameters[["xval"]]   #xval =x1 = depth corr; caracteristics toujours la meme mais valeurs différentes
minX=0
maxX=ceiling(max(vectorX, na.rm = T)) # ceiling take higher value entiere
xlims=c(minX, maxX)

vectorY=parameters[["yval"]]
minY=min(vectorY, na.rm = T)
maxY=max(vectorY, na.rm = T)
ylims=c(minY, maxY)
ytext=parameters[["ytext"]]

logwhat = parameters[['log']]

linecolor=parameters[["linecolor"]]
linetypelty=parameters[["linetypelty"]]
axisside=parameters[["axisside"]]
axispostion=parameters[["axispostion"]]

makepoints=parameters[["makepoints"]]

plot(x=vectorY, y=vectorX,   #Exchange X-Y axis to pivot chart
     axes = F, 
     xlim=ylims, 
     ylim=rev(xlims),
     main="", # overall plot title
     xlab="", # title for x axis
     ylab="", # title for y axis
     type="l", # plot type (line)
     col=linecolor, # line color
     lty=linetypelty, # line type, 1 is normal, 2 is dotted line (---)
     lwd=plotlwd,
     # log=logwhat
     )


if (makepoints) {points(x=vectorY, y=vectorX, pch=20, col=linecolor, lwd=plotlwd*1.5)} # add points?

# tickdifs=round(((maxY-minY)/5), 3)
# # draw axis
# if (tickdifs > 5) {
#   axis(axisside, # axis 2 (to the left) 
#        xlim=ylims, 
#        lwd=axislwd, # line width (set in parameters as axislwd)
#        line=axispostion # where to draw the element
#   )
# } else {
#   axis(axisside, # axis 2 (to the left) 
#        at=seq(floor(minY), ceiling(maxY), by=tickdifs),
#        lwd=axislwd, # line width (set in parameters as axislwd)
#        line=axispostion # where to draw the element
#   )
# }

axis(axisside, # axis 2 (to the left)
     xlim=ylims,
     lwd=axislwd, # line width (set in parameters as axislwd)
     line=axispostion # where to draw the element
  )


# write axis title
mtext(axisside, 
      text=ytext, 
      line=axispostion+2.5,
      cex = mtextcex
      )


par(new=T)  

} 
# end of parameters Loop


# 5. Draw X axis ----
axis(2, pretty(c(minX, maxX),10), lwd=axislwd)
mtext(text="Depth (m)", side=2, line=2, cex = mtextcex ) # side (1=bottom, 2=left, 3=top, 4=right)
title(main=PlotTitle, line=10, cex.main=1.5)




# Add a legend
# legend("topright",
#        inset=c(-0.5, 0),
#        legend = c(y1legend, y2legend, y3legend, y4legend), 
#        col = c(y1color, y2color, y3color, y4color), 
#        # pch = c(17,19), 
#        # bty = "n", 
#        # pt.cex = 2, 
#        # cex = 1.2, 
#        text.col = "black", 
#        horiz = F
# )
# 
# 

return(LegendList)  # 2 listes une pour le graph une pour la légende (créer en dehors de la finction)

} # end of plotting function


# 4. make all plots and add legends ----
for (Lake in LakeCampaignList) {  # prend chaque lake dans la "boite" LakeCmpaignList
  
  # par(mfrow=c(1,3))
  par(mfrow=c(1,3), oma = c(4,1,1,1)) # bottom,left, top, right
  
  for (Campaign in Lake) {  #prend chaque campagne de lake pour faire des trucs apres:
  
    filename=Campaign[["filename"]]   # Where the information is
    plottitle=Campaign[["plottitle"]]   #Give title
    
    lakecampaign=readCampaign(filename)
    testylisty=plotCampaign(lakecampaign, plottitle)
  }
  
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  
  legend(x=-0.58, y=-0.9, #"bottom",   # Add legend, one for the 3 graph
         testylisty[["legends"]],
         col = testylisty[["legendcols"]], 
         xpd = TRUE, 
         horiz = TRUE, 
  #       inset = c(0, -2), 
         bty = "n", 
         lty = c(1, 1 , 1, 2), 
         lwd = 3,
         pch = c(NA, NA, NA, 19), # only dot line for CH4
         cex = 1.5
  )
  # xpd = TRUE tells R that it is OK to plot outside the region horiz = TRUE
  # tells R that I want a horizontal legend inset = c(x,y) tells R how to move
  # the legend relative to the 'bottom' location bty = 'n' means that 'no' box
  # will be drawn around it pch and col are the types and colors of points cex
  # = 2 makes the legend twice as large as other fonts
  # http://dr-k-lo.blogspot.com
  
} 



#########################################
# Loop examples
ListyLoop <- list(1,2,3,4,5)

for (thing in ListyLoop) {
  print(thing)
}

words <- c("cat", "computer", "shoe", "chou")

for (word in words) {
  print(paste("Hello ", word))
}

#Loop tests

loopvec<- c(1,2,3,4,5,6)

for(loopitem in loopvec){
  print(loopitem)
}
 



