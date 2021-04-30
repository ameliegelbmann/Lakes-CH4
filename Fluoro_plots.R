library(readxl)
library(tidyr)
library(dplyr)

# 1. read excel data and match excel sheet with title---- 
#Example: LakeCampaignList[["Bretaye"]] #Call Bretaye in LakeCampaignList

LakeCampaignList=list(
  Bretaye=list( # include filename and desired title for each lake campaign
    list(filename="Bre_17June2018_932.xlsx", plottitle="Bretaye June 2018"),
    list(filename="Bre_3Sept2018_0925.xlsx", plottitle="Bretaye Sept 2018"),
    list(filename="Bretaye20July2019.xlsx", plottitle="Bretaye July 2019")
  ), # from here icnlude other lakes,
  Noir=list( # include filename and desired title for each lake campaign
    list(filename="Noir_20June2018.xlsx", plottitle="Noir June 2018"), 
    list(filename="Noir_04Sept2018_0944.xlsx", plottitle="Noir Sept 2018"),
    list(filename="Noir24July2019.xlsx", plottitle="Noir July 2019")
  ),  
  Chavonnes=list( # include filename and desired title for each lake campaign
    list(filename="Cha_19June2018.xlsx", plottitle="Chavonnes June 2018"),
    list(filename="Cha_05Sep2018_1153.xlsx", plottitle="Chavonnes Sept 2018"),
    list(filename="Chavonnes_20190723.xlsx", plottitle="Chavonnes July 2019")
  ),
  Lioson=list( # include filename and desired title for each lake campaign
    list(filename="Lio_24June2018 copie.xlsx", plottitle="Lioson June 2018"),
    list(filename="Lio_30Aug2018_1430 copie.xlsx", plottitle="Lioson Sept 2018"),
    list(filename="Lioson19July2019 copie.xlsx", plottitle="Lioson July 2019")
  )
)






# testing=LakeCampaignList[[1]][[1]]
# plottitle=testing[["plottitle"]]

#2. Create new function to read excel data ----
readCampaign <- function(campaign) { # { } open and closes function (closes in line 61)
  
  # Read first line only (later use first line to match column name)
  LakeCampaignColnames <- readxl::read_xlsx(   # les 2 points appelle la fonction dans le package
    path = campaign, 
    sheet = 1, 
    n_max = 1,  #max quantity of lines to read
    col_names = F
  ) %>% #same as header = F ; %>% pipe qui appelle la fonction= prend tout ce qui il y a a gauche pour le mettre dans la fonction à droite
    array()  #vector, comme une liste
  
  # les donnes sans headers
  LakeCampaignDF <- readxl::read_xlsx(
    path = campaign, 
    sheet = 1, 
    skip = 2,  #Skip the first 3 lines
    col_names = F)
  
  # apply colnames to dataframe LakeCampaign
  colnames(LakeCampaignDF) <- LakeCampaignColnames
  return(LakeCampaignDF)  # Do some processing and return back to the result
  
} #End of new function

# use these for testing function
# LakeCampaign <- readCampaign("N2_Bretaye_2018_06_16")
# PlotTitle="Bretaye July 2019"

#3.
plotCampaign <- function(LakeCampaign, PlotTitle) {
  
  # PLOT: x <- depth, y1 <- N^2, y2 <- Kd, y3 <- CH4, y4 <- T°
  # 3. Set parameters ----
  
  
  # x axis'
  x1 <- LakeCampaign[["Depth"]]
  
  
  # list(LakeCampaign)
  
  # y axis'
  y1 <- LakeCampaign[["Total conc."]] # Algea
  y2 <- LakeCampaign[["Green Algae"]] # green lagea
  y3 <- LakeCampaign[["Bluegreen"]] # Bluegreen
  y4 <- LakeCampaign[["Diatoms"]] # Diatoms
  
  Yall=c(y1, y2, y3, y4)
  
  minAllY=0 #min(Yall, na.rm = T)
  maxAllY=max(Yall, na.rm = T)
  
  # colors
  y1color="#00A572"
  y2color="#98FB98"
  y3color="#57A0D3"
  y4color="#8A9A5B"
  
  # axis titles
  y1text=expression(paste("Concentration"," (µg L"^-1,")"))
  y1legend=expression(paste("Total"))
  
  # y2text=expression(paste("Green"," (µg L"^-2,")"))
  y2legend=expression(paste("Green"))
  # 
  # y3text=expression(paste("Blue"," (µg L"^-2,")"))
  y3legend=expression(paste("Blue"))
  # 
  # y4text=expression(paste("Diatoms"," (µg L"^-2,")"))
  y4legend=expression(paste("Diatoms"))
  
  LegendList <- list(   #List of axis title and color for each variables
    legends = c(y1legend, y2legend, y3legend, y4legend), 
    legendcols = c(y1color, y2color, y3color, y4color)
  ) #Legendlist for the legend
  
  # list must have: x values, x axis title, y values, y value title, line color, line type, point or no points, 
  LoopList <- list(
    list(xval=x1, yval=y1, ytext=y1text, ylegend=y1legend, linecolor=y1color, linetypelty=1, makeaxis=T, axisside=1, axispostion=1, makepoints=F),  # N2
    list(xval=x1, yval=y2, ytext=y2text, ylegend=y2legend, linecolor=y2color, linetypelty=1, makeaxis=F, axisside=1, axispostion=5, makepoints=F),  # Kd
    list(xval=x1, yval=y3, ytext=y3text, ylegend=y3legend, linecolor=y3color, linetypelty=1, makeaxis=F, axisside=3, axispostion=1, makepoints=F),   # T°
    list(xval=x1, yval=y4, ytext=y4text, ylegend=y4legend, linecolor=y4color, linetypelty=1, makeaxis=F, axisside=3, axispostion=5, makepoints=F)  # CH4
  ) #Looplist using for graph
  
  
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
  
  par(mar=c(10, 4, 3, 2) + 0.1) # b,l, t, r (margins)
  
  # parameters=LoopList[[4]] # used for testing each parameters in Looplist
  # Run all the parameters (on assigne les parametres à des nouvelles variales -> VectorX,VectorY, etc) for each variables (Loopslit):
  for (parameters in LoopList) {  
    
    vectorX=parameters[["xval"]]   #xval =x1 = depth corr; caracteristics toujours la meme mais valeurs différentes
    minX=0
    maxX=ceiling(max(vectorX, na.rm = T)) # ceiling take higher value entiere
    xlims=c(minX, maxX)
    
    vectorY=parameters[["yval"]]
    minY=min(vectorY, na.rm = T)
    maxY=max(vectorY, na.rm = T)
    # ylims=c(minY, maxY) # allows for various different axis for each variable
    ylims=c(minAllY, maxAllY) # use this as we only want one axis, uses max of all data points, min is 0
    ytext=parameters[["ytext"]]
    
    linecolor=parameters[["linecolor"]]
    linetypelty=parameters[["linetypelty"]]
    axisside=parameters[["axisside"]]
    axispostion=parameters[["axispostion"]]
    
    makepoints=parameters[["makepoints"]]
    makeaxis=parameters[["makeaxis"]]
    
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
         lwd=plotlwd
    )
    
    
  #  if (makepoints) {points(x=vectorY, y=vectorX, pch=20, col=linecolor, lwd=plotlwd*1.5)} # add points?
    
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
    
    if (makeaxis) {
    
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
    
    
  }
    
    
    par(new=T)  
    
  } 
  # end of parameters Loop
  
  # 5. Draw X axis ----
  axis(2, pretty(c(minX, maxX),10), lwd=axislwd)
  mtext(text="Depth (m)", side=2, line=2, cex = mtextcex ) # side (1=bottom, 2=left, 3=top, 4=right)
  title(main=PlotTitle, cex.main=1.5)
  
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
  par(mfrow=c(1,3), oma = c(1.3,1,1,1)) # bottom,left, top, right
  
  for (Campaign in Lake) {  #prend chaque campagne de lake pour faire des trucs apres:
    
    filename=Campaign[["filename"]]   # Where the information is
    plottitle=Campaign[["plottitle"]]   #Give title
    
    lakecampaign=readCampaign(filename)
    testylisty=plotCampaign(lakecampaign, plottitle)
  }
  
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  
  legend(x=-0.7, y=-0.9, #"bottom",   # Add legend, one for the 3 graph
         testylisty[["legends"]],
         col = testylisty[["legendcols"]], 
         xpd = TRUE, 
         horiz = TRUE, 
         #       inset = c(0, -2), 
         bty = "n", 
         lty = c(1, 1 , 1, 1), 
         lwd = 3,
         pch = c(NA, NA, NA, NA), # only dot line for CH4
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




