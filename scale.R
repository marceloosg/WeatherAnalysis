scale=function(char){
        char=toupper(char)
        valid=c(as.character(0:9),"H","K", "M","B")
        value=c(0:9,2,3,6,9)
        if(char %in%  valid){
                return(10^value[which(char==valid)])
        }
        return(1)
}
ApplyScale=function(charv,valuev){
        charv=unlist(lapply(charv,scale))
        valuev=as.numeric(as.character(valuev))*charv
        valuev
}

getTop=function(label,dt,maketotal=T,n=1){
        if(maketotal){
                dt[,TOTAL_PROPERTY_DAMAGE:=PROPERTY_DAMAGE+CROP_DAMAGE]
                dt[,TOTAL_HEALTH_DAMAGE:=FATALITIES+INJURIES]
        }
        setorder(dt,-TOTAL_PROPERTY_DAMAGE)
        top_event=data.table(SOURCE=label,TOTAL_PROPERTY_DAMAGE=head(dt,n)$EVTYPE)
        setorder(dt,-TOTAL_HEALTH_DAMAGE)
        top_event$AFFECTED_PEOPLE=head(dt,n)$EVTYPE
        setorder(dt,-FATALITIES)
        top_event$FATALITIES=head(dt,n)$EVTYPE
        top_event
}
getTopValues=function(label,dt,n=1){
        setorder(dt,-TOTAL_PROPERTY_DAMAGE)
        top_event=data.table(SOURCE=label,TOTAL_PROPERTY_DAMAGE=head(dt,n)$TOTAL_PROPERTY_DAMAGE)
        setorder(dt,-TOTAL_HEALTH_DAMAGE)
        top_event$AFFECTED_PEOPLE=head(dt,n)$TOTAL_HEALTH_DAMAGE
        setorder(dt,-FATALITIES)
        top_event$FATALITIES=head(dt,n)$FATALITIES
        top_event
}
getItem=function(item,mapset,label=item){
        index=grep(item,mapset[["ALL"]])
        mapset[["index"]]=setdiff(mapset[["index"]],index)
        if(length(index)>0){
                mapset[["keys"]]=c(mapset[["keys"]],label)
                mapset[label]=list(index)
                mapset[["labels"]]=c(mapset[["labels"]],item)
        }
        mapset
}
lastItem=function(label,mapset){
        index=mapset[["index"]]
        mapset[["index"]]=setdiff(mapset[["index"]],index)
        if(length(index)>0){
                mapset[["keys"]]=c(mapset[["keys"]],label)
                mapset[["labels"]]=c(mapset[["labels"]],label)
                mapset[label]=list(index)
        }
        mapset
}
getEvents=function(storm){
        set=toupper(levels(storm$EVTYPE))
        mapset=list()
        mapset["index"]=list(1:length(set))
        mapset["ALL"]=list(set)
        mapset=getItem("CURRENT|SEA|MARINE",mapset,"SEA")
        mapset=getItem("SLIDE|SLUMP",mapset,"LANDSLIDE")
        mapset=getItem("LIGN|LIGHTNING|LIGHTING",mapset,"LIGHTNING")
        mapset=getItem("WIND|WND",mapset,"WIND")
        mapset=getItem("CLOUD|FUNNEL",mapset,"FUNNEL CLOUD")
        mapset=getItem("SPOUT",mapset,"WATERSPOUT")
        mapset=getItem("DRY|DROUGHT|DRIEST",mapset,"DRY")
        mapset=getItem("AVALAN",mapset,"AVALANCHE")
        mapset=getItem("BLIZZARD",mapset)
        mapset=getItem("FOG|SMOKE",mapset)
        mapset=getItem("DROWNING",mapset)
        mapset=getItem("SURF",mapset)
        mapset=getItem("SWELL",mapset)
        mapset=getItem("GUST",mapset)
        mapset=getItem("DAM",mapset)
        mapset=getItem("DUST",mapset)
        mapset=getItem("TORNADO|TORNDAO",mapset,"TORNADO")
        mapset=getItem("HURRICANE|TYPHOON",mapset)
        mapset=getItem("HEAT|HOT|WARM|HIGH TEMP|RECORD HIGH|RECORD TEMP|TEMPERATURE RECORD",mapset,"HEAT")
        mapset=getItem("WINTER|COLD|FREEZ|COOL|LOW TEMP|FROST|RECORD LOW|HYP|ICE|ICY",mapset,"COLD")
        mapset=getItem("STORM|TSTM",mapset,"STORM")
        mapset=getItem("URBAN|FLOO|FLDG|STREAM|RISING WAT|HIGH WATER",mapset,"FLOOD")
        mapset=getItem("TSUNAMI|WAVE|TIDE",mapset,"TSUNAMI")
        mapset=getItem("FIRE",mapset)
        mapset=getItem("HAIL",mapset)
        mapset=getItem("SLEET",mapset)
        mapset=getItem("SNOW",mapset)
        mapset=getItem("VOLCANIC",mapset)
        mapset=getItem("RAIN|PRECIP|WET",mapset,"RAIN")
        mapset=lastItem("OTHER",mapset)
}
applyMapset=function(dataset,mapset){
        dataset$EVTYPE=toupper(dataset$EVTYPE)
        for(key in mapset[["keys"]]){
                dataset[EVTYPE %in% mapset[["ALL"]][mapset[[key]]],]$EVTYPE=key
        }
        dataset
}

# Multiple plot function
# source:http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        library(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
                print(plots[[1]])
                
        } else {
                # Set up the page
                grid.newpage()
          #      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                slice.wd=0.8/numPlots
                space=0.2/numPlots
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        p=0.95-slice.wd/2-(slice.wd+space)*(i-1)
                        print(plots[[i]], vp = viewport(x=0.5, y=p, w=1, h=slice.wd))
                              #(layout.pos.row = matchidx$row,
                               #                         layout.pos.col = matchidx$col))
                }
        }
}