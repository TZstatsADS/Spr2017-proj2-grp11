#Step 1 Load the dataset
input_data =  read.csv("mydata.csv",header = T,as.is = T)
#remove NA and country that has zero trade value
input_data = na.omit(input_data)
input_data = input_data[!input_data$value == 0,]
#nrow(input_data) gives 17039

#Step 2 Convert df into Spatial Format

source_df<-data.frame(SourceLong=input_data$longitude,SourceLat=input_data$latitude)
# Create object of class SpatialPoints using SpatialPoints function from sp package
source_sp<-SpatialPoints(source_df, proj4string=CRS("+proj=longlat"))
str(source_sp)
# Convert to Spatial Dataframe
source_spdf <- SpatialPointsDataFrame(source_sp, data = source_df)
str(source_spdf)

comb_df<-input_data

#Step 3 Plot the interactive globe

#(1) create the underneath map
    data(wrld_simpl)                             # Basic country shapes
    bgcolor <- "#000025"
    earth <- tempfile(fileext=".jpg")
    jpeg(earth, width=2048, height=1024, quality=100, bg=bgcolor, antialias="default")
    par(mar = c(0,0,0,0),    pin = c(4,2),    pty = "m",    xaxs = "i",
        xaxt = "n",          xpd = FALSE,    yaxs = "i",    yaxt = "n")
    
    map_palette <- apply(col2rgb(heat.colors(5)[5:1])/768,2,function(x)rgb(x[1],x[2],x[3]))
    # Restrict bono data to countries from the maptools package
    bono <- bono[rownames(bono) %in% wrld_simpl$NAME, ,drop=FALSE]
    # Set a default color for each country and the colors from the bono data
    clrs <- rep(map_palette[1], length(wrld_simpl$NAME))
    names(clrs) <- wrld_simpl$NAME
    clrs[rownames(bono)] <- map_palette[bono$index]
    
    plot(wrld_simpl,  col=clrs,   bg=bgcolor,        border="cyan",  ann=FALSE,
         axes=FALSE,  xpd=FALSE,  xlim=c(-180,180), ylim=c(-90,90),  setParUsrBB=TRUE)
    
    graphics.off()
    legendcol=heat.colors(5)[5:1]

#(2)plot the 3D globe
globejs(img=earth, bg="white", emissive="#aaaacc",
        lat=comb_df[,4], long=comb_df[,3],value = 1,color = "yellow", 
        arcs=comb_df[,c(4,3,9,8)],
        arcsHeight= 0.4, 
        arcsLwd   = 0.5, 
        arcsColor = "cornflowerblue", 
        arcsOpacity=0.5,
        atmosphere=TRUE, height=600, width = 600
)