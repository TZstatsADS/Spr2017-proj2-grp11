globe_3D <- function(spatial_df){
  comb_df = spatial_df
  earth <- "../fig/worldmap.jpg"
  globejs(img=earth, bg="white", emissive="#aaaacc",fov = 35,
          lat=comb_df[,4], long=comb_df[,3],value = 1,color = "yellow", 
          arcs=comb_df[,c(4,3,9,8)],
          arcsHeight=0.4, 
          arcsLwd=comb_df$log, 
          arcsColor="cornflowerblue", 
          arcsOpacity=comb_df$log/max(comb_df$log)+0.5,
          atmosphere=TRUE, height=600, width = 600
          )
}