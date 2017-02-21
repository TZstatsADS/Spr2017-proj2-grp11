double_axis_graph <- function(graf1,graf2){
  graf1 <- graf1
  
  graf2 <- graf2
  
  
  
  gtable1 <- ggplot_gtable(ggplot_build(graf1))
  
  gtable2 <- ggplot_gtable(ggplot_build(graf2))
  
  
  
  par <- c(subset(gtable1[['layout']], name=='panel', select=t:r))
  
  graf <- gtable_add_grob(gtable1, gtable2[['grobs']][[which(gtable2[['layout']][['name']]=='panel')]],
                          
                          par['t'],par['l'],par['b'],par['r'])
  
  
  
  ia <- which(gtable2[['layout']][['name']]=='axis-l')
  
  ga <- gtable2[['grobs']][[ia]]
  
  ax <- ga[['children']][[2]]
  
  ax[['widths']] <- rev(ax[['widths']])
  
  ax[['grobs']] <- rev(ax[['grobs']])
  
  ax[['grobs']][[1]][['x']] <- ax[['grobs']][[1]][['x']] - unit(1,'npc') + unit(0.15,'cm')
  
  graf <- gtable_add_cols(graf, gtable2[['widths']][gtable2[['layout']][ia, ][['l']]], length(graf[['widths']])-1)
  
  graf <- gtable_add_grob(graf, ax, par['t'], length(graf[['widths']])-1, par['b'])
  
  return(graf)
  
}