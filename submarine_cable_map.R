library(geojsonsf)
library(sf)
library(rayrender)
library("rstudioapi") 

# From https://gist.github.com/tylermorganwall/b222fcebcac3de56a6e144d73d166322
# Data source: https://github.com/telegeography/www.submarinecablemap.com

curDir = dirname(getSourceEditorContext()$path)
geojsonFile = paste(curDir,"/cables.geojson",sep = "")
imageTextureFile = paste(curDir,"/2k_earth_daymap.jpg",sep = "")

cables = geojson_sf(geojsonFile)

cablescene = list()
counter = 1
for(i in 1:length(cables$geometry)) {
  for(j in 1:length(cables$geometry[[i]])) {
    temp = cables$geometry[[i]][[j]]
    cableval = data.frame(x=sinpi(temp[,1]/180)*cospi(temp[,2]/180),
                          y=sinpi(temp[,2]/180),
                          z=cospi(temp[,1]/180)*cospi(temp[,2]/180))
    #Don't lower start of line at the 180/0 longitude border
    if(abs(temp[1,1] - 180) > 0.001 && abs(temp[1,1] + 180) > 0.001) {
      cableval[1,] = cableval[1,] * 1/1.02
    }
    nr = nrow(temp)
    #Don't lower end of line at the 180/0 longitude border
    if(abs(temp[nr,1] - 180) > 0.001 && abs(temp[nr,1] + 180) > 0.001) {
      nr = nrow(cableval)
      cableval[nr,] = cableval[nr,] * 1/1.02
    } 
  cablescene[[counter]] = path(cableval, width = 0.005,material=diffuse(color=cables$color[i]))
  counter = counter + 1
  }
}
fullcablescene = do.call(rbind,cablescene)

for(i in seq(1,720,by=1)) {
  group_objects(fullcablescene,scale=c(1,1,1)*1.02) %>% 
    add_object(sphere(radius=0.99,material=diffuse(image_texture = imageTextureFile),angle=c(0,-90,0))) %>% 
    group_objects(angle=c(0,-i/2,0)) %>% 
    add_object(sphere(y=5,z=5,x=5,material=light(intensity = 80,color="lightblue"))) %>% 
    add_object(sphere(y=5,z=5,x=-5,material=light(intensity = 10,color="orange"))) %>% 
    add_object(sphere(y=-10,material=light(intensity = 3,color="white"))) %>%
    render_scene(samples=64,width=1200,height=1200,fov=0,aperture=0, ortho_dimensions = c(2.3,2.3),
                 sample_method = "sobol_blue",filename=paste(curDir,sprintf("/smallcables%d.png",i),sep = ""))
}

