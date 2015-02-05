
# access SQL server to collect residential rents from scrapes
# only past 3 months data is collected
cat('computing....do not exit\n\n')

#suppressMessages(install.packages("RODBC",repos="http://cran.rstudio.com/",lib="S:/GarrettW/R/R-3.1.2/library/"))
suppressMessages(library(RODBC,lib="S:/GarrettW/R/R-3.1.2/library/"))
sqlconn = odbcConnect("Residential", uid="gwiegele",pwd="Midfield16")

rent.data = sqlQuery(sqlconn, "SELECT tblResi_scrape.propertyID, 
                                      tblResi_scrape.bed, tblResi_scrape.bath, tblResi_scrape.sf, 
                                      tblResi_scrape.price, tblResi_scrape.PSF, 
                                      tblResi_propertyKey.zip, tblResi_propertyKey.latitude, 
                                      tblResi_propertyKey.longitude
                              FROM    tblResi_scrape INNER JOIN
                                      tblResi_propertyKey ON tblResi_scrape.propertyID = tblResi_propertyKey.propertyID
                              WHERE   (tblResi_scrape.date >= GETDATE() - 90)
                              ORDER BY zip")
close(sqlconn)

# get zips to numeric for aggregate

rent.data$zip = substring(rent.data$zip,1,5)
rent.data$zip = as.numeric(as.character(rent.data$zip))
rent.data = na.omit(rent.data)

# aggregate data by common zips

#agg.data = aggregate(rent.data,list(rent.data$zip),mean)

#suppressMessages(install.packages("maps",repos="http://cran.rstudio.com/",lib="S:/GarrettW/R/R-3.1.2/library/"))
#suppressMessages(install.packages("mapdata",repos="http://cran.rstudio.com/",lib="S:/GarrettW/R/R-3.1.2/library/"))
#suppressMessages(install.packages("spatstat",repos="http://cran.rstudio.com/",lib="S:/GarrettW/R/R-3.1.2/library/"))
suppressMessages(library(maps,lib="S:/GarrettW/R/R-3.1.2/library/"))
suppressMessages(library(mapdata,lib="S:/GarrettW/R/R-3.1.2/library/"))
suppressMessages(library(spatstat,lib="S:/GarrettW/R/R-3.1.2/library/"))

longitudeLimits=c(-125, -66)
latitudeLimits=c(25, 50)
spatstat.options(npixel=c(1000,1000));
my.palette = colorRampPalette(c("gray90","blue","green","yellow","orange","red"), 
                              bias=5, space="rgb") 
points=ppp(rent.data$longitude, 
           rent.data$latitude, longitudeLimits,
           latitudeLimits, check=FALSE)

png(filename="S:/GarrettW/Mapping/USheatmap-psf.png", bg="gray90", width=10.*240, height=10.*240, pointsize=1) #saves a png file to the working directory
densitymap=density(points, sigma=0.15, weights=rent.data$psf)
map("state", col="white", fill=TRUE, bg="white", lwd=1.0, xlim=longitudeLimits, ylim=latitudeLimits); #loads the US state map
image(densitymap, col=my.palette(300), add=TRUE); #adds the heatmap image
map("state", col="black", lwd=1.0, xlim=longitudeLimits, ylim=latitudeLimits, add=TRUE); #adds state borders
map.cities(country="USA",label=TRUE,xlim=c(-125,-66),ylim=c(25,50),cex=15,minpop=350000);
text(x=mean(longitudeLimits),y=max(latitudeLimits) - 0.01*max(latitudeLimits),"US Heatmap of Resi Rent Price per Square Foot",cex=60)
box()
dev.off()

# Alternative with Google Maps
suppressMessages(library(RgoogleMaps,lib="S:/GarrettW/R/R-3.1.2/library/"))

back.map = GetMap.bbox(lonR=longitudeLimits,latR=latitudeLimits,destfile="G:/Mapping/googmap.png",format="png32");
PlotOnStaticMap(back.map)
image(densitymap, col=my.palette(300), add=TRUE)
points(nyc.rent$longitude, 
       nyc.rent$latitude)










