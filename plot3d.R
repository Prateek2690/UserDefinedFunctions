dataFrameTrace <- merge.data.frame(x = dataFrameTrace, y = dataReadUsingDelim, by.x = "SYMBOL", by.y = "SYMBOL", all.x = T)
dataFrame <- merge.data.frame(x = dataFrame, y = dataFrameTrace, by.x ="Cusip", by.y = "Cusip", all.x = T)

library(lattice)


 cloud(top20Hy$PriceChange ~ top20Hy$TRC_VOl_inBillions*top20Hy$Price,     screen = list(z = 105, x = -70), panel.aspect =  1, xlab = "Price", ylab = "Trc_vol", zlab = "ChangeInPrice", main = "3D Scatterplot", r = 0.2)
 
# text(top20Hy$PriceChange,  top20Hy$TRC_VOl_inBillions, top20Hy$Price, labels=top20Hy$Ticker, cex= 0.7)

dat <- top20Hy

top20Hy$TRC_VOl_inBillions



library(rgl)
with(dat,plot3d(dat$PriceChange,dat$Price,dat$TRC_VOl_inBillions, r= 0.2,  screen = list(z = 105, x = -70), panel.aspect =  1, xlab = "Price", ylab = "Trc_vol", zlab = "ChangeInPrice"))
with(dat,text3d(dat$PriceChange,dat$Price,dat$TRC_VOl_inBillions,dat$Ticker))
rgl.close()

# Basic 3d Plot
rgl.open()
rgl.points(dat$PriceChange,dat$Price,dat$TRC_VOl_inBillions, color = "blue", size = 5) #scatter plot
rgl.clear(); rgl.close()

# spheres
rgl.open()
rgl.bg(color = "white") # setup the background color
rgl.spheres(dat$PriceChange,dat$Price,dat$TRC_VOl_inBillions, r = 0.5, color = "grey")
rgl.clear(); rgl.close()



data(iris)

x<- sep.l <- iris$Sepal.Length
y <- pet.l <- iris$Petal.Length
z<- sep.w <- iris$Sepal.Width

rgl.open()# Open a new RGL device
rgl.bg(color = "white") # Setup the background color
rgl.spheres(x, y, z, r = 0.2, color = "grey") 




rgl_init()
rgl.spheres(x, y, z, r = 0.2 , color = "yellow")  # Scatter plot
rgl.bbox(color = "#333377") # Add bounding box decoration

x <- dat$TRC_VOl_inBillions/10^3; y<- dat$PriceChange; z<-dat$Price/10

radius <- top20Hy$TRC_VOl_inBillions/max(top20Hy$TRC_VOl_inBillions)/2 

rgl_init()
rgl.spheres(x, y, z, r = radius, color = "#D95F02") 
#rgl.add(x, y, z, show.bbox = TRUE)
# Compute and draw the ellipse of concentration
ellips <- ellipse3d(cov(cbind(x,y,z)), 
                    centre=c(mean(x), mean(y), mean(z)), level = 0.95)
shade3d(ellips, col = "#D95F02", alpha = 0.1, lit = FALSE)
aspect3d(1,1,1)
nrow(x)
text3d(x, y, z, dat$Ticker, cex = 1, adj = c(1,2), adjustcolor(rainbow(length(x))))




#' @param new.device a logical value. If TRUE, creates a new device
#' @param bg the background color of the device
#' @param width the width of the device
rgl_init <- function(new.device = FALSE, bg = "white", width = 640) { 
  if( new.device | rgl.cur() == 0 ) {
    rgl.open()
    par3d(windowRect = 50 + c( 0, 0, width, width ) )
    rgl.bg(color = bg )
  }
  rgl.clear(type = c("shapes", "bboxdeco"))
  rgl.viewpoint(theta = 15, phi = 20, zoom = 0.7)
  
}



colgroup <- c(1,2,3,4,5,6,7,8,9,10)
brands <-dat$Ticker



text3d(x = x, y = yv, z = zvar, text = brands, adj = c(2.5,2.5), cex = 0.7)

plot3d(x, y, z, bbox=TRUE,
       type="s", col=colgroup, 
       size=0.05, alpha=0.50, radius=radius,
       xlab="TRC_Vol", ylab="PriceChange", zlab="Price")
rgl.bbox(color=c("#333377","black"), emission="#333377",
         specular="#3333FF", shininess=5, alpha=0.8 )
text3d(x = x, y = y, z = z,
       text = brands, adj=c(0,1), cex=1)

rgl.clear()

rgl_add_axes <- function(x, y, z, axis.col = "grey",
                         xlab = "", ylab="", zlab="", show.plane = TRUE, 
                         show.bbox = FALSE, bbox.col = c("#333377","black"))
{ 
  
  lim <- function(x){c(-max(abs(x)), max(abs(x))) * 1.1}
  # Add axes
  xlim <- lim(x); ylim <- lim(y); zlim <- lim(z)
  rgl.lines(xlim, c(0, 0), c(0, 0), color = axis.col)
  rgl.lines(c(0, 0), ylim, c(0, 0), color = axis.col)
  rgl.lines(c(0, 0), c(0, 0), zlim, color = axis.col)
  
  # Add a point at the end of each axes to specify the direction
  axes <- rbind(c(xlim[2], 0, 0), c(0, ylim[2], 0), 
                c(0, 0, zlim[2]))
  rgl.points(axes, color = axis.col, size = 3)
  
  # Add axis labels
  rgl.texts(axes, text = c(xlab, ylab, zlab), color = axis.col,
            adj = c(0.01, -0.03), size = 0.1)
  
  # Add plane
  if(show.plane) 
    xlim <- xlim/1.1; zlim <- zlim /1.1
  rgl.quads( x = rep(xlim, each = 2), y = c(0, 0, 0, 0),
             z = c(zlim[1], zlim[2], zlim[2], zlim[1]))
  
  # Add bounding box decoration
  if(show.bbox){
    rgl.bbox(color=c(bbox.col[1],bbox.col[2]), alpha = 0.5, 
             emission=bbox.col[1], specular=bbox.col[1], shininess=5, 
             xlen = 3, ylen = 3, zlen = 3) 
  }
}

rgl.clear(); rgl.close()

rgl_add_axes(x,y,z)

rgl.init()




#****************************************
# Using htmlwidgets 
#***************************************

install.packages(htmlwidgets)
install.packages("threejs")

# imports
library(htmlwidgets)
library(threejs)

N <- 100
i <- sample(3,N, replace = TRUE)
x <- matrix(rnorm(N*3), ncol =  3)
lab <- c("small", "bigger", "biggest")
scatterplot3js(x, color = rainbow(N), labels = lab[i], size=i, renderer = "canvas")


# 3d Plot for TraceVol, Price and OAS
x <- top20Hy$TRC_VOl_inBillions/10^3
y <- top20Hy$PriceChange/10
z <- (top20Hy$OAS-top20Hy$Lastspread)/100

size <- x/5

scatterplot3js(x, y, z, color = rainbow(length(z)), axis =TRUE, size = size, renderer = "canvas" , bg = "white",  label.margin = "10x", grid = TRUE , signif = 2, axisLabels = c('TRC_vol', 'PriceChange', 'SpreadChange'), labels=sprintf("TRC_Vol (in millions) = %.2f, PriceChange /10 = %.2f, SpreadChange /100 =%.2f", x, y, z))
























