### EDA Function with creating PDF

eda_pdf <- function( 
    data                    #no default data
    , filename = "eda.pdf"   #default filename
    , col_id = ""
    ) {
  
  ## extract numeric and integer columns
  x <- Filter(is.numeric, data)

  # set identifier for points
  if( length(col_id) > 0 ) {
  	if( col_id %in% colnames(x) ) {
  		y = x[col_id]
			x = x[ grep( col_id, colnames(x), invert = TRUE)]
  	} else {
  		warning( paste0( col_id, " not a column in data! Outlier labled with rownames."))
  		y$id = rownames(x)
  	}
  } else {
  	y$id = rownames(x)
  }

  ## create pdf where the diagrams are put in
  pdf(
    file = filename                #the filename defined in function eda_pdf
    , paper = "a4"
    , title = "Exploratory Data Analyses"
    ) 
  def.par <- par( no.readonly = TRUE)      # save the default graphical parameters

  ## create 6 diagrams for each pdf page with the numeric/integer data
  
  if( ncol(x) > 0) {                    #if there is numeric/integer data
    sapply( 1:ncol(x), function(i) {    #apply the function to all columns with data stored (without id)
      
      #divide the device into three rows and two columns
      layout( matrix( c(1:6), 3, 2, byrow = TRUE))      # allocate each figure in one cell

      # define the graphical parameters
      par( mfrow = c( 3,2)
          , cex = 0.8                       # reduces the size of the text and symbols
          , oma = c(0, 0, 2, 0)             # set outer margin
          , mar = c( 3.5,3,2.6,0.5)+0.1     # set inner margins
          , mgp = c( 1.7, 0.6,0)            # set margin lines for the axis title, axis labels & axis line
          )  
      
      ## density histogram 
      hist( x[ ,i]
          , freq = FALSE      # set to density
          , ylab = "Dichte"
          , xlab = ""
          , main = "Histogramm"
          )
    
      ## add the colname as title above the diagrams
      mtext( colnames(x)[i]
          , side = 3             # set to top of the diagrams
          , outer = TRUE         # inside the outer margins
          , line = .7            # set line for the title
          , cex = 1.5            # magnify the title
          , font = 2             # set text to bold
          )
    
      ## boxplot with labelled outliers
      
      # create boxplot
      bxpdat <- boxplot( x[i]
                        , horizontal = TRUE
                        , outpch = NA       # supress plotting of outliers
                        , main = "Boxplot"
                        )
	    # create y values of points for jittering
      set.seed(42)
      ploty = jitter( rep( 1, length( x[[i]]))
                    , factor = 16
                    )
      
      # add points to boxplot
      points( x = x[[i]]
            , y = ploty
            , col = "blue"
            )
  
      # label outlier in boxplot
      if( length(bxpdat$out) > 0 ) {
            # label outliers
            text( x = bxpdat$out         
                , y = ploty[ x[[i]] %in% bxpdat$out ]
                , labels = y$id[ x[[i]] %in% bxpdat$out ]
                , pos = 3
                , cex = .8
                )
      } # end if for outlier
      

      ##density plot
      # define the bandwidth of the density 
      qa <- diff( quantile( x[ ,i]
                            , c( 1/4, 3/4)
                            , na.rm=TRUE)
                  )
      # save density
      dest <- density( x[ ,i]
                       , width = qa
                       , na.rm = TRUE
                       )
      
      # plot density 
      plot( dest
            , ylab = "Dichte"
            , main = "Dichtesch?tzer"
            ) 
      
      ## qq plot
      qqnorm( x[ ,i]
              , datax = TRUE
              , ylab = "Stichprobenquantile"
              , xlab = "Theoretische Quantile"
              )
      qqline( x[ ,i]
              , datax = TRUE
              )
      
      ## time series plot 
      ts.plot( x[ ,i]
               , ylab = ""
               , xlab = "Zeit bzw. Index"
               , main = "Zeitreihe"
               )
      
      ## autocorrelation plot
      acf( x[ ,i]         #plot estimates of the autocorrelation function 
           , main = "Autokorrelation" 
           ) 
    
    }) # end sapply
  } # end if
  
  
  par( def.par)  # reset to default
 
  ## add factor data
  
  fac <- Filter(is.factor, data)
  x2 <- cbind(fac, x)
  
  ## new layout for next plot
  layout( matrix( c(1)))
  
  ## Basic Scatterplot Matrix
  
  if( ncol(x2) > 0) {
      pairs( ~.                 #Warum dieses Zeichen?
           , data=x2
           , main="Scatterplot Matrix"
           )
  }  # end if

  graphics.off()  #close current active graphic device 

}   # end function