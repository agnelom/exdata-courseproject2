## Function plot4 will create a bar char using ggplot2's quick plot (qplot)
## The goal is to explore the how the emissions changed across the US due to Coal Combustions 
plot4 <- function(){
    library(sqldf)
    library(dplyr)
    library(ggplot2)

    ## Define the Path     
    fPath <- file.path("exdata-data-NEI_data")
    
    ## Load the data from .rds files 
    NEI <- readRDS(file.path(fPath,"summarySCC_PM25.rds"))
    SCC <- readRDS(file.path(fPath,"Source_Classification_Code.rds"))
    
    ## Look for SCC codes from the SCC file related to Coal Combustion
    CoalCombSCC <- sqldf("SELECT SCC FROM SCC WHERE [Short.Name] like '%Comb%'and [SCC.Level.Four] like '%Coal%'")
    
    ## Select the relevane rows from NEI, these are those rows that match SCC in CoalCombSCC
    ## And perform a group_by to group the data by year
    CoalCombNEI <- sqldf("SELECT * FROM NEI JOIN CoalCombSCC using(SCC)")
    NEIyearGrp <- group_by(CoalCombNEI,year)

    ## Summerizing the the grouped subset data for year to get sum totals of Emissions by year 
    yearTots <- summarise(NEIyearGrp,Emissions = sum(Emissions))

    ## Dividing Emissions by 1000, in order to represent as kilo tons on the chart
    yearTots <- mutate(yearTots, Emissions = Emissions/1000)
    
    ## Opening a PNG port to copy the chart to a PNG file called plot4.png
    png("plot4.png",height = 480, width = 480,bg="transparent")
    
    ## Using qplot from ggplot2's plotting system to create a simple bar chart of emissions dies to Coal combustion across US
    ggPlot4 <- qplot(factor(year),
                     Emissions, 
                     data=yearTots, 
                     geom = "bar", 
                     stat = "identity",
                     main = "1999 - 2008 PM2.5 Emissions across US due to Coal Combustion",
                     xlab = "Year",
                     ylab = "PM2.5 Emissions (Kilo. tons)"
                    )
    print(ggPlot4)
    ## Closing the port
    dev.off()
}
