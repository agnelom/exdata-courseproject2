plot5 <- function(){
    
    library(sqldf)
    library(dplyr)
    library(ggplot2)

        ## Define the Path     
    fPath <- file.path("exdata-data-NEI_data")
    
    ## Load the data from .rds files 
    NEI <- readRDS(file.path(fPath,"summarySCC_PM25.rds"))
    SCC <- readRDS(file.path(fPath,"Source_Classification_Code.rds"))
    
    ## Look for SCC codes from the SCC file related to Coal Combustion
    MobileVahicleSCC <- sqldf("SELECT SCC FROM SCC WHERE [EI.Sector] like '%Mobile%' or [EI.Sector] like '%Vehicle%'  ")
    
    ## Select the relevane rows from NEI, these are those rows that match SCC in CoalCombSCC
    ## And perform a group_by to group the data by year
    MotorVehicleNEI <- sqldf("SELECT * FROM NEI JOIN MobileVahicleSCC using(SCC)")
    NEIyearGrp <- group_by(MotorVehicleNEI,year)
    NEIfips24510 <- filter(NEIyearGrp, fips==24510)

    ## Summerizing the the grouped subset data for year to get sum totals by year 
    yearTots <- summarise(NEIfips24510,Emissions = sum(Emissions))
    
    ## Dividing Emissions by 1000, in order to represent as kilo tons on the chart
    ##yearTots <- mutate(yearTots, Emissions = Emissions/1000)
    
    ## Opening a PNG port to copy the chart to a PNG file called plot5.png
    png("plot5.png",height = 480, width = 480,bg="transparent")
    ggPlot5 <- ggplot(yearTots,aes(factor(year), Emissions)) +
        geom_bar(stat="identity") + 
        labs(title="1999 - 2008 Emissions Changes in Baltimore City due to Motor Vehicles" ,x="Year", y="PM2.5 Emissions (tons)")
    print(ggPlot5)
    dev.off()
}