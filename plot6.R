## Function plot6() creates a faceted bar plot for comparison of PM2.5 emissions in Baltimore City and Los Angeles County
## Effected due to Motor Vehicle sources
plot6 <- function(){
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
    MotorVehicleNEI <- sqldf("SELECT * FROM NEI JOIN MobileVahicleSCC using(SCC)")
    ## Filter MotorVehicleNEI to get rows only for 06037 and 24510
    NEIfips2451006037 <- filter(MotorVehicleNEI, fips=="06037" | fips=="24510")
    ## factorizing fips with City and County Names
    NEIfips2451006037$fips <- factor(NEIfips2451006037$fips,labels = c("Los Angeles County","Baltimore City"))
    ## And perform a group_by to group the data by year and fips
    NEIyearGrp <- group_by(NEIfips2451006037,year,fips)
    ## Summerizing the the grouped subset data for year to get sum totals by year 
    yearTots <- summarise(NEIyearGrp,Emissions = sum(Emissions))
    
    ## Factorize 'year' so as to use it in the fill argument of aes, this will help create legends named 'year'
    yearTots$year <- factor(yearTots$year)
    
    ## Opening a PNG port to copy the chart to a PNG file called plot6.png
    png("plot6.png",height = 480, width = 480,bg="transparent")

        ## using ggplot() to create a facted bar chart for comparison between Baltimore City and Los Angeles County
        ggPlot6 <- ggplot(yearTots,aes(factor(year), Emissions,fill = year)) +
            geom_bar(stat="identity") + 
            facet_grid(. ~ fips) +
            labs(title="1999 - 2008 Motor Vehicle Emissions Change Comparison" ,x="Year", y="PM2.5 Emissions (tons)")
    
        print(ggPlot6)
    ## Close the port
    dev.off()
}