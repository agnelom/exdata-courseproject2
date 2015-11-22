## Function plot3() will create a faceted bar charts using the ggplot2 plotting system
## The goal is to explore increase or decrease in emissions in Baltimore City from 1999 - 2008 for each of the four source types (point, nonpoint, onroad, nonroad)
plot3 <- function(){

    library(dplyr)
    library(ggplot2)
    
    ## Define the Path     
    fPath <- file.path("exdata-data-NEI_data")
    
    ## Load the data from .rds files 
    NEI <- readRDS(file.path(fPath,"summarySCC_PM25.rds"))
    SCC <- readRDS(file.path(fPath,"Source_Classification_Code.rds"))
    
    ## Retrieve all records for fips=24510, I am going to use the filter command of the dplyr package
    ## But we need to first group the data from NEI by year and type
    NEItypeGrp <- group_by(NEI,year,type)
    NEIfips24510 <- filter(NEItypeGrp, fips==24510)
    
    ## Summerizing the the grouped subset data for fips=24510 to get sum totals by year for Baltimore city
    yearTots <- summarise(NEIfips24510,Emissions = sum(Emissions))
    ## Factorize 'year' so as to use it in the fill argument of aes, this will help create legends named 'year'
    yearTots$year <- factor(yearTots$year)

    ## open the PNG port to save the chart as a PNG file
    png("plot3.png",height = 640, width = 640,bg="transparent")
    ## create the bar chart with facets in source type, using the ggplot2 ploting system
    ggPlot3 <- ggplot(yearTots,aes(factor(year), Emissions,fill = year)) +
            geom_bar(stat="identity") + 
            facet_grid(. ~ type)+
            labs(title="1999 - 2008 Totak PM2.5 Emissions in Baltimore City, by Source Type" ,x="Year", y="PM2.5 Emissions (tons)")
    print(ggPlot3)
    
    ## Close the port
    dev.off()
}
