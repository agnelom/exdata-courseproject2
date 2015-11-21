plot3 <- function(){
    
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

##    png("plot3.png",height = 480, width = 480,bg="transparent")
    ggPlot4 <- ggplot(yearTots,aes(factor(year), Emissions),fill=type) +
            geom_bar(stat="identity") + theme_bw()
            facet_grid(. ~ type)+
            labs(title="1999 - 2008 Totak PM2.5 Emissions in Baltimore City, by Source Type" ,x="Year", y="PM2.5 Emissions (tons)")
    print(ggPlot4)
##    dev.off()
}
