### US CENSUS - IMPROVED FARMLAND 

test <- readGDAL('./data/artif_drained/grids/cty2mc/cty2mc.e00')

acr_file = "./data/artif_drained/grids/cty2mc/cty2mc.e00"
dr = ogr.GetDriverByName("AVCE00")
f = dr.Open(arc_file)

library(RArcInfo)

#Number of polygons
nmuni<-length(palmuni[[1]][[1]])
e00toavc('./data/artif_drained/grids/cty2mc/cty2mc.e00', "valencia")




# this gets tehe data
palmuni<-get.paldata(".", "valencia")



patmuni<-get.tabledata("./info", "VALENCIA.PAT")

patmuni <- as.data.frame(patmuni)


plotpal(arc=arcsmuni, palmuni)



arcsmuni<-get.arcdata(".", "valencia")
bnd.muni<-get.bnddata("info/", "VALENCIA.BND")
l <- get.labdata(datadir, coverage, filename="lab.adf")


#Number of polygons
nmuni<-length(patmuni[[1]][[1]])
municipios<-data.frame(1:nmuni, patmuni$"VALENCIA-ID")
names(municipios)<-c("INDEX", "CODMUNICI")
#Datafiles to be used
unemp<-read.table(file="data_valencia.csv", sep=";",
                  dec = ",",skip=1)
