###########################################################################
#--------------------------------------------------------------------------
# HARVARD PROFESSIONAL DATA SCIENCE SPECIALIZATION
# CAPSTONE 2 - Italian Airports Flights Prevision
# PIETRO D'AMBROSIO  pietro.dambrosio@ordingsa.it
# https://github.com/pietrodambrosio/HarvardX_DataScience
#--------------------------------------------------------------------------
###########################################################################

#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
# disable scientific notation
options(scipen=999)

# load packages
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(xlsx)) install.packages("xlsx", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(viridis)) install.packages("viridis", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(caTools)) install.packages("caTools", repos = "http://cran.us.r-project.org")
if(!require(ggridges)) install.packages("ggridges", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(ranger)) install.packages("ranger", repos = "http://cran.us.r-project.org")


#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------

##############################################################################################################
##############################################################################################################
##############################################################################################################
# STEP 1 - DATA HARVESTING
##############################################################################################################
##############################################################################################################
##############################################################################################################


#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
# DATA HARVESTING - from www.assaeroporti.com - data of many italian airports
#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
# in this case the data are made available on excel file sheets downloadable from specific addresses 
# (these addresses have been taken from the button on the html page) 
#-------------------------------------------------------------------------------------------------------------


df_tot2 = data.frame()

# with this loop 
# all the files (in excel format) published on the assaeroporti site are downloaded

for (year in 2001:2019) {
  for (mm in 1:12) {
    # we only take the data from July 2001 onwards (before this date there are some missing months)
    if (year == 2001 & mm <= 6) {next}  
    # the data was downloaded until 2019 august
    if (year == 2019 & mm > 8) {next}   
    month = formatC(mm, width=2, flag="0")
      urlfile = paste(year,"_",month,".xls",sep="")
      url = paste("https://assaeroporti.com/wp-content/plugins/multipage_xls_reader/excel_file/",urlfile,sep="")
      # we use only a temporary file for the download.
      tempfile = "tempfile.xls"
      
      # we are forced to download the file because it contains several sheets, 
      # each with different sets of information to be acquired.
      tryCatch(
        {
          print(paste("Download file",url))
          download.file(url,destfile=tempfile,mode = "wb",quiet = TRUE)
        },
        error=function(cond) {
          print(paste("FILE",urlfile,"NOT FOUND !!!"))
        },
        warning=function(cond) {
          
        }
      )
       if (file.exists(tempfile) & file.size(tempfile) > 0) {
  
          # we read the file just downloaded to be able to acquire the information of interest from the various sheets
          
          # SHEET 6 elaboration
          df = read.xlsx(tempfile,sheetIndex = 6)
          if (length(colnames(df)) < 14) {print ("Error! column numbers sheet 6");next()}
          df = df[-1,]
          df = df[,c(2:14)]
          df$year = year
          df$month = mm
          df = df[,c(1,14,15,2:13)]
          colnames(df) = c("airport","year","month","tm_mov_naz","p_tm_mov_naz","tm_mov_internaz","p_tm_mov_internaz",
                           "tm_mov_UE","p_tm_mov_UE","tm_mov_tcomm","p_tm_mov_tcomm","tm_mov_tgenav","p_tm_mov_tgenav",
                           "tm_mov_tot","p_tm_mov_tot")
          df_tot = df
          
          # SHEET 3 elaboration
          df = read.xlsx(tempfile,sheetIndex = 3)
          if (length(colnames(df)) < 14) {print ("Error! column numbers sheet 3");next()}
          df = df[-1,]
          df = df[,c(2:14)]
          colnames(df) = c("airport","tp_mov_naz","p_tp_mov_naz","tp_mov_internaz","p_tp_mov_internaz","tp_mov_UE","p_tp_mov_UE",
                           "tp_mov_tcomm","p_tp_mov_tcomm","tp_mov_tgenav","p_tp_mov_tgenav","tp_mov_tot","p_tp_mov_tot")
          df_tot = merge(df_tot,df,by="airport")
 
          df_tot2 = rbind(df_tot2,df_tot)
      } else {
         print("----------- missing file")
      }
   }
}

file.remove(tempfile)

#### dfap contains all data downloaded from www.assaeroporti.com
dfap = df_tot2
save(dfap,file="save_dfap.Rdata")


#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
# DATA HARVESTING - from www.flightradar24.com - rating of all italian airports
#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
# in this case the information of interest has been extracted directly from the html page relating to 
# the italian airports. 
#-------------------------------------------------------------------------------------------------------------

url = "https://www.flightradar24.com/data/airports/italy"

# extraction of the table containing the codes, names and rating of Italian airports
tab <- url %>%
  xml2::read_html() %>%
  html_nodes(xpath='//*[@id="tbl-datatable"]') %>%
  html_table()

tab = as.data.frame(tab)[,2]
ltab = length(tab)

# create empty data.frame
df = data.frame(airport=character(),code=character(),rating=numeric(),stringsAsFactors = FALSE)

# extract data from html table
for (i in 1:ltab) {
  riga = tab[i]
  if (grepl("Rating:", riga))  {
    print (riga)
    p = str_locate(riga,"[(]")
    if (p[1] > 0) {
      airport = substr(riga,1,p[1]-1)
      resto = substr(riga,p[1],nchar(riga))
      p = str_locate(resto,"[])]")
      if (p[1] > 0) {
        code = substr(resto,2,p[1]-1)
        resto = substr(resto,p[1],nchar(resto))
        p = str_locate(resto,"Rating: ")
        if (p[2] > 0) {
          rating = substr(resto,p[2]+1,p[2]+2)
          print (paste(airport,"-",code,"-",rating))
          df = rbind(df,c(airport,code,as.numeric(rating)),stringsAsFactors = FALSE)
        } else {
          print(paste("Error! rating",i,resto))
          stop()
        }
      } else {
        print(paste("Error! code airport",i,resto))
        stop()
      }
    } else {
      print(paste("Error! airport",i,riga))
      stop()
    }
  }
}
colnames(df) = c("airport","code","rating")

#### dfrt contains all data downloaded from www.flightradar24.com
dfrt = df
save(dfrt,file="save_dfrt.Rdata")

#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
# DATA HARVESTING - from https://openflights.org/ - routes of all italian airports
#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
# in this case the "routes" file is published on the site and is in text format. 
# Routes are coded using airport codes (which we will take from the information extracted previously).
#-------------------------------------------------------------------------------------------------------------

url = "https://raw.githubusercontent.com/jpatokal/openflights/master/data/routes.dat"

#### dfro contains all data downloaded from openflight.com
dfro = fread(url)

colnames(dfro) = c("comp","d1","from","d2","to","d3","d4","d5","d6")

# we only extract company, airport_from, airport_to
dfro = dfro[,c(1,3,5)]
p = dfro %>% group_by(from) %>% summarize(part = n())
a = dfro %>% group_by(to) %>% summarize(arr = n())
nrow(dfro)
dfro = dfro %>% left_join(p,by="from") %>% left_join(a,by="to")
nrow(dfro)

# we are not interested in knowing the individual flights made by the individual companies, 
# but only the different destinations or origins. Then we carry out a grouping of data
ap = dfro %>% group_by(from) %>% summarise(tot_par = sum(part))
aa = dfro %>% group_by(to) %>% summarise(tot_par = sum(arr))

colnames(ap) = c("cod3","nroutes")
colnames(aa) = c("cod3","nroutes")
dfro = rbind(ap,aa)

dfro = dfro %>% group_by(cod3) %>% summarise(nroutes = sum(nroutes))

#### dfro contains all data downloaded from openflights.org
save(dfro,file="save_dfro.Rdata")


##############################################################################################################
##############################################################################################################
##############################################################################################################
# STEP 2 - DATA CLEANING AND NORMALIZATION
##############################################################################################################
##############################################################################################################
##############################################################################################################

# in this step we identify the rows containing NULL values and the rows with total or those containing notes 
# and all references to notes. So airport names are normalized because in some cases there are differences 
# that do not allow the union of the various data sets.
# The differences are due to changes in the name, language, standards and abbreviations used.
# For each of the data sets, a copy of the normalized data set is saved for any reuse
# or to be able to restart with processing without having to perform the previous processing steps.

#-------------------------------------------------------------------------------------------------------------
# pre-analysis and cleaning of dfap dataset
#-------------------------------------------------------------------------------------------------------------
nrow(dfap)
nrow(dfap[is.na(dfap$airport),])

# delete NA 
dfap = dfap[!is.na(dfap$airport),]
nrow(dfap)

table(dfap$year)
table(dfap$month)
sort(unique(dfap$airport))

# delete info and totals
dfap = dfap[substr(dfap$airport,1,2) != "(*",]
dfap$airport = gsub(" [*]","",dfap$airport)
dfap$airport = gsub(" [(][*][])]","",dfap$airport)
dfap = dfap[dfap$airport != "TOTALI",]
sort(unique(dfap$airport))

# there are some airports with a double nomenclature in assaeroporti data
dfap[dfap$airport == "Milano LIN",]$airport = "Milano Linate"
dfap[dfap$airport == "Milano MXP",]$airport = "Milano Malpensa"
dfap[dfap$airport == "Trieste - Ronchi dei L.",]$airport = "Trieste"
dfap[dfap$airport == "Ronchi dei L.",]$airport = "Trieste"
dfap[dfap$airport == "Trieste - Ronchi dei Legionari",]$airport = "Trieste"
dfap[dfap$airport == "Roma FCO",]$airport = "Roma Fiumicino"
dfap[dfap$airport == "Roma CIA",]$airport = "Roma Ciampino"
dfap[dfap$airport == "Lamezia T.",]$airport = "Lamezia Terme"
dfap[dfap$airport == "Reggio Cal.",]$airport = "Reggio Calabria"
dfap[dfap$airport == "Forlì",]$airport = "Forli"
dfap[dfap$airport == "Forli'",]$airport = "Forli"
sort(unique(dfap$airport))

summary(dfap)
# there are some NA values and "Dati non disponibili"
dfap = dfap[!is.na(dfap$tm_mov_naz),]
dfap = dfap[!is.na(dfap$tm_mov_internaz),]
dfap = dfap[!is.na(dfap$tm_mov_UE),]
summary(dfap)

# convert NA to 0 for all p_ fields (percentual fields)
dfap[is.na(dfap$p_tm_mov_naz),]$p_tm_mov_naz = 0
dfap[is.na(dfap$p_tm_mov_internaz),]$p_tm_mov_internaz = 0
dfap[is.na(dfap$p_tm_mov_tcomm),]$p_tm_mov_tcomm = 0
dfap[is.na(dfap$p_tm_mov_tgenav),]$p_tm_mov_tgenav = 0
dfap[is.na(dfap$p_tm_mov_tot),]$p_tm_mov_tot = 0
dfap[is.na(dfap$p_tm_mov_UE),]$p_tm_mov_UE = 0
dfap[is.na(dfap$p_tp_mov_internaz),]$p_tp_mov_internaz = 0
dfap[is.na(dfap$p_tp_mov_naz),]$p_tp_mov_naz = 0
dfap[is.na(dfap$p_tp_mov_tcomm),]$p_tp_mov_tcomm = 0
dfap[is.na(dfap$p_tp_mov_tgenav),]$p_tp_mov_tgenav = 0
dfap[is.na(dfap$p_tp_mov_UE),]$p_tp_mov_UE = 0
dfap[is.na(dfap$p_tp_mov_tot),]$p_tp_mov_tot = 0
summary(dfap)

# the dataset contains data relating to "movements" (flights landing and departing from the airport), 
# "passengers" and "freight". for information relating to "movements" we cannot accept null values, 
# but it is possible, instead, for an airport not to carry passenger or freight traffic for a given month. 
# We select only the data of interest for the scope of analysis.

dfap_norm = dfap

# dfap_norm contains all normalized data of assaeroporti
save(dfap_norm,file="save_dfap_norm.Rdata")

#-------------------------------------------------------------------------------------------------------------
# pre-analysis and cleaning of dfrt dataset
#-------------------------------------------------------------------------------------------------------------

nrow(dfrt)
summary(dfrt)

# There are no NA's in the data, but the names of the airports do not coincide with those of the dfap dataset
# We need to do a normalization

dfrt_norm = dfrt
dfrt_norm$airport = gsub(" Airport","",dfrt_norm$airport)
dfrt_norm$airport = trimws(dfrt_norm$airport)

el_ap = unique(dfap_norm$airport)
dfrt_norm[!(dfrt_norm$airport %in% el_ap),]

# some airports have a slightly different designation from that used by airports.
dfrt_norm[dfrt_norm$airport == "Alghero Fertilia",]$airport = "Alghero"
dfrt_norm[dfrt_norm$airport == "Bari Karol Wojtyla",]$airport = "Bari"
dfrt_norm[dfrt_norm$airport == "Bologna Guglielmo Marconi",]$airport = "Bologna"
dfrt_norm[dfrt_norm$airport == "Cagliari Elmas",]$airport = "Cagliari"
dfrt_norm[dfrt_norm$airport == "Catania Fontanarossa",]$airport = "Catania"
dfrt_norm[dfrt_norm$airport == "Cuneo Levaldigi",]$airport = "Cuneo"
dfrt_norm[dfrt_norm$airport == "Elba Marina di Campo",]$airport = "Elba"
dfrt_norm[dfrt_norm$airport == "Florence Peretola",]$airport = "Firenze"
dfrt_norm[dfrt_norm$airport == "Genoa Cristoforo Colombo",]$airport = "Genova"
dfrt_norm[dfrt_norm$airport == "Milan Malpensa",]$airport = "Milano Malpensa"
dfrt_norm[dfrt_norm$airport == "Milan Linate",]$airport = "Milano Linate"
dfrt_norm[dfrt_norm$airport == "Milan Bergamo Il Caravaggio International",]$airport = "Bergamo"
dfrt_norm[dfrt_norm$airport == "Naples",]$airport = "Napoli"
dfrt_norm[dfrt_norm$airport == "Olbia Costa Smeralda",]$airport = "Olbia"
dfrt_norm[dfrt_norm$airport == "Palermo Falcone-Borsellino",]$airport = "Palermo"
dfrt_norm[dfrt_norm$airport == "Perugia Umbria International",]$airport = "Perugia"
dfrt_norm[dfrt_norm$airport == "Pescara Abruzzo International",]$airport = "Pescara"
dfrt_norm[dfrt_norm$airport == "Pisa Galileo Galilei",]$airport = "Pisa"
dfrt_norm[dfrt_norm$airport == "Rimini Federico Fellini",]$airport = "Rimini"
dfrt_norm[dfrt_norm$airport == "Rome Leonardo da Vinci Fiumicino",]$airport = "Roma Fiumicino"
dfrt_norm[dfrt_norm$airport == "Rome Ciampino",]$airport = "Roma Ciampino"
dfrt_norm[dfrt_norm$airport == "Trapani Birgi",]$airport = "Trapani"
dfrt_norm[dfrt_norm$airport == "Trieste Friuli Venezia Giulia",]$airport = "Trieste"
dfrt_norm[dfrt_norm$airport == "Turin Caselle",]$airport = "Torino"
dfrt_norm[dfrt_norm$airport == "Venice Marco Polo",]$airport = "Venezia"

dfrt_norm[!(dfrt_norm$airport %in% el_ap),]
# there is only one airport not present in assaeroporti list (dfap dataset). we delete it
dfrt_norm = dfrt_norm[(dfrt_norm$airport %in% el_ap),]

# now we separate codes in "code" field
dfrt_norm = dfrt_norm %>% separate(code,c("cod3","cod4"),"/")

# dfrt_norm contains all normalized data of flightradar24
save(dfrt_norm,file="save_dfrt_norm.Rdata")

#-------------------------------------------------------------------------------------------------------------
# pre-analysis and cleaning of dfro dataset
#-------------------------------------------------------------------------------------------------------------

# first of all we create an index based on the number of routes that are made to and from the airport. 
# This will help us classify the airport based on traffic.

dfro$nro = 0
dfro[dfro$nroutes < 100,]$nro = 1
dfro[dfro$nroutes >= 100 & dfro$nroutes < 500,]$nro = 2
dfro[dfro$nroutes >= 500 & dfro$nroutes < 1000,]$nro = 3
dfro[dfro$nroutes >= 1000 & dfro$nroutes < 5000,]$nro = 4
dfro[dfro$nroutes >= 5000 & dfro$nroutes < 10000,]$nro = 5
dfro[dfro$nroutes >= 10000 & dfro$nroutes < 20000,]$nro = 6
dfro[dfro$nroutes >= 20000 & dfro$nroutes < 50000,]$nro = 7
dfro[dfro$nroutes >= 50000 & dfro$nroutes < 100000,]$nro = 8
dfro[dfro$nroutes >= 100000 ,]$nro = 9

# now you need to select only the airports of interest
dfro_norm = dfro[dfro$cod3 %in% dfrt_norm$cod3,]

# dfro_norm contains all normalized data of openflights.org
save(dfro_norm,file="save_dfro_norm.Rdata")

#-------------------------------------------------------------------------------------------------------------
# JOIN all dataset
#-------------------------------------------------------------------------------------------------------------
nrow(dfap_norm)
df_all = left_join(dfap_norm,dfrt_norm,by="airport")
nrow(df_all)
df_all = left_join(df_all,dfro_norm,by="cod3")
nrow(df_all)
# There are NAs due to the fact that not all airport codes are present in all files. Let's eliminate these lines.
df_all = df_all[!is.na(df_all$nroutes),]

nrow(df_all)
table(df_all$year)
table(df_all$month)
table(df_all$airport)

# We delete the lines referring to airports for which we have little data
df_all = df_all[df_all$airport != "Comiso",]
nrow(df_all)

df_all$rating = as.numeric(df_all$rating)
df_all[,4:27] = apply(df_all[,4:27], 2, function(x) as.numeric(as.character(x)));
df_all[is.na(df_all)] <- 0

# df_all contains all merged data
save(df_all,file="save_df_all.Rdata")

#------------------------------------------------------------------------------
write.table(df_all,file="dp_italian_airports_oct2019_raw.csv",sep=";",row.names = FALSE)
# this file will be published on github (https://github.com/pietrodambrosio/HarvardX_DataScience)
# if you use this file you can bypass harvesting phase and restart from here
#------------------------------------------------------------------------------



##############################################################################################################
##############################################################################################################
##############################################################################################################
# STEP 3 - EXPLORATIVE ANALYSIS
##############################################################################################################
##############################################################################################################
##############################################################################################################

#-------------------------------------------------------------------------------------------------------------
#--- month distribution of mov_tot values

# As a first observation we would like to understand if there are substantial differences on the average 
# values of the various months of the year. In fact, we expect that in particular months such as summer or 
# Christmas holidays, there may be more travel and therefore more flights at all airports.

df_all %>%  mutate(year = as.character(year)) %>% filter(year>=2002 & year <= 2018) %>% group_by(year,month) %>% 
  summarise(tot = sum(tm_mov_tot), n = n(), mean = mean(tm_mov_tot), sd = sd(tm_mov_tot)) %>%
  ggplot(aes(x=month, y=tot,fill=year)) +
  geom_bar(stat='identity', color="darkred",position = position_stack(reverse = TRUE)) + 
  scale_fill_viridis(discrete = T) +
  labs(x="months", y="Total Movements") +
  scale_x_discrete(limits = c(1,2,3,4,5,6,7,8,9,10,11,12),
                   labels = c("Gen","Feb","Mar","Apr","Mag","Giu","Lug","Ago","Set","Ott","Nov","Dic")) +
  theme_minimal()

# the previous graph clearly shows that on average (also taking into account a wide range of years)
# the traffic volume vary within the year. This applies to all years.
# From the graph, of course, we have excluded the years 2001 and 2009 because they are not complete.
# The graph also shows that substantially the monthly average does not vary significantly over the years.


#-------------------------------------------------------------------------------------------------------------
#--- month distribution of others values  

# Let's try to understand now if, instead, there are differences between the types of movements (national, international, eu)
# compared with the total movements.

df_all %>%  filter(year>=2002 & year <= 2018) %>% group_by(month) %>% 
  summarise(mov_tot = sum(tm_mov_tot),mov_naz = sum(tm_mov_naz),mov_internaz = sum(tm_mov_internaz),mov_UE = sum(tm_mov_UE)) %>% 
  gather("var", "value", c(2:5)) %>%
  ggplot(aes(x = month, y = value, fill = var)) + geom_bar(stat='identity') +
  labs(title="Scatterplot", x="Month", y="Value") +
  scale_x_discrete(limits = c(1,2,3,4,5,6,7,8,9,10,11,12),
                   labels = c("Gen","Feb","Mar","Apr","Mag","Giu","Lug","Ago","Set","Ott","Nov","Dic")) +
  theme_minimal() +
  facet_wrap( ~ var, ncol=4) 

# The graph above shows that the distribution of the monthly averages of the various types of movements is absolutely homogeneous

#-------------------------------------------------------------------------------------------------------------
#--- month distribution of percents variations  

# Now let's look at how the percentage changes compared to the previous year vary in the months of the year, 
# in order to understand whether this information may or may not be useful for the forecasts.

df_all %>%  filter(year>=2002 & year <= 2018) %>% group_by(month) %>% 
  summarise(p_mov_tot = mean(p_tm_mov_tot),p_mov_naz = mean(p_tm_mov_naz),p_mov_internaz = mean(p_tm_mov_internaz),p_mov_UE = mean(p_tm_mov_UE)) %>% 
  gather("var", "value", c(2:5)) %>%
  ggplot(aes(x = month, y = value, fill = var)) + geom_bar(stat='identity') +
  labs(title="Scatterplot", x="Month", y="Value") +
  scale_x_discrete(limits = c(1,2,3,4,5,6,7,8,9,10,11,12),
                   labels = c("Gen","Feb","Mar","Apr","Mag","Giu","Lug","Ago","Set","Ott","Nov","Dic")) +
  theme_minimal() +
  facet_wrap( ~ var, ncol=4) 

# the graph above shows some variability in the monthly average values of the percentage changes.

#-------------------------------------------------------------------------------------------------------------
#--- correlation matrix

df_all %>% select(-c(airport,cod3,cod4)) %>%
          ggcorr(label = TRUE,label_size = 2.5,hjust = 0.95,palette = "RdGy",nbreaks = 4,label_color = "white")

# The correlation matrix shows a strong correlation of the number of total movements with some 
# variables such as, for example, the number of international, national and EU movements. 
# It can be noted, however, that other variables, including the "rating", are not very correlated 
# with all other variables and therefore could be useful in the model.



##############################################################################################################
##############################################################################################################
##############################################################################################################
# STEP 4 - DATA PRE-ELABORATION
##############################################################################################################
##############################################################################################################
##############################################################################################################

# The value that you intend to forecast is the total of the movements of the month following the current one.
# For the purposes of the forecast, considering the characteristic periodicity of these values and the obvious 
# relationship with the airport size, we think it may be useful to integrate the input dataset with other information
# obtainable from historical data and in particular the values of previous years (current and next month).  
# It is also necessary to add to the data the value of the following month (the one we want to predict) 
# to be able to train the algorithm.

#-------------------------------------------------------------------------------
# value of the following month, information we want to predict (tm_mov_tot_nm).
# This will then be our dependent output variable of the process.

df_all$tm_mov_tot_nm = 0
nel = nrow(df_all)
for (i in 1:nel) {
  ap = df_all[i,]$airport
  yy = df_all[i,]$year
  mm = df_all[i,]$month
  w_yy = yy
  w_mm = mm + 1
  if (w_mm == 13) {
    w_yy = w_yy + 1
    w_mm = 1
  }
  if (nrow(df_all[df_all$airport == ap & df_all$year == w_yy & df_all$month == w_mm,]) > 0) {
    df_all[i,]$tm_mov_tot_nm = df_all[df_all$airport == ap & df_all$year == w_yy & df_all$month == w_mm,]$tm_mov_tot
  }
}

nrow(df_all[df_all$tm_mov_tot_nm == 0,])
df_all = df_all[df_all$tm_mov_tot_nm != 0,]
table(df_all$year)

#-------------------------------------------------------------------------------
# previous year's values for the current month and the following month
# tm_mov_tot_pycm = total month movements of previous year - current month
# tm_mov_tot_pynm = total month movements of previous year - next month
# The choice to use the values of the previous years obliges us to no longer 
# be able to take into consideration the values of the year 2002 
# (as they will be used to enrich the data of the following year)

df_all$tm_mov_tot_pycm = 0
df_all$tm_mov_tot_pynm = 0
el_ap = unique(df_all$airport)
for (yy in 2003:2018) {
  for (mm in 1:12) {
    w_mm = mm + 1
    w_yy = yy
    if (w_mm == 13) {
      w_mm = 1
      w_yy = w_yy+1
    }
    for (ap in el_ap) {
      if (nrow(df_all[df_all$airport == ap & df_all$year == yy-1 & df_all$month == mm,]) > 0 & 
          nrow(df_all[df_all$airport == ap & df_all$year == yy & df_all$month == mm,]) > 0) {
          df_all[df_all$airport == ap & df_all$year == yy & df_all$month == mm,]$tm_mov_tot_pycm = 
          df_all[df_all$airport == ap & df_all$year == yy-1 & df_all$month == mm,]$tm_mov_tot
      }
      if (nrow(df_all[df_all$airport == ap & df_all$year == w_yy-1 & df_all$month == w_mm,]) > 0 &
          nrow(df_all[df_all$airport == ap & df_all$year == yy & df_all$month == mm,]) > 0) {
          df_all[df_all$airport == ap & df_all$year == yy & df_all$month == mm,]$tm_mov_tot_pynm = 
          df_all[df_all$airport == ap & df_all$year == w_yy-1 & df_all$month == w_mm,]$tm_mov_tot
      }
    }
  }
}

nrow(df_all[df_all$tm_mov_tot_pynm == 0,])
df_all = df_all[df_all$tm_mov_tot_pynm != 0,]
table(df_all$year)


##############################################################################################################
# the current df_all represents the data set that will be used for the subsequent modeling phases.
save(df_all,file="save_df_all_clean.Rdata")
#------------------------------------------------------------------------------
write.table(df_all,file="dp_italian_airports_oct2019_clean.csv",sep=";",row.names = FALSE)
# this file will be published on github (https://github.com/pietrodambrosio/HarvardX_DataScience)
# if you use this file you can bypass previous phases and restart from here
#------------------------------------------------------------------------------
##############################################################################################################



##############################################################################################################
##############################################################################################################
##############################################################################################################
# STEP 5 - MODELING
##############################################################################################################
##############################################################################################################
##############################################################################################################

#---------------------------------------------------------------------------------
# Define Loss Functions

# RMSE Root Mean Squared Error
# We will use the RMSE standard loss function, knowing that it is probably not suitable to give a sense of the 
# effectiveness of the forecast, since the number to predict can in some cases be very large.
RMSE <- function(true_ratings,predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# We have therefore defined a new loss function that gives us the measure of the absolute error in percentage 
# measure on the reference value. 
# This measure can be more intuitive and help us better understand how our forecast approaches reality.
# MAPE = Mean Absolute Percentage Error
MAPE <- function(true_ratings,predicted_ratings){
  sum(abs(true_ratings - predicted_ratings))/sum(true_ratings)
}  
#---------------------------------------------------------------------------------
# Model 1 - the simplest model
# we use the next month data of previous year to predict the next month of this year

predict = df_all$tm_mov_tot_pynm
rmse_1 = round(RMSE(df_all$tm_mov_tot_nm,predict),4)
mape_1 = round(MAPE(df_all$tm_mov_tot_nm,predict),4)

# create a tab to collect all results 
res_tab <- tibble(method = "Model 1 - simply next month of previous year", RMSE = rmse_1, MAPE = mape_1)
res_tab


#---------------------------------------------------------------------------------
# Model 2 - add a percent variation of current month
# apply to theprevious year data the same percent of variation of current month

predict = df_all$tm_mov_tot_pynm + df_all$tm_mov_tot_pynm * df_all$p_tm_mov_tot / 100
rmse_2 = round(RMSE(df_all$tm_mov_tot_nm,predict),4)
mape_2 = round(MAPE(df_all$tm_mov_tot_nm,predict),4)

res_tab <- rbind(res_tab,c(method = "Model 2 - Add percent variation of current month", RMSE = rmse_2, MAPE = mape_2))
res_tab



#---------------------------------------------------------------------------------
# airport, cod3, cod4 represent the same information. 
# We leave only one and convert it into a numerical code

df = df_all
df$cod3 <- NULL
df$cod4 <- NULL
df$airport <- as.numeric(as.factor(df$airport))

#---------------------------------------------------------------------------------
# create train and test dataset
set.seed(1)
test_index <- createDataPartition(y = df$tm_mov_tot_nm, times = 1, p = 0.2, list = FALSE)
train_set <- df[-test_index,]
test_set <- df[test_index,]

y = train_set$tm_mov_tot_nm
x = train_set
x$tm_mov_tot_nm <- NULL

ty = test_set$tm_mov_tot_nm
tx = test_set
tx$tm_mov_tot_nm <- NULL


#---------------------------------------------------------------------------------
# Model 3 - 'glmboost' (gradient boosting)
set.seed(1)
train_glm <- train(x,y,method = "glmboost") 
glm_preds <- predict(train_glm,tx)
rmse_3 = round(RMSE(ty,glm_preds),4)
mape_3 = round(MAPE(ty,glm_preds),4)

res_tab <- rbind(res_tab,c(method = "Model 3 - 'glmboost' model (gradient boosting)", RMSE = rmse_3, MAPE = mape_3))
res_tab

#---------------------------------------------------------------------------------
# Model 4 - 'saic' (glm with Stepwise Feature Selection)
# this model starts from the best one-variable model, and each time add the variable 
# that brings the largest improvement (in terms of AIC).
# AIC stands for (Akaike's Information Criteria), a metric developped Hirotugu Akaike in 1970. 
# The basic idea of AIC is to penalize the inclusion of additional variables to a model. 
# It adds a penalty that increases the error when including additional terms. 
# The lower the AIC, the better the model.
set.seed(1)
ControlParamteres <- trainControl(method = "cv",
                                  number = 20,
                                  savePredictions = TRUE,
                                  allowParallel = TRUE)
# ATTENTION ! use right numbers of threads
train_saic <- train(x,y,method="glmStepAIC",preProc = c("center","scale"),trControl=ControlParamteres,nthread = 12)
saic_preds <- predict(train_saic,tx)
rmse_4 = round(RMSE(ty,saic_preds),4)
mape_4 = round(MAPE(ty,saic_preds),4)

res_tab <- rbind(res_tab,c(method = "Model 4 - 'saic' model (glm with Stepwise Feature Selection)", RMSE = rmse_4, MAPE = mape_4))
res_tab

# since the processing takes a long time we save it
save(train_saic,file="save_train_saic.RData")


#---------------------------------------------------------------------------------
# Model 5 - classical 'random forest' 
# (the control parameters was tuned separately)
# WARNING ! This processing may take a long time
set.seed(1)
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        savePredictions = TRUE,
                        allowParallel = TRUE,
                        repeats=3)
train_rf <- train(x,y, 
                  method='rf', 
                  trControl=control,
                  nthread = 12) # Use the correct number of threads
train_rf$bestTune
rf_preds <- predict(train_rf, tx)
rmse_5 = round(RMSE(ty,rf_preds),4) 
mape_5 = round(MAPE(ty,rf_preds),4) 

res_tab <- rbind(res_tab,c(method = "Model 5 - classical 'random forest' model", RMSE = rmse_5, MAPE = mape_5))
res_tab

# since the processing takes a long time we save it
save(train_rf,file="save_train_rf.RData")



#---------------------------------------------------------------------------------
# Model 6 - 'ranger' (fast implementation of random forests)
set.seed(1)
train_ranger <- train(x,y,method = "ranger")
ranger_preds <- predict(train_ranger,tx)
rmse_6 = round(RMSE(ty,ranger_preds),4)
mape_6 = round(MAPE(ty,ranger_preds),4)

res_tab <- rbind(res_tab,c(method = "Model 6 - 'ranger' model (fast implementation of random forests)", RMSE = rmse_6, MAPE = mape_6))
res_tab

# since the processing takes a long time we save it
save(train_ranger,file="save_train_ranger.RData")



#---------------------------------------------------------------------------------
# Model 7 - 'xgb' (extreme gradient boosting)
# (the control parameters was tuned separately)
set.seed(1)
ControlParamteres <- trainControl(method = "cv",
                                  number = 5,
                                  savePredictions = TRUE,
                                  allowParallel = TRUE)
parametersGrid <-  expand.grid(eta = 0.1,
                               colsample_bytree=0.7,
                               max_depth=7,
                               nrounds=250,
                               gamma=3,
                               subsample=1,
                               min_child_weight=3)
train_xgb <- train(x,y,method="xgbTree",trControl = ControlParamteres, tuneGrid = parametersGrid, nthread = 12)
train_xgb$bestTune
xgb_preds <- predict(train_xgb, tx)
rmse_7 = round(RMSE(ty,xgb_preds),4)
mape_7 = round(MAPE(ty,xgb_preds),4) 

res_tab <- rbind(res_tab,c(method = "Model 7 - 'xgb' model (extreme gradient boosting)", RMSE = rmse_7, MAPE = mape_7))
res_tab

# since the processing takes a long time we save it
save(train_xgb,file="save_train_xgb.RData")



#---------------------------------------------------------------------------------
# Model 8 - Ensamble of 'ranger' and 'xgb' models (best performed)

prev_ens = as.data.frame(xgb_preds)
prev_ens$ranger_preds = ranger_preds 

prev_ens$ens_ranger_xgb = (prev_ens$xgb_preds + prev_ens$ranger_preds) / 2
rmse_8 = round(RMSE(ty,prev_ens$ens_ranger_xgb),4)
mape_8 = round(MAPE(ty,prev_ens$ens_ranger_xgb),4)

res_tab <- rbind(res_tab,c(method = "Model 8 - Ensamble of 'ranger' and 'xgb' models (best performed)", RMSE = rmse_8, MAPE = mape_8))
res_tab


#---------------------------------------------------------------------------------
# Best performed model
best_mrse = rmse_8
best_mape = mape_8


# MAPE about 4.75% of error in the test set is a good result
# RMSE a mean of 358 is good if we consider that max(x$tm_mov_tot) is 32931 and sd(x$tm_mov_tot) is 5103.857

min(x$tm_mov_tot)
max(x$tm_mov_tot)
mean(x$tm_mov_tot)
sd(x$tm_mov_tot)



