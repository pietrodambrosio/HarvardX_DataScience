# **DATA DICTIONARY - dp_italian_airports_oct2019.csv**

## *fields common to both the raw and the clean version* 

* airport  = airport name (in italian)
* year = year of data 
* month = month of data

* tm_mov_naz = total mounth movements (total number of aircraft arriving or departing) of national flights
* p_tm_mov_naz = percentage increase or decrease of tm_mov_naz value compared to the corresponding month of the previous year 
* tm_mov_internaz = total mounth movements (total number of aircraft arriving or departing) of international flights
* p_tm_mov_internaz = percentage increase or decrease of tm_mov_internaz value compared to the corresponding month of the previous year 
* tm_mov_UE = total mounth movements (total number of aircraft arriving or departing) of European Union flights
* p_tm_mov_UE = percentage increase or decrease of tm_mov_UE value compared to the corresponding month of the previous year 
* tm_mov_comm = total mounth movements (total number of aircraft arriving or departing) of commercial flights
* p_tm_mov_comm = percentage increase or decrease of tm_mov_comm value compared to the corresponding month of the previous year 
* tm_mov_genav = total mounth movements (total number of aircraft arriving or departing) of general aviation flights
* p_tm_mov_genav = percentage increase or decrease of tm_mov_avgen value compared to the corresponding month of the previous year 
* tm_mov_tot = total mounth movements (total number of aircraft arriving or departing) 
* p_tm_mov_tot = percentage increase or decrease of tm_mov_tot value compared to the corresponding month of the previous year 

* tp_mov_naz = total year progressive movements (total number of aircraft arriving or departing) of national flights
* p_tp_mov_naz = percentage increase or decrease of tp_mov_naz value compared to the corresponding month of the previous year 
* tp_mov_internaz = total year progressive movements (total number of aircraft arriving or departing) of international flights
* p_tp_mov_internaz = percentage increase or decrease of tm_mov_internaz value compared to the corresponding month of the previous year 
* tp_mov_UE = total year progressive movements (total number of aircraft arriving or departing) of European Union flights
* p_tp_mov_UE = percentage increase or decrease of tm_mov_UE value compared to the corresponding month of the previous year 
* tp_mov_comm = total year progressive movements (total number of aircraft arriving or departing) of commercial flights
* p_tp_mov_comm = percentage increase or decrease of tm_mov_comm value compared to the corresponding month of the previous year 
* tp_mov_genav = total year progressive movements (total number of aircraft arriving or departing) of general aviation flights
* p_tp_mov_genav = percentage increase or decrease of tm_mov_avgen value compared to the corresponding month of the previous year 
* tp_mov_tot = total year progressive movements (total number of aircraft arriving or departing) 
* p_tp_mov_tot = percentage increase or decrease of tm_mov_tot value compared to the corresponding month of the previous year 

* cod3 = airport international 3 digit code 
* cod4 = airport international 4 digit code

* rating = flightradar24 rating for the airport
* nroutes = total number of routes from and to the airport
* nro = quality index calculated on total number of routes

## *fields fields present only in the clean version* 
* tm_mov_tot_nm = total mounth movements of next month. OUTPUT VARIABLE - VALUE TO PREDICT
* tm_mov_tot_pycm = total mounth movements - previous year - current month
* tm_mov_tot_pynm = total mounth movements - previous year - next month
