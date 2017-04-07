An R package to handle instrument data files and to produce plots in the openair package

To install: 

```
# Install package
devtools::install_github("willdrysdale/wsdmiscr")
```

Included Functions:

*aggregate_by_date_span*   
**Data Handling**  
Used for comparing GC runs,or other data with a start and end time, to a time series of Met data or other continuous data

*bt_fastnox_param_monitor*  
**Data Handling**  
Produces a quick monitor file from the AQD NOx insturment at the BT

*btnox_adddata*  
**Data Handling**  
Add Data to time series when both NOx (Teledyne) and O3 (Thermo) are connected to the envirologger

*btnox_adddata2*  
**Data Handling**  
Add Data to time series when NOx AQD (AQD) is logged on Google drive and O3 (Thermo) is connected to the envirologger

*chooseday*  
**Data Handling**  
Can be supplied start and end date separated by day month and year, openair's chooseBydate supersedes this

*convert_met_data*  
**Data Handling**  
Converts wind vectors u and v to ws and wd and temperature as speed of sound to celcius

*convert_temp_sos_c*  
**Data Handling**  
Component of convert met data - must be fed dataframe line by line

*convert_wind_vectors*  
**Data Handling**  
Component of convert met data - must be fed dataframe line by line

*exp_day_comparison*  
**Experiment**  
When Given a date, it is compared to the average of a user specified number of previous day of week (i.e X previous wednesdays) and previous days

*exp_unique_event*  
**Experiment**  
Componenent of exp_day_comparison that handels day of week only

*exp_unique_event_days*  
**Experiment**  
Componenent of exp_day_comparison that handels days only

*flagdata*  
**Data Handling**  
Used to flag the data from the BT Tower

*load_BTT*  
**Data Handling**  
loads timeseries into the global enviroment and combines it with met data, from a folder specifing in the btconfig file

*massPlot*  
**Plotting**  
Produces a variety of rough plots from a data series 

*ncas_filename*  
**Data Handling**  
Formats the BT Tower time series filename to the NCAS data archive format

*padtimeseries*  
**Data Handling**  
Fills in time series' gaps with date and blanks at specified intervals

*pollution_colour*  
**Plotting**  
Used to keep plot colours consistent

*sub_all*  
**Data Handling**  
Performs large amounts of subsetting to a time series, accessible through nested lists
