An R package to handle instrument data files and to produce plots in the openair package

To install: 

```
# Install package
devtools::install_github("willdrysdale/wsdmiscr")
```

Included Functions:

*aircraft_cal_flags*  
**Data Handling**  
Takes Aircraft NOx (AQD) and recalculates concentration data from in flight calibration information

*aggregate_by_date_span*   
**Data Handling**  
Used for comparing GC runs,or other data with a start and end time, to a time series of Met data or other continuous data

*%b%*  
**Data Handling**  
Between Operator, returns logical comparison of numerical lhs to a numerical pair on rhs. 

*BTT_calibration_profile*  
**Data Handling**  
Produces a profile of the corrected calibration paramaters from the BTT AQD NOx instrument at a 1 min average  

*BTT_raw_data_processing*  
**Data Handling**  
Converts the BTT AQD NOx output into a file ready for use in custom EC code  

*CEH_data_reprocess*  
**Data Handling**  
Alternative preprocess script to supply the 2012-13 data to new flux code.  

*BTT_EP_raw_data_processing*  
**Data Handling**  
Converts the BTT AQD NOx output into a file ready for use in Eddy Pro  

*BTT_parse_1hz_met*  
**Data Handling**  
Processed the 1Hz met data from BTT met station into 1 min data for use in both raw_data_processing functions  

*bt_fastnox_param_monitor*  
**Data Handling**  
Produces a quick monitor file from the AQD NOx insturment at the BT Tower  

*btnox_adddata*  
**Data Handling**  
Add Data to time series when both NOx (Teledyne) and O3 (Thermo) are connected to the envirologger

*btnox_adddata2*  
**Data Handling**  
Add Data to time series when NOx (AQD) is logged on Google drive and O3 (Thermo) is connected to the envirologger

*BTT_clean_high*  
**Data Handling**  
Removes NO,No2 and NOx values greater than 500 ppb and o3 greater than 300 ppb

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

*date_from_decimal_day*
**Data Handling**  
converts DOY.decimalday into a POSIXct object

*exp_day_comparison*  
**Experiment**  
When Given a date, it is compared to the average of a user specified number of previous day of week (i.e X previous wednesdays) and previous days

*exp_unique_event*  
**Experiment**  
Componenent of exp_day_comparison that handels day of week only

*exp_unique_event_days*  
**Experiment**  
Componenent of exp_day_comparison that handels days only

*FAAM_core_nox_merge*  
**Data Handling**  
Merges processed nox data into a faam core netcdf

*find_ranges*  
**Data Handling**  
Used to return ranges of rows where a valve has switched from 0 to 1 or vice versa. Improves upon find_cal_range and find_zero_range

*flagdata*  
**Data Handling**  
Used to flag the data from the BT Tower

*flight_range_subset*  
**Data Handling**  
Creates list of range events in a flight

*get_DOY*  
**Data Handling**  
Returns DOY.decialday from a POSIXct date

*length_of_lat(lon)*  
**Data Handling**  
For a given latitude in degrees, return the length of 1 degree lat(lon)

*load_BTT*  
**Data Handling**  
loads timeseries into the global enviroment and combines it with met data, from a folder specifing in the btconfig file

*massPlot*  
**Plotting**  
Produces a variety of rough plots from a data series 

*mol_dict_query*
**Chemistry**
Querey a small database of molecules relevant to atmospheric chemistry for simple parameters such as mol weight, \code{mdq()} is a wrapper for this

*ncas_filename*  
**Data Handling**  
Formats the BT Tower time series filename to the NCAS data archive format

*padtimeseries*  
**Data Handling**  
Fills in time series' gaps with date and blanks at specified intervals

*process.aircraft*  
**Data Handling**  
Reads in an AQD NOx aircraft file and runs aircraft_cal_flags, prodcuing quicklook and full outputs

*process_BTT_flags*  
**Data Handling**  
Applies the rules required by data flags to data

*pollution_colour*  
**Plotting**  
Used to keep plot colours consistent

*Plot Flight Legs*  
**Plotting**  
Creates plots of the flight and its sections cloured by supplied parameters

*read_1D_ncdf*  
**File Handling**  
Quickly reads data from a 1D netcdf

*read.aircraft*  
**File Handling**  
Reads in an aircraft CSV and creates the UNIX_TS column used by aircraft_cal_flag 

*read.faam_flight_sum*  
**File Handling**  
Reads in the FAAM flight summary

*read.faam_ncdf*  
**Data Handling**  
Takes the NetCDF for FAAM flight data and returns the key inforamtion as a csv

*sub_all*  
**Data Handling**  
Performs large amounts of subsetting to a time series, accessible through nested lists
