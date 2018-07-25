Various functions supporting data handling in an atmospheric chemistry PhD

To install: 

```
# Install package
devtools::install_github("willdrysdale/wsdmiscr")
```

Included Functions:

*aircraft_cal_flags*  
**Data Handling**  
Takes Aircraft NOx (AQD) and recalculates concentration data from in flight calibration information  

*%b%*  
**Data Handling**  
Between Operator, returns logical comparison of numerical lhs to a numerical pair on rhs 

*BTT_calibration_profile*  
**Data Handling**  
Produces a profile of the corrected calibration paramaters from the BTT AQD NOx instrument at a 1 min average  

*BTT_parse_1hz_met*  
**Data Handling**  
Processed the 1Hz met data from BTT met station into cleaned 1 min data  

*bt_fastnox_param_monitor*  
**Data Handling**  
Produces a quick monitor file from the AQD NOx insturment at the BT Tower  

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

*mol_dict_query*  
**Chemistry**  
Query a small database of molecules relevant to atmospheric chemistry for simple parameters such as mol weight, mdq() is a wrapper for this  

*process.aircraft*  
**Data Handling**  
Reads in an AQD NOx aircraft file and runs aircraft_cal_flags, prodcuing quicklook and full outputs  

*Plot Flight Legs*  
**Plotting**  
Creates plots of the flight and its sections cloured by supplied parameters  

*read_1D_ncdf*  
**File Handling**  
Quickly reads data from a 1D netcdf  

*read.aircraft*  
**File Handling**  
Reads in an aircraft CSV and creates the UNIX_TS column used by aircraft_cal_flag  

*read.eddypro*  
**File Handling**  
Read is the eddy pro output file into a tidy dataframe  

*read.faam_flight_sum*  
**File Handling**  
Reads in the FAAM flight summary  

*read.faam_ncdf*  
**File Handling**  
Takes the NetCDF for FAAM flight data and returns the key columns  

*read.1D_ncdf*  
**File Handling**  
Reads all variables for a given dimention in a one dimentional NCDF    

*reshape_pan*  
**Data Processing**  
Reshapes the PAN instrument output into a sensible timeseries

