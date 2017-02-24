# BeijingAirQuality
Analysis of Beijing air quality data. The data can be found at http://www.stateair.net/web/historical/1/1.html. There they have a [fact sheet](http://www.stateair.net/web/assets/USDOS_AQDataFilesFactSheet.pdf) as well as a data use statement that interested parties should read.

You'll notice there are no scripts to pull `Beijing_201*_Hourly.csv` files. To access these files, one must first agree to some terms and conditions, making it difficult to write a script which pulls the data.

We also pull data related to the weather in Beijing from 2011 - 2016. The data comes from [the weather underground](https://www.wunderground.com/history/airport/ZBAA). This data was collected using the `read_weather_data.py` script. Note that I could have pulled that data using `data.table::fread` in `R`, but that wouldn't have been as much fun would it?