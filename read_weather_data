import pandas as pd


datafile = pd.read_csv(
        "https://www.wunderground.com/history/airport/ZBAA/" +
        `2011` +
        "/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=" +
        `2011` +
        "&req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo=&format=1"
        )

##need to remove <br /> from the last column

datafile.iloc[:,22] = [s[:-6] for s in datafile.iloc[:, 22]]


for year in range(2012, 2017):
    
    year_table = pd.read_csv(
        "https://www.wunderground.com/history/airport/ZBAA/" +
        `year` +
        "/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=" +
        `year` +
        "&req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo=&format=1"
        )
        
    year_table.iloc[:, 22] = [s[:-6] for s in year_table.iloc[:, 22]]
        
    datafile = datafile.append(year_table)

## fix up column names
datafile.columns = [s.replace(" ", "").replace("<br/>", "") for s in datafile.columns]



datafile.to_csv("BeijingWeatherData11_16.csv", index = False)