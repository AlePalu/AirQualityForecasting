from lib.WiseairClient import WiseairClient
from lib.WiseairClient import WiseairUtils

import pandas as pd
import contextlib
import requests
import json
import tempfile

from datetime import timezone
from datetime import datetime

def progressBar(iteration, total, length):
    decimals = 1
    percent = ("{0:." + str(decimals) + "f}").format(100 * (iteration / float(total)))
    filledLength = int(length * iteration // total)
    bar = "#" * filledLength + '-' * (length - filledLength)
    print(f'\rprogress [{bar}] {percent}%', end = '\r')
    if iteration + 1 == total:
        bar = "#" * length
        print(f'\rprogress [{bar}] 100.0%', end = "\n")

class DataCollector:
    def __init__(self):
        self.client = WiseairClient(r"../src/lib/personalAccessToken.csv")

        # openWeather API key
        self.apiKey = "87715dd7d6b3e559e4b290a4d492828f"
        
    def getPotID(self):
        # get data from wiseair server
        serverInfo = self.client.getLiveAirQuality()

        # store pot_id informations
        potIdList = []
        for item in serverInfo["data"]:
            potIdList.append(item["pot_id"])
        return potIdList

    # store wiseair data for the specified period in a CSV file
    def getData(self, beginData, endData, outputFile):
        # get sensor ID
        potID = self.getPotID()
        # create empty dataset
        wiseairData = pd.DataFrame()
        utils = WiseairUtils()
        
        print("fetching data from Wiseair server")
        T = len(potID)
        t = 1

        nullPotID = []
        for ID in potID:
            # suppress messages from  wiseair server
            with contextlib.redirect_stdout(None):
                try:
                    data = self.client.getDataOfPotByInterval(ID, beginData, endData)                    
                    if data:
                        data = utils.getPandasDataFrameFromDataOfSingleSensor(data)
                        # store data
                        wiseairData = pd.concat([wiseairData,data])
                    else:
                        nullPotID.append(ID)
                except:
                    # hey, this ID rised some exception... probably is an invalid ID, remove it
                    nullPotID.append(ID)

            # show progress bar
            progressBar(t, T, 80)
            t = t + 1

        potID = [i for i in potID if i not in nullPotID]
        print("got not null data from "+str(len(potID))+" sensors"+" "*100)

        # save dataset in CSV
        wiseairData.to_csv(outputFile)
        print("data stored in " + outputFile)

    # perfrom query to openWeather servers to get historical weather data
    def queryForWeatherData(self, day, lat, lon):
        # get timestamp from day string
        dt = datetime.strptime(day, "%Y-%m-%d")
        timestamp = int(dt.replace(tzinfo=timezone.utc).timestamp())
        
        url = "https://api.openweathermap.org/data/2.5/onecall/timemachine?lat=%s&lon=%s&dt=%s&appid=%s&units=metric" % (lat, lon, timestamp, self.apiKey)

        response = requests.get(url)
        data = json.loads(response.text)

        # filter data we are interested in...
        interestingData = []
        for d in data['hourly']:
            t = tuple([datetime.utcfromtimestamp(d['dt']).strftime('%Y-%m-%d %H:%M:%S'), d['wind_speed']])
            interestingData.append(t)
        
        return interestingData

    # add to an existing CSV file data for the selected period (avoid to download data we already have)
    def addData(self, data, beginDate, endDate):
        print("updating " + data)
        # request data from wiseair server
        with tempfile.NamedTemporaryFile() as tmp:
            self.getData(beginDate, endDate, tmp.name)
            newData = pd.read_csv(tmp.name)
            # reindex dataset with respect to datetime
            newData["created_at"] = pd.to_datetime(newData["created_at"])
            newData.set_index("created_at", inplace=True)

            oldData = pd.read_csv(data)
            # reindex dataset with respect to datetime
            oldData["created_at"] = pd.to_datetime(oldData["created_at"])
            oldData.set_index("created_at", inplace=True)

            # concat old and new data
            oldData = pd.concat([oldData, newData])

        # finally overwrite old CSV
        oldData.to_csv(data)
        print(data + " updated with data from "+ beginDate +" to "+ endDate)

    # automatically update data from the last recorded day up to now
    def updateData(self, data):
        dataFrame = pd.read_csv(data)
        
        # reindex dataset with respect to datetime
        dataFrame["created_at"] = pd.to_datetime(dataFrame["created_at"])
        dataFrame.set_index("created_at", inplace=True)

        # get last datetime in dataset
        lastDay = dataFrame.tail(1).index.item().date()

        # today is...
        today = datetime.today().date()

        # make the query
        self.addData(data, str(lastDay), str(today))

# d = DataCollector()

# lat = "45.464664"
# lon = "9.188540"

# d.queryForWeatherData("2020-10-24", lat, lon)
