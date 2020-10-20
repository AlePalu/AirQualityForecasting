from lib.WiseairClient import WiseairClient
from lib.WiseairClient import WiseairUtils
import pandas as pd
import time

client = WiseairClient(r"./lib/personalAccessToken.csv")

currentMeasures = client.getLiveAirQuality()
#print(currentMeasures)

potIdList = []

# cycle over received informations
for item in currentMeasures["data"]:
    potIdList.append(item["pot_id"])

print(potIdList)
print(len(potIdList))

utils = WiseairUtils()
# get data for each pot

wiseairdata=pd.DataFrame()
count = 0
potIdList.remove(1040)
potIdList.remove(1036)
potIdList.remove(1025)
potIdList.remove(1048)
potIdList.remove(1038)
potIdList.remove(1052)

for ID in potIdList:
    BEGIN_DATE,END_DATE = "2020-05-01","2020-10-16"
    data = client.getDataOfPotByInterval(ID,BEGIN_DATE,END_DATE)
    data=utils.getPandasDataFrameFromDataOfSingleSensor(data)
    wiseairdata= pd.concat([wiseairdata,data])
    count += 1
    #if count == 10:
        #time.sleep(40)
        #count=0
        

wiseairdata.info()

wiseairdata.to_csv("wiseairdata.csv")