from lib.WiseairClient import WiseairClient
from lib.WiseairClient import WiseairUtils
import pandas as pd

client = WiseairClient(r"./lib/personalAccessToken.csv")

currentMeasures = client.getLiveAirQuality()

potIdList = []

for item in currentMeasures["data"]:
    potIdList.append(item["pot_id"])

print(potIdList)
print(len(potIdList))

utils = WiseairUtils()

wiseairdata=pd.DataFrame()

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

wiseairdata.to_csv("wiseairdata.csv")
