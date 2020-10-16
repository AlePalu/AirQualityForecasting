from WiseairClient import WiseairClient
from WiseairClient import WiseairUtils

import matplotlib.pyplot as plt

client = WiseairClient(r"/home/alessandro/Desktop/wiseair-client/personalAccessToken.csv")

currentMeasures = client.getLiveAirQuality()
#print(currentMeasures)

potIdList = []

# cycle over received informations
for item in currentMeasures["data"]:
    potIdList.append(item["pot_id"])

print(potIdList)
print(len(potIdList))

# get data for each pot
pmData = {}
for ID in potIdList:
    BEGIN_DATE,END_DATE = "2020-10-14","2020-10-16"
    data = client.getDataOfPotByInterval(ID,BEGIN_DATE,END_DATE)
    pmData[ID] = []
    for cell in data:
        pmData[ID].append(cell["pm2p5"])

k1 = list(pmData.keys())[0]
k2 = list(pmData.keys())[1]
k3 = list(pmData.keys())[2]
k4 = list(pmData.keys())[3]

fig, axs = plt.subplots(2,2)
axs[0,0].plot(range(len(pmData[k1])), pmData[k1])
axs[0,0].set_title("Pot"+str(k1))
axs[0,1].plot(range(len(pmData[k2])), pmData[k2])
axs[0,1].set_title("Pot"+str(k2))
axs[1,0].plot(range(len(pmData[k3])), pmData[k3])
axs[1,0].set_title("Pot"+str(k3))
axs[1,1].plot(range(len(pmData[k4])), pmData[k4])
axs[1,1].set_title("Pot"+str(k4))

plt.show()
# build final CSV file
utils = WiseairUtils()
#data = utils.getPandasDataFrameFromDataOfSingleSensor(data)
#data.to_csv("sampleData.csv")
