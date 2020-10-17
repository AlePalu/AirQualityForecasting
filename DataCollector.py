from lib.WiseairClient import WiseairClient
from lib.WiseairClient import WiseairUtils

client=WiseairClient(r"./lib/personalAccessToken.csv")

class DataCollector:
    def __init__(self,client):
        self.client=client
   # currentMeasures = client.getLiveAirQuality()

    def listaId(self,currentMeasures):
        potIdList = []
        for item in currentMeasures["data"]:
            potIdList.append(item["pot_id"])
        return potIdList

    def get_data(self,potIdList,data_begin, data_end,features):
        datiPot={}
        mappainterna={}


        for ID in potIdList:
            data = client.getDataOfPotByInterval(ID, data_begin, data_end)
            datiPot[ID] = {}
            for cell in data:
                valori = []
                for v in features:
                    mappainterna[v]={}
                    valori.append(cell[v])
                    mappainterna[v]=valori
                    datiPot[ID]=mappainterna

        return datiPot













