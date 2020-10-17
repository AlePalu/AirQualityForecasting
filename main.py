from lib.WiseairClient import WiseairClient
from lib.WiseairClient import WiseairUtils


if __name__=="__main__":
    from DataCollector import DataCollector
    dati=DataCollector(r"./lib/personalAccessToken.csv")
    client=WiseairClient(r"./lib/personalAccessToken.csv")
    currentMeasures=client.getLiveAirQuality()
    potIdList = dati.listaId(currentMeasures)
    features=["pm2p5","pm10","humidity","temperature"]
    datiPot =dati.get_data(potIdList,"2020-10-14","2020-10-16",features)

    print(datiPot)
