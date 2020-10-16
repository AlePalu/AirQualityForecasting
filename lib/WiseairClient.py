# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np

# In[82]:


import csv
import requests
import json
import pandas as pd
import json
import datetime
from datetime import date
from datetime import timedelta


class WiseairClient:
    """
    This class is used to access Wiseair's air quality data, providing methods to obtain them in JSON format.
    This client wraps Wiseair API's, whose full documentation is available at https://www.wiseair.it/documentation.
    """

    def put_json(self, url, data):
        """
        General purpose authenticated PUT request
        :param url:
        :param data:
        :return:
        """
        headers = {'Content-type': 'application/json', 'Accept': 'text/plain'}
        headers["Authorization"] = "Bearer {}".format(self.__userToken)
        r = requests.put(url, data=json.dumps(data), headers=headers)
        return r

    def post_json(self, url, data):
        """
        General purpose authenticated POST request
        :param url:
        :param data:
        :return:
        """
        headers = {'Content-type': 'application/json', 'Accept': 'text/plain'}
        headers["Authorization"] = "Bearer {}".format(self.__userToken)
        r = requests.post(url, data=json.dumps(data), headers=headers)
        return json.loads(r.content.decode("utf-8"))

    def get_json(self, url, data):
        """
        General purpose authenticated GET request
        :param url:
        :param data:
        :return:
        """
        headers = {'Content-type': 'application/json', 'Accept': 'text/plain'}
        headers["Authorization"] = "Bearer {}".format(self.__userToken)
        r = requests.get(url, data=json.dumps(data), headers=headers)
        return json.loads(r.content.decode("utf-8"))

    def __getClientToken(self, clientId, clientSecret):
        url = self.__baseUrl + "/oauth/token"
        data = {"client_id": clientId, "client_secret": clientSecret, "grant_type": "client_credentials"}
        headers = {'Content-type': 'application/json', 'Accept': 'text/plain'}
        r = requests.post(url, data=json.dumps(data), headers=headers)
        return json.loads(r.text)["access_token"]

    def __getPersonalToken(self, userEmail, userPassword, clientToken):
        url = self.__baseUrl + "/api/auth/login"
        data = {"email": userEmail, "password": userPassword, "grant_type": "client_credentials"}
        headers = {'Content-type': 'application/json', 'Accept': 'text/plain'}
        headers["Authorization"] = "Bearer {}".format(clientToken)
        r = requests.post(url, data=json.dumps(data), headers=headers)
        return json.loads(r.text)["access_token"]

    def __init__(self, pathToClientCredentials="personalAccessToken.csv", baseUrl="https://api.wiseair.it"):
        """
        Authenticate into Wiseair backend, using client credentials.
        Client credentials are stored in a file, an example of whose format can be found at
        https://github.com/fbambusi/wiseair-client/blob/milan-analysis/personalAccessTokenMock.csv
        To obtain the actual token, first of all register a free wiseair account by downloading Wiseair application
        on App Store or Play Store.
        Then, log in at http://api.wiseair.it/home and issue a new token. Copy the token into the .csv file and input
        the absolute path of the file.
        :param pathToClientCredentials:
        :param baseUrl: the base URL of Wiseair backend.
        """
        with open(pathToClientCredentials, "r") as csvFile:
            dic = csv.DictReader(csvFile)
            for row in dic:
                token = row["personalAccessToken"]

        self.__baseUrl = baseUrl
        self.__userToken = token

    def __getIntervalBetweenLastTenMeasures(self, potId, fromDate, toDate):
        results = self.getDataOfPotByInterval(potId, fromDate, toDate)
        df = pd.DataFrame(results)
        df['ts'] = pd.DatetimeIndex(df.created_at).asi8 // (10 ** 9)
        return df["ts"].diff()[1:]

    def __isHavingRegularPace(self, timeIntervalBetweenLastMeasuresInSecond):
        variance = np.std(timeIntervalBetweenLastMeasuresInSecond)
        mean = np.mean(timeIntervalBetweenLastMeasuresInSecond)
        rep = variance / mean

        # extremely regular: std/mean close to zero: 0.003 for correct sensor
        # 1.22 for exponential sleep
        if rep > 0.2:
            return False
        return True

    def lastTenMeasuresOfSensorAreEquallyPaced(self, sensorId):
        today = date.today()
        differences = self.__getIntervalBetweenLastTenMeasures(sensorId, str(today - timedelta(days=1)),
                                                               str(today + timedelta(days=1)))
        return self.__isHavingRegularPace(differences[-10:])

    def getDataOfPotByPage(self, pot_id, pageNr):
        url = self.__baseUrl + "http://www.wiseair.it/backend-test/public/measures/by-pot/{}?page={}".format(pot_id,
                                                                                                             pageNr)
        headers = {'Content-type': 'application/json', 'Accept': 'text/plain'}
        headers["Authorization"] = "Bearer {}".format(self.__userToken)
        r = requests.get(url, headers=headers)
        data = json.loads(r.text)["data"]
        return data

    def update_pot_sleeping_time(self, pot_id, interval_between_measures_in_seconds=3600, beginning_sleep_hour=23,
                                 end_sleep_hour=1):
        """
        This function is used to change the pace of a given pot. You should be an administrator to run this function.
        :param pot_id: the pot
        :param interval_between_measures_in_seconds: the interval between the measures of a pot
        :param beginning_sleep_hour: the hour of the day when the pot starts to sleep (min 22pm)
        :param end_sleep_hour: the hour of the day when the pot wakes up (max 5am)
        :return: the new state of the pot
        """
        request_body = {"interval_between_measures_in_seconds": interval_between_measures_in_seconds,
                        "beginning_sleep_hour": beginning_sleep_hour, "end_sleep_hour": end_sleep_hour,
                        "pot_id": pot_id}
        return self.post_json(self.__baseUrl + "/api/update-pot-sleep", request_body)

    def get_pot_details(self, pot_id):
        """
        This function return all the details of a given Arianna: location, last measure and active tests,
        as well as physical parameters
        :param pot_id:
        :return:
        """
        request_body = {"pot_id": pot_id}
        return self.get_json(self.__baseUrl + "/api/full-state-of-pot", request_body)

    def getDataOfPotByInterval(self, pot_id, fromDate, toDate):
        """
        Get all the data measured by a given Arianna during a given time interval.
        The pot_id can be retrieved using the method getLiveAirQuality.
        :param pot_id: the id of the Arianna
        :param fromDate: the date of the first measure to take, in the format yyyy-mm-dd, eg 2020-02-15
        :param toDate: the date of the last measure to take, in the format yyyy-mm-dd, eg 2020-02-15
        :return: array of air quality measures
        """
        url = self.__baseUrl + "/api/measures-by-time-interval"
        data = {"until_date": toDate, "from_date": fromDate, "pot_id": pot_id}
        print(data)
        headers = {'Content-type': 'application/json', 'Accept': 'text/plain'}
        headers["Authorization"] = "Bearer {}".format(self.__userToken)
        r = requests.get(url, data=json.dumps(data), headers=headers)
        return json.loads(r.text)

    def createPot(self, longitude, latitude):
        """
        Create a pot and locate it at a specific latitude and longitude. You should be an administrator to run this
        function.
        :param longitude:
        :param latitude:
        :return:
        """
        data = {"latitude": latitude, "longitude": longitude, "pm2p5": -1, "pm10": -1}
        url = self.__baseUrl + "/api/create-pot"
        print(url)
        print(data)
        response = self.post_json(url, data)
        return response

    def registerPot(self, activationCode, country, city, streetName, houseNumber, postalCode):
        """
        Activate a pot at a specific address.
        :param activationCode: the activation code of the pot.
        :param country:
        :param city:
        :param streetName:
        :param houseNumber:
        :param postalCode:
        :return:
        """
        data = {
            "activation_token": activationCode,
            "streetname": streetName,
            "housenumber": houseNumber,
            "city": city,
            "postalcode": postalCode
        }
        url = self.__baseUrl + "/api/activate-pot"
        response = self.put_json(url, data)
        print(response)
        print(response.content)

    def createMeasure(self, chipId="", pm1=-1, pm2p5=-1, pm4=-1, pm10=-1, humidity=-1, temperature=-1, voltage=-1):
        data = {
            "chip_id": chipId,
            "pm2p5": pm2p5,
            "pm10": pm10,
            "humidity": humidity,
            "temperature": temperature,
            "voltage": voltage,
            "pm1SPS": pm1,
            "pm2p5SPS": pm2p5,
            "pm10SPS": pm10,
            "pm4SPS": pm4}
        url = self.__baseUrl + "/measures/createV2"
        response = self.post_json(url, data)
        return response

    def getAllLocations(self):
        url = self.__baseUrl + "/api/get-all-locations"
        response = self.get_json(url, {})
        return response

    def getStateOfPots(self, page=1):
        """
        Get basic information about all the pots in the infrastructure, such as address and last measure. Data are
        paginated.
        :param page:
        :return:
        """
        data = {
            "page": page,
        }
        url = self.__baseUrl + "/api/get-state-of-pots"
        response = self.get_json(url, data)
        return response

    def getLiveAirQuality(self, latitude="45.458453", longitude="9.1782493", page=0):
        """
        Get the most recent values of air quality measured by Ariannas close to a given point.
        Data are paginated.
        :param latitude: the latitude of the point, in degrees and decimal
        :param longitude: the longitude of the point, in degrees and decimal
        :param page: the number of page
        :return: Array of air quality measures.
        """
        data = {
            "longitude": longitude,
            "latitude": latitude,
            "tolerance": "40",
            "untilDate": datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
            "items": 100,
            "page": page
        }
        url = self.__baseUrl + "/api/live-air-quality"
        response = self.get_json(url, data)
        return response

    def get_all_firmware_tests(self, page=1):
        """
        Get all firmware tests, paginated. You should be an administrator to run this function.
        :param page: the page of the tests to get
        :return:
        """
        data = {
            "page": page
        }
        url = self.__baseUrl + "/api/all-tests"
        response = self.get_json(url, data)
        return response

    def get_firmware_test_details_by_id(self, firmware_test_id):
        """
        Get the details of a specific firmware test. You should be an administrator to run this function.
        :param firmware_test_id:
        :return:
        """
        data = {
            "firmware_test_id": firmware_test_id
        }
        url = self.__baseUrl + "/api/state-of-test-by-id"
        response = self.get_json(url, data)
        return response

    def get_most_recent_test_by_firmware_version_id(self, firmware_version_id):
        """
        Get the details of the most recent test executed for a specific firmware version. You should be an
        administrator to run this funciton
        :param firmware_version_id: the firmware version to check
        :return:
        """
        data = {
            "firmware_version_id": firmware_version_id
        }
        url = self.__baseUrl + "/api/state-of-test"
        response = self.get_json(url, data)
        return response


class WiseairUtils:
    """
    This class is used to filter and transform Wiseair's JSON data.
    """

    def __init__(self):
        pass

    def getPandasDataFrameFromDataOfSingleSensor(self, pollutionData):
        """
        This function turns the data measured by a single sensor into a time-indexed Pandas DataFrame.
        :param pollutionData: the JSON array of data, in the format returned by WiseirClient.getDataOfPotByInterval
        :return: a time-indexed pandas dataframe containing the data
        """
        data = pd.DataFrame(pollutionData)
        data["created_at"] = pd.to_datetime(data["created_at"])
        data.index = data["created_at"]
        # data.drop("created_at",axis=1,inplace=True)
        return data

    FORMAT_STRING_WITH_HOURS = "%Y-%m-%dT%H:%M:%S"
    MEASURES_FORMAT_STRING = "%Y-%m-%d %H:%M:%S"
    THRESHOLDS = {"pm2p5": {"limit": 25}, "pm10": {"limit": 25}}

    def filterByDateAndLocations(self, pollutionData, beginningDate="2000-01-01T10:10:10",
                                 endDate="2100-01-01T10:10:10", interestingLocations=[]):
        """
        Keep only the 4h mean of measures taken at a given location, in a given period,  indexed by location.
        :param pollutionData: the Pandas DataFrame of air quality measures.
        :param beginningDate: the beginning of the period
        :param endDate: the end of the period
        :param interestingLocations: array of location ids
        :return: the data taken in the given location and period
        """
        condition = np.zeros(len(pollutionData), dtype=np.int8)
        for locationId in interestingLocations:
            condition = condition | (pollutionData.location_id == locationId)
            # print(condition)
        pollutionData = pollutionData[condition]
        begD = datetime.datetime.strptime(beginningDate, "%Y-%m-%dT%H:%M:%S")
        endD = datetime.datetime.strptime(endDate, "%Y-%m-%dT%H:%M:%S")
        pollutionData = pollutionData[pollutionData.created_at > begD]
        pollutionData = pollutionData[pollutionData.created_at < endD]
        return pollutionData.groupby("location_id").resample(rule="4H").mean()

    def getSummaryOfPeriod(self, pollutionData, beginningDate="2000-01-01T10:10:10",
                           endDate="2100-01-01T10:10:10"):
        formatStringWithHour = WiseairUtils.FORMAT_STRING_WITH_HOURS
        begD = datetime.datetime.strptime(beginningDate, formatStringWithHour)
        endD = datetime.datetime.strptime(endDate, formatStringWithHour)
        pollutionData = pollutionData[pollutionData.created_at > begD]
        pollutionData = pollutionData[pollutionData.created_at < endD]
        summary = {}
        quantities = ["pm2p5", "pm10"]
        for quantity in quantities:
            curr = {}
            curr["mean"] = pollutionData[quantity].mean()
            curr["std"] = pollutionData[quantity].var() ** 0.5
            dailyMean = pollutionData.resample(rule="24H").mean()
            dailyMeanTooMuch = len(dailyMean[dailyMean[quantity] > WiseairUtils.THRESHOLDS[quantity]["limit"]])
            dailyMeanOk = len(dailyMean[dailyMean[quantity] <= WiseairUtils.THRESHOLDS[quantity]["limit"]])
            curr["excessDays"] = dailyMeanTooMuch
            curr["daysOk"] = dailyMeanOk

            summary[quantity] = curr
        return summary

    @staticmethod
    def is_pot_alive(pot_with_attributes, last_measure_of_pot,current_moment=datetime.datetime.now()):
        """
        Check whether the last measure produced by a pot is recent enough to declare it alive
        :param pot_with_attributes:
        :param last_measure_of_pot:
        :param current_moment:
        :return:
        """
        seconds_elapsed_from_last_measure = (
                current_moment - datetime.datetime.strptime(last_measure_of_pot["created_at"],
                                                            WiseairUtils.MEASURES_FORMAT_STRING)).total_seconds()
        if seconds_elapsed_from_last_measure < pot_with_attributes["interval_between_measures_in_seconds"]:
            return True
        minutes_from_beginning_of_day = current_moment.hour * 60 + current_moment.minute
        minutes_of_beginning_of_sleep = pot_with_attributes["beginning_sleep_hour"] * 60
        minutes_of_end_of_sleep = pot_with_attributes["end_sleep_hour"] * 60
        if minutes_of_beginning_of_sleep > minutes_of_end_of_sleep:
            minutes_of_end_of_sleep += 24 * 60
            if minutes_from_beginning_of_day < minutes_of_beginning_of_sleep:
                minutes_from_beginning_of_day += 24 * 60
        if minutes_of_beginning_of_sleep < minutes_from_beginning_of_day < minutes_of_end_of_sleep:
            return pot_with_attributes["interval_between_measures_in_seconds"]/60 + (
                        minutes_from_beginning_of_day
                        - minutes_of_beginning_of_sleep) > seconds_elapsed_from_last_measure/60
        else:
            return False
