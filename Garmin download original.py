import numpy as np
import pandas as pd
import plotly.graph_objects as go
from datetime import date, datetime
import datetime
from garminconnect import Garmin

my_garmin_username = 'bosredon.mathieu@gmail.com' #Replace with your username
my_garmin_password = 'Freerider19'#Replace with your pw
output_folder = "Directory" #Folder where the file(s) will be saved

#Enter the date you want to pull data for
for_date = datetime.date.today() #E.g. pulling data from sept 26
for_date = for_date.isoformat()

#Login to Garmin Connect
client = Garmin(my_garmin_username, my_garmin_password)
client.login()
# Download Activity
activities=client.get_activities_fordate(for_date)
activities=client.get_activities_fordate(for_date)
activities = activities["ActivitiesForDay"]["payload"]
# Extract the "activityId" values
activity_ids = [activity["activityId"] for activity in activities]
activity_names = [activity["activityName"] for activity in activities]

# Loop through the activity IDs and download each activity
for activity_id, activity_name in zip(activity_ids, activity_names):
    zip_data = client.download_activity(activity_id, dl_fmt=client.ActivityDownloadFormat.ORIGINAL)
    # Construct the output file path with the specified output folder and activity name
    output_file = f"{output_folder}/{activity_name}{activity_id}.zip"    
    with open(output_file, "wb") as fb:
        fb.write(zip_data)
