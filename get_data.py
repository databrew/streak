from stravalib.client import Client
import os
import yaml
import units
import polyline
import pandas as pd
import itertools
import numpy as np

os.chdir('/home/joebrew/Documents/streak')
credentials = yaml.load(open('credentials.yaml'))

# Read in the data files
data_files = os.listdir(os.getcwd() + '/data')
if not 'athletes.csv' in data_files:
	print 'STOP'
else:
	athletes = pd.read_csv('data/athletes.csv')
if not 'activities.csv' in data_files:
	columns = ['athlete_id', 'average_speed', 'description', 'distance', 'elapsed_time', 'id', 'map','moving_time', 'name', 'start_date', 'start_date_local', 'start_latitude',  'start_longitude', 'timezone', 'total_elevation_gain', 'type', 'upload_id', 'utc_offset']
	index = []
	activities = pd.DataFrame(index=index, columns=columns)
else:
	activities = pd.read_csv('data/activities.csv')
if not 'polylines.csv' in data_files:
	index = []
	columns = ['activity_id', 'lng', 'lat']
	polylines = pd.DataFrame(index=index, columns=columns)
else:
	polylines = pd.read_csv('data/polylines.csv')

# Streams
stream_index = []
stream_columns = ['activity_id', 'time', 'distance', 'altitude', 'lat', 'lng']
empty_streams = pd.DataFrame(index = stream_index, columns = stream_columns)
if not 'streams.csv' in data_files:
	streams = empty_streams
else:
	streams = pd.read_csv('data/streams.csv')

client = Client()


# Have the user click the authorization URL, a 'code' param will be added to the redirect_uri
# authorize_url = client.authorization_url(client_id=credentials['client_id'], redirect_uri='http://localhost:8282/authorized')

# # use code to get token
# code = '2643ddfd5c65ffd57b945221957794e483695cb4' # ben
# # Extract the code from your webapp response
# # code = request.get('code') # or whatever your framework does
# access_token = client.exchange_code_for_token(client_id=credentials['client_id'], client_secret = credentials['client_secret'], code=code)
# if 'access_token.txt' in files:
# 	text_file = open('access_token.txt')
# 	access_token = text_file.read()
# 	text_file.close

# Now store that access token somewhere (a database?)
# store
# files = os.listdir(os.getcwd())
# text_file = open("access_token.txt", "w")
# text_file.write(access_token)
# text_file.close()

# Define function for getting recent activity ids
def get_recent_activity_ids(client):
	activities_list = client.get_activities(after = "2018-01-01T00:00:00Z",  limit=20)
	out = []
	for activity in activities_list:
		out.append("{0.id}".format(activity))		
		print("{0.id}".format(activity))
	return out

# Define function for getting relevant activity details based on activity id
def get_activity_details(client, activity_id):
	activity = client.get_activity(activity_id)
	columns = ['athlete_id', 'average_speed', 'description', 'distance', 'elapsed_time', 'id', 'map','moving_time', 'name', 'start_date', 'start_date_local', 'start_latitude',  'start_longitude', 'timezone', 'total_elevation_gain', 'type', 'upload_id', 'utc_offset']
	# Create a df to return
	index = [0]
	df = pd.DataFrame(index=index, columns=columns)
	df = df.fillna(0)
	df.athlete_id = activity.athlete.id
	df.average_speed = activity.average_speed
	df.description = activity.description
	df.distance = activity.distance
	df.elapsed_time = activity.elapsed_time
	df.id = activity.id
	df.map = activity.map.summary_polyline
	df.moving_time = activity.moving_time
	df.name = activity.name
	df.start_date = activity.start_date
	df.start_date_local = activity.start_date_local
	df.start_latitude = activity.start_latitude
	df.start_longitude = activity.start_longitude
	df.timezone = activity.timezone
	df.total_elevation_gain = activity.total_elevation_gain
	df.type = activity.type
	df.upload_id = activity.upload_id
	df.utc_offset = activity.upload_id
	return df

# Define function for getting stream
def get_activity_stream(client, activity_id):
	print 'Getting activity stream for activity id ' + str(activity_id)
	types = ['time', 'distance', 'latlng', 'altitude']
	the_stream = client.get_activity_streams(activity_id, types=types, resolution='high')
	# See if there is a stream
	has_stream = hasattr(the_stream, 'keys')
	if has_stream:
		new_stream = pd.DataFrame(index = stream_index, columns = stream_columns)
		for i in range(0, len(types)):
			this_type = types[i]
			if this_type in the_stream.keys():
				new_stream[this_type] = the_stream[this_type].data
		if len(new_stream) > 0:
			df = new_stream
			if 'latlng' in df.columns:
				df['latlng'] = df['latlng'].astype(str)
				df['lat'], df['lng'] = df['latlng'].str.split(', ', 1).str
				df = df.drop(['latlng'], axis = 1)
				df['lng'] = df['lng'].str.replace(r"]","")
				df['lat'] = df['lat'].str.replace(r"[","")
			else:
				df['lat'] = np.NaN
				df['lng'] = np.NaN
			df['activity_id'] = activity_id
			return df
	else:
		print '--- no stream for this activity id'
	


# Loop through each athlete, populating the relevant activities and polylines tables
new_activities = pd.DataFrame(index = [],
		columns = ['athlete_id', 'activity_id'])
for athlete_id in athletes.id:
	print 'Getting activities for ' + str(athlete_id)
	# Define the access token
	access_token = athletes.access_token[athletes.id == athlete_id]
	access_token = access_token.to_string()
	at_split = access_token.split(' ')
	access_token = at_split[len(at_split) - 1]
	# Make a new client
	client = Client()
	# Use the access token for this athlete
	client.access_token = access_token
	# Get recent activities
	recent_activities = get_recent_activity_ids(client = client)
	out = pd.DataFrame(index = range(0, len(recent_activities)),
		columns = ['athlete_id', 'activity_id'])
	out['athlete_id'] = athlete_id
	out['activity_id'] = recent_activities
	new_activities = new_activities.append(out, ignore_index = True)

# Get info on each activity
columns = ['athlete_id', 'average_speed', 'description', 'distance', 'elapsed_time', 'id', 'map','moving_time', 'name', 'start_date', 'start_date_local', 'start_latitude',  'start_longitude', 'timezone', 'total_elevation_gain', 'type', 'upload_id', 'utc_offset']
index = []
details = pd.DataFrame(index = index, columns = columns)
new_streams = pd.DataFrame(index = stream_index, columns = stream_columns)
for i in range(0, len(new_activities)):
	print i
	activity_id = new_activities['activity_id'][i]
	athlete_id = new_activities['athlete_id'][i]
	already = activity_id in activities.id
	already_stream = activity_id in streams.activity_id
	if already and already_stream:
		'Skipping since we already have activity ' + str(activity_id) + ' in both activities and streams'
	else:
		# Define the access token
		access_token = athletes.access_token[athletes.id == athlete_id]
		access_token = access_token.to_string()
		at_split = access_token.split(' ')
		access_token = at_split[len(at_split) - 1]
		# Make a new client
		client = Client()
		# Use the access token for this athlete
		client.access_token = access_token
		if not already:
			# Get new activities details
			out = get_activity_details(client = client, activity_id = activity_id)
			# Add to old details
			details = details.append(out, ignore_index = True)
		if not already_stream:
			ns = get_activity_stream(client = client, activity_id = activity_id)
			# Add to new_streams
			new_streams = new_streams.append(ns, ignore_index = True)

# Add all the old activities and the new ones
activities = activities.append(details, ignore_index = True)
# Overwrite the old activities table
activities.to_csv('data/activities.csv', index = False)

# Add all the old streams to the new ones
streams = streams.append(new_streams, ignore_index = True)
# Overwrite the old streams table
streams.to_csv('data/streams.csv', index = False)

# Get the polylines too
index = []
columns = ['activity_id', 'lng', 'lat']
new_polylines = pd.DataFrame(index=index, columns=columns) 
for i in range(0, len(details)):
	activity_id = details['id'][i]
	already = activity_id in activities.id
	if already:
		'Skipping since we already have activity' + str(activity_id)
	else:
		this_map = details['map'][i]
		if this_map is not None:
			coords = polyline.decode(this_map)
			new_poly = pd.DataFrame({'activity_id': activity_id, 'coords':coords})
			df = new_poly
			df['coords'] = df['coords'].astype(str)
			df['lat'], df['lng'] = df['coords'].str.split(', ', 1).str
			df = df.drop(['coords'], axis = 1)
			df['lng'] = df['lng'].str.replace(r")","")
			df['lat'] = df['lat'].str.replace(r"(","")
			new_polylines = new_polylines.append(df, ignore_index = True)
# Add old and new
polylines = polylines.append(new_polylines, ignore_index = True)
polylines.to_csv('data/polylines.csv', index = False)





# client = Client()
# client.access_token = access_token

# # Get list of activities
# activities_list = client.get_activities(after = "2018-01-01T00:00:00Z",  limit=20)



# for activity in activities_list:
# 	print("{0.id}".format(activity))

# # Get activities
# for activity in client.get_activities(after = "2018-01-01T00:00:00Z",  limit=10):
#     print("{0.name} {0.moving_time} {0.date}".format(activity))


# athlete = client.get_athlete()
# print("For {id}, I now have an access token {token}".format(id=athlete.id, token=access_token))

# # Currently-authenticated (based on provided token) athlete
# # Will have maximum detail exposed (resource_state=3)
# curr_athlete = client.get_athlete()

# # Get an activity
# activity = client.get_activity(1344189350) # chris activity
# # If activity is owned by current user, will have full detail (resource_state=3)
# # otherwise summary-level detail.

# # See all attributes:
# # dir(activity)

# # Get map
# map = activity.map
# map_poly = map.summary_polyline
# decoded_poly = polyline.decode(map_poly)


# # Activities can have many streams, you can request n desired stream types
# types = ['time', 'latlng', 'altitude', 'heartrate', 'temp', ]

# streams = client.get_activity_streams(1344189350, types=types, resolution='high')

# #  Result is a dictionary object.  The dict's key are the stream type.
# if 'altitude' in streams.keys():
#     print(streams['altitude'].data)




# activity = client.get_activity(1344189350)
# assert isinstance(activity.distance, units.quantity.Quantity)
# print(activity.distance)
# # 22530.80 m

# # Meters!?

# from stravalib import unithelper

# print(unithelper.miles(activity.distance))
# # 14.00 mi

# # And to get the number:
# num_value = float(unithelper.miles(activity.distance))
# # Or:
# num_value = unithelper.miles(activity.distance).num