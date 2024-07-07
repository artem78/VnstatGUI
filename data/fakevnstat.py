#!/usr/bin/python3

import json
import random
from datetime import datetime, date, time, timedelta


start_date = datetime(2022, 6, 1, 0, 0)
end_date = datetime.now()
max_daily_traffic = 1024 * 1024 * 100



data = {}
data['interfaces'] = [{
    'name': 'interface1',
    'traffic': {
		'year': [
			#{'date': {'year': 2024}, 'rx': 99999999, 'tx': 555555555}
		],
		'month': [],
		'day': [],
		'hour': []		
    }
}]



#for year in range(year_start, year_end+1):
#delta = timedelta(days=1)
day_rx = 0
day_tx = 0
month_rx = 0
month_tx = 0
year_rx = 0
year_tx = 0
delta = timedelta(hours=1)
curr_date = start_date
prev_date = curr_date #datetime(1970,1,1,0,0)
while curr_date <= end_date:
	hour_rx = random.randint(0, max_daily_traffic)
	hour_tx = random.randint(0, max_daily_traffic)
	year_rx += hour_rx
	year_tx += hour_tx
	month_rx += hour_rx
	month_tx += hour_tx
	day_rx += hour_rx
	day_tx += hour_tx
	
	
	#data['interfaces'][0]['traffic']['year'].append({'date': {'year': year}, 'rx': random.randint(0, 9999999999), 'tx': random.randint(0, 9999999999)})
	data['interfaces'][0]['traffic']['hour'].append(
		{
			'date': {
				'year': curr_date.year,
				'month': curr_date.month,
				'day': curr_date.day
			},
			'time': {
				'hour': curr_date.hour,
				'minute': 0
			},
			
			'rx': hour_rx,
			'tx': hour_tx
		}
	)
	
	
	if prev_date.year != curr_date.year:
		data['interfaces'][0]['traffic']['year'].append(
			{'date': {'year': prev_date.year},
			'rx': year_rx,
			'tx': year_tx}
		)
		year_rx = 0
		year_tx = 0
		
		
	if prev_date.month != curr_date.month:
		data['interfaces'][0]['traffic']['month'].append(
			{'date': {
				'year': prev_date.year,
				'month': prev_date.month
				},
			'rx': month_rx,
			'tx': month_tx}
		)
		month_rx = 0
		month_tx = 0
		
		
	if prev_date.day != curr_date.day:
		data['interfaces'][0]['traffic']['day'].append(
			{'date': {
				'year': prev_date.year,
				'month': prev_date.month,
				'day': prev_date.day
				},
			'rx': day_rx,
			'tx': day_tx}
		)
		day_rx = 0
		day_tx = 0
		
	
	#print(start_date.strftime("%Y-%m-%d"))
	prev_date = curr_date
	curr_date += delta
	
#for hdata in data['interfaces'][0]['traffic']['hour']:
#	


#json_data = json.dumps(data, sort_keys=True, indent=2)
json_data = json.dumps(data, indent=2)
print(json_data)
