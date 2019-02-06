from datetime import timedelta, date

def daterange(start_date, end_date):
    for n in range(int ((end_date - start_date).days)):
        yield start_date + timedelta(n)

def date_split(date):
    return tuple([int(x) for x in date])

print('Preparing daily rejects...')
collector = {2007: {}, 2008: {}, 2009: {}, 2010: {}, 2011: {}, 2012:{}}
start_date = date(2007, 5, 26)
end_date = date(2013, 1, 1)
for single_date in daterange(start_date, end_date):
    datatime = single_date.strftime("%Y-%m-%d")
    datatime = datatime.replace('"','').split('-')
    year, month, day = date_split(datatime)
    if month not in collector[year].keys():
        collector[year][month] = {}
    collector[year][month][day] = 0


dataset = open('RejectStatsA.csv', 'r')
data = dataset.read().splitlines()
output = 'Amount requested,Day,Month,Year\n'
dataset.close()

for line in data[1::]:
    line = line.replace('"', '').split(',')
    datatime = line[1].split('-')
    year, month, day = date_split(datatime)
    value = float(line[0])
    
    collector[year][month][day] += int(value)

for year, year_data in collector.items():
    for month, month_data in collector[year].items():
        for day, amount in collector[year][month].items():
            output += ','.join([str(amount), str(day), str(month), str(year)]) + '\n'

output_file = open('rejected_stats.csv', 'w')
output_file.write(output)
print('Daily rejects done...')
output_file.close()

print('Preparing monthly rejects...')
output_monthly = 'Amount requested,Month,Year\n'
collector_monthly = {2007: {}, 2008: {}, 2009: {}, 2010: {}, 2011: {}, 2012:{}}

for line in data[1::]:
    line = line.replace('"', '').split(',')
    datatime = line[1].split('-')
    year, month, day = date_split(datatime)
    value = float(line[0])
    if month not in collector_monthly[year].keys():
        collector_monthly[year][month] = int(value)

    else:
        collector_monthly[year][month] += int(value)

for year, year_data in collector_monthly.items():
    for month, amount in collector_monthly[year].items():
        output_monthly += ','.join([str(amount), str(month), str(year)]) + '\n'

output_file = open('rejected_monthly_stats.csv', 'w')
output_file.write(output_monthly)
print('Monthly rejects done...')
output_file.close()

print('Finalizing...')
cash = 0
cars = 0
for line in data[1::]:
    line = line.replace('"', '')
    line = line.split(',')
    if float(line[0])>40000:
        cash += 1
        if 'car' in line[2]:
            cars += 1

print('For {} records, {} requested for more than 40000 dollars and {} of them was for car.\nIt was {} percent requests for a car.'.format(
        len(data[1::]), cash, cars, round(cars/cash*100, 2)))
