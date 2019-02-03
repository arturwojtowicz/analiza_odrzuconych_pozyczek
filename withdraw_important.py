from datetime import timedelta, date

def daterange(start_date, end_date):
    for n in range(int ((end_date - start_date).days)):
        yield start_date + timedelta(n)

def date_split(date):
    return tuple([int(x) for x in date])

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
output_file.close()

