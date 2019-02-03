dataset = open('RejectStatsA.csv', 'r')
data = dataset.read().splitlines()
output = 'Amount requested,Day,Month,Year\n'
collector = {2007: {}, 2008: {}, 2009: {}, 2010: {}, 2011: {}, 2012:{}}
dataset.close()

for line in data[1::]:
    line = line.replace('"', '').split(',')
    datatime = line[1].split('-')
    day = int(datatime[2])
    month = int(datatime[1])
    year = int(datatime[0])
    value = float(line[0])

    if month in collector[year].keys():
        if day in collector[year][month].keys():
            collector[year][month][day] += int(value)
        else:
            collector[year][month][day] = int(value)
    else:
        collector[year][month] = {}
        collector[year][month][day] = int(value)

for year, year_data in collector.items():
    for month, month_data in collector[year].items():
        for day, amount in collector[year][month].items():
            output += ','.join([str(amount), str(day), str(month), str(year)]) + '\n'

output_file = open('rejected_stats.csv', 'w')
output_file.write(output)
output_file.close()