# namegener

# Mining Names

Names mined from the [Office of Scotland](http://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/births/popular-names/archive/babies-first-names-2007/detailed-tables) & from [Princeton](http://www.cs.princeton.edu/introcs/data/names.csv).
These were found by Mark Rushakoff on [stack overflow](https://stackoverflow.com/a/1452049)

pop-names-07-t4.csv processed using
```bash
iconv -c -f ISO8859-1 -t utf-8 pop-names-07-t4.csv | awk -F ',' 'NR>4 { print $1 "\n" $4 }' < pop-names-07-t4.csv | tr '[:upper:]' '[:lower:]' | sed '/^$/d'
```

names.csv is processed using
```bash
awk -F ',' 'NR>4 { print $1 "\n" $4 }' < names.csv | tr '[:upper:]' '[:lower:]' | sed '/^$/d'
```

Finally, these are merged by running them through my a script to remove
redundancies, then they should be fine for use.
