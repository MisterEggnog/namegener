# namegener

# Mining Names

Names mined from [Princeton](http://www.cs.princeton.edu/introcs/data/names.csv).
These were found by Mark Rushakoff on [stack overflow](https://stackoverflow.com/a/1452049)

names.csv is processed using
```bash
awk -F ',' 'NR>4 { print $1 "\n" $4 }' < names.csv | tr '[:upper:]' '[:lower:]' | sed '/^$/d'
```

## Future changes
Change this to either query a public database or to have somekind of local
database.
