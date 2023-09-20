echo "year,hits" > crossref.csv

for year in {1800..2020}
do
  x=`curl "https://api.crossref.org/works?filter=from-pub-date:$year,until-pub-date:$year,type:journal-article&rows=0" | jq '.["message"]["total-results"]'`
  echo $x
  echo $year,$x >> crossref.csv
#   echo $year
  sleep 1
done
