echo "Creating file RESULTS.org";
echo "" > RESULTS.org;
for n in `find $1/* -type d`; do
  echo "* "$n>> RESULTS.org;
  echo ""  >> RESULTS.org;
  racket parse-profile.rkt $n/*.txt >> RESULTS.org;
done
