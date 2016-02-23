cd untyped;
echo "Running untyped --------------------------------------------------";
for i in {1..100}; do
  echo "U iteration "$i;
  racket main.rkt >> log.txt;
done;

cd ../typed;
echo "Running typed --------------------------------------------------";
for i in {1..100}; do
  echo "T iteration "$i;
  racket main.rkt >>log.txt;
done;
