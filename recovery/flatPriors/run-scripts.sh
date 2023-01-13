cd Documents/Projects/RectangleWorld/RectangleWorld-sims/flatPriors/recovery

for f in *.R; do
  Rscript "$f" &
done