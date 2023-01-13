cd Documents/Projects/RectangleWorld/RectangleWorld-sims/recovery

for f in *.R; do
  Rscript "$f" &
done