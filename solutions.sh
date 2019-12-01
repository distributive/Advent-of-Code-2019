echo "Compiling"

for d in */ ; do
    ghc -o "$d/solution" "$d/solution.hs"
done

printf "Done\n"

for d in */ ; do
    cd $d
    printf "\nSolution ${d////}\n"
    ./solution | sed "s/^/ /"
    cd ..
done
