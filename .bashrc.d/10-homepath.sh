# Add various things to the path
for element in      \
    ~/.cabal/bin    \
    ~/.cargo/bin    \
    ~/.local/bin    \
    ~/bin
do
    if [ -d "$element" ]; then
        export "PATH=$element:$PATH"
    fi
done
