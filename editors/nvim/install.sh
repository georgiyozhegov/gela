export NVIM_CONFIG="$HOME/.config/nvim"
# Make sure the path is correct
mkdir -p "$NVIM_CONFIG/syntax"
cp Gela.vim "$NVIM_CONFIG/syntax/gela.vim"
# Appends the autocmd at the end of your config file
cat "autocmd.lua" >> "$NVIM_CONFIG/init.lua"
