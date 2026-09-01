" Map plugin-provided commands here.  The vimrc runs before plugin/
" scripts load, so these guards are false there and the mappings never
" appear.  Vim and Neovim source after/plugin/ once plugins have loaded.

" Use :SyntasticInfo to see which tools Syntastic presently is using
if exists(':SyntasticToggleMode')
    noremap <silent> <Leader>s :SyntasticToggleMode<CR>
endif

" Tabularize mappings from
" http://vimcasts.org/episodes/aligning-text-with-tabular-vim/
if exists(":Tabularize")
  nmap <Leader>a| :Tabularize /|<CR>
  vmap <Leader>a| :Tabularize /|<CR>
  nmap <Leader>a= :Tabularize /=<CR>
  vmap <Leader>a= :Tabularize /=<CR>
  nmap <Leader>a: :Tabularize /:\zs<CR>
  vmap <Leader>a: :Tabularize /:\zs<CR>
  nmap <Leader>a<Space> :Tabularize / \zs<CR>
  vmap <Leader>a<Space> :Tabularize / \zs<CR>
endif
