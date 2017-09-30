
"vim-plug plugin manager
"put plugins to install (github repos) between begin() and end()
"run :PlugInstall" and restart vim
call plug#begin()
"----graphical improvements------------------------
Plug 'vim-airline/vim-airline'                            "vim status bar
Plug 'vim-airline/vim-airline-themes'                     "vim status bar themes
Plug 'tpope/vim-fugitive'                                 "git integration (works with the status bar above)
"----text motion-----------------------------------
Plug 'tpope/vim-surround'                                 "change surrounding delimiters efficiently
Plug 'tpope/vim-repeat'                                   "repeat actions correctly for plugins
Plug 'easymotion/vim-easymotion'                          "jump around text *way* easier
"----completion-----------------------------------
"Plug 'Shougo/neocomplete.vim'
"go here for install instruction: https://github.com/Valloric/YouCompleteMe
"Plug 'Valloric/YouCompleteMe', { 'do': './install.py --tern-completer --clang-completer' }
Plug 'vim-scripts/L9'
Plug 'vim-scripts/AutoComplPop'
"----language specific improvements----------------
Plug 'luisjure/csound',  { 'for': ['csound'] }            "csound syntax highlighting
"Plug 'kovisoft/paredit', { 'for': ['clojure', 'scheme'] } "easy lisp structural editing
"Plug 'racer-rust/vim-racer'
Plug 'derekwyatt/vim-scala'
Plug 'rhysd/vim-crystal'
Plug 'fatih/vim-go'
"----color themes----------------------------------
Plug 'dracula/vim'
Plug 'tomasr/molokai'
Plug 'liuchengxu/space-vim-dark'
Plug 'christophermca/meta5'
Plug 'chriskempson/base16-vim'
call plug#end()
"

"----color scheme and syntax highlighting-----------
syntax on
"make background dark (might change color scheme slightly)
set bg=dark
" configure space-vim-dark
"   range:   233 (darkest) ~ 238 (lightest)
"   default: 235
let g:space_vim_dark_background = 233
" configure molokai
"let g:rehash256 = 1
"
" set colorscheme and fail silently otherwise
silent! color desert
silent! color dracula
silent! color space-vim-dark
silent! color meta5
silent! color molokai

"
"
" change leader key to space
let mapleader=" "
" show commands (so we can see when timeouts happen)
set showcmd

"----configure plugins----------------------------
"
"--language specific configuration
"--golang
" tabs are displayed as 8 spaces (and we use tab characters not spaces)
autocmd Filetype go setlocal ts=8 sw=8 sts=8 noexpandtab
"vim-go mappings
au FileType go nmap <leader>r <Plug>(go-run)
au FileType go nmap <leader>b <Plug>(go-build)
au FileType go nmap <leader>t <Plug>(go-test)
au FileType go nmap <leader>c <Plug>(go-coverage)
"
"
"--------------------------------------------------
"" Use neocomplete.
"let g:neocomplete#enable_at_startup = 1
"" Use smartcase.
"let g:neocomplete#enable_smart_case = 1
"" Set minimum syntax keyword length.
"let g:neocomplete#sources#syntax#min_keyword_length = 3
"" AutoComplPop like behavior.
""let g:neocomplete#enable_auto_select = 1
"" Enable omni completion.
""autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
""autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
""autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
""autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
""autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
"
"rust completion configuration
let g:racer_cmd = "/home/j/.cargo/bin/"
"
" Enable and configure vim-airline
set laststatus=2
set ttimeoutlen=1                                " Fix the insert mode to normal mode delay
let g:airline#extensions#tabline#enabled = 1     " Enable the list of buffers
let g:airline#extensions#tabline#fnamemod = ':t' " Show just the filename
let g:airline_theme='onedark'                    " Set theme
let g:airline_theme='kalisi'                     " Set theme
let g:airline_theme='distinguished'              " Set theme
"
" fix a acp plugin issue when typing '<'
" found here: 
" https://github.com/sukima/xmledit/issues/15
autocmd FileType xml set omnifunc=xmlcomplete#CompleteTags noci
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags noci
autocmd FileType css set omnifunc=csscomplete#CompleteCSS
"
"configure easymotion
let g:EasyMotion_smartcase=1
"
"configure netrw (vim's file browser)
"configuration gleaned from here:
"https://shapeshed.com/vim-netrw/
"let g:netrw_banner = -1
"let g:netrw_liststyle = 2
"let g:netrw_browse_split = 3
"let g:netrw_altv = 0
"let g:netrw_winsize = 24
"augroup ProjectDrawer
"  autocmd!
"  autocmd VimEnter * :Vexplore
"augroup END


"----vim indentation/tab configuration------------
filetype plugin indent on
" show existing tab with N spaces width
set tabstop=2
" when backspacing delete N spaces width
set softtabstop=2
" when indenting with '<' or '>' use N spaces width
set shiftwidth=2
" insert spaces instead of tab characters
set expandtab


"----miscellaneous-------------------------------
"allow buffers to be hidden
set hidden
" turn off line wrapping
set nowrap
" turn on hybrid mode line numbers ----
set number         " turn on line numbers
set relativenumber " turn on relative line numbers
" turn on case insensitivity unless caps are present
set ignorecase
set smartcase
" move cursor to matched searches
set incsearch
" to display (some) invisible characters
" type ':set !list' to toggle
set listchars=tab:..,trail:_,extends:>,precedes:<,nbsp:~
set listchars=tab:→\ ,eol:↲,nbsp:␣,trail:•,extends:⟩,precedes:⟨
set list

" turn off vim automatic backup
set nobackup
set noswapfile
"turn on mouse
set mouse=a
" turn off folding for all files
set nofoldenable
" vim's ACP plugin is painfully slow unless this is used
" solution found on this page:
" http://stackoverflow.com/questions/2169645/vims-autocomplete-is-excruciatingly-slow
" exact link to specific answer (on same page above):
" http://stackoverflow.com/a/4277400
set foldmethod=manual


"----csound specific configuration---------------
" configure csound indentation and use of tab characters
" http://stackoverflow.com/questions/158968/changing-vim-indentation-behavior-by-file-type
"autocmd Filetype csound setlocal ts=8 sw=8 sts=8 noexpandtab
" fix weird issue with csound files not using foldmethod=manual
autocmd BufNewFile,BufRead *.orc,*.sco,*.csd,*.udo   set foldmethod=manual"


"----completion configuration--------------------
"turn on omnicompletion
filetype plugin on
set omnifunc=syntaxcomplete#Complete
"make omni-completion pop-up menu match longest 
" as well as don't select an option initially
set completeopt=longest,menuone
"smart tab completion lovingly stolen from here:
"http://vim.wikia.com/wiki/Smart_mapping_for_tab_completion
function! Smart_TabComplete()
  let line = getline('.')                         " current line

  let substr = strpart(line, -1, col('.')+1)      " from the start of the current
                                                  " line to one character right
                                                  " of the cursor
  let substr = matchstr(substr, "[^ \t]*$")       " word till cursor
  if (strlen(substr)==0)                          " nothing to match on empty string
    return "\<tab>"
  endif
  let has_period = match(substr, '\.') != -1      " position of period, if any
  let has_slash = match(substr, '\/') != -1       " position of slash, if any
  if (!has_period && !has_slash)
    return "\<C-X>\<C-P>"                         " existing text matching
  elseif ( has_slash )
    return "\<C-X>\<C-F>"                         " file matching
  else
    return "\<C-X>\<C-O>"                         " plugin matching
  endif
endfunction


"----mappings------------------------------------
"turn on smart tab completion in insert mode
inoremap <tab> <c-r>=Smart_TabComplete()<CR>
"remap C-PageDown and C-PageUp to buffer movement (much like tabs in a web browser)
"It turns out remapping C-Tab and C-S-Tab are tricky, see this:
"http://stackoverflow.com/questions/2686766/mapping-c-tab-in-my-vimrc-fails-in-ubuntu
nnoremap <C-PageDown> :bnext<CR>
nnoremap <C-PageUp> :bprevious<CR>
inoremap <C-PageDown> <Esc>:bnext<CR>
inoremap <C-PageUp> <Esc>:bprevious<CR>
vnoremap <C-PageDown> :bnext<CR>
vnoremap <C-PageUp> :bprevious<CR>
" remap the arrow keys to more useful modal actions (and NOPs otherwise)
" normal mode maps:
" <down> & <up> move the (current) line down & up (respectively)
noremap <down> ddp
noremap <up> ddkP
noremap <left> <Nop>
noremap <right> <Nop>
"visual mode maps:
vnoremap <down> <Nop>
vnoremap <up> <Nop>
vnoremap <left> <Nop>
vnoremap <right> <Nop>
"insert mode maps:
"make pop-up menu more friendly
"found here:
"http://vim.wikia.com/wiki/Improve_completion_popup_menu
"http://vim.wikia.com/wiki/Make_Vim_completion_popup_menu_work_just_like_in_an_IDE
" NB. we can't map <Esc> in insert mode for some reason (it turns the arrow keys into letters)
inoremap <expr> <CR>       pumvisible() ? "\<C-y>" : "\<CR>"
inoremap <expr> <Down>     pumvisible() ? "\<C-n>" : "\<Down>"
inoremap <expr> <Up>       pumvisible() ? "\<C-p>" : "\<Up>"
inoremap <expr> <PageDown> pumvisible() ? "\<PageDown>\<C-p>\<C-n>" : "\<PageDown>"
inoremap <expr> <PageUp>   pumvisible() ? "\<PageUp>\<C-p>\<C-n>" : "\<PageUp>"
"
" easymotion mappings
nmap <Leader>w <Plug>(easymotion-overwin-w)
nmap <Leader>f <Plug>(easymotion-overwin-f)
"2 character version of f motion
nmap <Leader>s <Plug>(easymotion-overwin-f2)
nmap <Leader>l <Plug>(easymotion-overwin-line)
"replace '/' with bidirectional easymotion search
"map  / <Plug>(easymotion-sn)
"omap / <Plug>(easymotion-tn)
