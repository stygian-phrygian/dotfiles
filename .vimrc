" vim-plug plugin manager
" put plugins to install (github repos) between begin() and end()
" run :PlugInstall" and restart vim
call plug#begin()
"----workflow--------------------------------------
Plug 'tpope/vim-fugitive'                                       " git integration
Plug 'tpope/vim-surround'                                       " change surrounding delimiters efficiently
Plug 'tpope/vim-repeat'                                         " repeat actions correctly for plugins
Plug 'tpope/vim-commentary'                                     " toggle comments
Plug 'tpope/vim-unimpaired'                                     " pairs of useful mapping
Plug 'tpope/vim-apathy'                                         " fix path
Plug 'easymotion/vim-easymotion'                                " jump around text *way* easier
Plug 'junegunn/goyo.vim'                                        " distraction free writing
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'                                         " fuzzy file finder (hopefully installed)
Plug 'dense-analysis/ale'                                       " syntax linter
"----languages-------------------------------------
Plug 'xavierd/clang_complete'                                   " c++ completion
Plug 'luisjure/csound',  { 'for': ['csound'] }                  " csound syntax highlighting/completion
Plug 'rust-lang/rust.vim'                                       " rust syntax highlighting
Plug 'racer-rust/vim-racer'                                     " rust completion
Plug 'fatih/vim-go'                                             " go syntax highlighting/completion
Plug 'tpope/vim-fireplace'                                      " clojure highlighting/repl/completion
Plug 'davidhalter/jedi-vim'                                     " python completion
Plug 'psf/black'                                                " python formatter
"----color themes----------------------------------
" Plug 'MidnaPeach/neonwave.vim'   " <--- colors don't set correctly for some reason
Plug 'cinaeco/neonwave.vim'        " <--- these do however in this forked (modified) version
Plug 'jnurmine/Zenburn/'
Plug 'morhetz/gruvbox'
Plug 'fcpg/vim-farout'
Plug 'whatyouhide/vim-gotham'
Plug 'ajh17/Spacegray.vim'
Plug 'axvr/photon.vim'
Plug 'nikolvs/vim-sunbather'
Plug 'kyoz/purify', { 'rtp': 'vim' }
call plug#end()

"-----------------------------------------------
" configure options ----------------------------
"-----------------------------------------------
" turn off intro message
set shortmess+=I
" turn off automatic backup and hidden swap file
set nobackup
set noswapfile
" turn on syntax highlighting
syntax on
" must set this for color to work correctly
set background=dark
" set 256 colors (if not already set)
set t_Co=256
set termguicolors
" turn on filetype detection, plugin, and indentation
" https://vi.stackexchange.com/a/10125
filetype plugin indent on
" show existing tab with N spaces width
set tabstop=4
" when backspacing delete N spaces width
set softtabstop=4
" when indenting with '<' or '>' use N spaces width
set shiftwidth=4
" insert spaces instead of tab characters
set expandtab
" tab inserts shiftwidth amount of spaces
set smarttab
" make backspace not insane
set backspace=indent,eol,start
" textwidth (what width paragraphs are formatted to)
set textwidth=78
" provide a column indicator of text width
" this is turned off because it doesn't necessarily match the colorscheme colors
" set colorcolumn=80
" show commands (so we can see when timeouts happen)
set showcmd
" allow buffers to be hidden
" (buffers persist when not displayed by a window)
set hidden
" turn off line wrapping
set nowrap
" turn on hybrid mode line numbers
set number         " turn on line numbers
set relativenumber " turn on relative line numbers
" to display (some) invisible characters
" type ':set !list' to toggle
set listchars=tab:→\ ,eol:↲,nbsp:␣,trail:•,extends:⟩,precedes:⟨ " british newline
set listchars=tab:→\ ,eol:↵,nbsp:␣,trail:•,extends:⟩,precedes:⟨ " american newline
set listchars=tab:→\ ,nbsp:␣,trail:•,extends:⟩,precedes:⟨       " no newline
set list
" turn on mouse
set mouse=a
" turn off folding for all files by default
set nofoldenable
" turn on case insensitivity unless caps are present
set ignorecase
set smartcase
" move cursor to matched searches
set incsearch
" turn off completion scanning current/included files
set complete-=i
" turn on syntax completion
set omnifunc=syntaxcomplete#Complete
" make omni-completion pop-up menu match longest and don't select an option initially
set completeopt=longest,menuone
" enhance command completion
set wildmode=full
set wildignorecase
set wildmenu
" mapping delay
set timeoutlen=420
" keycode delay (fixes perceptible latency moving from insert mode to normal mode)
set ttimeoutlen=1
" turn off interpreting leading zero numbers as octal
set nrformats-=octal
" turn on statusline
set laststatus=2
" configure statusline -------------------------
let g:currentmode={'n':'NORMAL', 'v':'VISUAL','V':'V-LINE','t':'TERMINAL',"\<C-V>":'V-BLOCK','i':'INSERT','R':'REPLACE','Rv':'V-REPLACE','c':'COMMAND'}
function! GetMode()
return get(g:currentmode, mode(), '       ')
endfunction
set statusline=
set statusline+=\ %-{GetMode()}\                " current mode
set noshowmode                                  " stop redundant mode printing
set statusline+=%f                              " file name
set statusline+=%m                              " is modified?
set statusline+=%=                              " switching to right section
set statusline+=\ %y                            " file type
set statusline+=\ %{&fileencoding?&fileencoding:&encoding} " file encoding
set statusline+=\[%{&fileformat}]               " file line ending
set statusline+=\ ln:%l/%{line('$')}            " current line number
set statusline+=\ cn:%c                         " current col number
set statusline+=\ %p%%                          " percentage scrolled through

"-----------------------------------------------
" configure plugins ----------------------------
"-----------------------------------------------
"--netrw
" disable the banner
let g:netrw_banner=0
" view file listing as a tree
let g:netrw_liststyle=3

"--c++
let g:clang_library_path='/usr/lib/llvm-10/lib/libclang.so.1'
augroup filetype_cpp
    " clear previous autocommands in this autocommand group
    autocmd!
    " avoid needing to press shift for scope operator
    autocmd FileType cpp inoremap ;; ::
augroup END

"--golang
augroup filetype_go
    " clear previous autocommands in this autocommand group
    autocmd!
    " vim-go requires you to first :GoInstallBinaries to get completion
    " tabs are displayed as 8 spaces (and we use tab characters not spaces)
    autocmd FileType go setlocal ts=8 sw=8 sts=8 noexpandtab
    " vim-go mappings
    autocmd FileType go nnoremap <leader>b  <plug>(go-build)
augroup END
" toggle automatic type info under the cursor
let g:go_auto_type_info = 1

"--rust
" turn on auto-format  on save
let g:rustfmt_autosave = 1
" turn on racer
let g:racer_cmd = '~/.cargo/bin/racer'
let g:racer_experimental_completer = 1
augroup filetype_rust
    " clear previous autocommands in this autocommand group
    autocmd!
    autocmd FileType rust nmap <leader>b :Cbuild <cr>
augroup END

"--python
" no automatic dot completion
let g:jedi#popup_on_dot = 0
augroup filetype_python
    " clear previous autocommands in this autocommand group
    autocmd!
    " auto format python according to pep8
    " autocmd BufWritePre *.py execute ':Black'
    " turn off jedi keybindings
    let g:jedi#rename_command = ""
    let g:jedi#goto_command = "gd"
    let g:jedi#goto_assignments_command = ""
    let g:jedi#goto_stubs_command = ""
    " let g:jedi#usages_command = ""
augroup END

"--csound
augroup filetype_csound
    " clear previous autocommands in this autocommand group
    autocmd!
    " fix weird issue with csound files not using foldmethod=manual
    autocmd BufNewFile,BufRead *.orc,*.sco,*.csd,*.udo set foldmethod=manual
    " turn off line wrapping
    autocmd FileType csound setlocal formatoptions-=t
    " turn off relative line numbering which slows it down for some reason
    autocmd FileType csound set norelativenumber
augroup END

"--easymotion
let g:EasyMotion_smartcase=1
" turn off the default bindings
" if on, can't do <leader><leader> below as easymotion wants that
let g:EasyMotion_do_mapping=0

"------------------------------------------------
" configure mappings ----------------------------
"------------------------------------------------
" smart tab completion lovingly stolen from here:
"http://vim.wikia.com/wiki/Smart_mapping_for_tab_completion
function! Smart_TabComplete()
    let substr = strpart(getline('.'), -1, col('.'))    " from the start of the current line to the cursor
    let substr = matchstr(substr, "[^ \t]*$")           " word till cursor
    if (strlen(substr)==0)                              " nothing to match on empty string
        return "\<tab>"
    endif
    let has_period        = match(substr, '\.') != -1   " position of period, if any
    let has_forward_slash = match(substr, '\/') != -1   " position of forward slash, if any
    let has_double_colon  = match(substr, '::') != -1   " position of double colon, if any (for rust... I added this)
    if (!has_period && !has_forward_slash && !has_double_colon)
        return "\<C-X>\<C-P>"                           " existing text matching (syntax omnicompletion)
    elseif (has_forward_slash)
        return "\<C-X>\<C-F>"                           " file matching
    else
        return "\<C-X>\<C-O>"                           " plugin matching
    endif
endfunction
" turn on smart tab completion in insert mode
inoremap <tab> <c-r>=Smart_TabComplete()<cr>
" make pop-up menu more friendly
" found here:
" http://vim.wikia.com/wiki/Improve_completion_popup_menu
" http://vim.wikia.com/wiki/Make_Vim_completion_popup_menu_work_just_like_in_an_IDE
" NB. we can't map <esc> in insert mode for some reason (it turns the arrow keys into letters)
inoremap <expr> <cr>       pumvisible() ? "\<c-y>" : "\<cr>"
inoremap <expr> <down>     pumvisible() ? "\<c-n>" : "\<down>"
inoremap <expr> <up>       pumvisible() ? "\<c-p>" : "\<up>"
inoremap <expr> <pagedown> pumvisible() ? "\<pagedown>\<c-p>\<c-n>" : "\<pagedown>"
inoremap <expr> <pageup>   pumvisible() ? "\<pageup>\<c-p>\<c-n>" : "\<pageup>"
"
" open vimrc (in new tab)
nnoremap <leader>v <esc>:tabnew $MYVIMRC<cr>
" open buffer (in window)
nnoremap <leader>e <esc>:e<space>
" open buffer (in new tab)
nnoremap <leader>t <esc>:tabnew<space>
" open terminal (in bottom right window)
nnoremap <leader>r <esc>:botright terminal<cr>
" open terminal (in new tab)
nnoremap <leader>rr <esc>:tab terminal<cr>
" save buffer
nnoremap <leader>w <esc>:w<cr>
" reload vimrc
noremap  <f5> :source $MYVIMRC<cr>
" close buffer
nnoremap <leader>x <esc>:bd<cr>
" close window or tab
nnoremap <leader>q <esc>:q<cr>
" force close
nnoremap <leader>qq <esc>:qa!<cr>
" command
nnoremap <leader><leader> <esc>:
" help
nnoremap <leader>h <esc>:h<space>
" make <tab> switch to alternate file in normal mode
nnoremap <tab> <c-^>
" make <c-^> (switch to alternate-file (the last buffer)) work in terminal
tnoremap <c-^> <c-w>:b#<cr>
tnoremap <c-6> <c-w>:b#<cr>
" make ctrl-d work as it does in terminal
tnoremap <c-d> <c-d>
" terminal enter normal mode https://github.com/vim/vim/issues/2216#issuecomment-337566816
tnoremap <esc><esc> <c-\><c-n>
" make terminal tab movement similar to window tab movement
tnoremap <c-pagedown>      <c-w>gt<cr>
tnoremap <c-pageup>        <c-w>gT<cr>
" indent while keeping visual highlighting
vnoremap > >gv
vnoremap < <gv
" map Shift-Arrow to window switching
nnoremap <silent> <s-up>         <c-w>k
nnoremap <silent> <s-down>       <c-w>j
nnoremap <silent> <s-right>      <c-w>l
nnoremap <silent> <s-left>       <c-w>h
inoremap <silent> <s-up>    <esc><c-w>ki
inoremap <silent> <s-down>  <esc><c-w>ji
inoremap <silent> <s-right> <esc><c-w>li
inoremap <silent> <s-left>  <esc><c-w>hi
tnoremap <silent> <s-up>         <c-w>k
tnoremap <silent> <s-down>       <c-w>j
tnoremap <silent> <s-right>      <c-w>l
tnoremap <silent> <s-left>       <c-w>h
" map Ctrl-Shift-Arrow to window dimensions
nnoremap <silent> <c-s-up>         <c-w>+
nnoremap <silent> <c-s-down>       <c-w>-
nnoremap <silent> <c-s-right>      <c-w>>
nnoremap <silent> <c-s-left>       <c-w><
inoremap <silent> <c-s-up>    <esc><c-w>+
inoremap <silent> <c-s-down>  <esc><c-w>-
inoremap <silent> <c-s-right> <esc><c-w>>
inoremap <silent> <c-s-left>  <esc><c-w><
tnoremap <silent> <c-s-up>         <c-w>+
tnoremap <silent> <c-s-down>       <c-w>-
tnoremap <silent> <c-s-right>      <c-w>>
tnoremap <silent> <c-s-left>       <c-w><
" map Ctrl-Shift-Page{Up,Down} for tab reordering
nnoremap <c-s-pageup>        :tabmove -1<cr>
nnoremap <c-s-pagedown>      :tabmove +1<cr>
inoremap <c-s-pageup>   <esc>:tabmove -1<cr>
inoremap <c-s-pagedown> <esc>:tabmove +1<cr>
tnoremap <c-s-pageup>   <c-w>:tabmove -1<cr>
tnoremap <c-s-pagedown> <c-w>:tabmove +1<cr>
" toggle spellchecking
function! ToggleSpell()
    set spell!
    if &spell
        echo 'spellcheck on'
        nnoremap <buffer> s 1z=
    else
        echo 'spellcheck off'
        nnoremap <buffer> s s
    endif
endfunction
nnoremap <leader>s :call ToggleSpell()<cr>
" -plugins---------------------------------------
" goyo: toggle
nnoremap <leader>y :Goyo<cr>
" easymotion: find (must be nmap not nnoremap)
nmap <leader>f <plug>(easymotion-overwin-w)
" fugitive: git blame
nnoremap gb :Git blame<cr>
" fzf: fuzzy find file
nnoremap <leader>p :FZF!<cr>
" fzf: grep
nnoremap <leader>g :Rg!<space>
" fzf: grep <cword>
nnoremap gh :Rg!<space><c-r><c-w><cr>
" ale: cycle warnings
nnoremap ]a :ALENext<cr>
nnoremap [a :ALEPrevious<cr>
nnoremap <leader>a :ALEDetail<cr>

"------------------------------------------------
" configure colorscheme -------------------------
"------------------------------------------------
" select colorscheme and fail silently otherwise
" silent! color farout
" silent! color gotham
" silent! color gruvbox
" silent! color neonwave
" silent! color photon
" silent! color purify
" silent! color spacegray
" silent! color sunbather
let g:zenburn_high_Contrast=1 " <--- configure zenburn
silent! color zenburn
