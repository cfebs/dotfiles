""""""""""""""""""""""""""""""""""""""""""""""""""
" https://github.com/junegunn/vim-plug
""""""""""""""""""""""""""""""""""""""""""""""""""
call plug#begin('~/.vim/plugged')

Plug 'kchmck/vim-coffee-script'
Plug 'groenewege/vim-less'
Plug 'altercation/vim-colors-solarized'
Plug 'godlygeek/tabular'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-markdown'
Plug 'mattn/emmet-vim'
Plug 'jpo/vim-railscasts-theme'
Plug 'scrooloose/nerdcommenter'
Plug 'aaronbieber/quicktask'
Plug 'itchyny/lightline.vim'
Plug 'nanotech/jellybeans.vim'
Plug 'gcmt/breeze.vim'
Plug 'gregsexton/gitv'
Plug 'christoomey/vim-tmux-navigator'
Plug 'scrooloose/syntastic'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': 'yes \| ./install' }

set fileformat=unix
set fileformats=unix,dos

call plug#end()

""""""""""""""""""""""""""""""""""""""""""""""""""
" Fundamentals
""""""""""""""""""""""""""""""""""""""""""""""""""
set nocp
filetype plugin indent on

set noerrorbells visualbell t_vb=
autocmd GUIEnter * set visualbell t_vb=

set history=10000
"set cursorline
let mapleader = ","
set autoread
set ruler
set backspace=eol,start,indent
set hidden
set scrolloff=3
" Prevent Vim from clobbering the scrollback buffer. See
" http://www.shallowsky.com/linux/noaltscreen.html
set t_ti= t_te=
set shell=bash
set nonu

set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp

"" Noice
nnoremap ; :
map <C-J> <C-W>j
map <C-K> <C-W>k

"" Markdown
nmap <leader>1 VypVr=
nmap <leader>2 VypVr-
nmap <leader>3 I### <Esc>
nmap <leader>4 I### <Esc>

"" Pasting
nmap <leader>p :setlocal paste! paste?<cr>

"" Searching
set ignorecase
set smartcase
set hlsearch
set incsearch
set showmatch

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Colors
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"set t_Co=256   " 256 colors
set t_Co=16
" Usually term not set up for solarized
set background=dark
colorscheme solarized
"colorscheme railscasts
"colorscheme desert
"colorscheme lucius
"colorscheme jellybeans
set nolazyredraw
syntax on

"" Tabbing
set ai
set expandtab
set shiftwidth=4
set tabstop=4
set smarttab

"" Wilds
set wildmenu
set wildmode=longest,list
set wildignore+=*tmp/*,*.so,*.swp,*/.git/*,.gitkeep

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" Status
"" Raw status line ditched for lightline
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"set statusline=\ %{HasPaste()}\ %<%f\ (%{&ft})\ %-4(%m%)%=%-19(%3l,%02c%03V%)
"set statusline+=\ %{fugitive#statusline()}

set laststatus=2

""""""""""""""""""""""""""""""""""""""""""""""""""
"" Experimental Turn OFF
""""""""""""""""""""""""""""""""""""""""""""""""""
"set cpo-=<
"set wcm=<C-Z>

""""""""""""""""""""""""""""""""""""""""""""""""""
"" Auto Commands
""""""""""""""""""""""""""""""""""""""""""""""""""

"" Whitespace
augroup vimrcEx
  autocmd!
  autocmd FileType text setlocal textwidth=78

  " Jump to last cursor position unless it's invalid or in an event handler
  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

  autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
  autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
  autocmd InsertLeave * match ExtraWhitespace /\s\+$/
  autocmd BufWinLeave * call clearmatches()

  "" 4 spaces
  autocmd FileType python,javascript setlocal tabstop=4 softtabstop=4 shiftwidth=4
  "" 2 spaces
  autocmd FileType coffee,ruby,haml,eruby,yaml,html,sass,cucumber set ai sw=2 sts=2 et

  autocmd! BufRead,BufNewFile *.sass setfiletype sass
  autocmd! BufRead,BufNewFile *.phtml set ft=phtml
augroup end

function! SetLocalTabSize()
    let size = input('Tab Size: ')
    execute "set ts=" . size . " sts=" . size . " sw=" . size
endfunction

highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
nnoremap <silent> <Leader>ws :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"" Quick leader functions
nnoremap <leader><leader> <c-^>
nnoremap <leader><cr> :noh<cr>
nnoremap <leader>o :only<cr>
nnoremap <leader>c :clo<cr>
imap <c-c> <esc>


"" Split nav
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l


"" Quick file access
nnoremap <leader>sv :source $MYVIMRC<cr>
nnoremap <leader>ev :vsp $MYVIMRC<cr>
nnoremap <leader>eb :vsp ~/.bashrc<cr>
nnoremap <leader>eba :vsp ~/.bash_aliases<cr>

nnoremap <S-h> gT
nnoremap <S-l> gt

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MULTIPURPOSE TAB KEY
" Indent if we're at the beginning of a line. Else, do completion.
" Not used anymore
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"function! InsertTabWrapper()
    "let col = col('.') - 1
    "if !col || getline('.')[col - 1] !~ '\k'
        "return "\<tab>"
    "else
        "return "\<c-p>"
    "endif
"endfunction
"inoremap <tab> <c-r>=InsertTabWrapper()<cr>
"inoremap <s-tab> <c-n>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ARROW KEYS ARE UNACCEPTABLE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <Left> <Nop>
map <Right> <Nop>
map <Up> <Nop>
map <Down> <Nop>

"" %% expands to the current directory
cnoremap %% <C-R>=expand('%:h').'/'<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" RENAME CURRENT FILE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! RenameFile()
    let old_name = expand('%')
    let new_name = input('New file name: ', expand('%'), 'file')
    if new_name != '' && new_name != old_name
        exec ':saveas ' . new_name
        exec ':silent !rm ' . old_name
        redraw!
    endif
endfunction
map <leader>n :call RenameFile()<cr>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugins
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"" ctrlp
nmap <silent> <Leader>t :FZF<CR>
""nmap <silent> <Leader>d :CtrlPDir<CR>
""nmap <silent> <Leader>b :CtrlPBuffer<CR>
""nmap <silent> <Leader>m :CtrlPMRUFiles<CR>
""let g:ctrlp_working_path_mode = ''
""let g:ctrlp_max_files = 0
""let g:ctrlp_custom_ignore = {
""  \ 'dir':  '\v[\/]_site|\.(git|hg|svn)|node_modules|source_maps$',
""  \ 'file': '\v\.(exe|so|dll|pyc)$',
""  \ 'link': 'some_bad_symbolic_links',
""  \ }

"" Tabularize
if exists(":Tabularize")
 nmap <Leader>a= :Tabularize /=<CR>
 nmap <Leader>a: :Tabularize /:\zs<CR>
endif

"" Breeze
let g:breeze_active_filetypes = "*.html,*.htm,*.xhtml,*.xml,*.phtml"

"" Fugitive
nmap <silent> <leader>gs :Gstatus<cr>

"" Gitv
let g:Gitv_DoNotMapCtrlKey = 1

"" Lightline
let g:lightline = {
      \ 'colorscheme': '16color',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'fugitive', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component': {
      \   'fugitive': '%{exists("*fugitive#head")?fugitive#head():""}'
      \ }
      \ }


