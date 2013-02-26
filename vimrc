" Switch to pathogen
"runtime bundle/pathogen/autoload/pathogen.vim
"call pathogen#infect()
"call pathogen#helptags()

""""""""""""""""""""""""""""""""""""""""""""""""""
"" Vundle
""""""""""""""""""""""""""""""""""""""""""""""""""
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'gmarik/vundle'

Bundle "kchmck/vim-coffee-script"
Bundle "kien/ctrlp.vim"
Bundle "groenewege/vim-less"
Bundle "altercation/vim-colors-solarized"
Bundle "godlygeek/tabular"
Bundle "tpope/vim-fugitive"
Bundle "tpope/vim-markdown"
Bundle "mattn/zencoding-vim"
Bundle "xolox/vim-notes"
Bundle "cfebs/vim-prose"
Bundle "jpo/vim-railscasts-theme"
Bundle "scrooloose/nerdcommenter"
Bundle "aaronbieber/quicktask"

""""""""""""""""""""""""""""""""""""""""""""""""""
"" Fundamentals
""""""""""""""""""""""""""""""""""""""""""""""""""

filetype plugin indent on

"set t_Co=256   " 256 colors
set t_Co=16
set history=1000
let mapleader = ","
set autoread
set nocp
set ruler
set backspace=eol,start,indent
set hidden
set scrolloff=3
" Prevent Vim from clobbering the scrollback buffer. See
" http://www.shallowsky.com/linux/noaltscreen.html
set t_ti= t_te=
set shell=bash

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

"" Searching
set ignorecase
set smartcase
set hlsearch
set incsearch
set showmatch

"" Colors
syntax on
" Usually term not set up for solarized
colorscheme solarized
"colorscheme railscasts
"colorscheme desert
set background=dark
set nonu
set nolazyredraw

"" Tabbing
set ai
set expandtab
set shiftwidth=4
set tabstop=4
set smarttab

"" Status
set laststatus=2
"set statusline=\ %{HasPaste()}%F%m%r%h\ %w\ \ CWD:\ %r%{CurDir()}%h\ \ \ Line:\ %l/%L:%c
set statusline=\ %{HasPaste()}\ %<%f\ (%{&ft})\ %-4(%m%)%=%-19(%3l,%02c%03V%)
set statusline+=\ %{fugitive#statusline()}

function! CurDir()
    let home = $HOME
    let curdir = substitute(getcwd(), home, "~", "g")
    return curdir
endfunction

function! HasPaste()
    if &paste
        return 'PASTE MODE  '
    else
        return ''
    endif
endfunction

" wilds
set wildmenu
set wildmode=longest,list
set wildignore+=*tmp/*,*.so,*.swp,*/.git/*,.gitkeep

let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]_site|\.(git|hg|svn)$',
  \ 'file': '\v\.(exe|so|dll|pyc)$',
  \ 'link': 'some_bad_symbolic_links',
  \ }

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

  "" Python
  autocmd FileType python setlocal tabstop=4 softtabstop=4 shiftwidth=4
  "" Ruby
  autocmd FileType ruby,haml,eruby,yaml,html,javascript,sass,cucumber set ai sw=2 sts=2 et
  autocmd! BufRead,BufNewFile *.sass setfiletype sass

augroup end

highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
nnoremap <silent> <Leader>ws :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""
"" Auto Commands
""""""""""""""""""""""""""""""""""""""""""""""""""

"" Quick leader functions
nnoremap <leader><leader> <c-^>
nnoremap <leader><cr> :noh<cr>
nnoremap <leader>n :only<cr>
imap <c-c> <esc>


"" Split nav
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l


"" Quick file access
nnoremap <leader>sv :source $MYVIMRC<cr>
nnoremap <leader>ev :vsp $MYVIMRC<cr>
nnoremap <leader>ot :vsp $HOME/todo.quicktask<cr>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MULTIPURPOSE TAB KEY
" Indent if we're at the beginning of a line. Else, do completion.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! InsertTabWrapper()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<tab>"
    else
        return "\<c-p>"
    endif
endfunction
inoremap <tab> <c-r>=InsertTabWrapper()<cr>
inoremap <s-tab> <c-n>

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


"""" Plugins

"" ctrlp
nmap <silent> <Leader>t :CtrlP<CR>
nmap <silent> <Leader>d :CtrlPDir<CR>
nmap <silent> <Leader>b :CtrlPBuffer<CR>
let g:ctrlp_working_path_mode = ''

"" Tabularize
if exists(":Tabularize")
 nmap <Leader>a= :Tabularize /=<CR>
 nmap <Leader>a: :Tabularize /:\zs<CR>
 nmap <Leader>a, :Tabularize /,\zs<CR>
endif
