" Switch to pathogen
"runtime bundle/pathogen/autoload/pathogen.vim
"call pathogen#infect()
"call pathogen#helptags()

"" Vundle
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

filetype plugin indent on

"set t_Co=256
set t_Co=16
set history=1000
let mapleader = ","
set autoread
set nocp
set ruler
set backspace=eol,start,indent

set backupdir=/tmp/
set directory=/tmp/

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
syntax enable
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
set shiftwidth=2
set tabstop=2
set smarttab


"" Status
set laststatus=2
set statusline=\ %{HasPaste()}%F%m%r%h\ %w\ \ CWD:\ %r%{CurDir()}%h\ \ \ Line:\ %l/%L:%c
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

"" Better pasting
inoremap <S-Insert> <ESC>"+p`]a


set hidden
source $VIMRUNTIME/menu.vim
set wildmenu
set wildmode=list:longest
set wildignore+=*tmp/*,*.so,*.swp,*.zip,*/.git/*,.gitkeep

let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]_site|\.(git|hg|svn)$',
  \ 'file': '\v\.(exe|so|dll|pyc)$',
  \ 'link': 'some_bad_symbolic_links',
  \ }

set cpo-=<
set wcm=<C-Z>

"" whitespace
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

nnoremap <silent> <Leader>ws :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>

"" Quick leader functions
nnoremap <leader>g :e#<cr>
nnoremap <leader><cr> :noh<cr>
nnoremap <leader>n :only<cr>

"" Quick file access
nnoremap <leader>sv :source $MYVIMRC<cr>
nnoremap <leader>ov :vsp $MYVIMRC<cr>
nnoremap <leader>ot :vsp $HOME/todo.quicktask<cr>

"" Python
autocmd FileType python,markdown setlocal tabstop=4 softtabstop=4 shiftwidth=4
"" Ruby
autocmd FileType ruby,eruby setlocal tabstop=2 softtabstop=2 shiftwidth=2
"" HTML
autocmd FileType html setlocal tabstop=2 softtabstop=2 shiftwidth=2

"""" Plugins

""" notes

""" org

"" ctrlp
nmap <silent> <Leader>t :CtrlP<CR>
nmap <silent> <Leader>d :CtrlPDir<CR>
nmap <silent> <Leader>b :CtrlPBuffer<CR>
let g:ctrlp_working_path_mode = ''

"" tabman
let g:tabman_number = 0

"" Tabularize
if exists(":Tabularize")
 nmap <Leader>a= :Tabularize /=<CR>
 nmap <Leader>a: :Tabularize /:\zs<CR>
 nmap <Leader>a, :Tabularize /,\zs<CR>
endif
