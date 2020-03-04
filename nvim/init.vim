call plug#begin('~/.vim/plugged')

Plug 'fatih/vim-go', { 'tag': '*' }
Plug 'flazz/vim-colorschemes'
Plug 'scrooloose/nerdtree'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'w0rp/ale'
Plug 'vimwiki/vimwiki'
Plug 'elixir-editors/vim-elixir'
Plug 'rust-lang/rust.vim'
" Initialize plugin system
call plug#end()

let mapleader = " "

set clipboard=unnamedplus

set background=dark
" Set Colorscheme
" colorscheme Monokai
colorscheme arcadia

"No Swapfile!
set noswapfile

" Remove all trailing whitespace by pressing F5
nnoremap <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar><CR>

" Mute bells
set vb t_vb=

set cc=80 "set colum at 80 chars

highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

" Show tabs and spaces
set listchars=tab:..,trail:_,extends:>,precedes:<,nbsp:~
set list

" Enable line numbers
set nu

" Reload file automatically
set autoread

" Highlight current line
set cursorline

" Change how vim represents characters on the screen
set encoding=utf-8

" Set the encoding of files written
set fileencoding=utf-8

autocmd Filetype go setlocal tabstop=4 shiftwidth=4 softtabstop=4

set shiftwidth=4
filetype plugin indent on

set backspace=indent,eol,start

nnoremap <leader>bb :buffers<cr>:b<space>
nnoremap <leader><tab> :b#<cr>

" NERDTREE

map <C-b> :NERDTreeToggle<CR>

" General
"        stop once at the start of insert.
map <C-n> :cnext<CR>
map <C-m> :cprevious<CR>
nnoremap <leader>a :cclose<CR>

" VIM-GO

let g:go_highlight_build_constraints = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_operators = 1
let g:go_highlight_structs = 1
let g:go_highlight_types = 1
let g:go_auto_sameids = 1
autocmd FileType go nmap <leader>b  <Plug>(go-build)
autocmd FileType go nmap <leader>r  <Plug>(go-run)
" run :GoBuild or :GoTestCompile based on the go file
function! s:build_go_files()
    let l:file = expand('%')
    if l:file =~# '^\f\+_test\.go$'
	call go#test#Test(0, 1)
    elseif l:file =~# '^\f\+\.go$'
	call go#cmd#Build(0)
    endif
endfunction
let g:go_def_mode='gopls'
let g:go_info_mode='gopls'
" automatically load imports on save
" open the autocompletion selection upon pressing .
au filetype go inoremap <buffer> . .<C-x><C-o>
let g:go_fmt_command = "goimports"
" Automatically get signature/type info for object under cursor
let g:go_auto_type_info = 1
let g:go_metalinter_enabled = ['vet', 'golint', 'errcheck']
let g:go_metalinter_autosave = 1
let g:go_fmt_fail_silently = 1

" Ale

" Error and warning signs.
let g:ale_sign_error = '⤫'
let g:ale_sign_warning = '⚠'" Enable integration with airline.
let g:ale_fix_on_save = 1
let g:airline#extensions#ale#enabled = 1
let g:ale_linters = {
	    \ 'go': ['gopls'],
	    \ 'elixir': ['elixir-ls'],
	    \ 'rust': ['rls']
	    \ }
" compile LSP for Elixir
let g:ale_elixir_elixir_ls_release = '/home/nino/lsp'
let g:ale_fixers = {
\   'elixir': ['mix_format'],
\   'rust': ['rustfmt'],
\}
let g:ale_fixers = {'*': ['remove_trailing_lines', 'trim_whitespace']}
let g:ale_sign_column_always = 1
let g:ale_completion_enabled = 1
let g:vimwiki_list = [{'path': '~/vimwiki/',
                      \ 'syntax': 'markdown', 'ext': '.md'}]

let g:rustfmt_autosave = 1

autocmd FileType rust nmap <leader>r  :RustRun<CR>

nnoremap <Leader>rg :Rg<CR>
nnoremap <Leader>RG :Rg!<CR>
nnoremap <Leader>p :Files<CR>
nnoremap <silent> <Leader>p :Files<CR>
nnoremap <silent> <Leader>b :Buffers<CR>
