" Vim compiler file
" Compiler:     raco (Racket command-line tools)
" Maintainer:   D. Ben Knoble <ben.knoble+github@gmail.com>
" URL:          https://github.com/benknoble/vim-racket
" Last Change: 2022 Aug 12

let current_compiler = 'raco'

if exists(":CompilerSet") != 2
  command -nargs=* CompilerSet setlocal <args>
endif

CompilerSet makeprg=raco
CompilerSet errorformat=%f:%l:%c:%m
