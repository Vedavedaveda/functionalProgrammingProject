#r "nuget: FsLexYacc.Runtime, 10.2.0"
#r "vm.dll"
#r "asm.dll"
#load "syntax.fs" "parser.fs" "lexer.fs" "parse.fs" "compiler.fs" "interpreter.fs"

// Add more F# code here

let comps s = Asm.asm (Compiler.compProg (Parse.fromString s));;
let compf f = Asm.asm (Compiler.compProg (Parse.fromFile f));;
