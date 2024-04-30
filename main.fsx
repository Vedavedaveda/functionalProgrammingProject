#r "nuget: FsLexYacc.Runtime, 10.2.0"
#r "vm.dll"
#r "asm.dll"
#load "syntax.fs" "parser.fs" "lexer.fs" "parse.fs" "compiler.fs" //"interpreter.fs"

// Add more F# code here

let comps s = Asm.asm (Compiler.compProg (Parse.fromString s));;
let compf f = Asm.asm (Compiler.compProg (Parse.fromFile f));;

let tests = [
    "1";
    "1 + 2";
    "2 + 3 * 4";
    "2 * 3 + 4";
    "(2 + 3) * 4";
    "2 * (3 + 4)";
    "5 - 4";
    "5 - 3 - 2";
    "5 + (-(4))";
    "5 - (-(4))";
    "3 < 4";
    "4 < 3";
    "3 < 3";
    "3 > 4";
    "4 > 3";
    "3 > 3";
    "3 <= 4";
    "4 <= 3";
    "3 <= 3";
    "3 >= 4";
    "4 >= 3";
    "3 >= 3";
    "4 != 3";
    "3 != 3";
    "true";
    "true + true";
    "true  && false";
    "true  && true";
    "false && false";
    "false && true";
    "true  || false";
    "true  || true";
    "false || false";
    "false || true";
    "read";
    "write(read)";
    "read + read";
    "let x = 3 + 4 in x * x";
    "let x = 3 + 4 in let x = 1 in x + x";
    "let x = 3 + 4 in (let x = 1 in x) + x";
    "let x = 3 + 4 in x + (let x = 1 in x)";
    "let x = read in let y = read in x + y";
    "if true then 42 else 87";
    "if false then 42 else 87";
    "100 + (if true then 42 else 87)";
    "100 + (if false then 42 else 87)";
    "(if true then 42 else 87) + 100";
    "(if false then 42 else 87) + 100";
    "(if true then 42 else 87) + (if true then 100 else 200)";
    "(if true then 42 else 87) + (if false then 100 else 200)";
    "(if false then 42 else 87) + (if true then 100 else 200)";
    "(if false then 42 else 87) + (if false then 100 else 200)";
    "(if (if false then 1 else 0) then 42 else 87)";
    "(if false then (if false then 42 else 33) else 87)";
    "(if true then (if false then 42 else 33) else 87)";
    "(if false then (if true then 42 else 33) else 87)";
    "(if true then (if true then 42 else 33) else 87)";
    "(if false then 87 else (if false then 42 else 33))";
    "(if true then 87 else (if false then 42 else 33))";
    "(if false then 87 else (if true then 42 else 33))";
    "(if true then 87 else (if true then 42 else 33))";
    ]

let weird_tests = [
    "false && write(33)";
    "true  && write(33)";
    "false || write(33)";
    "true  || write(33)";
    "write(33) && write(55)";
    "write(0) && write(55)";
    "write(33) || write(55)";
    "write(0) || write(55)";
    "write(write(22))";
    "let x = read in let y = read in write(x + y)";
    "func five() = 5; five()";
    "func five() = 5; five() + 10";
    "func twice(n) = n + n; twice(5)";
    "func twice(n) = n + n; twice(5) + 100";
    "func twice(n) = n + n; 100 + twice(5)";
    "func twice(n) = n + n; twice(2 + 5) + 100";
    "func twice(n) = n + n; 100 + twice(2 + 5)";
    "func sqr(n) = n * n; sqr(sqr(3))";
    "func sqr(n) = n * n; sqr(1 + sqr(3))";
    "func sqr(n) = n * n; sqr(sqr(3) + 1)";
    "func foo(x) = x + 4; func bar(y) = 100 + y; bar(foo(1))";
    "func loop() = loop(); false && loop()";
    "func loop() = loop(); true  && loop()";
    "func loop() = loop(); false || loop()";
    "func loop() = loop(); true  || loop()";
    "func fac(n) = if n == 0 then 1 else n * fac(n - 1); fac(5)";
    "func fac(n, r) = if n == 0 then r else fac(n - 1, r * n); fac(5, 1)";
    "func loop(n, r) = if n == 0 then r else loop(n - 1, n * r); func fac(n) = loop(n, 1); write(fac(read))";
    "func even(n) = n == 0 || odd(n - 1); func odd(n) = n != 0 || even(n - 1); even(5)";
    "func pow(x, n) = if n == 0 then 1 else x * pow(x, n - 1); pow(3, 4)";
    "func pow(x, n, r) = if n == 0 then r else pow(x, n - 1, r * x); pow(3, 4, 1)";
    "func pow(x, n) = if n == 0 then 1 else if n % 2 == 0 then (let v = pow(x, n / 2) in v * v) else x * pow(x, n - 1); pow(2, 13)"
]

//for t in tests do
   // let code = comps t
    //let result: int = VM.exec code
   // System.Console.WriteLine("Input: {0}, Output: {1}" , t, result);;


//let code = Main.comps  "func f(x,y,z) = (x+z) * y; f(1,5,2)";;
//VM.exec code;;
// #load "main.fsx";;