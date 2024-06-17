# Monkey RS
This is my implementation of the _Monkey_ programming language, implemented in Rust.
The language, as well as the interpreter were described by Thorsten Ball [@mrnugget](https://github.com/mrnugget) 
in his [book "Writing an Interpreter in Go"](https://www.interpreterbook.com).

## The Monkey Language
_Monkey_ consists of the following elements:
```
// Basic datatypes:
>> 42; // Integers
42
>> false; // Booleans 
false
>> "this is a string"; // Strings
this is a string

// Operators:
    // On integers:
    >> 1 + 2;
    3
    >> 1 - 2;
    -1
    >> 1 * 2;
    2
    >> 1 / 2; //Integer devision!
    0
    >> !5; 
    false
    >> 1 == 1;
    true
    >> 1 != 1;
    false
    >> 1 < 1;
    false
    >> 2 > 1;
    true
    >> 1 <= 1; // not specified by Thorsten Ball, but implemented here
    true
    >> 2 >= 3; // not specified by Thorsten Ball, but implemented here
    false
    
    // On Booleans
    >> !true;
    false
    >> true == false;
    false
    >> true != false;
    true
    
    // On Strings
    >> "Foo" + "bar";
    Foobar

// Binding a value to an identifier:
>> let a = 42; // let <identifier> = <expression>;
>> a;
42

// Functions:
>> fn(x) { x + 1; } // fn(<identifiers, comma separated>) { <statements, semicolon separated> }
>> let add = fn(a,b) { a+b;};
>> add(1,2); // <function>(<expressions, comma separated>)
3

// Arrays
>> let a = [1,2, "three"]; // [<expressions, comma separated>]
>> a;
[1, 2, "three"]
>> a[0]; // <array>[<expression>]
1

// Hashes
>> let h = {"a": 1, true: false, 2: "two"}; // {<<expression>:<expression>, comma separated>}
>> h["a"];
1

// Builtin functions
>> len([1,2,3]);
3
>> len("three");
5
>> first([1,2,3]);
1
>> last([1,2,3]);
3
>> rest([1,2,3]);
[2,3]
>> puts("Hello World!", 1, true);
// Prints:
// Hello World!
// 1
// true

// Conditionals
>> if (1 > 2) { "Foo"; } else { "Bar" }; // if (<expression, which evaluates truthy>) {<statemens, semiclon separated>} [else {<statements, semicolon separated>}]
Bar
>> if (true) { return 5; "I'm lonely!";} // return early from within block statements (function or conditional)
5
```

## Using the interpreter
Simply clone this repo and execute it using [cargo](https://www.rust-lang.org/learn/get-started):
```bash
cargo run
```
You will start in an interactive shell, ready to interpret some _Monkey_ code!