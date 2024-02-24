# k

*k* is a programming language I built as an exercise to learn about parsers, interpreters and programming in general. It's syntax is heavily inspired by C but with a couple tweaks to make it unique. Is is a work in progress and tere aren't a lot of features yet, but it sure does the job.

Everything from the command line interface to the interpreter was made using Haskell, without relying on external packages to parse code or transverse abstract trees. 

The whole project took me about two weeks of work and I am very proud to present a couple working code snippets in the repo.

Of course, any feedback on the project is very valuable and you can reach out anytime!
 
## Installing
The installation of the interpreter relies on Haskell (of course) and on Cabal, a package manager. If you don't have them installed, I suggest you install first *ghcup* a manager that will take care of everything for you. 

To install ghcup on Linux, MacOS or WSL2, run the following command:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```
(This is extracted from the official documentation on https://www.haskell.org/ghcup/)

Feel free to install GHC (Haskell's compiler) and Cabal separatelly on your machine as they are the only programs you need.

Once you have ghcup (or GHC and Cabal) you can make a local executable by running:
```bash
make
```

To install it on your system as a binary, run
```bash
make install
```

## Usage
To use the interpreter:
```bash
./k <filename> [-d]
```
Where the flag `-d` indicates debug mode.

## Examples

You can find some examples under the `examples` folder. I have wrote:
- A `factorial` function
- A solution to the `two sum` problem.
- A `brainfuck` interpreter
- Conway's `Game of life`

Doom its not a thing yet, but I think this examples show that this language works quite okey.

## Language 

Here I'll list some of the main features of the language without boring everyone with formal grammars.

### Primitive types
Currently there is support for `int`, `float`,  `bool`, `nil`,  `char` and `str` that are represented by Haskells primitives.


### Variables and Expressions

```k
/* To declare a variable */
int my_int_variable;

/* Default values are assigned 
 * when you declare them.
 * In this case my_variable is 0
 */


/* Assigning values */
my_float := 2.0;

/* Both at the same time! */
str my_string := "look, im a string!";

/* You can operate on them as you'd expect */
int result := my_int + my_other_int;

/* Order of operation is not enforced, so use () */
float float_result := 
    (my_float - my_other_float) * 0.69 ;

/* Be careful of a couple different symbols from C */
bool my_bool := 
    (my_int = 42) | (false & true);

```

To name variables you can use any combination of letters, numbers and underscore.

### Control flow
If statements are of the form `if <boolean expr> then <statement> else <statement>`
 
```k
if i < 42 then
    x := 0;
else {
    flag := false;
    x := 1000;
}
 
/* Note that statments can be stacked via {} 
 * but not necessarily.
 */
 if b then x := 1; else x := 0;

```

Loops are achieved via the while statement `while <boolean expr> do <statement>`
```k
int i; /* Initialized to 0 */
while i < 10 do {
    x := 10 * x;
    i := i + 1;
}
```

### Functions
To declare functions you do like in C.
```k
int my_function (int arg1, float arg1){
    int x := 2 * arg1;
    if arg2 < 10.0 then return x;
    else return -x;
}

/* Single statements work just as fine! */
float relu (float x) 
    if x < 0.0 return 0.0; else return x;

```

To call functions, also the same drill;
```k
float x := relu(3.9);

/* You can also throw out the result */
my_function(1, 1.9);
```

If your function does not return anything (booo side-effects) you can mark it with `nil`.
```k
nil process_this(int x){
    /*...*/
}
```

### Arrays
Arrays behave a little bit different that you would expect here, so bear with me.
```k
/* Declaring an array of ints */
array<int> my_array;

/* At this point it is empty
 * But we can allocate some space like so
 */

make_array(my_array, 10);

/* Now it has 10 slots.
 * This cannot be changed unless you call make_array again
 * or reasign the variable to another value.
 */


/* You can assign or reference values as you'd expect */
my_array[2] := 3;
int x := my_array[0] + 1;


/* You can concat arrays to create a new one */
array<int> new_array :=
    first_array : second_array;

/* Of course you can stack them up as you'd like */
array<array<float>> matrix;
make_array(matrix, 10);
make_array(matrix[0], 5);
```
 
### Strings 
Strings are mainly for IO purposes in this language, but we make other methods available to process streams of characters.
```k 
/* Declare literals with double quotes */
str my_string := "Hi there!";

/* You can also concat them like arrays */
my_string := my_string : "My name is Guile.";


/* You can also transform them in chars */
array<char> stream := str2chars(my_string);
stream[0]; /* H */
stream[1]; /* i */

/* Char literals comme in single quotes! */
stream[2] := 'A';
stream[3] := '\n';
```

### IO
IO is hard to do in Haskell but very easy in k. There are to main functionalities that you should use: `print` and `read`, together with a bunch of conversions.
```k
/* Print something to the stdout */
print("Hi Mom!\n"); 


/* This function only accepts one str 
 * so if you want to print other values, you need to convert them.
 */
print("My values are:\n"
    : int2str(3) : "\n"
    : float2str(1.7) : "\n"
    : bool2str(true) : "\n"
    : char2str('a') : "\n"
);


/* Read works very similarly. 
 * It reads from the stdin until a new line is encountered.
 * It then returns you a str with the value read.
 */
str my_input := read();

/* You can convert the other way to! */
int my_int := str2int("24");
float my_float := str2float("2.438");
bool my_bool := str2bool("true");
char my_char := str2char("A");
```

### Types
The language is type safe since we check the whole program before interpreting.
However, it does not enforce type safety at run time as it would double the work.
To check a type at runtime, you can make use of a builtin typeof function.
```k
if typeof(x) = "int" then
    print("OMG it is an int \n");
else
    print("Nevermind...\n");
```

 
## Next steps
Of course the language still lacks many many features, but the ones we are most excited for the future are:

- Better error handling and error messages, showing common mistakes at parsing and execution time.
- Write a backend to convert the AST into assembly to get a more performant code. This can take some time because I don't want to use fancy tools like LLVM to do it....
- Enhanced type system with type definitions `type stream := array<char>;` and structs `struct s {int a; float b;}`
- Write an LSP for a better development environment.


