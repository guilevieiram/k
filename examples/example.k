/* defining a recurring function 
 * Note that we do not need braces if the function is
 * one statement only!
 * */
int recurrent_factorial(int n)
    if n < 2 then return 1;
    else return n * recurrent_factorial(n - 1);

/* Same thing but for an iterative aproach
 * You have while loops that work wonders!
 * */
int iterative_factorial(int n) {
    int result;
    result := 1;
    int i; 
    while i < n do {
        i := i + 1;
        result := result * i;
    }
    return result;
}

/* Main function
 * Normal function (as in Python for example)*/
nil main () {
    int n;
    /* doing some simple IO ...*/
    print_str("Enter a number:\n");
    n := read_int();

    print_str("Result via recurrent function: ");
    print_int(recurrent_factorial(n));
    print_str("\n");

    print_str("Result via iterative function: ");
    print_int(iterative_factorial(n));
    print_str("\n");

    /* Yes you can define funcions inside other functions
     * And closure is a thing.
     * */
    int my_other_function(int x) 
        return x + n;
    print_str("Result of my other function: ");
    print_int(my_other_function(n));
    print_str("\n\n");
}

/* Since it is all a script, you need to call your main*/
main();

/* And do anything you want after...*/
print_str("Look! A float: ");
print_float(0.0);
