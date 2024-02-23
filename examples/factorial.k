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
    int result := 1;
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
    /* doing some simple IO ...*/
    print("Enter a number:\n");
    int n := str2int(read());

    print(
        "Result via recurrent function: "
        : int2str(recurrent_factorial(n))
        : "\n"
    );

    print(
        "Result via iterative function: "
        : int2str(iterative_factorial(n))
        : "\n"
    );

    /* Yes you can define funcions inside other functions
     * And closure is a thing.
     * */
    int my_other_function(int x) 
        return x + n;

    print(
        "Result of another function: "
        : int2str(my_other_function(n))
        : "\n\n"
    );
    return none;
}

/* Since it is all a script, you need to call your main*/
main();

/* And do anything you want after...*/
print("Look! A float: " : float2str(420.69) : "\n");
