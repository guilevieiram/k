/* recreating the fibbonaci numbers*/
int fib [int n]{
    if      n < 0 then return  -1;
    else if n = 0 then return   0;
    else if n < 2 then return   1;
    else {
        int f1; f1 := 1;
        int f2; f2 := 1;
        int i;
        int tmp;
        while (i < (n - 2) ) do {
            tmp := f2;
            f2 := f2 + f1;
            f1 := tmp;
            i := i + 1;
        }
        return f2;
    }
}

nil main {

    /* read the number from the user*/
    int n; 
    n := read_int[];

    int res; 
    res := fib[n];

    print_int[res];
}

main[];
