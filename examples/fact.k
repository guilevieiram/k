/* recreating the fibbonaci numbers*/
int fact [int n] {
    if      n < 0 then return  -1;
    else if n = 0 then return   1;
    else {
        int x; 
        x := 1;
        int i;
        while i < n do {
            i := i + 1;
            x := x * i;
        }
        return x;
    }
}

nil main {

    /* read the number from the user*/
    int n; 
    n := read_int[];

    int res; 
    res := fact[n];

    print_int[res];
}

main[];
