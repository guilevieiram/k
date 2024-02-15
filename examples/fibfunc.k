int calculate_fibonacci [int n] {
    if n < 1 then return 1;
    else {
        int f1; 
        f1 := calculate_fibonacci[n-1];
        int f2; 
        f2 := calculate_fibonacci[n-2];
        return (f1 + f2);
    }
}

int f;
f := calculate_fibonacci[4];

