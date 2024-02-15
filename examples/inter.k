/* creating the fibbonaci numbers*/
int n;
n := 6;

int f1;
int f2;
f1 := 1;
f2 := 1;

int i;
int tmp;
while (i < n) do {
    tmp := f2;
    f2 := f2 + f1;
    f1 := tmp;
    i := i + 1;
}
/* f2 has the n-th fibbonaci number!*/
nil my_func [float x][float y]{
    float z; z := x + y;
}

