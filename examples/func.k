int f [float x][float y]{
    int a; int b;
    return 2;
}
int x;
x := f[1.0][2.0];

float 
sum_two_floats 
[float x][float y]
{return x + y;}

float z; 
z := sum_two_floats [1.2][3.5];


float func_without_args{
    int b;
    return 3.0;
}

z := func_without_args[];

int func_with_if [int x]{
    if x < 10 then 
        return 1;
    else return 3;
}

int y;
y := func_with_if[2];

