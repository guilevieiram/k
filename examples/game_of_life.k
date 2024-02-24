array<array<int>> create_matrix(int rows, int cols){
    if (rows <= 0) | (cols <= 0) then {
        print("Trying to create empty matrix\n");
    } else {}

    array<array<int>> matrix;
    make_array(matrix, rows);
    int row := 0;
    while row < rows do {
        make_array(matrix[row], cols);
        row := row + 1;
    }

    return matrix;
}

array<array<int>> copy_matrix(array<array<int>> matrix){
    int rows := len(matrix);
    int cols := len(matrix[0]);
    array<array<int>> new_matrix
        := create_matrix(rows, cols);

    int row;
    while row < rows do {
        int col;
        while col < cols do {
            new_matrix[row][col]
                := matrix[row][col];
            col := col + 1;
        }
        row := row + 1;
    }

    return new_matrix;
}

nil print_matrix(array<array<int>> matrix){
    int rows := len(matrix);
    int cols := len(matrix[0]);
    
    int row;
    while row < rows do {
        int col;
        while col < cols do {
            int el := matrix[row][col];
            if el = 1 then print("[]");
            else print("__");
            col := col + 1;
        }

        print("\n");
        row := row + 1;
    }

}

int min(int x , int y)
    if x < y then return x; else return y;

int max(int x , int y)
    if x < y then return y; else return x;

int count_neighbors (array<array<int>> matrix, int i, int j, int target){
    int rows := len(matrix);
    int cols := len(matrix[0]);
    int count;
    int row := max(i-1, 0);
    while row <= min(i+1, rows-1) do {
        int col := max(j-1, 0);
        while col <= min(j+1, cols-1) do {
            if 
                !((i = row) & (j = col))
                & (matrix[row][col] = target) 
             then count := count + 1;
            else {}
            col := col + 1;
        }
        row := row + 1;
    }

    return count;
}

array<array<int>> step(array<array<int>> matrix){
    int rows := len(matrix);
    int cols := len(matrix[0]);
    array<array<int>> new_matrix := copy_matrix(matrix);

    int row;
    while row < rows do {
        int col;
        while col < cols do {
            int neigs := count_neighbors(matrix, row, col, 1);
            int el := matrix[row][col];
            
            if (el = 1) & (neigs < 2) then
                /* dies by underpopulation */
                new_matrix[row][col] := 0;
            else if (el = 1) & (neigs > 3) then
                /* dies by overpopulation */
                new_matrix[row][col] := 0;
            else if (el = 0) & (neigs = 3) then
                /* born by reproduction */
                new_matrix[row][col] := 1;
            else {}

            col := col + 1;
        }
        row := row + 1;
    }
    return new_matrix;
}
array<array<int>> read_initial(){
    int rows := str2int(read());
    int cols := str2int(read());
    int n_alive := str2int(read());
    array<array<int>> matrix := create_matrix(rows, cols);

    int i;
    while i < n_alive do {
        int row := str2int(read());
        int col := str2int(read());
        matrix[row][col] := 1;
        i := i + 1;
    }
    return matrix;
}

nil main(){
    print("Welcome to Conways game of life in k!\n"
        : "To run this scipt you should provide via stdin the following\n"
        : "information, separated by new lines:\n"
        : "<number of iterations>\n"
        : "<number of rows>\n"
        : "<number of cols>\n"
        : "<number of alive cells in the start>\n"
        : "<1st alive cell row>\n"
        : "<1st alive cell col>\n"
        : "<2nd alive cell row>\n"
        : "<2nd alive cell col>\n"
        : "...\n"
        : "<nth alive cell row>\n"
        : "<nth alive cell col>\n"
        : "Final new line to run it!\n"
        : "\n Enjoy!\n"
    );
    int n_iterations := str2int(read());

    array<array<int>> matrix := read_initial();

    print("\nInitial matrix:\n");
    print_matrix(matrix);
    int i;
    while i < n_iterations do {
        matrix := step(matrix);
        print("\nIteration":int2str(i):"\n");
        print_matrix(matrix);
        i := i + 1;
    }
}


main();
