/* helper to print integer array */
nil print_int_arr(array<int> arr){
    print("[");
    int i;
    while i < len(arr) do {
        print(int2str(arr[i]):", ");
        i := i + 1;
    }
    print("]");
}

/* helper to print the program */
nil print_program(array<char> prog, int instruction_pointer){
    int i;
    str buffer;
    while i < len(prog) do {
        buffer := "(" : int2str(i) : ") "
            : char2str(prog[i]) : "\n";
        if i = instruction_pointer then 
            print("*" : buffer);
        else print(buffer);
        i := i + 1;
    }
}

nil execute(array<char> program){
    /* Initialize stack */
    int stack_size := 10;
    array<int> stack;
    make_array(stack, stack_size);

    /* Initialize loop stack */
    int loop_stack_size := 10;
    int loop_stack_ptr := -1;
    array<int> loop_stack;
    make_array(loop_stack, loop_stack_size);

    /* Instruction and stack pointers */
    int ip;
    int sp;
    bool stop_flag := false;

    /* Main execution loop */
    while (ip < len(program)) & (!stop_flag) do {
        /* if we undershot the stack pointer */
        if sp < 0 then {
            print("Stack Pointer < 0\n");
            stop_flag := true;
        } else {}

        /* allocating more stack if we overshot */
        if sp >= stack_size then {
            array<int> extra;
            make_array(extra, 10);
            stack := stack : extra;
            stack_size := stack_size + 10;
        } else {}

        /* executing each instruction */
        char ins := program[ip];
        if ins = ' ' then {
        } else if ins = '+' then {
            stack[sp] := stack[sp] + 1;
        } else if ins = '-' then {
            stack[sp] := stack[sp] - 1;
        } else if ins = '>' then {
            sp := sp + 1;
        } else if ins = '<' then {
            sp := sp - 1;
        } else if ins = '.' then {
            print(int2str(stack[sp]));
        } else if ins = ',' then {
            stack[sp] := str2int(read());
        } else if ins = '[' then {
            loop_stack_ptr := loop_stack_ptr + 1;
            if loop_stack_ptr >= loop_stack_size then {
                array<int> extra;
                make_array(extra, 10);
                loop_stack := loop_stack : extra;
                loop_stack_size := loop_stack_size + 10;
            } else {}
            loop_stack[loop_stack_ptr] := ip;
        } else if ins = ']' then {
            if loop_stack_ptr < 0 then {
                print(
                    "Closing not oppened loop on instruction: "
                    : int2str(ip) : "\n"
                );
                stop_flag := true;
            } 
            else if stack[sp] = 0 then
                loop_stack_ptr := loop_stack_ptr - 1;
            else 
                ip := loop_stack[loop_stack_ptr] - 1;
        } else {
            print(
                "instruction not recognized: "
                : char2str(ins) : "\n"
            );
            stop_flag := true;
        }

        ip := ip + 1;
    }
}

/* Interpret a program directly from string */
nil interpret(str program)
    execute(str2chars(program));

array<char> get_program()
    return str2chars(read());

nil main(){
    /* getting program from user */
    /* array<char> program := get_program(); */
    /* execute(program); */
    /* You can also input it here! */
    interpret(",>,[<+>-]<.");
    return none;
}

main();
