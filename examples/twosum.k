array<int> two_sum (array<int> arr, int tar){
    array<int> ans;
    int n := len(arr);
    int i;
    while i < n do {
        int j := i;
        while j < n do {
            if arr[j] + arr[i] = tar then {
                make_array(ans, 2);
                ans[0] := i;
                ans[1] := j;
                return ans;
            } else {}
            j := j + 1;
        }
        i := i + 1;
    }
    return ans;
}

nil main() {
    /* Gettin input from user */
    print("Input the target value: ");
    int target := str2int(read());
    print("Input the number of values in the array: ");
    int n := str2int(read());

    array<int> l;
    make_array(l, n);
    int i;
    print("Input the array elements: \n");
    while i < n do{
        l[i] := str2int(read());
        i := i + 1;
    }

    /* Running the algo */
    array<int> ans := two_sum(l, target);
    if len(ans) = 0 then
        print(
            "No pair summing to "
            : int2str(target)
            : " could be found.\n"
        );
    else
        print(
            "Pair found!\n"
            : "i = ": int2str(ans[0])
            : "\nj = ": int2str(ans[1])
            : "\n"
        );
}
main();
