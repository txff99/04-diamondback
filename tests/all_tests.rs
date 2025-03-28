mod infra;

// Your tests go here!
success_tests! {
    {
        name: fact,
        file: "fact.snek",
        input: "10",
        expected: "3628800",
    },
    {
        name: even_odd_1,
        file: "even_odd.snek",
        input: "10",
        expected: "10\ntrue\ntrue",
    },
    {
        name: even_odd_2,
        file: "even_odd.snek",
        input: "9",
        expected: "9\nfalse\nfalse",
    },
    {
        name: multibindings,
        file: "multibindings.snek",
        expected: "1\n2\n1\n1"
    },
    {
        name: vec_get,
        file: "vec_get.snek",
        input: "2",
        expected: "2\n",
    },
    {
        name: make_vec,
        file: "make_vec.snek",
        input: "3",
        expected: "[0 0 0]\n",
    },
    {
        name: vec_len,
        file: "vec_len.snek",
        expected: "3\n",
    },
    {
        name: vec,
        file: "vec.snek",
        expected: "[0 1 2 3 4]\n"
    },
    {
        name: vec_set,
        file: "vec_set.snek",
        expected: "[0 1 3 3]\n",
    }
}

runtime_error_tests! {
    {
        name: invalid_arg,
        file: "invalid_arg.snek",
        expected: "an error ocurred: invalid argument",
    },
    {
        name: overflow,
        file: "overflow.snek",
        expected: "an error ocurred: overflow",
    },
    {
        name: vec_make_error,
        file: "vec_make_error.snek",
        expected: "an error ocurred: invalid argument",
    },
    {
        name: vec_get_error1,
        file: "vec_get_error1.snek",
        expected: "an error ocurred: invalid argument",
    },
    {
        name: vec_get_error2,
        file: "vec_get_error2.snek",
        expected: "an error occured: index out of bound",
    }
}

static_error_tests! {
    {
        name: duplicate_params,
        file: "duplicate_params.snek",
        expected: "A function's parameter list has a duplicate name",
    }
}

