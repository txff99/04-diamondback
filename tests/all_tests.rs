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
    }
}

static_error_tests! {
    {
        name: duplicate_params,
        file: "duplicate_params.snek",
        expected: "A function's parameter list has a duplicate name",
    }
}
