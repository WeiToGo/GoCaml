package short_variable_declare_lvalue

func bad() {
        a, a.b, a[b] := 1, 2, 3
}
