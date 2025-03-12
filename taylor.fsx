let f_builtin x = exp (x * x)

let rec factorial n =
    if n = 0 then 1 else n * factorial (n - 1)

let naive_taylor x eps =
    let rec loop i term sum =
        if abs term < eps then
            sum, i
        else
            let term' = pown x (2 * i) / float (factorial i)
            loop (i + 1) term' (sum + term')
    loop 0 1.0 0.0

let smart_taylor x eps =
    let mutable sum = 1.0
    let mutable term = 1.0
    let mutable i = 1
    while abs term >= eps do
        term <- term * x * x / float i
        sum <- sum + term
        i <- i + 1
    sum, i

let print_table a b n eps =
    // Вывод заголовка таблицы
    printfn "x\tBuiltin\t\tSmart Taylor\t# terms\tDumb Taylor\t# terms"
    printfn "---------------------------------------------------------------------"
    // Вычисление и вывод значений функции на интервале [a, b]
    for i = 0 to n do
        let x = a + float i / float n * (b - a)
        let builtin = f_builtin x
        let smart, smartTerms = smart_taylor x eps
        let naive, naiveTerms = naive_taylor x eps
        printfn "%6.3f | %12g | %12g | %6d | %12g | %6d" x builtin smart smartTerms naive naiveTerms

let main =
    let a = 0.0
    let b = 1.0
    let eps = 1e-5
    let n = 10
    print_table a b n eps

main