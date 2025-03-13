open System

let rec dichotomy_rec f a b accuracy iter =
    let mid = (a + b) / 2.0
    if b - a < accuracy then mid
    else
        let fa = f a
        let fmid = f mid
        if fa * fmid > 0.0 
        then dichotomy_rec f mid b accuracy (iter + 1)
        else dichotomy_rec f a mid accuracy (iter + 1)

let dichotomy_method f a b = 
    dichotomy_rec f a b 0.00001 0
let rec iterations_method phi x0 prev accuracy iternum =
    if Math.Abs(x0 - prev : float) < accuracy || iternum > 100000 then
        x0
    else
        iterations_method phi (phi x0) x0 accuracy (iternum + 1)

let dichotomy f a b = dichotomy_method f a b
let iterations phi x0 = iterations_method phi x0 (x0 - 1.0) 0.00001 0

let newton f f' x0 =
    let phi_newton x = x - f x / f' x
    iterations phi_newton x0

let f1 x = cos (2.0 / x) - 2.0 * sin (1.0 / x) + 1.0 / x
let f2 x = sqrt 1.0 - 0.4 * x**2.0 - asin x
let f3 x = exp x - exp (-x) - 2.0

let f1' x = (2.0 * sin (2.0 / x) + 2.0 * cos (1.0 / x)) / x**2.0 - 1.0 / x**2.0
let f2' x = (-0.8 * x) / (2.0 * sqrt (1.0 - 0.4 * x**2.0)) - 1.0 / sqrt (1.0 - x**2.0)
let f3' x = exp x + exp (-x)

let phi1 x = x - 0.01 * f1 x
let phi2 x = sin (sqrt (1.0 - 0.4 * x**2.0))
let phi3 x = x - 0.01 * f3 x

let main =
    printfn "Выражение 1: cos(2/x) - 2sin(1/x) + 1/x = 0"
    printfn "Дихотомия: %10.5f" (dichotomy f1 1.0 2.0)
    printfn "Итерации:  %10.5f" (iterations phi1 1.5)
    printfn "Ньютон:    %10.5f" (newton f1 f1' 1.5)
    printfn ""

    printfn "Выражение 2: sqrt(1 - 0.4x^2) - arcsin(x) = 0"
    printfn "Дихотомия: %10.5f" (dichotomy f2 0.0 1.0)
    printfn "Итерации:  %10.5f" (iterations phi2 0.5)
    printfn "Ньютон:    %10.5f" (newton f2 f2' 0.5)
    printfn ""

    printfn "Выражение 3: e^x - e^(-x) - 2 = 0"
    printfn "Дихотомия: %10.5f" (dichotomy f3 0.0 1.0)
    printfn "Итерации:  %10.5f" (iterations phi3 0.5)
    printfn "Ньютон:    %10.5f" (newton f3 f3' 0.5)

main