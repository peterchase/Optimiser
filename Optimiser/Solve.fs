namespace Optimiser

module Solve =

    let rec solution tol dx f x =

        let step = Step.nextStep tol dx f x
        let fx = f x

        printfn "%10f %10f" x fx 

        match step with
        | Some s -> solution tol dx f (x + s)
        | None _ -> x
