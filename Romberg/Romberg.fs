namespace NumericalAnalysis

open System

module Integration =

    let private power n x = 
        [1..x+1] |> Seq.reduce (fun memo _ -> memo * n)

    let romberg f lower upper steps =
        let range = upper - lower

        let h i = range / (Math.Pow(2.0,float(i)))

        let RValues = Array2D.create (steps+1) (steps+1) Double.NaN

        let r term =
            match term with
            | (0,0) -> range/2.0 * ((f upper) + (f lower))
            | (i,0) -> 
                let hi = h i
                let sum =
                    [0..((power 2 (i-1))-1)] 
                    |> Seq.map (fun k -> f(lower + float(2*k+1)*hi))
                    |> Seq.sum

                (RValues.[i-1,0])/2.0 + hi * sum
            | (i, j) ->
                (RValues.[i, j-1] +
                    1.0/(float((power 4 j) - 1))*(RValues.[i, j-1] - RValues.[i-1,j-1]))

        for j = 0 to steps do
            for i = j to steps do
                RValues.[i,j] <- r (i,j)

        r (steps,steps)