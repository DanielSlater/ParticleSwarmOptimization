// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open ParticleSwarmOptimization
open ParticleSwarmOptimization_Parallel_1
open ParticleSwarmOptimization_Parallel_2
open System

let speed_test action title = 
    let iterations = 20
    let sw = System.Diagnostics.Stopwatch();
    let results =
        [for i in 0 .. iterations do
            GC.Collect()
            sw.Restart()
            action()
            sw.Stop()
            yield sw.Elapsed] |> List.sort
    let median_result = List.nth results (iterations/2)
    printfn "%s : %A" title median_result

[<EntryPoint>]
let main argv = 
    let loss_func x =   let mutable a = x |> List.head
                        let mutable b = x |> List.tail |> List.head
                        for i in 0 .. 100000 do
                            a <- sqrt (a*a)
                            b <- sqrt (b*a)
                        abs (a + b)+1.0
    let random = new System.Random()
    let initial_weights = seq{ while true do yield [random.NextDouble()*1000.0;random.NextDouble()*1000.0]}
    let iterations = 100
    let particles = 10
    let inertia_weight = [0.8; 0.8]

    let args_single_threaded = ParticleSwarmOptimization.Args(inertia_weight=inertia_weight, particles=particles, iterations = iterations);
    let run_single_threaded () = ParticleSwarmOptimization.execute args_single_threaded loss_func initial_weights |> ignore
    let time_single_threaded = speed_test run_single_threaded "Single threaded fixed time func"

    let args_multi_threaded = ParticleSwarmOptimization_Parallel_1.Args(inertia_weight=inertia_weight, particles=particles, iterations = iterations);
    let run_multi_threaded () = ParticleSwarmOptimization_Parallel_1.execute args_multi_threaded loss_func initial_weights |> ignore
    let time_multi_threaded = speed_test run_multi_threaded "Multi threaded fixed time func"

    let args_multi_threaded_2 = ParticleSwarmOptimization_Parallel_2.Args(inertia_weight=inertia_weight, particles=particles, iterations = iterations);
    let run_multi_threaded_2 () = ParticleSwarmOptimization_Parallel_2.execute args_multi_threaded_2 loss_func initial_weights |> ignore
    let time_multi_threaded_2 = speed_test run_multi_threaded_2 "Mutli threaded 2 fixed time func"

    //some deterministic random numbers
    let deterministic_random_numbers = [for i in 0 .. 1000000 do 
                                            yield random.Next(10000,200000)]
    let atomic_integer = ref 0
    let variable_length_loss_func (x : list<float>) =   let count = System.Threading.Interlocked.Increment(atomic_integer)
                                                        let mutable a = x |> List.head
                                                        let mutable b = x |> List.tail |> List.head
                                                        for i in 0 .. List.nth deterministic_random_numbers count do
                                                            a <- sqrt (a*a)
                                                            b <- sqrt (b*a)
                                                        abs (a + b)+1.0

    let run_single_threaded_variable_length () = ParticleSwarmOptimization.execute args_single_threaded variable_length_loss_func initial_weights |> ignore
    let time_single_threaded_variable_length = speed_test run_single_threaded_variable_length "Single threaded variable time func"

    let run_multi_threaded_variable_length () = ParticleSwarmOptimization_Parallel_1.execute args_multi_threaded variable_length_loss_func initial_weights |> ignore
    let time_multi_threaded_variable_length = speed_test run_multi_threaded_variable_length "Multi threaded variable time func"

    let run_multi_threaded_2_variable_length () = ParticleSwarmOptimization_Parallel_2.execute args_multi_threaded_2 loss_func initial_weights |> ignore
    let time_multi_threaded_2_variable_length = speed_test run_multi_threaded_2_variable_length "Mutli threaded 2 variable time func"

    0 // return an integer exit code
