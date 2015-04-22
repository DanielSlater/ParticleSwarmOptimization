#load "ParticleSwarmOptimization.fs"
open ParticleSwarmOptimization;

let target_point = [23.0;54.0]
let loss_func (x : list<float>) =  x |> List.map2 (fun x y -> sin 1.0/x-sin 1.0/y) target_point |> List.sum |> abs
let random = new System.Random()
let initial_weights = seq{ while true do yield [random.NextDouble()*1000.0;random.NextDouble()*1000.0]};

let args = Args(inertia_weight=[0.8; 0.8], particles=10, success_threshold=0.0, iterations = 10);
let mutable (global_best, particles) = execute args loss_func initial_weights;;

for i in 0 .. 10 do
    printfn "Particles %A" particles
    (global_best, particles) = execute args loss_func (particles |> Seq.map (fun x -> x.Parameters |> Seq.toList));;