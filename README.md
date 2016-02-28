# ParticleSwarmOptimization

This repo contains 3 implmentations of Particle Swarm Optimization

It goes with these 2 blog posts 
- [Particle Swarm Optimization in F#](http://www.danielslater.net/2015/05/particle-swarm-optimization-in-f.html)
- [Particle Swarm Optimization in F# part 2](http://www.danielslater.net/2015/05/particle-swarm-optimization-in-f-part-2.html)

If you are looking to use this in an application and want it to run multi-threaded parrallel 2 is the best to use.

## Example usage
```
open ParticleSwarmOptimization_Parallel_2

let target_point = [23.0;54.0]
let loss_func (x : list<float>) =  x |> List.map2 (fun x y -> sin 1.0/x-sin 1.0/y) target_point |> List.sum |> abs
let random = new System.Random()
let initial_weights = seq{ while true do yield [random.NextDouble()*1000.0;random.NextDouble()*1000.0]};

let args = Args(inertia_weight=[0.8; 0.8], particles=10, success_threshold=0.01, iterations = 10);
let (global_best_params, global_best_loss, particles) = execute args loss_func initial_weights;;

printfn "Particles %A" particles
printfn "Best loss %A" global_best_loss
```
