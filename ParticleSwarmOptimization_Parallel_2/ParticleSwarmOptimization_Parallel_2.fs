module ParticleSwarmOptimization_Parallel_2
open FSharp.Collections.ParallelSeq

//class that will hold all our algorythm variables
type Args(inertia_weight : list<float>, ?particles : int, ?iterations : int, ?max_velocity : float, ?success_threshold : float, ?c1 : float, ?c2 : float) =
    member this.inertia_weight = inertia_weight
    member this.particles = defaultArg particles 10
    member this.iterations = defaultArg iterations 100
    member this.max_velocity = defaultArg max_velocity 10.0
    member this.success_threshold = defaultArg success_threshold 0.0001
    member this.c1 = defaultArg c1 2.0
    member this.c2 = defaultArg c2 2.0

[<StructuredFormatDisplay("Particle {Parameters}")>]
type Particle =
    val Parameters : list<float>
    val Velocity : list<float>
    val Local_best : list<float>
    val Local_best_loss : float
    new(parameters, velocity, local_best, local_best_loss) = 
        { Parameters = parameters; Velocity = velocity; Local_best = local_best; Local_best_loss = local_best_loss }

let private update_particle (args : Args) loss_func (particle : Particle) (global_best_params : list<float>) : Particle =
    let limit_velocity velocity max_velocity =
        velocity |> List.map (fun x ->  if x > 0.0 then
                                            min x max_velocity
                                        else
                                            min x -max_velocity)
    let random = new System.Random()
    let r1 = random.NextDouble()*args.c1
    let r2 = random.NextDouble()*args.c2

    let velocity = (List.map2 (fun w v -> w*v) args.inertia_weight particle.Velocity, // multiple last velocity by inertia weight
                    List.map2 (fun l p -> r1*(l-p)) particle.Local_best particle.Parameters, //get attraction of local best
                    List.map2 (fun g p -> r2*(g-p)) global_best_params particle.Parameters)//get attration of global best
                    |||> List.map3 (fun x y z -> x+y+z)//add the result of these 3 calculations together
                    |> limit_velocity <| args.max_velocity //limit velocity by max

    let new_parameters = (particle.Parameters, velocity) ||> List.map2 (fun x y -> x + y)
    let new_loss = loss_func new_parameters

    if new_loss < particle.Local_best_loss then
        Particle(new_parameters, velocity, new_parameters, new_loss) 
    else 
        Particle(new_parameters, velocity, particle.Local_best, particle.Local_best_loss)

let private stop_condition (args : Args) iterations global_best_loss =
    iterations <= 0 || global_best_loss <= args.success_threshold

let rec private run_particle (args : Args) (particle : Particle) (global_best : ref<Particle>) check_particle_against_global_best loss_func iterations_to_run : Particle =
    let updated_particle = update_particle args loss_func particle global_best.Value.Parameters
    check_particle_against_global_best updated_particle

    let new_iterations_to_run = iterations_to_run - 1
    if stop_condition args iterations_to_run global_best.Value.Local_best_loss then
        updated_particle
    else
        run_particle args updated_particle global_best check_particle_against_global_best loss_func new_iterations_to_run
    
let execute (args : Args) (loss_func : list<float> -> float) (initail_weights : seq<list<float>>) =        
    let particles = initail_weights |> Seq.take args.particles
                        |> PSeq.map (fun w -> Particle(w, [for _ in 1 .. w.Length -> 0.0], w, loss_func w))
                        |> Seq.toList

    let global_best = ref (particles |> List.minBy (fun x -> x.Local_best_loss) )
    let monitor = new System.Object()
    let check_particle_against_global_best (particle : Particle) =
        lock monitor (fun() -> if particle.Local_best_loss < global_best.Value.Local_best_loss then
                                    global_best.contents <- particle)

    let final_particles = particles |> PSeq.map (fun x -> run_particle args x global_best check_particle_against_global_best loss_func args.iterations)
                                    |> PSeq.toList

    (global_best.Value.Local_best, global_best.Value.Local_best_loss, final_particles)