module ParticleSwarmOptimization

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
    //This is struct because most of these will only last a single iteration and we don't want to cause extra work for the garbage collector
    struct
        val Parameters : list<float>
        val Velocity : list<float>
        val Local_best : list<float>
        val Local_best_loss : float
        new(parameters, velocity, local_best, local_best_loss) = 
            { Parameters = parameters; Velocity = velocity; Local_best = local_best; Local_best_loss = local_best_loss }
    end

let limit_velocity velocity max_velocity =
    velocity |> List.map (fun x ->  if x > 0.0 then
                                        min x max_velocity
                                    else
                                        min x -max_velocity)

let update_particle (args : Args) loss_func (particle : Particle) (global_best : list<float>) : Particle =
    let random = new System.Random()
    let r1 = random.NextDouble()*args.c1
    let r2 = random.NextDouble()*args.c2

    let velocity = (List.map2 (fun w v -> w*v) args.inertia_weight particle.Velocity,
                   List.map2 (fun l p -> r1*(l-p)) particle.Local_best particle.Parameters,
                   List.map2 (fun g p -> r2*(g-p)) global_best particle.Parameters)
                   |||> List.map3 (fun x y z -> x+y+z)
                   |> limit_velocity <| args.max_velocity

    let parameters = (particle.Parameters, velocity) ||> List.map2 (fun x y -> x + y)
    let new_loss = loss_func parameters

    if new_loss < particle.Local_best_loss then
        Particle(parameters, velocity, parameters, new_loss) 
    else 
        Particle(parameters, velocity, particle.Local_best, particle.Local_best_loss)

let update_particles (args : Args) (particles : list<Particle>) (global_best : list<float> * float) loss_func : (list<float> * float) * list<Particle> =
    
    let updated_particles = particles |> List.map (fun x -> update_particle args loss_func x (fst global_best))

    let best_from_this_iteration = updated_particles |> List.minBy (fun x -> x.Local_best_loss)

    if (snd global_best) < best_from_this_iteration.Local_best_loss then
        (global_best, updated_particles)
    else
        ((best_from_this_iteration.Local_best, best_from_this_iteration.Local_best_loss), updated_particles)

let reached_stop_condition (args : Args) iterations global_best_loss =
    iterations > 0 || global_best_loss <= args.success_threshold

let rec run_pso (args : Args) (particles : list<Particle>) (global_best : list<float> * float) loss_func iterations_to_run =
    let (new_global_best, new_particles) = update_particles args particles global_best loss_func
    let new_iterations_to_run = iterations_to_run - 1

    if reached_stop_condition args iterations_to_run (snd new_global_best) then
        (new_global_best, new_particles)
    else
        run_pso args new_particles new_global_best loss_func new_iterations_to_run
    
let execute (args : Args) (loss_func : list<float> -> float) (initail_weights : seq<list<float>>) =        
    let particles = initail_weights |> Seq.take args.particles
                        |> Seq.map (fun w -> Particle(w, [for _ in 1 .. w.Length -> 0.0], w, loss_func w))
                        |> Seq.toList

    let global_best = particles
                        |> List.minBy (fun x -> x.Local_best_loss) 
                        |> (fun x -> (x.Local_best, x.Local_best_loss))

    run_pso args particles global_best loss_func args.iterations