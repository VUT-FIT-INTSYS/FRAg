path(hall, warehouseA, [warehouseA]).
path(construction, warehouseA, [hall, warehouseA]).
path(workshop, warehouseA, [hall, warehouseA]).
path(warehouseB, warehouseA, [workshop, hall, warehouseA]).

path(hall, warehouseB, [warehouseB]).
path(construction, warehouseB, [hall, warehouseB]).
path(workshop, warehouseB, [hall, warehouseB]).
path(warehouseA, warehouseB, [workshop, hall, warehouseB]).

path(construction, hall, [hall]).
path(workshop, hall, [hall]).
path(warehouseA, hall, [workshop, hall]).
path(warehouseB, hall, [workshop, hall]).

path(hall, construction, [construction]).
path(workshop, construction, [hall, construction]).
path(warehouseA, construction, [workshop, hall, construction]).
path(warehouseB, construction, [workshop, hall, construction]).

path(hall, workshop, [warehouseA, workshop]).
path(construction, workshop, [hall, warehouseA, workshop]).
path(warehouseA, workshop, [hall, workshop]).
path(warehouseB, workshop, [hall, workshop]).

warehouses(warehouseA).
warehouses(warehouseB).

!complete_tasks.

+!complete_tasks : carry(_)
    <- !go_to(hall);
       drop;
       go(construction);
       !complete_tasks.

+!complete_tasks : not(carry(_))
    & not(location(construction))
    <- !go_to(construction);
       !complete_tasks.

+!complete_tasks : not(carry(_))
    & location(construction)
    & task(Machine2, Material2)
    <- !complete_task(Machine2, Material2);
       !complete_tasks.

+!complete_tasks
    <- .println("Apparently no task available");
       !complete_tasks.

+!complete_task(Machine, Material) : not(carry(_))
    <- !go_to(hall);
       ?product(Machine, Material, _);
       pick(Machine, Material);
       go(construction);
       submit.

+!complete_task(Machine, Material) : not(carry(_))
    <- !go_to(warehouseA);
       ?resource(warehouseA, Material, _);
       pick(Material);
       go(workshop);
       !make_product(Machine, Material);
       go(hall);
       go(construction);
       submit.

+!complete_task(Machine, Material) : not(carry(_))
    <- !go_to(warehouseB);
       ?resource(warehouseB, Material, _);
       pick(Material);
       go(workshop);
       !make_product(Machine, Material);
       go(hall);
       go(construction);
       submit.

+!complete_task(_, _) : carry(product(_, _))
    <- .println("I'm carrying something right now").

+!make_product(Machine, Material) :
    machine(Machine, true)
    <- do(Machine, Material);
       .println("Product manufactured").

+!do_walk([]) : true <- true.

+!do_walk([Location | Locations]) : true <-
   go(Location);
   !do_walk(Locations).

+!go_to(Location) : location(Location) <- true.

+!go_to(Location) : not(location(Location))
    & location(Location_Now)
    <- ?path(Location_Now, Location, Path);
       !do_walk(Path).
