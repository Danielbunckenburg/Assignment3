#r "nuget:DIKU.Canvas"
#load "../asteroids/asteroids.fs"

open Canvas
open Color
open System
open Asteroids
open Asteroids.RandomGenerator

// The initial state of the light
let initialState = false


// Coordinates function to wrap around edges
let coordinates (value: float) (maxValue: float) =
    if value < 0.0 then maxValue + value
    elif value > maxValue then value - maxValue
    else value

// Size of the window
let w, h = 800.0, 600.0

// Light position
let lightPos: Vectors.Vec = (w / 2.0, h / 3.0 + 40.0)

type Moth(pos: Vectors.Vec, hdng: float) =
    let mutable pos2 = pos
    let mutable hdng2 = hdng

    new() =
        let randomX = float (rnd.Next(0, int w))
        let randomY = float (rnd.Next(0, int h))
        let randomHeading = rnd.NextDouble() * 360.0
        Moth((randomX, randomY), randomHeading)

    member this.pos = pos2
    member this.hdng = hdng2

    member this.draw(): PrimitiveTree =
        let (x, y) = pos2
        filledEllipse lightGray 5 5
        |> translate x y


    member this.move(lightOn: bool) =
        if lightOn then
            // Calculate vector to light and normalize
            let direction = Vectors.sub lightPos pos2 |> Vectors.norm
            pos2 <- Vectors.add pos2 (Vectors.scale direction 2.0)
            pos2 <- (coordinates (fst pos2) w, coordinates (snd pos2) h)
        else
            // Random wandering when light is off
            hdng2 <- hdng2 + (rnd.NextDouble() - 0.5) * 10.0
            let movement = Vectors.rot (1.0, 0.0) (hdng2 * Math.PI / 180.0)
            let movementScaled = Vectors.scale movement 2.0
            pos2 <- Vectors.add pos2 movementScaled
            pos2 <- (coordinates (fst pos2) w, coordinates (snd pos2) h)




// Function generating a list of moths
let makeMoths amount: Moth list =
    [ for i in 1..amount -> Moth() ]

let moths = makeMoths 5


// Function that separates the moths in the Moth list and draws them via the this.draw member
let drawMoths (moths: Moth list) =
    moths
    |> List.fold (fun acc moth ->onto  acc  (moth.draw())) emptyTree

// Function that makes the moths move
let moveMoths (moths: Moth list) (lightOn: bool) =
    moths |> List.iter (fun moth -> moth.move(lightOn))


// Light design
let lightBulb =
    filledEllipse white 25.0 25.0
    |> translate (fst lightPos) (snd lightPos)

// Function of light 
let drawLight (state: bool) = 
    if state then
        lightBulb
    else 
        emptyTree

// Draw function that collects the different objects drawn above into one
let draw (state: bool) :Picture =
    onto (drawMoths moths)  (drawLight state) |> make



// React function, considers interactions in the canvas
let react (state: bool) (event: Event): bool option =
    match event with
    | Event.TimerTick -> 
        moveMoths moths state // Update moths' positions at each tick
        Some state 
    | Event.Key k ->
        match Key k with
        | Key ' ' -> // Handle space as the toggle for the light
            Some (not state)
        | _ -> Some state
    | _ -> Some state

// Simulation, the unknown function makes sure the draw function takes the initialState and react into consideration
interact "Moths" (int w) (int h) (Some 20) (fun state -> draw state) react initialState

