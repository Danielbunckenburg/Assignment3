#r "nuget:DIKU.Canvas"
#load "asteroids.fs"


open Canvas
open Color
open System
open Asteroids
open Asteroids.Asteroids
open Asteroids.Vectors
open Asteroids.RandomGenerator

// The initial state of the light
let initialState = false

// Coordinates function to wrap around edges
let coordinates (koordinates: float) (maxValue: float) =
    if koordinates < 0.0 then maxValue + koordinates
    elif koordinates > maxValue then koordinates - maxValue
    else koordinates

// Size of the window
let w, h = 800, 600

// Light position
let lightPos = (float w / 2.0, (float h / 3.0) + 40.0)

// Moth type definition
type Moth(pos: Vec, hdng: float) =
    let mutable pos2 = pos
    let mutable hdng2 = hdng

    new () =
        let randomX = float (rnd.Next(0, w))
        let randomY = float (rnd.Next(0, h))
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
            // Calculate vector to light
            let (lx, ly) = lightPos
            let (x, y) = pos2
            let dx = lx - x
            let dy = ly - y
            let dist = Math.Sqrt(dx * dx + dy * dy)
            // Normalize and move towards light
            let normDx = dx / dist
            let normDy = dy / dist
            pos2 <- (coordinates (x + normDx * 2.0) (float w),
                    coordinates (y + normDy * 2.0) (float h))
        else
            // Random wandering when light is off
            hdng2 <- hdng2 + (rnd.NextDouble() - 0.5) * 10.0
            let dx = Math.Cos(hdng2 * Math.PI / 180.0) * 2.0
            let dy = Math.Sin(hdng2 * Math.PI / 180.0) * 2.0
            let (x, y) = pos2
            pos2 <- (coordinates (x + dx) (float w),
                    coordinates (y + dy) (float h))


// Function generating a list of moths
let makeMoths amount: Moth list =
    [ for i in 1..amount -> Moth() ]

let moths = makeMoths 100

// Function easing the usage of the onto function - taken from miniGame.fsx on DIKU-Canvas GitHub
let (++) p1 p2 = onto p1 p2

// Function that separates the moths in the Moth list and draws them via the this.draw member
let drawMoths (moths: Moth list) =
    moths
    |> List.fold (fun acc moth -> acc ++ (moth.draw())) emptyTree

// Function that makes the moths move
let moveMoths (moths: Moth list) (lightOn: bool) =
    moths |> List.iter (fun moth -> moth.move(lightOn))

// Light design elements
let lightBulbOn =
    filledEllipse white 25.0 25.0
    |> translate (fst lightPos) (snd lightPos)

// Function that draws the light and turns it on/off
let drawLight (state: bool) = 
    if state then
        lightBulbOn
    else 
        emptyTree

// Draw function that collects the different objects drawn above into one
let draw (state: bool) =
    (drawMoths moths) ++ (drawLight state) |> make

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
interact "Moths" w h (Some 30) (fun state -> draw state) react initialState



