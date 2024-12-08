#r "nuget:DIKU.Canvas"
#load "asteroids.fs"
open Canvas
open Color
open System
open Asteroids
open Asteroids.Asteroids
open Asteroids.Vectors
open Asteroids.RandomGenerator



//Canvas Size
let width, height = 800, 600

//The initial state of the light and the state change
let initialState = false

//Auxiliary functions for Moth-class
//let lightPosition = (float width / 2.0, (float height / 3.0) + 40.0)

let donutCoordinate (coord: float) (maxVal: float) =
    if coord < 0.0 then maxVal + coord
    elif coord > maxVal then coord - maxVal
    else coord

//Moth-class
type Moth (pos: Vec, hdng: float) =
    let mutable _hdng = hdng
    let mutable _pos = pos
    new () = 
        let randomX = float (rnd.Next(0, width))
        let randomY = float (rnd.Next(0, height))
        let randomHeading = rnd.NextDouble() * 360.0
        Moth((randomX, randomY), randomHeading)
    member this.hdng = _hdng
    member this.pos = _pos
    member this.draw (): PrimitiveTree =
      let (x,y) = _pos
      filledEllipse lightGray 5 5 
      |> translate x y
    member this.move (lightOn: bool) = 
        _hdng <- _hdng + (rnd.NextDouble() - 0.5) * 10.0
        // Konverter retning til vektorer
        let dx = Math.Cos(_hdng * Math.PI / 180.0) * 2.0
        let dy = Math.Sin(_hdng * Math.PI / 180.0) * 2.0
        // Hold møllen inden for lærredet
        _pos <- (donutCoordinate ((_pos |> fst) + dx) (float width),
                donutCoordinate ((_pos |> snd) + dy) (float height))
    

//Function generating a list of moths
let generateMoths amount : Moth list =
    [ for i in 1..amount -> Moth() ]
let moths = generateMoths 5

//Function easing the usage of the onto function - taken from miniGame.fsx on DIKU-Canvas GitHub
let (++) p1 p2 = onto p1 p2

//Function that separates the moths in the Moth list and draws them via the this.draw member
let drawMoths (moths: Moth list) =
    moths
    |> List.fold (fun acc moth -> acc ++ (moth.draw())) emptyTree

//function that makes the moths move
let moveMoths (moths: Moth list) (lightOn: bool) =
    moths |> List.iter (fun moth -> moth.move(lightOn))


//Light design elements
let lampCoord =
    filledRectangle darkGrey 2.0 (float height / 3.0)
    |> translate (float width / 2.0) 0.0
let lampShade = 
    filledPolygon darkGray [(-80.0,40.0);(-40.0,0.0);(40.0,0.0);(80.0, 40.0)]
    |> translate (float width / 2.0) (float height / 3.0)
let lightBulbOff = 
    filledEllipse whiteSmoke 25.0 25.0
    |> translate (float width / 2.0) ((float height / 3.0)+40.0)
let lightBulbOn =
    filledEllipse yellow 25.0 25.0
    |> translate (float width / 2.0) ((float height / 3.0)+40.0)
let lightEffect =
    let effectColor = fromRgba 235 186 52 100
    filledEllipse effectColor 40.0 40.0
    |> translate (float width / 2.0) ((float height / 3.0)+40.0)
let uplitWall = 
    let litUp = filledRectangle (fromRgba 254 207 2 160) width height
    let shadows = filledPolygon (fromRgb 33 27 22) [(-(float width / 2.0),-float height/3.0);(-float width / 2.0,float height * 2.0/3.0);(-80.0,40.0);(80.0,40.0);(float width/2.0, float height * 2.0/3.0);((float width / 2.0),-float height/3.0)] 
                    |> translate (float width / 2.0) (float height / 3.0)
    shadows ++ litUp

//Function that draws the light and turns it on/off
let drawLight (state: bool) = 
  if state then
      lampCoord ++ lampShade ++ lightEffect ++ lightBulbOn ++ uplitWall 
  else 
    //   lampCoord ++ lampShade ++ lightBulbOff
    emptyTree

//Draw function that collects the different object drawn above into one
let draw (state: bool) =
    (drawMoths moths) ++ (drawLight state) |> make

//React function, considers intercations in the canvas
let react (state: bool) (ev: Event) : bool option =
    match ev with
    | Event.TimerTick -> 
        moveMoths moths state // Opdater møllernes position ved hvert tick
        Some state
    | Event.Key k ->
        match Key k with
        | Key ' ' -> // Handle space as the toggle for the light
            Some (not state)
        | _ -> Some state
    | _ -> Some state




//Simulation, the unknown function makes sure, the draw function takes the initialState and react into consideration
interact "Moths" width height (Some 30) (fun state -> draw state) react initialState


