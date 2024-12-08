//module MothSimulator.fsx
#r "nuget:DIKU.Canvas"
open Canvas
open Color
open System


//initalt værdier

let w,h = 600,400 // The size of the canvas
let rand = Random() // Initialize a random number generator
let num_lines = 3
type line = color * float * (float * float) list // canvas color, line thickness, list of 2 coordinates
type state = int*(line option array)


//Funktionen skal generere randome punkter

let generate_points () = 
    let mutible x1 = 300
    let mutible y1 = 400 
    (white,(rand.NextDouble()+0.1)*10.0,
    [(x1, y1)])


//Move member funktion der skal tilføje
let move_member() pointx pointy= 
    


//Draw picturet

let draw (s:state): Picture = 
    let addLine acc elm =
        match elm with
            Some (col,sw,lst) -> onto (ellipse white 3.0 5 5) acc
            | _ -> acc
    let (_, lines) = s
    Array.fold addLine emptyTree lines |> make



//Funktionen skal opdatere hver punkt med en random værdi og tilføje det til punktet

let react (s:state) (ev:Event) : state option =
    let idx, lines = s;
    match ev with
        | Event.TimerTick -> 
            let nextIdx = (idx+1)%lines.Length
            lines[nextIdx] <- Some (randomLine())
            Some (nextIdx, lines)
        | _ -> None // all other events are ignored

// Render the picture to the screen
let initialState = (-1, Array.init num_lines (fun _ -> None)) // First state drawn by draw
let delayTime = (Some 100) // microseconds (as an option type)
interact "animate" w h delayTime draw react initialState
