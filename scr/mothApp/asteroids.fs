namespace Asteroids

open System

module Vectors =
    type Vec = float*float

    let add ((v1x,v1y) : Vec) ((v2x,v2y) : Vec) : Vec = (v1x + v2x, v1y + v2y)

    let sub ((v1x,v1y) : Vec) ((v2x,v2y) : Vec) : Vec = (v1x - v2x, v1y - v2y)

    let scale ((vx,vy) : Vec) (c : float) : Vec =
        (c * vx, vy * c)

    let div ((vx,vy) : Vec) (c : float) : Vec =
        if c = 0 then (infinity, infinity)
        else (vx / c, vy / c)

    let rot ((vx,vy) : Vec) (r : float) : Vec =
        (vx * cos(r) - vy * sin(r), vx * sin(r) + vy * cos(r))

    let dist (v1 : Vec) (v2 : Vec) : float =
        sub v1 v2 ||> fun x y -> let (dx,dy) = (x, y) in sqrt (dx*dx + dy*dy)

    let len (v : Vec) : float = dist v (0, 0)

    let norm (v : Vec) : Vec = div v (len v)

    let ang ((vx, vy) : Vec) : float = atan2 vy vx

module RandomGenerator =
    let rnd = new System.Random()

    let GetRandomNum (n:float) = n*rnd.NextDouble ()
    
    let GetRandomRange (n1:float) (n2:float) = n1 + GetRandomNum (n2-n1)
    
    let GetRandomPosition (n11:float) (n12:float) (n21:float) (n22:float) : Vectors.Vec = (GetRandomRange n11 n12, GetRandomRange n21 n22)
    
    let GetRandomUnitVector() : Vectors.Vec =
        Vectors.norm (GetRandomPosition -1.0 1.0 -1.0 1.0)

module Asteroids =
    type Vec = Vectors.Vec

    exception GameOver
    type Kind = SpaceShip | Bullet | Asteroid
    type IComparableWrapper<'T>(x: 'T) = // Dummy comparison extension
        member this.x = x
        interface IComparable with
            member this.CompareTo(obj) = 
                match obj with
                | :? IComparableWrapper<'T> as other ->
                    0 // Replace with actual comparison logic
                | _ -> invalidArg "obj" "objects are not comparable"
        override this.Equals(obj) =
            match obj with
            | :? IComparableWrapper<'T> as other -> 
                true // Replace with actual equality logic
            | _ -> false
        override this.GetHashCode() =
            0 // Replace with actual hash code logic
    // The data is stored as a tuple: (k,t,p,o,h,s,a,r,age) denoting:
    //   kind, representation, position, orientation, heading, speed, acceleration, collisionRadius, and age
    // State is implemented as a Set, which requires its elements to be comparable. Since State
    // is intented to be of Object<'T> and 'T can be anything, then we ensure compatibility with Set
    // by wrapping it in a dummy comparable class.
    type Object<'T> = Kind*IComparableWrapper<'T>*Vec*float*float*float*float*float*float
    type State<'T when 'T: comparison> = Set<'T> // spaceship, bullets, asteroids

    let maxSpeed = 15.0 // Ship can fly fast
    let maxBulletSpeed = 20.0 // Bullets can fly faster

    let cyclic (vMin:float) (vMax:float) (v:float) : float =
        let rem = (v-vMin) % (vMax-vMin)
        if rem < 0 then vMax + rem
        else vMin + rem

    let makeObject (k:Kind) (s:'T) (r:float): Object<'T> = (k, IComparableWrapper<'T>(s), (0.0, 0.0), 0.0, 0.0, 0.0, 0.0, r, 0.0)

    let kind ((k,t,p,o,h,s,a,r,age): Object<'T>): Kind = k

    let setKind (k: Kind) ((_,t,p,o,h,s,a,r,age): Object<'T>) : Object<'T> = (k,t,p,o,h,s,a,r,age)

    let shape ((k,t,p,o,h,s,a,r,age):Object<'T>): 'T =  t.x

    let setShape (t:'T) ((k,_,p,o,h,s,a,r,age): Object<'T>): Object<'T> = (k,IComparableWrapper<'T>(t),p,o,h,s,a,r,age)

    let position ((k,t,p,o,h,s,a,r,age): Object<'T>): Vec =  p

    let setPosition (p:Vec) ((k,t,_,o,h,s,a,r,age): Object<'T>): Object<'T> = (k,t,p,o,h,s,a,r,age)

    let orientation ((k,t,p,o,h,s,a,r,age): Object<'T>): float =  o

    let setOrientation (o:float) ((k,t,p,_,h,s,a,r,age): Object<'T>): Object<'T> = (k,t,p,o,h,s,a,r,age)

    let heading ((k,t,p,o,h,s,a,r,age): Object<'T>): float =  h

    let setHeading (h:float) ((k,t,p,o,_,s,a,r,age): Object<'T>): Object<'T> = (k,t,p,o,h,s,a,r,age)

    let speed ((k,t,p,o,h,s,a,r,age): Object<'T>): float =  s

    let setSpeed (s:float) ((k,t,p,o,h,_,a,r,age): Object<'T>): Object<'T> = (k,t,p,o,h,s,a,r,age)

    let acceleration ((k,t,p,o,h,s,a,r,age): Object<'T>): float =  a

    let setAcceleration (a:float) ((k,t,p,o,h,s,_,r,age): Object<'T>): Object<'T> = (k,t,p,o,h,s,a,r,age)

    let radius ((k,t,p,o,h,s,a,r,age): Object<'T>): float =  r

    let setRadius (r:float) ((k,t,p,o,h,s,a,_,age): Object<'T>): Object<'T> = (k,t,p,o,h,s,a,r,age)

    let age ((k,t,p,o,h,s,a,r,age): Object<'T>): float =  age

    let setAge (age:float) ((k,t,p,o,h,s,a,r,_): Object<'T>): Object<'T> = (k,t,p,o,h,s,a,r,age)

    let updateAge (dAge:float) (obj: Object<'T>): Object<'T> = 
        obj |> setAge (dAge + age obj)

    let isAlive (obj: Object<'T>): bool = radius obj > 0.0

    let accelerate (da:float) (obj: Object<'T>): Object<'T> =
        let a = acceleration obj
        obj |> setAcceleration (max 0.0 (min 5.0 (a+da)))

    let rotate (dOrient:float) (obj: Object<'T>): Object<'T> =
        let orient = orientation obj
        obj |> setOrientation (cyclic 0.0 (2.0*Math.PI) (orient+dOrient))

    // The following functions are restricted to State<Object<'T>> and not the more general State<'T>
    let partitionShip (state:State<Object<'T>>): (Object<'T> option)*State<Object<'T>> =
        let ships, rest = state |> Set.partition (fun o -> SpaceShip = kind o)
        let ship = ships |> Set.toList |> List.tryHead
        (ship,rest)

    let asteroidsExist (state:State<Object<'T>>): bool =
        Set.exists (fun a -> Asteroid = kind a) state

    let move (dt:float) (xLim:float*float) (yLim:float*float) (state: State<Object<'T>>) : State<Object<'T>> =
        let updateStep (dt : float)  (obj:Object<'T>) : Object<'T> =
            let p = position obj
            let s = speed obj
            let a = acceleration obj
            let o = orientation obj
            let h = heading obj
            let tmpV = Vectors.add (s*cos h, s*sin h) (a*cos o, a*sin o)
            let newH = Vectors.ang tmpV |> cyclic 0.0 (2.0*Math.PI)
            let maxS = if Bullet = kind obj then maxBulletSpeed else maxSpeed
            let newS = min maxS (s+dt*a)
            let newP = 
                let (x,y) = Vectors.add p (Vectors.scale (s*cos newH,s*sin newH) dt)
                (cyclic (fst xLim) (snd xLim) x, cyclic (fst yLim) (snd yLim) y)
            obj |> setPosition newP |> setSpeed newS |> setAcceleration 0.0 |> setHeading newH |> updateAge dt
        Set.map (updateStep dt) state

    let detectCollisions (state: State<Object<'T>>) : (List<Object<'T>*Object<'T>>*State<Object<'T>>) =
        let rec makePairs lst =
            match lst with
                a::rst -> List.map (fun b -> (a,b)) rst @ makePairs rst
                | _ -> []
        let collidedPairsLst =  // Collect pairs of objects that have collided
            state
            |> Set.toList
            |> makePairs
            |> List.filter (fun (a,b) -> 0.0 >= (Vectors.dist (position a) (position b)) - (radius a) - (radius b))
        let notCollided =  // Gather unique set of elements of colleded objects
            collidedPairsLst 
            |> List.collect (fun (a,b) -> [a;b])
            |> Set.ofList 
            |> Set.difference state
        (collidedPairsLst, notCollided)
