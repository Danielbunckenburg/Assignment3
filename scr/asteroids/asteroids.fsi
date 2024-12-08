namespace Asteroids

open System

/// <summary>
/// A module containing functions and types for performing operations on 2D vectors.
/// </summary>
module Vectors =
    /// <summary>
    /// Defines a 2D vector as a tuple of two floating-point numbers (x, y).
    /// </summary>
    type Vec = float * float

    /// <summary>
    /// Adds two 2D vectors component-wise.
    /// </summary>
    /// <param name="v1">The first vector (v1x, v1y).</param>
    /// <param name="v2">The second vector (v2x, v2y).</param>
    /// <returns>A new vector that is the sum of v1 and v2.</returns>
    val add: v1: float * float -> v2 : float * float -> Vec

    /// <summary>
    /// Subtracts the second 2D vector from the first vector component-wise.
    /// </summary>
    /// <param name="v1">The first vector (v1x, v1y).</param>
    /// <param name="v2">The second vector (v2x, v2y).</param>
    /// <returns>A new vector that is the difference between v1 and v2.</returns>
    val sub: v1: float * float -> v2 : float * float -> Vec

    /// <summary>
    /// Scales a 2D vector by a given constant.
    /// </summary>
    /// <param name="v">The vector (vx, vy) to scale.</param>
    /// <param name="c">The constant to scale the vector by.</param>
    /// <returns>A new vector that is the result of scaling v by c.</returns>
    val scale: v : float * float -> c: float -> Vec

    /// <summary>
    /// Divides a 2D vector by a given constant.
    /// </summary>
    /// <param name="v">The vector (vx, vy) to divide.</param>
    /// <param name="c">The constant to divide the vector by.</param>
    /// <returns>
    /// A new vector that is the result of dividing v by c. 
    /// If c is zero, returns (infinity, infinity).
    /// </returns>
    val div: v: float * float -> c: float -> Vec


    /// <summary>
    /// Rotates a 2D vector by a given angle in radians.
    /// </summary>
    /// <param name="v">The vector (vx, vy) to rotate.</param>
    /// <param name="r">The angle in radians to rotate the vector.</param>
    /// <returns>A new vector that is the result of rotating v by r radians.</returns>
    val rot: v : float * float -> r: float -> Vec

    /// <summary>
    /// Calculates the Euclidean distance between two 2D vectors.
    /// </summary>
    /// <param name="v1">The first vector (v1x, v1y).</param>
    /// <param name="v2">The second vector (v2x, v2y).</param>
    /// <returns>The distance between v1 and v2.</returns>
    val dist: v1 : float * float -> v2 : float * float -> float

    /// <summary>
    /// Calculates the length (magnitude) of a 2D vector.
    /// </summary>
    /// <param name="v">The vector (vx, vy) whose length is to be calculated.</param>
    /// <returns>The length of the vector.</returns>
    val len: v : float * float -> float

    /// <summary>
    /// Normalizes a 2D vector to have a length of 1.
    /// </summary>
    /// <param name="v">The vector (vx, vy) to normalize.</param>
    /// <returns>
    /// A new vector that is the normalized version of v.
    /// If the length of v is zero, returns (infinity, infinity).
    /// </returns>
    val norm: v : float * float -> Vec

    /// <summary>
    /// Calculates the angle in radians of a 2D vector from the positive x-axis.
    /// </summary>
    /// <param name="v">The vector (vx, vy) whose angle is to be calculated.</param>
    /// <returns>The angle in radians between the vector and the positive x-axis.</returns>
    val ang: v : float * float -> float

/// <summary>
/// A module for generating random numbers and vectors for use in the Asteroids game.
/// </summary>
module RandomGenerator =
    /// <summary>
    /// Generates a random number between 0 and n.
    /// </summary>
    /// <param name="n">The upper bound for the random number.</param>
    /// <returns>A random float between 0 and n.</returns>
    val GetRandomNum: n: float -> float

    /// <summary>
    /// Generates a random number within a specified range.
    /// </summary>
    /// <param name="n1">The lower bound of the range.</param>
    /// <param name="n2">The upper bound of the range.</param>
    /// <returns>A random float between n1 and n2.</returns>
    val GetRandomRange: n1: float -> n2: float -> float

    /// <summary>
    /// Generates a random position within the specified x and y bounds.
    /// </summary>
    /// <param name="n11">The lower bound for the x-coordinate.</param>
    /// <param name="n12">The upper bound for the x-coordinate.</param>
    /// <param name="n21">The lower bound for the y-coordinate.</param>
    /// <param name="n22">The upper bound for the y-coordinate.</param>
    /// <returns>A random vector (x, y) within the specified bounds.</returns>
    val GetRandomPosition: n11: float -> n12: float -> n21: float -> n22: float -> Vectors.Vec

    /// <summary>
    /// Generates a random unit vector.
    /// </summary>
    /// <returns>A random unit vector with a length of 1.</returns>
    val GetRandomUnitVector: unit -> Vectors.Vec


/// <summary>
/// A module for handling game objects and operations in the Asteroids game.
/// </summary>
module Asteroids =
    /// <summary>
    /// Alias for a 2D vector type from the Vectors module.
    /// </summary>
    type Vec = Vectors.Vec

    /// <summary>
    /// Exception that indicates the game is over.
    /// </summary>
    exception GameOver

    /// <summary>
    /// Represents the different kinds of objects in the game.
    /// </summary>
    type Kind = SpaceShip | Bullet | Asteroid

    type IComparableWrapper<'T> =
        interface System.IComparable
        new: x: 'T -> IComparableWrapper<'T>
        override Equals: obj: obj -> bool
        override GetHashCode: unit -> int
        member x: 'T

    /// <summary>
    /// Represents an object in the game.
    /// </summary>
    /// <typeparam name="'T">The type of the shape associated with the object.</typeparam>
    /// <param name="Kind">The kind of the object (e.g., SpaceShip, Bullet, or Asteroid).</param>
    /// <param name="Shape">The shape of the object, wrapped in an IComparableWrapper.</param>
    /// <param name="Position">The position of the object as a vector.</param>
    /// <param name="Orientation">The orientation of the object in radians.</param>
    /// <param name="Heading">The heading direction of the object in radians.</param>
    /// <param name="Speed">The speed of the object.</param>
    /// <param name="Acceleration">The acceleration of the object.</param>
    /// <param name="Radius">The collision radius of the object.</param>
    /// <param name="Age">The age of the object in time units.</param>
    type Object<'T> = Kind*IComparableWrapper<'T>*Vec*float*float*float*float*float*float

    /// <summary>
    /// Represents the state of the game, consisting of a set of objects.
    /// </summary>
    type State<'T when 'T: comparison> = Set<'T>

    /// <summary>
    /// The maximum speed for bullets.
    /// </summary>
    val maxBulletSpeed: float

    /// <summary>
    /// The maximum speed for objects in the game.
    /// </summary>
    val maxSpeed: float

    /// <summary>
    /// Cycles a value within a given range, wrapping around if necessary.
    /// </summary>
    /// <param name="vMin">The minimum value of the range.</param>
    /// <param name="vMax">The maximum value of the range.</param>
    /// <param name="v">The value to be cycled.</param>
    /// <returns>The cyclically constrained value.</returns>
    val cyclic: vMin: float -> vMax: float -> v: float -> float

    /// <summary>
    /// Creates a new game object.
    /// </summary>
    /// <param name="k">The kind of the object.</param>
    /// <param name="s">The shape of the object.</param>
    /// <param name="r">The collision radius of the object.</param>
    /// <returns>The created game object.</returns>
    val makeObject: k: Kind -> s: 'T -> r: float -> Object<'T>

    /// <summary>Gets the kind of an object.</summary>
    val kind: Object<'T> -> Kind

    /// <summary>Returns a copy of the old with a new kind.</summary>
    val setKind: k: Kind -> Object<'T> -> Object<'T>

    /// <summary>Gets the shape of an object.</summary>
    val shape: Object<'T> -> 'T

    /// <summary>Returns a copy of the old with a new shape.</summary>
    val setShape: t: 'T -> Object<'T> -> Object<'T>

    /// <summary>Gets the position of an object.</summary>
    val position: Object<'T> -> Vec

    /// <summary>Returns a copy of the old with a new position.</summary>
    val setPosition: p: Vec -> Object<'T> -> Object<'T>

    /// <summary>Gets the orientation of an object.</summary>
    val orientation: Object<'T> -> float

    /// <summary>Returns a copy of the old with a new orientation.</summary>
    val setOrientation: o: float -> Object<'T> -> Object<'T>

    /// <summary>Gets the heading of an object.</summary>
    val heading: Object<'T> -> float

    /// <summary>Returns a copy of the old with a new heading.</summary>
    val setHeading: h: float -> Object<'T> -> Object<'T>

    /// <summary>Gets the speed of an object.</summary>
    val speed: Object<'T> -> float

    /// <summary>Returns a copy of the old with a new speed.</summary>
    val setSpeed: s: float -> Object<'T> -> Object<'T>

    /// <summary>Gets the acceleration of an object.</summary>
    val acceleration: Object<'T> -> float

    /// <summary>Returns a copy of the old with a new acceleration.</summary>
    val setAcceleration: a: float -> Object<'T> -> Object<'T>

    /// <summary>Gets the collision radius of an object.</summary>
    val radius: Object<'T> -> float

    /// <summary>SReturns a copy of the old with a new radius.</summary>
    val setRadius: r: float -> Object<'T> -> Object<'T>

    /// <summary>Gets the age of an object.</summary>
    val age: Object<'T> -> float

    /// <summary>Returns a copy of the old with a new age.</summary>
    val setAge: age: float -> Object<'T> -> Object<'T>

    /// <summary>Returns a copy where the age is dAge larger than the old.</summary>
    val updateAge: dAge: float -> o: Object<'T> -> Object<'T>

    /// <summary>Determines whether an object has a postive radius hence alive.</summary>
    val isAlive: Object<'T> -> bool

    /// <summary>Returns a copy where the acceleration is da larger than the old within limits.</summary>
    val accelerate: da: float -> Object<'T> -> Object<'T>

    /// <summary>Returns a copy where the orientation is dOrient larger than the old.</summary>
    val rotate: dOrient: float -> Object<'T> -> Object<'T>

    /// <summary>Partitions the game state into a optional spaceship and the remaining objects.</summary>
    val partitionShip: State<Object<'T>> -> Object<'T> option * State<Object<'T>>

    /// <summary>Checks if any asteroids exist in the game state.</summary>
    val asteroidsExist: State<Object<'T>> -> bool

    /// <summary>
    /// Returns a new state, where all objects have been moved based on their speed, acceleration, orientation, and heading. No collision handling is performed.
    /// </summary>
    /// <param name="dt">The forward step size in time.</param>
    /// <param name="xLim">The limits in the first coordinate direction.</param>
    /// <param name="yLim">The limits in the second coordinate direction.</param>
    /// <returns>The new state.</returns>
    val move: dt: float -> xLim: float*float -> yLim: float*float -> State<Object<'T>> -> State<Object<'T>>

    /// <summary>
    /// Detects collisions between objects in the game state and returns collided pairs and remaining objects.
    /// </summary>
    val detectCollisions: state: State<Object<'T>> -> List<Object<'T> * Object<'T>> * State<Object<'T>>
