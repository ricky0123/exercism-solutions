module LuciansLusciousLasagna

// TODO: define the 'expectedMinutesInOven' binding
let expectedMinutesInOven = 40

// TODO: define the 'remainingMinutesInOven' function
let remainingMinutesInOven t = 40 - t

// TODO: define the 'preparationTimeInMinutes' function
let preparationTimeInMinutes layers = 2 * layers

// TODO: define the 'elapsedTimeInMinutes' function
let elapsedTimeInMinutes layers minutes =
    (preparationTimeInMinutes layers) + minutes
