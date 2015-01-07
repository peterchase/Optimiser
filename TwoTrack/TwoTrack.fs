namespace TwoTrack

/// Represents a value with two "tracks": success and failure
type Result<'TSuccess, 'TFailure> = 
    | Success of 'TSuccess
    | Failure of 'TFailure

module Binding =

    /// Turns a function with a simple input, but two-track output, into one with two-track input and output
    let bindTwoTrack (switchFunction: 'TIn -> Result<'TSuccessOut, 'TFailure>) twoTrackInput = 
        match twoTrackInput with
        | Success s -> switchFunction s
        | Failure f -> Failure f

    /// Turns a function with a simple input and simple output, into one with with two-track input and output
    let bindSimple (simpleFunction: 'TIn -> 'TSuccessOut) twoTrackInput = 
        match twoTrackInput with
        | Success s -> Success (simpleFunction s)
        | Failure f -> Failure f
