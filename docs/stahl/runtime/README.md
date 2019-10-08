Runtime
=======

Execution
---------

Since Stahl does not yet have codata, the runtime system handles the infinite looping required for an infinitely running program. Stahl takes cues from Arduino's language here; the entrypoint is defined by three items in the `main` module:

### `State`

`(the Type State)` describes the state of the program between iterations of the main `loop`.

### `setup`

`(the (-> String PID State) setup)` returns the initial `State` of the program, given the program arguments and PID.

### `loop`

`(the (-> (List MessageIn) State (Option (Pair (List MessageOut) State))) loop)` implements the main body of the program. All messages that were received during the previous execution are passed to the function, as well as the previous state. It returns any new messages to be sent, as well as the new state.
