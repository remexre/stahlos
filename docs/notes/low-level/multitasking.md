Multitasking
============

Currently, StahlOS is designed to allow multitasking on a single CPU. Eventually this will probably be extended to support SMP systems, but for now, this isn't an immediate priority.

The overall model is inspired by the Tokio model. A global bitset is maintained, mapping process IDs to readiness, where an unset bit corresponds to a blocked process and vice versa. When a process executes a call that cannot be completed immediately, the process' corresponding bit is unset. An action to register the queued work should then be performed (e.g. putting an IO request in a queue). Once the work actually occurs (e.g. in response to an interrupt), the process' bit is set. Typically once work is enqueued, the process yields. (The Stahl compiler may also insert yields, to prevent a CPU-heavy function from impacting system responsivity.)

On yield, the scheduler finds the next set bit in the bitset, wrapping around. If none could be found, puts the CPU into an idle state, and resumes searching the bitset once an interrupt is received.
