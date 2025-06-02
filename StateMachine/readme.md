# Finite State Machine Simulator in C

A console-based C program simulating a finite state machine (FSM) with 8 states (A–H) and two input commands (0 and 1). It supports dynamic state transitions, state deletion, garbage state detection, and configuration updates—all via simple text commands.

---

## 🔍 Features

- Navigate FSM by inputting `0` or `1` to move to the next state.
- Modify transitions dynamically using commands.
- Print the current state transition table.
- Detect and display “garbage” states unreachable from the current state.
- Delete individual states or all garbage states.
- Interactive command-line interface for FSM management.

---

## 🛠️ How to Use

1. **Compile the program:**

   ```bash
   gcc fsm.c -o fsm
