# EEBot Maze Navigation

An assembly language program developed for the HCS12 microcontroller to enable autonomous maze navigation using the EEBot platform.

---

## 🚀 Features

- **Sensor Calibration:** Utilizes calibrated sensors to detect obstacles and lane markings, allowing precise autonomous navigation.
- **Stack-Based Path Tracking:** Implements a stack structure to record turns at intersections for efficient backtracking and rerouting when dead ends are encountered.
- **State Machine Control:** Employs a state machine to manage navigation logic and real-time decision-making within the maze.
- **LCD Interfacing:** Displays current navigation states and debugging info directly on an LCD screen.
- **Robust Debugging:** Extensive use of breakpoints to ensure stable and reliable navigation performance.

---

## 💻 Technical Details

- Written entirely in Assembly for the HCS12 microcontroller.
- Manages sensor input and motor control for smooth autonomous movement.
- Handles complex maze navigation logic with minimal hardware resources.
- Integrates with LCD module for real-time status output.

---

## 🛠️ Usage

- Load the program onto the HCS12 microcontroller controlling the EEBot.
- Ensure sensors and LCD are properly connected and calibrated.
- Run the robot in a maze environment for autonomous navigation.
- Use the LCD display for monitoring current state and troubleshooting.

---
