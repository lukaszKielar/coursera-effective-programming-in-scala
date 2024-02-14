# Fireworks

As a first assignment, we want to celebrate the beginning of your journey into Scala with fireworks!

Your task consists of implementing the trajectory of fireworks particles.

## Overview

In this assignment, you will implement a simple model for simulating fireworks.

A firework can be in one of the following phases (in the following order):

- Waiting to be launched: the firework is not yet visible,
- Launched: we see one particle going upwards,
- Exploding: we see dozens of particles moving in an explosion,
- Done: the particles have all burnt.

We model this in Scala with a sealed trait Firework, which is extended by the case classes Waiting, Launched, and Exploding and the case object Done.

The classes that model each phase of the firework have a method next that computes the next state of a firework. Your task is to implement them so that at the end of the assignment you can appreciate the show!
