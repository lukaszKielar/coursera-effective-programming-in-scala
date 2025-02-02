# Fireworks

As a first assignment, we want to celebrate the beginning of your journey into Scala with fireworks!

![pic](./fireworks.gif)

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

## Fill in the Blanks

As you can see, the project is already partially implemented. However, there are still unimplemented parts.

In Scala, we use the symbol ??? as a placeholder for an unimplemented part. So, what you have to do is to read the file Fireworks.scala, and replace the occurrences of ??? with proper implementations. Note: don’t change the existing definitions, only replace the ??? with proper expressions. If you change the signature of a method, the grader might not work anymore and you will fail the assignment.

## Run the Tests

At any point, you can run the tests to follow your progress.

With IntelliJ, click on the green arrow in the left margin, in front of the class definition FireworksSuite, in the file src/test/scala/fireworks/FireworksSuite.scala.

With Metals and VS Code, open the file src/test/scala/fireworks/FireworksSuite.scala and click “test” just above the class definition FireworksSuite.

## Run the Program

Eventually, when you have finished implementing the project and all the tests pass, run it and enjoy the show!

With IntelliJ, click on the green arrow in the left margin, in front of the definition @main def main(): Unit, in the file src/main/scala/fireworks/Fireworks.scala, and click on “Run 'Main'”.

With Metals and VS Code, open the file src/main/scala/firework/FireworksRunner.scala and click “run” above the definition @main def main(): Unit.
