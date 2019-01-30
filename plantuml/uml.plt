@startuml

skinparam monochrome true

left to right direction

hide circle
hide members

class File
class Module
class Program
class Variable
class SubProgram
class Parameter
class Function
class Subroutine
class Body
class Access
class Invocation

File "*" <-- File : include
File  *-- "*"  Module
File  *-- "*" SubProgram
File  *-- "*" Program

Module "*" <-- Module : use
Module <-- Program : use

Module  *-- "*" Variable
Module  *-- "*" SubProgram
Program *-- "*" Variable

SubProgram <|-- Function
SubProgram <|-- Subroutine

SubProgram  *-- "*" Parameter
SubProgram  *-- "*" Variable

Body  *-- "*" Access
Body  *-- "*" Invocation

SubProgram  *-- "1" Body
Program  *-- "1" Body


@enduml
