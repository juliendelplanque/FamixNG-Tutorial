---
title: "Bind two metamodels"
author: Benoît Verhaeghe
date: \today
theme: metropolis
header-includes:
    - \metroset{block=fill}
---

## Why

- Extend a metamodel
  - AST →  AST Java
- Link similar entities between two metamodels
  - FAMIX Outgoing invocation → FAST Method Invocation
  - A Pharo Method in our presentation → a real Pharo method

## How

Two possibilities

- Create a metamodel linked to another one
- Create two metamodels then linked them with a third one

# Moose

## Moose - Define metamodel

Almost the same as for one metamodel

***BUT***

- The remote entity are defined with `remoteEntity: withPrefix:`
- The class side of the generator should implement `submetamodels` witch return the collection of the remote metamodels.

::: {.thinkReset}

# Warning

Do not forget to reset all the metamodels

:::

# Example -- Our presentation

## New Metamodel

![meta-binding](./img/presentation-meta-binding.pdf){height=400px}

## What do we need

1. A new class named: 'SelectorReference'
2. A relations between 'Slide' and 'SelectorReference'
3. A relations between 'FamixStMethod' and 'SelectorReference'

## Example - Define entities

```st
defineClasses
  super defineClasses.
  slide := self remoteEntity: #Slide withPrefix: #'Fmx'.
  selectorReference :=
    builder newClassNamed: #SelectorReference.
  famixStMethod :=
    self remoteEntity: #Method withPrefix: #'FamixSt'
```

::: block

### Define remote entity

'`#remoteEntity: anEntityName withPrefix: aPrefixName`' computes the entity 'anEntityName' of the model prefixed by 'aPrefixedName'. 

:::

## Example - Define the binding

```st
defineRelations
  super defineRelations.
  (slide property: #outgoingReferences) -*
    (selectorReference property: #source).
  (selectorReference property: #candidates) *-*
    (famixStMethod property: #incomingReferences).
```

## Example - Define the submodels

```st
submetamodels
 ^ { FmxNGSlidesGenerator.
     FamixPharoSmalltalkGenerator }
```

## Example - Reset the metamodels

```st
FmxNGSlidesGenerator resetMetamodel.
FamixPharoSmalltalkGenerator resetMetamodel.
FamixNGSlidesSmalltalkGenerator resetMetamodel.
```

## So

1. Create new metamodel generator
2. Define the submetamodels
3. Define the remote entities
4. Define the new entities (optional)
5. Define the new relations
6. Generate and resetMetamodel
