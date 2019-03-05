---
title: "Bind two metamodels"
author: Benoît Verhaeghe
date: \today
theme: metropolis
header-includes:
    - \metroset{block=fill}
---

## Introduction

**Objectives**

- Create two linked metamodels
- Extend a metamodel

**Resources**

- [https://github.com/SquareBracketAssociates/Booklet-FamixNG](https://github.com/SquareBracketAssociates/Booklet-FamixNG) (in progress)
- [https://github.com/moostetechnology/FAST-Java](https://github.com/moostetechnology/FAST-Java)

# The presentation case

## Let's get back to the past

In the previous presentation

![meta](./img/presentation-meta.pdf)

## Possible link

- Which Pharo method are referenced by the presentation?
- How represent those links?

## Adding reference to a method

![meta-binding](./img/presentation-meta-binding.pdf){height=250px}

## What do we need

1. A new entity named: 'SelectorReference'
2. A reference to the entity 'Slide'
3. A reference to the entity 'FamixStMethod'
4. A relations between 'Slide' and 'SelectorReference'
5. A relations between 'FamixStMethod' and 'SelectorReference'

## How to do it?

Almost the same as for one metamodel

***BUT***

- The remote entity are defined with `remoteEntity: withPrefix:`
- The class side of the generator should implement `submetamodels` witch return the collection of the remote metamodels.

::: block

### Define remote entity

'`#remoteEntity: anEntityName withPrefix: aPrefixName`' computes the entity 'anEntityName' of the model prefixed by 'aPrefixedName'.

:::

# Le's do it

## Define generator

```st
FamixMetamodelGenerator subclass: #MyGenerator
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'MyPackage-Generator'
```

## Define the submetamodels

```st
MyGenerator class>>#submetamodels
 ^ { FmxNGSlidesGenerator.
     FamixPharoSmalltalkGenerator }
```

## Define entities

- New entity: 'SelectorReference'
- Reference to 'Slide'
- Reference to 'FamixStMethod'

```st
defineClasses
  super defineClasses.
  selectorReference :=
    builder newClassNamed: #SelectorReference.

  slide := self remoteEntity: #Slide withPrefix: #'Fmx'.

  famixStMethod :=
    self remoteEntity: #Method withPrefix: #'FamixSt'
```

## Define the binding

- Relation between 'Slide' and 'SelectorReference'
- Relation between 'SelectorReference' and 'FamixStMethod'

```st
defineRelations
  super defineRelations.
  
  (slide property: #outgoingReferences) -*
    (selectorReference property: #source).

  (selectorReference property: #candidates) *-*
    (famixStMethod property: #incomingReferences).
```

## Reset the metamodels

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
