---
title: "Bind two metamodels"
author: Benoît Verhaeghe
institute: Berger-Levrault
date: \today
theme: metropolis
header-includes:
    - \metroset{block=fill}
---

## Why

- Extend a metamodel
  - AST -->  AST Java
- Link similar entity between two metamodels
  - FAMIX Outgoing invocation --> FAST Method Invocation

## How

Two possibilities

- Create a metamodel linked to another one
- Create two metamodels then linked them with a third one

# Moose

## Moose - Message

::: block

### defineClass

```st
FamixMetamodelGenerator >> #remoteEntity: anEntityName withPrefix: aPrefixName
```

:::

It computes the entity `anEntityName` of the model prefixed by `aPrefixName`.

## Moose - Define metamodel

It is the same as for the definition of a standalone model.
__BUT__

- The remote entity are defined with `remoteEntity: withPrefix:`
- The class side of the generator should implement `submetamodels` witch return the collection of the remote metamodels.

## Moose - Reset

*Do not forget to reset all the metamodels, begin with the submetamodels then the binding one*

# Example

## Example - Binding between FAMIX and FAST

The binding is done with a third metamodels (witch is called Carrefour).

Carrefours defines a binding between FAMIX and FAST.

```st
Insert here the code to create the class
```

## Example - Define entities

```st
defineClasses
    super defineClasses.
    famixMethod := self remoteEntity: #Method withPrefix: #FAMIX.
    fastJavaMethodEntity := self remoteEntity: #JavaMethodEntity withPrefix: #FAST.
```

## Example - Define the binding

```st
defineRelations
    super defineRelations.
    (famixInvocation property: #fast) - (fastJavaMethodInvocation property: #famix).
```

## Example - Define the submodels

```st
submetamodels
    ^ {FASTJavaMetamodelGenerator. FamixCompatibilityGenerator}
```

## Example - Reset the metamodels

```st
FASTJavaMetamodelGenerator resetMetamodel.
FamixCompatibilityGenerator resetMetamodel.
CRFMetamodelGenerator resetMetamodel.
```

# Thanks

## Your turn

Try to bind the FamixInvocation entity with the FASTJavaMethodInvocation.
The relation is one-to-one

Enjoy !
