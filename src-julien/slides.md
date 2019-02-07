% Famix Next-Generation
% Julien Deplanque
% julien.deplanque@inria.fr

## Introduction

### Objectives
- Understand Famix Next Generation (NG)
- Get familiar with the DSL

### Resources
- [https://github.com/SquareBracketAssociates/Booklet-FamixNG](https://github.com/SquareBracketAssociates/Booklet-FamixNG) (in progress)

## How it works
\begin{center}
\includegraphics[width=0.5\textwidth]{famixng-how-it-works.pdf}
\end{center}

## Differences with previous version

| (Old) Famix                           | Famix NG                   |
|:--------------------------------------|:---------------------------|
| Implement MM as Pharo classes.        | Implement MM using the DSL |
| Huge usage of inheritance.            | Huge usage of traits.      |
| Single MM.                            | Multiple MM.               |
| Verifications on the MM once created. | Verifications on the MM from its specification written using the DSL. |

## Take a step back

\begin{center}
\includegraphics[width=0.9\textwidth]{presentation-step-back.pdf}
\end{center}

## Take a step back

\begin{center}
\includegraphics[width=0.9\textwidth]{presentation-step-back2.pdf}
\end{center}

## A simple meta-model for presentations

\begin{center}
\includegraphics[width=0.9\textwidth]{presentation-meta-simple.pdf}
\end{center}

## FamixNG-ified meta-model for presentations

\begin{center}
\includegraphics[width=0.9\textwidth]{presentation-meta.pdf}
\end{center}

## The DSL - Entities
| Selector                  | Meaning                                 |
|:-------------------------:|:----------------------------------------|
| `#newClassNamed:comment:` | Creates a new class for the MM          |
| `#newTraitNamed:comment:` | Creates a new stateful trait for the MM |

> Question: How to choose?

- Classes **must** be used for entities that will be instantiated.
- Traits **can not** be instantiated.
- Can only inherit from one class.
- Can "inherit" from multiple traits.

## Example
```
presentation := builder newClassNamed: #Presentation.
slide := builder newClassNamed: #Slide.
uri := builder newClassNamed: #Uri.
uriReference := builder newClassNamed: #UriReference.
```

## The DSL - Inheritance

| Selector         | Short version | Meaning                          |
|:----------------:|:-------------:|:---------------------------------|
|`generalization:` | `--|>`        | Inheritance relation             |

> Remark: Can be written the other way around (`<|--`).

## Example (1)

\begin{center}
\includegraphics[width=0.9\textwidth]{presentation-meta-inheritance-classes.pdf}
\end{center}

## Example (2)

\begin{center}
\includegraphics[width=0.9\textwidth]{presentation-meta-inheritance-traits.pdf}
\end{center}

## The DSL - Relations

| Selector       | Short version | Meaning                          |
|:--------------:|:-------------:|:---------------------------------|
|`oneBelongsTo:` | `-<>`         | Composition with `1` child       |
|`manyBelongTo:` | `*-<>`        | Composition with `0..*` children |
|`containsOne:`  | `<>-`         | Composition with `1` child       |
|`containsMany:` | `<>-*`        | Composition with `0..*` children |
|`oneToOne:`     | `-`           | Association `0..1` to `0..1`     |
|`oneToMany:`    | `-*`          | Association `0..1` to `0..*`     |
|`manyToOne:`    | `*-`          | Association `0..*` to `0..1`     |
|`manyToMany:`   | `*-*`         | Association `0..*` to `0..*`     |

> Remark: As in the old Famix, Famix NG relations ensure that if one side of the relation is modified, the other side is updated accordingly.

## Example

\begin{center}
\includegraphics[width=0.9\textwidth]{presentation-meta-relations.pdf}
\end{center}

## The DSL - Properties

| Selector          | Meaning                                 |
|:-----------------:|:----------------------------------------|
| `#property:type:` | Creates a property for the class/trait. |

## Example

\begin{center}
\includegraphics[width=0.9\textwidth]{presentation-meta-properties.pdf}
\end{center}

## Summary (part 1)
```
defineClasses
	presentation := builder newClassNamed: #Presentation.
	slide := builder newClassNamed: #Slide.
	uri := builder newClassNamed: #Uri.
	uriReference := builder newClassNamed: #UriReference.
```

```
defineHierarchy
	presentation --|> namedEntity.
	slide --|> namedEntity.
	slide --|> #TWithReferences.
	uriReference --|> association.
	uriReference --|> #TReference.
	uri --|> entity.
	uri --|> #TReferenceable.
```

## Summary (part 2)

```
defineRelations
	(presentation property: #slides)
		<>-* (slide property: #presentation)
```

```
defineProperties
	uri property: #uri type: #String
```


## Basic infrastructure traits catalog

- `Famix-Traits` package provides a set of traits implementing generic concepts reusable accross meta-models.
	+ `FamixTReference`, `FamixTReferenceable` and `FamixTWithReferences`
	+ `FamixTAccess`, `FamixTAccessible` and `FamixTWithAccesses`
	+ `FamixTClass` and `FamixTWithClasses`
	+ ...
- To use one of these traits in your meta-model builder, reference it via a symbol (without `Famix` prefix).

```
"Here you make Uri class use FamixTReferenceable"
uri --|> #TReferenceable.
```

## In practice: Step 1
Create a subclass of one of

- `FamixMetamodelGenerator`
- `FamixBasicInfrastructureGenerator`
- `FamixFileBasedLanguageGenerator`

## In practice: Step 2
Implement class-side methods `#packageName` (name of the package in which the MM will be generated) and `prefix` (prefix for your generated classes)

## In practice: Step 3
Override the following instance-side methods depending on what part of the MM you describe:

+ `#defineClasses` for classes definitions
+ `#defineHierarchy` for classes inheritance definitions
+ `#defineRelations` for classes relations definitions
+ `#defineProperties` to define classes properties
+ `#defineTraits` for traits definitions

## In practice (summary)

1. Create a subclass of one of
	- `FamixMetamodelGenerator`
	- `FamixBasicInfrastructureGenerator`
	- `FamixFileBasedLanguageGenerator`
2. Implement class-side methods `#packageName` (name of the package in which the MM will be generated) and `prefix` (prefix for your generated classes)
3. Override the following instance-side methods depending on what part of the MM you describe:
	+ `#defineClasses` for classes definitions
	+ `#defineHierarchy` for classes inheritance definitions
	+ `#defineRelations` for classes relations definitions
	+ `#defineProperties` to define classes properties
	+ `#defineTraits` for traits definitions

## Tutorial time: Presentation

Load a fresh Moose 7 image from the CI and implement the previous meta-model.

```
Metacello new
	repository:
	'github://juliendelplanque/FamixNG-Slides/src';
	baseline: 'FamixNGSlides';
	load: 'Tutorial'.
```

## Hint: the meta-model

\begin{center}
\includegraphics[width=0.9\textwidth]{presentation-meta.pdf}
\end{center}

## Tutorial time: Fortran
[https://github.com/juliendelplanque/FamixNGFortran](https://github.com/juliendelplanque/FamixNGFortran)

```
Metacello new
	repository:
	'github://juliendelplanque/FamixNGFortran/src';
	baseline: 'FamixNGFortran';
	load: 'Tutorial'.
	```

## Tutorial time: Fortran

\begin{center}
\includegraphics[width=0.9\textwidth]{uml.png}
\end{center}
