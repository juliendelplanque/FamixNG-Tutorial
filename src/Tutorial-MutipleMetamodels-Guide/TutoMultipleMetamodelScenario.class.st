Class {
	#name : #TutoMultipleMetamodelScenario,
	#superclass : #Object,
	#category : #'Tutorial-MutipleMetamodels-Guide'
}

{ #category : #'as yet unclassified' }
TutoMultipleMetamodelScenario class >> runTutorial [
	<example>
	self new runTutorial
]

{ #category : #'as yet unclassified' }
TutoMultipleMetamodelScenario >> runTutorial [
	
	(LGScenaristPresenter new openWithSpec; adaptToLGScenarist: (LGScenarist read: self scenario))
]

{ #category : #'as yet unclassified' }
TutoMultipleMetamodelScenario >> scenario [ 
	^ LGScenario new addEntry: (LGCreatePackage named: 'TutoFamixFAST').
]
