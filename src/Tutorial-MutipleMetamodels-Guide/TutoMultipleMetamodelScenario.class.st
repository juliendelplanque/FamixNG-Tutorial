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
TutoMultipleMetamodelScenario >> addBasicClassSideArchitecture [
	^ (LGCreateMethod new
		isClassSide: true;
		targetClassName: #TutoFamixFASTMetamodelGenerator;
		sourceCode:
			'packageName
	<ignoreForCoverage>
	^ #''Tutorial-MultipleMetamodels-Model''')
		+
			(LGCreateMethod new
				isClassSide: true;
				targetClassName: #TutoFamixFASTMetamodelGenerator;
				sourceCode:
					'prefix
	<ignoreForCoverage>
	^ #Tuto')  + ((LGShowMethod class: #TutoFamixFASTMetamodelGenerator methodSelectorToShow: #packageName) isClassSide: true  )
]

{ #category : #'as yet unclassified' }
TutoMultipleMetamodelScenario >> addBasicInstanceSideArchitecture [
	^ (LGCreateMethod new
		targetClassName: #TutoFamixFASTMetamodelGenerator;
		sourceCode:
			'defineClasses
	super defineClasses')
		+
			(LGCreateMethod new
				targetClassName: #TutoFamixFASTMetamodelGenerator;
				sourceCode:
					'defineHierarchy
	super defineHierarchy')
		+
			(LGCreateMethod new
				targetClassName: #TutoFamixFASTMetamodelGenerator;
				sourceCode:
					'defineProperties
	super defineProperties')
		+
			(LGCreateMethod new
				targetClassName: #TutoFamixFASTMetamodelGenerator;
				sourceCode:
					'defineRelations
	super defineRelations')
]

{ #category : #'as yet unclassified' }
TutoMultipleMetamodelScenario >> addRemoteEntity [
	^ (LGCreateClass new
				name: #TutoFamixFASTMetamodelGenerator;
				superClass: #FamixMetamodelGenerator;
				slots: { #famixMethod. #fastJavaMethodEntity };
				package: #'TutoFamixFAST-Metamodel-Generator') + (LGCreateMethod new
		targetClassName: #TutoFamixFASTMetamodelGenerator;
		sourceCode:
			'defineClasses
	super defineClasses.
	"add new instance variables"
	famixMethod := self remoteEntity: #Method withPrefix: #FAMIX.
	fastJavaMethodEntity := self remoteEntity: #JavaMethodEntity withPrefix: #FAST.')
		+
			(LGShowMethod
				class: #TutoFamixFASTMetamodelGenerator
				methodSelectorToShow: #defineClasses)
]

{ #category : #'as yet unclassified' }
TutoMultipleMetamodelScenario >> addRemoteRelations [
	^ (LGCreateMethod new
		targetClassName: #TutoFamixFASTMetamodelGenerator;
		sourceCode:
			'defineRelations
	super defineRelations.
	(famixMethod property: #fast) - (fastJavaMethodEntity property: #famix).')
		+
			(LGShowMethod
				class: #TutoFamixFASTMetamodelGenerator
				methodSelectorToShow: #defineRelations)
]

{ #category : #'as yet unclassified' }
TutoMultipleMetamodelScenario >> addSpecificMethodForMultipleMetamodels [
	^ (LGCreateMethod new
		isClassSide: true;
		targetClassName: #TutoFamixFASTMetamodelGenerator;
		sourceCode:
			'modifyMetamodel: aMetamodel
	"Just copy and paste"
	super modifyMetamodel: aMetamodel.
	self fixRemoteMetamodelRelationsIn: aMetamodel')
		+
			((LGShowMethod
				class: #TutoFamixFASTMetamodelGenerator
				methodSelectorToShow: #modifyMetamodel:) isClassSide: true)
		
]

{ #category : #'as yet unclassified' }
TutoMultipleMetamodelScenario >> addSubmetamodelsMethod [
	^ (LGCreateMethod new
		isClassSide: true;
		targetClassName: #TutoFamixFASTMetamodelGenerator;
		sourceCode:
			'submetamodels
	"We add the metamodel we will bind.. Here there are two metamodels. It can be more"
	^ {FASTJavaMetamodelGenerator.
	FamixCompatibilityGenerator}')
		+
			((LGShowMethod
				class: #TutoFamixFASTMetamodelGenerator
				methodSelectorToShow: #submetamodels) isClassSide: true)
]

{ #category : #'as yet unclassified' }
TutoMultipleMetamodelScenario >> openPlaygroundWithResetMetaModel [
	^ LGShowPlayground
		thenSetContent:
			'"Run (twice.. an error can appear the first time... An issue is already open)"
FamixCompatibilityGenerator resetMetamodel.
FASTJavaMetamodelGenerator resetMetamodel.
TutoFamixFASTMetamodelGenerator new generate.
TutoFamixFASTMetamodelGenerator resetMetamodel'
]

{ #category : #'as yet unclassified' }
TutoMultipleMetamodelScenario >> runTutorial [
	
	(LGScenaristPresenter new openWithSpec; adaptToLGScenarist: (LGScenarist read: self scenario))
]

{ #category : #'as yet unclassified' }
TutoMultipleMetamodelScenario >> scenario [
	^ LGScenario new
		addEntry:
			(LGCreatePackage named: 'TutoFamixFAST-Metamodel-Generator')
				+ (LGShowPackage packageNamed: 'TutoFamixFAST-Metamodel-Generator');
		addEntry:
			(LGCreateClass new
				name: #TutoFamixFASTMetamodelGenerator;
				superClass: #FamixMetamodelGenerator;
				package: #'TutoFamixFAST-Metamodel-Generator')
				+ (LGShowClass classToShow: #TutoFamixFASTMetamodelGenerator);
		addEntry: self addBasicInstanceSideArchitecture;
		addEntry: self addBasicClassSideArchitecture;
		addEntry: self addSpecificMethodForMultipleMetamodels;
		addEntry: self addSubmetamodelsMethod;
		addEntry: self addRemoteEntity;
		addEntry: self addRemoteRelations;
		addEntry: self openPlaygroundWithResetMetaModel
]
