Class {
	#name : #BaselineOfFamixNGTutorial,
	#superclass : #BaselineOf,
	#category : #BaselineOfFamixNGTutorial
}

{ #category : #baselines }
BaselineOfFamixNGTutorial >> baseline: spec [
	<baseline>
	spec
		for: #common
		do: [ 
			self defineDependencies: spec.
			self definePackages: spec.
			self defineGroups: spec ]
]

{ #category : #baselines }
BaselineOfFamixNGTutorial >> defineDependencies: spec [
	spec
		baseline: 'LectureGuide'
		with: [ spec
				loads: #('all');
				repository: 'github://juliendelplanque/LectureGuide' ].
	spec
		baseline: 'FASTJava'
		with: [ spec
				loads: #('smacc' 'visitor');
				repository: 'github://moosetechnology/FAST-Java:master/src' ].
]

{ #category : #baselines }
BaselineOfFamixNGTutorial >> defineGroups: spec [
	spec
		group: 'default' with: #('metamodel' 'multiplemetamodels');
		group: 'metamodel' with: #();
		group: 'multiplemetamodels' with: #('Tutorial-MutipleMetamodels-Guide' 'FASTJava')
		
]

{ #category : #baselines }
BaselineOfFamixNGTutorial >> definePackages: spec [
	spec
		repository: 'https://github.com/juliendelplanque/FamixNG-Tutorial';
		package: 'Tutorial-MutipleMetamodels-Guide' with: [ spec requires: #('LectureGuide') ]
]
