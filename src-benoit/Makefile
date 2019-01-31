all: light

light: BindTwoMetamodels.md
	pandoc -s --highlight-style monochrome -t beamer BindTwoMetamodels.md -o prez.pdf

dark: BindTwoMetamodels.md
	pandoc -s -t beamer BindTwoMetamodels.md -o prez.pdf -V colortheme:owl 

clean:
	rm prez.pdf