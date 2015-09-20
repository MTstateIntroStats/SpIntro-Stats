all: coursePack

.PHONY: coursePack

coursePack: MWF-coursePack.pdf TR-coursePack.pdf

MWF-coursePack.pdf: MWF-coursePack.tex
	texi2pdf $<

TR-coursePack.pdf: TR-coursePack.tex
	texi2pdf $<

clean:
	-rm *.out *.log *.aux common/*.aux
