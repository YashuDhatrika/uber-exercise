HOCKING-uber-exercises.pdf: HOCKING-uber-exercises.tex
	pdflatex HOCKING-uber-exercises
figure-logins/index.html: figure-logins.R logins.RData
	R --no-save < $<
logins.RData: logins.R
	R --no-save < $<
