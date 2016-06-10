figure-logins/index.html: figure-logins.R logins.RData
	R --no-save < $<
logins.RData: logins.R
	R --no-save < $<
