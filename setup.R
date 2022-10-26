# dir.create('~/.R')
# file.create('~/.R/Makevars')
# readLines('~/.R/Makevars')
# writeLines(c("FC = /opt/homebrew/Cellar/gcc/12.2.0/bin/gfortran",
#              "F77 = /opt/homebrew/Cellar/gcc/12.2.0/bin/gfortran",
#              "FLIBS = -L/opt/homebrew/Cellar/gcc/12.2.0/lib/gcc/11"),'~/.R/Makevars')
# writeLines(c("CPPFLAGS+=-I/usr/local/include -Xclang -fopenmp",
#              "LDFLAGS+=-L/usr/local/lib -lomp",
#              "FC=/opt/R/arm64/gfortran/bin/gfortran -mtune=native",
#              "FLIBS=-L/opt/R/arm64/gfortran/lib/gcc/aarch64-apple-darwin20.6.0/12.0.1 -L/opt/R/arm64/gfortran/lib -lgfortran -lemutls_w -lm"),
#            '~/.R/Makevars')




