pr4:
	ifort -module . -std08 -D"PR=4" -fpp gauss.f90 heat.f90 -o heat4
pr8:
	ifort -module . -std08 -D"PR=8" -fpp gauss.f90 heat.f90 -o heat8
pr16:
	ifort -module . -std08 -D"PR=16" -fpp gauss.f90 heat.f90 -o heat16
clean:
	rm -f *.mod heat4 heat8 heat16
