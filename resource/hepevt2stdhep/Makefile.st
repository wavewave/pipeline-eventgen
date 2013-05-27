hepevt2stdhep: 
	gfortran -o hepevt2stdhep hepevt2stdhep.f $pgslib$/libpgslib.a $pgslib$/libexthep.a  $pgslib$/libstdhep.a $pgslib$/libFmcfio.a $pythialib$/libpythiaext.a   $pgslib$/libtauola.a


