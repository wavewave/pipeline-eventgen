#include <sstream> 
#include "Pythia.h"


using namespace Pythia8; 

int convertHepMCStatusCode( int orig ) 
{
  if( orig == 1 || orig == 2 ) return orig; 
  else if( orig == 21 ) return -1 ;
  else return 2 ; 
}


int main( int argc, char** argv ) {
  cout << "argc =" << argc << endl ;
  if (argc != 3 ) { 
    cout << "pythia8run inputfile outputfile" << endl; 
    exit(-1); 
  }
  Pythia pythia; 
  pythia.readString("PartonLevel:ISR = on");              // on 
  pythia.readString("PartonLevel:FSR = off");             // off
  pythia.readString("PartonLevel:MPI = off");              // on
  pythia.readString("HadronLevel:Hadronize = on");        // on
  pythia.readString("PartonLevel:all = on");              // on

  // LHAupFromPYTHIA8 myLHA(&pythia.event, &pythia.info);  
  // myLHA.openLHEF("weakbosons.lhe"); 


  pythia.init(argv[1]);  // ("./main99.lhe"); 


  // myLHA.setInit();

  // myLHA.initLHEF();

  ofstream myfile; 
  myfile.open (argv[2]); // ("output.hepevt") ; 

  int nAbort = 10;
  int iAbort = 0;

  for (int iEvent = 1;  ; ++iEvent ) { 
    if( !pythia.next() ) { 
      if (pythia.info.atEndOfFile()) break; 
      if (++iAbort < nAbort) continue;
      break;
    } 
   
    // for test
    if ( iEvent > 10 ) break; 
    // 

    Event&  ev  = pythia.event;
    stringstream os ; 

    double var = 0 ; 
    double nptl = 0 ; 
    for ( int i = 3 ; i < ev.size() ; i++ ) { 
      Particle& pt = ev[i]; 

      //if( ev.statusHepMC(i) <= 2 ) { 
      { 
	int st = ev.statusHepMC(i); 
        if( st == 1 ) {
          nptl++;  
	  os << setw(5) << i             << setw(5) << ev.statusHepMC(i) // convertHepMCStatusCode( ev.statusHepMC(i) ) 
	    /*   setw(5) << pt.status()  */   << setw(9) << pt.id() 
	     << setw(5) << pt.mother1()  << setw(5) << pt.mother2() 
	     << setw(5) << pt.daughter1() << setw(5) << pt.daughter2() 
	     << scientific << setprecision(11) << setw(19) << pt.px() 
	     << scientific << setprecision(11) << setw(19) << pt.py() 
	     << scientific << setprecision(11) << setw(19) << pt.pz() 
	     << scientific << setprecision(11) << setw(19) << pt.e()
	     << scientific << setprecision(11) << setw(19) << pt.m()
	     << scientific << setprecision(11) << setw(19) << pt.xProd()
	     << scientific << setprecision(11) << setw(19) << pt.yProd()
	     << scientific << setprecision(11) << setw(19) << pt.zProd()
	     << scientific << setprecision(11) << setw(19) << pt.tProd()
	     << endl ;
        }
	if( st==1 ) { var = var + pt.px (); }
      }
        
    }  
    cout << " var = " << var << endl;  
    myfile << setw(7) << iEvent << setw(7) << /* ev.size() - 3 */ nptl  << endl ; 
    myfile << os.str();
  }
  myfile.close();
    //   myLHA.setEvent();

    //   myLHA.eventLHEF();



    // myLHA.closeLHEF(true);

  // Done.                           
  return 0;
}


