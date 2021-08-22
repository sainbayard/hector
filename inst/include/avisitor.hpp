/* Hector -- A Simple Climate Model
   Copyright (C) 2014-2015  Battelle Memorial Institute

   Please see the accompanying file LICENSE.md for additional licensing
   information.
*/
#ifndef AVISITOR_H
#define AVISITOR_H
/*
 *  avisitor.h
 *  hector
 *
 *  Created by Pralit Patel on 10/29/10.
 *
 */

namespace Hector {

// Forward declare all visitable subclasses.
class Core;
class DummyModelComponent;
class ForcingComponent;
class slrComponent;
class HalocarbonComponent;
class SimpleNbox;
class CarbonCycleSolver;
class CH4Component;
class OHComponent;
class N2OComponent;
class TemperatureComponent;
class BlackCarbonComponent;
class OrganicCarbonComponent;
class OceanComponent;
class SulfurComponent;
class OzoneComponent;

//------------------------------------------------------------------------------
/*! \brief AVisitor abstract class provides a base for subclasses to visit only
 *         the IVisitable subclasses that they are interested in.
 */
class AVisitor {
public:
    inline virtual ~AVisitor();

    //------------------------------------------------------------------------------
    /*! \brief Determine if the visitor needs to collect data at the given model
     *         date.
     *  \param date The model date which just finished solving.
     *  \return True if the visitor wants to visit at date.
     */
    virtual bool shouldVisit( const bool in_spinup, const double date ) = 0;

    //------------------------------------------------------------------------------
    /*! \brief Allow visitors to reset their data upon a core reset.
     *  \param reset_date The date to reset to.
     */
    virtual void reset( const double reset_date ) {}

    //------------------------------------------------------------------------------
    // Add a visit for all visitable subclasses here.
    // TODO: should we create a .cpp for these?
    virtual void visit( Core* core ) {}
    virtual void visit( DummyModelComponent* c ) {}
    virtual void visit( ForcingComponent* c ) {}
    virtual void visit( slrComponent* c ) {}
    virtual void visit( CarbonCycleSolver* c ) {}
    virtual void visit( SimpleNbox* c ) {}
    virtual void visit( HalocarbonComponent* c ) {}
    virtual void visit( OHComponent* c ) {}
    virtual void visit( CH4Component* c ) {}
    virtual void visit( N2OComponent* c ) {}
    virtual void visit( TemperatureComponent* c ) {}
    virtual void visit( BlackCarbonComponent* c ) {}
    virtual void visit( OrganicCarbonComponent* c ) {}
    virtual void visit( OceanComponent* c ) {}
	virtual void visit( SulfurComponent* c ) {}
	virtual void visit( OzoneComponent* c ) {}
};

// Inline methods
AVisitor::~AVisitor() {
}

}


// Macros used by both csv_tracking_visitor and csv_outputstream_visitor

// TODO: consolidate these macros into the two MESSAGE ones,
// and shift string literals to D_xxxx definitions

// Macro to send a variable with associated unitval units to some output stream
// Takes s (stream), c (component), xname (variable name), x (output variable)
#define STREAM_UNITVAL( s, c, xname, x ) { \
s << linestamp() << c->getComponentName() << DELIMITER \
<< xname << DELIMITER << x.value( x.units() ) << DELIMITER \
<< x.unitsName() << std::endl; \
}

// Macro to send a variable with associated unitval units to some output stream
// This uses new sendMessage interface in imodel_component
// Takes s (stream), c (component), xname (variable name)
#define STREAM_MESSAGE( s, c, xname ) { \
unitval x = c->sendMessage( M_GETDATA, xname ); \
s << linestamp() << c->getComponentName() << DELIMITER \
<< xname << DELIMITER << x.value( x.units() ) << DELIMITER \
<< x.unitsName() << std::endl; \
}
// Macro for date-dependent variables
// Takes s (stream), c (component), xname (variable name), date
#define STREAM_MESSAGE_DATE( s, c, xname, date ) { \
unitval x = c->sendMessage( M_GETDATA, xname, message_data( date ) ); \
s << linestamp() << c->getComponentName() << DELIMITER \
<< xname << DELIMITER << x.value( x.units() ) << DELIMITER \
<< x.unitsName() << std::endl; \
}

#endif // AVISITOR_H
