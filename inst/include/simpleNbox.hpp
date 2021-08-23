/* Hector -- A Simple Climate Model
   Copyright (C) 2014-2015  Battelle Memorial Institute

   Please see the accompanying file LICENSE.md for additional licensing
   information.
*/
#ifndef SIMPLE_N_BOX_HPP_
#define SIMPLE_N_BOX_HPP_

/* SimpleNbox.hpp
 *
 * Header file for the simple box model.
 *
 */

#include "temperature_component.hpp"
#include "ocean_component.hpp"
#include "tseries.hpp"
#include "unitval.hpp"
#include "fluxpool.hpp"
#include "carbon-cycle-model.hpp"

#define SNBOX_ATMOS 0
#define SNBOX_VEG 1
#define SNBOX_DET 2
#define SNBOX_SOIL 3
#define SNBOX_OCEAN 4
#define SNBOX_EARTH 5
#define SNBOX_PERMAFROST 6
#define SNBOX_THAWEDP 7

#define MB_EPSILON 0.001                //!< allowed tolerance for mass-balance checks, Pg C
#define SNBOX_PARSECHAR "."             //!< input separator between <biome> and <pool>
#define SNBOX_DEFAULT_BIOME "global"    //!< value if no biome supplied

namespace Hector {

/*! \brief The simple global carbon model, not including the ocean
 *
 *  SimpleNbox tracks atmosphere (1 pool), land (3 pools), ocean (1 pool from its p.o.v.),
 *  and earth (1 pool). The ocean component handles ocean processes; SimpleNbox just
 *  tracks the total ocean C.
 */
class SimpleNbox : public CarbonCycleModel {
    friend class CSVOutputVisitor;
    friend class CSVOutputStreamVisitor;
    friend class CSVFluxPoolVisitor;

public:
    SimpleNbox();
    ~SimpleNbox() {}

    // overrides for IModelComponent methods
    std::string getComponentName() const { return std::string( SIMPLENBOX_COMPONENT_NAME ); }
    std::vector<std::string> getBiomeList() const { return(biome_list); }

    virtual void init( Core* core );

    virtual unitval sendMessage( const std::string& message,
                                const std::string& datum,
                                const message_data info=message_data() );

    virtual void setData( const std::string& varName,
                          const message_data& data );

    virtual void prepareToRun();

    virtual void run( const double runToDate );

    virtual bool run_spinup( const int step );

    virtual void reset(double date);

    virtual void shutDown();

    // IVisitable methods
    virtual void accept( AVisitor* visitor );

    // Carbon cycle model interface
    void getCValues( double t, double c[]);
    int  calcderivs( double t, const double c[], double dcdt[] ) const;
    void slowparameval( double t, const double c[] );
    void stashCValues( double t, const double c[] );
    void record_state(double t);                        //!< record the state variables at the end of the time step

    void createBiome(const std::string& biome);
    void deleteBiome(const std::string& biome);
    void renameBiome(const std::string& oldname, const std::string& newname);

private:
    virtual unitval getData( const std::string& varName,
                            const double date );

    // typedefs for two map types, to make things easier
    // TODO: these should probably be defined in h_util.hpp or someplace similar?
    typedef std::map<std::string, fluxpool> fluxpool_stringmap;
    typedef std::map<std::string, double> double_stringmap;

    /*****************************************************************
     * Component state
     * All of this information will be saved at the end of each time
     * step so that we can reset to any arbitrary past time.
     *****************************************************************/

    // List of biomes
    std::vector<std::string> biome_list;

    // Carbon pools -- global
    fluxpool earth_c;               //!< earth pool, Pg C; for mass-balance
    fluxpool atmos_c;                //!< atmosphere pool, Pg C
    fluxpool    Ca;                  //!< current [CO2], ppmv

    // Carbon pools -- biome-specific
    fluxpool_stringmap veg_c;        //!< vegetation pools, Pg C
    fluxpool_stringmap detritus_c;   //!< detritus pools, Pg C
    fluxpool_stringmap soil_c;       //!< soil pool, Pg C
    // Permafrost documented in:
    // Woodard, D. L., Shiklomanov, A. N., Kravitz, B., Hartin, C., and Bond-Lamberty, B.:
    // A permafrost implementation in the simple carbon–climate model Hector v.2.3pf, GMD 14:4751–4767, 2021.
    // http://dx.doi.org/10.5194/gmd-14-4751-2021
    fluxpool_stringmap permafrost_c; //!< permafrost C pool, Pg C
    // Track thawed peramfrost separately than soil, so that
    // we can apply rh_ch4_frac (the CH4:CO2 ratio) to it
    fluxpool_stringmap thawed_permafrost_c;     //!< thawed permafrost pool, Pg C
    fluxpool_stringmap static_c;  				//!< static carbon total in thawed permafrost pool

    fluxpool_stringmap NPP_veg;                         //!< Net primary productivity of vegetation;
    fluxpool_stringmap RH_det, RH_soil;                 //!< Heterotrophic CO2 respiration of detritus and soil
    fluxpool_stringmap RH_thawed_permafrost, RH_ch4;    //!<  Heterotrophic CO2 and CH4 respiration of thawed permafrost

    unitval residual;               //!< residual (when constraining Ca) flux, Pg C

    double_stringmap tempfertd, tempferts; //!< temperature effect on respiration (unitless)
    double_stringmap f_frozen, new_thaw;   //!< relative amounts and changes in permafrost

    /*****************************************************************
     * Records of component state
     * These vectors record the component state over time.  When we do
     * a reset, we will retrieve the state at the reset time from these
     * arrays.
     *****************************************************************/
    tseries<fluxpool> earth_c_ts;  //!< Time series of earth carbon pool
    tseries<fluxpool> atmos_c_ts;  //!< Time series of atmosphere carbon pool
    tseries<fluxpool> Ca_ts;       //!< Time series of atmosphere CO2 concentration

    tvector<fluxpool_stringmap> veg_c_tv;      //!< Time series of biome-specific vegetation carbon pools
    tvector<fluxpool_stringmap> detritus_c_tv; //!< Time series of biome-specific detritus carbon pools
    tvector<fluxpool_stringmap> soil_c_tv;     //!< Time series of biome-specific soil carbon pools
    tvector<fluxpool_stringmap> permafrost_c_tv;     	//!< Time series of biome-specific permafrost carbon pools
    tvector<fluxpool_stringmap> thawed_permafrost_c_tv; //!< Time series of biome-specific thawed permafrost
    tvector<fluxpool_stringmap> static_c_tv; 			//!< Time series of biome-specific thawed permafrost

    // Time series versions of flux variables
    tvector<fluxpool_stringmap> NPP_veg_tv, RH_det_tv, RH_soil_tv, RH_thawed_permafrost_tv, RH_ch4_tv;

    tseries<unitval> residual_ts; //!< Time series of residual flux values

    tvector<double_stringmap> tempfertd_tv, tempferts_tv; //!< Time series of temperature effect on respiration
    tvector<double_stringmap> f_frozen_tv;                //!< Time series of frozen permafrost fraction


    /*****************************************************************
     * Derived quantities
     * Unlike state varaibles, these can be calculated from other
     * information; therefore, they need not be stored over time, but
     * they do need to be recalculated whenever we reset.
     *****************************************************************/

    double_stringmap co2fert;           //!< CO2 fertilization effect (unitless)
    tseries<double> Tgav_record;        //!< Record of global temperature values, for computing soil RH
    bool in_spinup;                     //!< flag tracking spinup state
    double tcurrent;                    //!< Current time (last completed time step)
    double masstot;                     //!< tracker for mass conservation
    unitval atmosland_flux;             //!< Atmosphere -> land C flux
    tseries<unitval> atmosland_flux_ts; //!< Atmosphere -> land C flux (time series)

    /*****************************************************************
     * Input data
     * This information isn't part of the state; it's either read from
     * an input file or pushed in by another model.
     *****************************************************************/

    // Carbon fluxes
    tseries<unitval> ffiEmissions;      //!< fossil fuels and industry emissions, Pg C/yr
    tseries<unitval> daccsUptake;       //!< direct air carbon capture and storage, Pg C/yr
    tseries<unitval> lucEmissions;      //!< land use change emissions, Pg C/yr

    // Albedo
    tseries<unitval> Ftalbedo;   //!< terrestrial albedo forcing, W/m2

    // Constraints
    tseries<fluxpool> CO2_constrain;      //!< input [CO2] record to constrain model to

    /*****************************************************************
     * Model parameters
     * If you change any of these (in a Monte Carlo run, for example),
     * you will at the very least need to reset to the beginning of the
     * run.  You may need to redo the spinup.
     *****************************************************************/

    // Partitioning
    double_stringmap f_nppv, f_nppd;      //!< fraction NPP into vegetation and detritus
    double_stringmap f_litterd;           //!< fraction of litter to detritus

    double f_lucv, f_lucd;      //!< fraction LUC from vegetation and detritus

    // Initial fluxes
    fluxpool_stringmap npp_flux0;       //!< preindustrial NPP

    // Atmospheric CO2, temperature, and their effects
    fluxpool    C0;                      //!< preindustrial [CO2], ppmv

    double_stringmap beta;           //!< shape of CO2 response
    double_stringmap warmingfactor;  //!< regional warming relative to global (1.0=same)
    double_stringmap q10_rh;         //!< Q10 for heterotrophic respiration (1.0=no response, unitless)
    double_stringmap rh_ch4_frac;    //!< Fraction of RH from thawed permafrost that is CH4
    double_stringmap pf_sigma;       //!< Standard deviation for permafrost-temp model fit
    double_stringmap pf_mu;       	 //!< Mean for permafrost-temp model fit
    double_stringmap fpf_static;     //!< Permafrost C non-labile fraction
    
    
    /*****************************************************************
     * Functions computing sub-elements of the carbon cycle
     *****************************************************************/
    double calc_co2fert(std::string biome, double time = Core::undefinedIndex()) const; //!< calculates co2 fertilization factor.
    fluxpool npp(std::string biome, double time = Core::undefinedIndex()) const; //!< calculates NPP for a biome
    fluxpool sum_npp(double time = Core::undefinedIndex()) const; //!< calculates NPP, global total
    fluxpool rh_fda( std::string biome, double time = Core::undefinedIndex() ) const;  //!< calculates RH from detritus for a biome
    fluxpool rh_fsa( std::string biome, double time = Core::undefinedIndex() ) const;  //!< calculates RH from soil for a biome
    fluxpool rh_ftpa_co2( std::string biome ) const; //!< calculates current CO2 RH from thawed permafrost for a biome
    fluxpool rh_ftpa_ch4( std::string biome ) const; //!< calculates current CH4 RH from thawed permafrost for a biome
    fluxpool rh( std::string biome, double time = Core::undefinedIndex() ) const;      //!< calculates RH for a biome
    fluxpool sum_rh(double time = Core::undefinedIndex()) const; //!< calculates RH, global total
    fluxpool ffi(double t, bool in_spinup) const;
    fluxpool ccs(double t, bool in_spinup) const;
    fluxpool luc_emission(double t, bool in_spinip) const;
    fluxpool luc_uptake(double t, bool in_spinip) const;

    /*****************************************************************
     * Private helper functions
     *****************************************************************/
    void sanitychecks();                                //!< performs mass-balance and other checks
    fluxpool sum_map( fluxpool_stringmap pool ) const;    //!< sums a unitval map (collection of data)
    double sum_map( double_stringmap pool ) const;      //!< sums a double map (collection of data)
    void log_pools( const double t );                   //!< prints pool status to the log file
    void set_c0(double newc0);                          //!< set initial co2 and adjust total carbon mass

    bool has_biome(const std::string& biome);

    OceanComponent *omodel;           //!< pointer to the ocean model in use

    // Add a biome to a time-series map variable (e.g. veg_c_tv)
    template <class T_data>
    void add_biome_to_ts(tvector<std::map<std::string, T_data>>& ts,
                         const std::string& biome,
                         T_data init_value) {
        // First, check if a biome of this name already exists in the data
        if ( ts.get(ts.firstdate()).count( biome ) ) {
            H_THROW( "Biome '" + biome + "' already exists in data." );
        }

        // Loop over time steps, and set the variable to the provided `init_value`
        std::map<std::string, T_data> newval;
        for ( double i = ts.firstdate(); i < ts.lastdate(); i++ ) {
            if (ts.exists(i)) {
                newval = ts.get(i);
                newval[ biome ] = init_value;
                ts.set(i, newval);
            }
        }
    }

    // Remove a biome from a time-series map variable
    template <class T_map>
    void remove_biome_from_ts(tvector<T_map>& ts,
                              const std::string& biome) {
        // We don't need to check for presence of `biome` here because the
        // `<std::map>.erase()` method is effectively a no-op when given a
        // non-existent key.
        T_map currval;
        for ( double i = ts.firstdate(); i < ts.lastdate(); i++ ) {
            if (ts.exists(i)) {
                currval = ts.get(i);
                currval.erase(biome);
                ts.set(i, currval);
            }
        }
    }

    // Rename a biome in a time-series map variable. At each time
    // step, this creates a new biome called `newname`, gives it all
    // of the values of `oldname`, and then erases that time step's
    // `oldname`.
    template <class T_map>
    void rename_biome_in_ts(tvector<T_map>& ts,
                            const std::string& oldname,
                            const std::string& newname) {
        if ( !ts.get(ts.firstdate()).count( oldname ) ) {
            H_THROW( "Biome '" + oldname + "' not found in data.");
        }
        if ( ts.get(ts.firstdate()).count( newname ) ) {
            H_THROW( "Biome '" + newname + "' already exists in data.");
        }

        T_map currval;
        for ( double i = ts.firstdate(); i < ts.lastdate(); i++ ) {
            if (ts.exists(i)) {
                currval = ts.get(i);
                currval[newname] = currval.at(oldname);
                currval.erase(oldname);
                ts.set(i, currval);
            }
        }
    }

    // Turn on tracking for all simpleNbox pools
    void start_tracking(){
        earth_c.tracking = true;
        atmos_c.tracking = true;
        for( auto it = biome_list.begin(); it != biome_list.end(); it++ ) {
            std::string biome = *it;
            veg_c[ biome ].tracking = true;
            soil_c[ biome ].tracking = true;
            permafrost_c[ biome ].tracking = true;
            thawed_permafrost_c[ biome ].tracking = true;
            static_c[ biome ].tracking = true;
            detritus_c[ biome ].tracking = true;
        }
    }

};

}

#endif
