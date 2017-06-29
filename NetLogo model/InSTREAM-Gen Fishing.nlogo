extensions [matrix array table csv time profiler]

globals 
 [ 
  ;; Reach & cell environmental and habitat state variables & parameters
  
  initial-date  initial-Juliandate          ;; Parameter that sets the first day of simulation
  space-scale  timestep-scale               ;; Spatial and temporal scale factors so that the model can be used with every spatio-temporal scale. Basic units are cm for space and day for time. 
  siteLatitude                              ;; Parameter for calculating day length
  simulation-length                         ;; Length in ticks of the simulation
  environmental-time-series  tick-date      ;; Time series objects to store environmental data linked to real dates
  depth-data  velocity-data                 ;; Matrices for hydraulic variables (values for every day along the whole simulation period). Cell variables.
  depth-file-name                           ;; File names for input files when hydraulics are calculated within the model and not read as inputs 
  velocity-file-name                        
  depth-flow-list                           ;; List of flows in the depth hydraulic input
  velocity-flow-list                        ;; List of flows in the velocity hydraulic input
  cell-ID-table                             ;; A *table* linking cell numbers to corresponding cells (only when hydraulics are calculated within the model from lookup tables)
  habitat-features                          ;; Array storing habitat features
  flow  temp  dayLength                     ;; Environmental variable's value
  wettedArea                                ;; Reach's daily wetted area
  reachLength                               ;; Reach length
  PiscivFishDens                            ;; Density of piscivorous fish in the reach 
  cmaxTempFunction                          ;; Temperature function of the fish physiological maximum daily food consumption 
  habShelterSpeedFrac                       ;; Constant fraction of cell’s mean water velocity if trout have velocity shelters  
;  habDriftConc                              ;; Drift food concentration in the reach --Moved to input box in the interface.
;  habDriftRegenDist                         ;; Distance over which drift depleted by foraging fish is regenerated --Moved to input box in the interface. 
;  habSearchProd                             ;; Search food concentration in the reach --Moved to input box in the interface.
;  habPreyEnergyDensity                      ;; Prey energy density (parameter used to convert grams of prey eaten to joules of energy intake) --Moved to input box in the interface.
  habMaxSpawnFlow                           ;; Maximum flow limit for spawning
  
  
  
  ;; Trout population parameters for initialisation
  
  init-N                                                                                         ;; Initial population abundance
  prop-Age0  prop-Age1  prop-Age2  prop-Age3  prop-Age4  prop-Age5Plus                                                  ;; Proportion of fish of each age-class within the population 
  fishLengthMeanAge0 fishLengthMeanAge1 fishLengthMeanAge2 fishLengthMeanAge3 fishLengthMeanAge4 fishLengthMeanAge5Plus ;; Mean length of fish of different age-classes
  fishLengthSdAge0  fishLengthSdAge1  fishLengthSdAge2  fishLengthSdAge3 fishLengthSdAge4 fishLengthSdAge5Plus          ;; Standard deviation of fish length for different age-classes 
  fishEmergenceDateMean                                                                          ;; Population mean date of emergence (in Julian days)
  fishNewLengthMean                                                                              ;; Population mean length at emergence
  fishNewLengthVar                                                                               ;; Population variance of length at emergence
  fishNewLengthHeritability                                                                      ;; Narrow-sense heritability of length at emergence
  fishMinNewLength  fishMaxNewLength                                                             ;; Minimum/Maximum value of length at emergence (new trout cannot be below/above this threshold value)
  fishSpawnMinLengthMeanM                                                                        ;; Population mean length maturity threshold (minimum length to spawn) for males
  fishSpawnMinLengthMeanF                                                                        ;; Population mean length maturity threshold (minimum length to spawn) for females
  fishSpawnMinLengthVarM                                                                         ;; Population variance of length maturity threshold (minimum length to spawn) for males
  fishSpawnMinLengthVarF                                                                         ;; Population variance of length maturity threshold (minimum length to spawn) for females
  fishSpawnMinLengthHeritability                                                                 ;; Narrow-sense heritability of length maturity threshold (minimum length to spawn)
  fishNeutralTraitMean                                                                           ;; Population mean value of neutral trait 
  fishNeutralTraitVar                                                                            ;; Population variance of neutral trait
  fishNeutralTraitHeritability                                                                   ;; Narrow-sense heritability of neutral trait  
  mutationalVarParam                                                                             ;; Parameter for the mutational variance
;  mutationFactor                                                                                 ;; Parameter defining the amplitude of mutation --Moved to input box in the interface.
  
  
  ;; Trout parameters
  
  fishFitnessHorizon                                                                             ;; Parameter representing the fitness time horizon
  fishMoveDistParamA  fishMoveDistParamB                                                         ;; Parameters for the maximum movement distance function  
  fishCmaxParamA  fishCmaxParamB                                                                 ;; Parameters for the physiological maximum consumption function
  fishCmaxTempT1 fishCmaxTempT2 fishCmaxTempT3 fishCmaxTempT4 fishCmaxTempT5 fishCmaxTempT6 fishCmaxTempT7 ;; Parameter values for temperature function of maximum consumption
  fishCmaxTempF1 fishCmaxTempF2 fishCmaxTempF3 fishCmaxTempF4 fishCmaxTempF5 fishCmaxTempF6 fishCmaxTempF7 ;; Parameter values for temperature function of maximum consumption
  fishMaxSwimParamA  fishMaxSwimParamB  fishMaxSwimParamC  fishMaxSwimParamD  fishMaxSwimParamE  ;; Parameters for the maximum swimming speed function
  fishDetectDistParamA  fishDetectDistParamB                                                     ;; Parameters for the detection distance function
  fishCaptureParam1  fishCaptureParam9                                                           ;; Parameters for the capture success logistic function
  fishRespParamA  fishRespParamB  fishRespParamC  fishRespParamD                                 ;; Parameters for the standard respiration function
  fishSearchArea                                                                                 ;; Parameter for the production of stationary (non-drifting) food 
  fishWeightParamA  fishWeightParamB                                                             ;; Parameters for the length-weight relationship
  fishEnergyDensity                                                                              ;; Parameter used to convert a fish's energy intake to growth in weight
  fishPiscivoryLength                                                                            ;; Parameter defining the size threshold over which trout are a potential predator on smaller trout
  fishSpawnMinAge                                                                                ;; Parameter defining the minimum age for spawning (in days)
  fishSpawnMinCond                                                                               ;; Parameter defining the minimum body condition for spawning  
  fishSpawnStartDate  fishSpawnEndDate                                                           ;; Parameters defining the date window for spawning (in Julian day) 
  fishSpawnMinTemp  fishSpawnMaxTemp                                                             ;; Parameters for spawning temperature range
  fishSpawnMaxFlowChange                                                                         ;; Parameter for spawning flow stability
  fishSpawnProb                                                                                  ;; Parameter defining the probability of spawning on any such day
  fishSpawnDSuitD1  fishSpawnDSuitD2  fishSpawnDSuitD3  fishSpawnDSuitD4  fishSpawnDSuitD5               ;; Parameters for spawning depth suitability (depth values)
  fishSpawnDSuitS1  fishSpawnDSuitS2  fishSpawnDSuitS3  fishSpawnDSuitS4  fishSpawnDSuitS5               ;; Parameters for spawning depth suitability (suitability values)
  fishSpawnVSuitV1 fishSpawnVSuitV2 fishSpawnVSuitV3 fishSpawnVSuitV4 fishSpawnVSuitV5 fishSpawnVSuitV6  ;; Parameters for spawning velocity suitability (velocity values)
  fishSpawnVSuitS1 fishSpawnVSuitS2 fishSpawnVSuitS3 fishSpawnVSuitS4 fishSpawnVSuitS5 fishSpawnVSuitS6  ;; Parameters for spawning velocity suitability (suitability values)
  fishFecundParamA  fishFecundParamB                                                             ;; Parameters for the length-fecundity relationship
  fishSpawnEggViability                                                                          ;; Parameter defining the fraction of females's eggs that become viable in the redd
  max-n-males-per-female                                                                         ;; Parameter defining the maximum number of males contributing to the fertilization of the eggs of one female
  fishSpawnWtLossFraction                                                                        ;; Parameter defining the proportion of body Weight which is reduced after spawning 
    
  
  ;; Variables defining which Mortality functions are included in the simulations (True/False variables) --Moved to switch in the interface.
  
;  MortHighTemp  MortHighVel  MortStrand  MortPoorCond
;  MortTerrPredDepth  MortTerrPredDLength  MortTerrPredVel  MortTerrPredFeedTime  MortTerrPredCover
;  MortAqPredDens MortAqPredDepth  MortAqPredDLength  MortAqPredFeedTime  MortAqPredTemp
;  MortAnglingHooking
  
  
  ;; Parameters for logistic survival functions
    
  mortFishHiTT1  mortFishHiTT9                                                                   ;; Parameters for High Temperature survival function
  mortFishVelocityV1  mortFishVelocityV9                                                         ;; Parameters for High Velocity survival function
  mortFishStrandD1  mortFishStrandD9                                                             ;; Parameters for Stranding survival function
  mortFishConditionK1  mortFishConditionK9                                                       ;; Parameters for Poor Condition survival function
;  mortFishTerrPredMin                                                                            ;; Parameter for Terrestrial Predation survival function --Moved to input box in the interface.
  mortFishTerrPredD1  mortFishTerrPredD9                                                         ;; Parameters for Depth survival function for Terrestrial Predation
  mortFishTerrPredL1  mortFishTerrPredL9                                                         ;; Parameters for Length survival function for Terrestrial Predation
  mortFishTerrPredF1  mortFishTerrPredF9                                                         ;; Parameters for Feeding Time survival function for Terrestrial Predation
  mortFishTerrPredV1  mortFishTerrPredV9                                                         ;; Parameters for Velocity survival function for Terrestrial Predation
  mortFishTerrPredH1  mortFishTerrPredH9                                                         ;; Parameters for Hiding Distance survival function for Terrestrial Predation
;  mortFishAqPredMin                                                                              ;; Parameter for Aquatic Predation survival function --Moved to input box in the interface.
  mortFishAqPredP1  mortFishAqPredP9                                                             ;; Parameters for Piscivorous fish density survival function for Aquatic Predation
  mortFishAqPredD1  mortFishAqPredD9                                                             ;; Parameters for Depth survival function for Aquatic Predation
  mortFishAqPredL1  mortFishAqPredL9                                                             ;; Parameters for Length survival function for Aquatic Predation
  mortFishAqPredF1  mortFishAqPredF9                                                             ;; Parameters for Feeding Time survival function for Aquatic Predation
  mortFishAqPredT1  mortFishAqPredT9                                                             ;; Parameters for Temperature survival function for Aquatic Predation
  
  SurvProbHighTemp                                                                               ;; Survival and increasing survival probabilities from logistic functions dependent on global reach environmental conditions,
  terrPredFeedTimeF                                                                              ;; but neither on cells conditions nor on the fish state. So their values are constant along the time step across cells and fish.
  aqPredFeedTimeF 
  aqPredTempF 
  aqPredDensF 
  
 ;; Parameters for angling and hooking mortality 
 
  startAnglingSeason  endAnglingSeason                                                           ;; Parameters setting the timing and duration of the angling season (in Julian day) 
  mortFishAngleL1  mortFishAngleL9                                                               ;; Parameters for Length survival function for Angling Mortality
  mortFishAngleSuccess                                                                           ;; Parameter that represents fishing success as the fraction of fish hooked per catchable fish per angler hour
;  anglePressure                                                                                  ;; Parameter representing angler pressure with units of angler-hrs × km-1 × d-1 --Moved to input box in the interface.
;  mortFishAngleSlotLower                                                                         ;; Parameter defining the lower end of the length range in which fish are legal to keep --Moved to input box in the interface.
;  mortFishAngleSlotUpper                                                                         ;; Parameter defining the upper end of the length range in which fish are legal to keep --Moved to input box in the interface.
  mortFishAngleFracKeptLegal                                                                     ;; Parameter for Probability of fish of legal length being kept by anglers
  mortFishAngleFracKeptIllegal                                                                   ;; Parameter for Probability of fish not of legal length being kept by anglers
  mortFishAngleHookSurvRate                                                                      ;; Parameter defining Survival probability for released trout (or trout that shake the hook)
  anglingEfficiency                                                                              ;; Number of angler hours necessary to catch and keep a trout. From creel surveys: (angler-hrs per ha per day)/(caught trout per ha per year)
  anglingStock                                                                                   ;; Projected number of trout of legal size over the angling season
;  exploitationRate                                                                               ;; Angling exploitation rate (%): trout caught and kept per trout catchable --Moved to input box in the interface.
  angling-simulation-data                                                                        ;; matrix to store the experimental design to perform angling simulation experiments
  
  ;; Redd Parameters
  
  mortReddDewaterSurv                                                                            ;; Parameter defining the daily fraction of eggs surviving dewatering
  habShearParamA  habShearParamB                                                                 ;; Parameters for the peak Shield stress vs flow relationship
  mortReddScourDepth                                                                             ;; Parameter representing the egg burial depth (distance down from the gravel surface to the top of a redd’s egg pocket)
  mortReddLoTT1  mortReddLoTT9                                                                   ;; Parameters for Low Temperature survival function
  mortReddHiTT1  mortReddHiTT9                                                                   ;; Parameters for High Temperature survival function
  reddSize                                                                                       ;; Parameter representing the size of a redd
  reddDevelParamA  reddDevelParamB  reddDevelParamC                                              ;; Parameters for the redd fractional development function
  
  ;; Variables for Model ouputs
  fishDeadHighTempTot  fishDeadHighVelTot  fishDeadStrandTot  fishDeadPoorCondTot                ;; Cumulative counter of total fish dead from each mortality source along the whole simulation 
  fishDeadTerrPredTot  fishDeadAqPredTot  fishDeadAnglingTot  fishDeadHookingTot
  fishDeadHighTempAge0  fishDeadHighVelAge0  fishDeadStrandAge0  fishDeadPoorCondAge0            ;; Cumulative counter of total fish dead from each mortality source broken out by age-classes
  fishDeadTerrPredAge0  fishDeadAqPredAge0  fishDeadAnglingAge0  fishDeadHookingAge0
  fishDeadHighTempAge1  fishDeadHighVelAge1  fishDeadStrandAge1  fishDeadPoorCondAge1           
  fishDeadTerrPredAge1  fishDeadAqPredAge1  fishDeadAnglingAge1  fishDeadHookingAge1
  fishDeadHighTempAge2  fishDeadHighVelAge2  fishDeadStrandAge2  fishDeadPoorCondAge2           
  fishDeadTerrPredAge2  fishDeadAqPredAge2  fishDeadAnglingAge2  fishDeadHookingAge2
  fishDeadHighTempAge3Plus  fishDeadHighVelAge3Plus  fishDeadStrandAge3Plus  fishDeadPoorCondAge3Plus           
  fishDeadTerrPredAge3Plus  fishDeadAqPredAge3Plus  fishDeadAnglingAge3Plus  fishDeadHookingAge3Plus
  fishDeadAnglingSeason  fishDeadHookingSeason  fishDeadAnglingSeasonAge0  fishDeadHookingSeasonAge0           ;;Seasonal counters for angling & hooking mortality
  fishDeadAnglingSeasonAge1  fishDeadHookingSeasonAge1  fishDeadAnglingSeasonAge2  fishDeadHookingSeasonAge2
  fishDeadAnglingSeasonAge3Plus  fishDeadHookingSeasonAge3Plus
  ReachNumberOfEggs  ReachIniNumberOfEggs  ReachNumberFryHatched
  MaleSpawnersLength  FemaleSpawnersLength  MaleSpawnersAge  FemaleSpawnersAge  SpawningDate  
  EmergenceDate  AgeatDeath   
  MaleBreedGenMinSpawnLength  FemaleBreedGenMinSpawnLength  GenBreedEmergenceLength  GenBreedNeutralTrait
  
  ;; Variables and parameters for model calibration
  calibration?                                                                                    ;; Parameter defining whether the simulation is set for calibration purpose
  TScountAge0  TScountAge1  TScountAge2  TScountAge3Plus                                          ;; List to store historical time series of abundance by age-classes
  TSbiomassAge0  TSbiomassAge1  TSbiomassAge2  TSbiomassAge3Plus                                  ;; List to store historical time series of biomass by age-classes
  TSlengthAge0  TSlengthAge1  TSlengthAge2  TSlengthAge3Plus                                      ;; List to store historical time series of mean length by age-classes
  calibration-data                                                                                ;; Matrix defining the factorial design for parameters to be calibrated
  SSSEcountAge0  SSSEcountAge1  SSSEcountAge2  SSSEcountAge3Plus                                  ;; List to store time series of sum of standardized square errors between observed and simulated abundance by age-classes
  SSSEbiomassAge0  SSSEbiomassAge1  SSSEbiomassAge2  SSSEbiomassAge3Plus                          ;; List to store time series of sum of standardized square errors between observed and simulated biomass by age-classes
  SSSElengthAge0  SSSElengthAge1  SSSElengthAge2  SSSElengthAge3Plus                              ;; List to store time series of sum of standardized square errors between observed and simulated mean length by age-classes
  
 ]

patches-own [patchArea]

breed [cells cell] 
breed [trout a-trout]
breed [redds redd]

cells-own 
 [
  my-patches                                                ;; Patches composing the cell
  my-adjacentCells                                          ;; Adjacent cells
  transect  cellNumber                                      ;; Identity features of the cell
  cellArea  cellFracShelter  cellFracCover  cellFracGravel  ;; Fixed habitat features of the cell. They change across space but not along time.
  cellAreaShelter  cellAreaCover                            ;; Area of the cell with shelters and cover. They dinamically change as they are used by trout.
  cellDistanceToHide                                        ;; Average distance a fish in the cell would need to move to hide from a predator    
  cellDepth  cellVelocity                                   ;; Hydraulic features of the cell. They change with flow. 
  driftHourlyCellTotal                                      ;; Drift food production rate in the cell. It changes with hydraulic features (and hence with flow).
  searchHourlyCellTotal                                     ;; Search food production rate in the cell
  
  ;; Hydraulic variables used when hydraulycs are modelled witihn the model
  depth-lookup                                              ;; Lookup list for depth
  velocity-lookup                                           ;; Lookup list for velocity
  flow-at-wetting                                           ;; Lowest flow at which cell is wet
 ]  
 
trout-own 
 [
   my-cell                                  ;; Cell where the trout is currently located
   sex                                      ;; "M" if male/"F" if female
   age  age-class                           ;; Fish age (in days) and age-class (Age0 = 0, Age1, Age2, Age3, Age4 and Age5Plus = 5)
   fishLength  fishWeight  fishCondition    ;; Body size variables
   fishMaxSwimSpeed                         ;; Maximum sustainable swimming speed
   cMax                                     ;; Physiological maximum daily food consumption
   energyAvailableforGrowth                 ;; Food energy that the trout will consume in my-cell during the time step
   is-sheltered?  is-covered?               ;; True if the trout has access to a velocity shelter/cover element
   status                                   ;; "Alive"/"Dead"
   CauseOfDeath                             ;; Records the cause of death of the fish
   
   maturityStatus                           ;; "Mature" if the fish is over the phenotypic length maturity threshold. "Non-mature" otherwise. 
   spawnedThisSeason?                       ;; True if the trout has already spawned during the spawning season
   
   fishSpawnMinLength                       ;; Phenotypic length maturity threshold (minimum length to spawn)
   fishNewLength                            ;; Phenotypic length at emergence
   fishNeutralTrait                         ;; Phenotypic value of the neutral trait
   genSpawnMinLength                        ;; Genetic length maturity threshold (minimum length to spawn)
   genNewLength                             ;; Genetic length at emergence 
   genNeutralTrait                          ;; Genetic value of the neutral trait
 ]     
 
redds-own
 [
  ReddID                                       ;; Identification number of redd
  creationDate                                 ;; Date when the redd was created
  my-cell                                      ;; Cell where the redd is currently located
  ininumberOfEggs                              ;; Number of viable eggs in the redd when it was created
  numberOfEggs                                 ;; Number of viable eggs in the redd
  reddFathersgenNewlength                      ;; List containing the genetic length at emergence of male spawners fertilizing the eggs
  reddMothergenNewlength                       ;; Genetic length at emergence  of the female spawner that created the redd
  reddFathersgenSpawnMinLength                 ;; List containing the genetic length maturity threshold of male spawners fertilizing the eggs
  reddMothergenSpawnMinLength                  ;; Genetic length maturity threshold of the female spawner that created the redd
  reddFathersgenNeutralTrait                   ;; List containing the genetic value of neutral trait of male spawners fertilizing the eggs
  reddMothergenNeutralTrait                    ;; Genetic value of neutral trait of the female spawner that created the redd
  fracDeveloped                                ;; Developmental status of a redd’s eggs
  days-after-hatch                             ;; Number of days since emergence starts in the redd
  
  eggsLostToLowTempTot  eggsLostToHighTempTot  ;; Number of eggs lost by each source of mortality
  eggsLostToScourTot  eggsLostToDewateringTot  
  eggsLostToSuperimpTot  
  numberOfHatchedEggs                          ;; Number of eggs hatched (new created trout)
 ] 




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SETUP PROCEDURE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

__includes ["Parameters setup - Fishing.nls"] 

to setup
  ca
  reset-ticks
  reset-timer
;  profiler:start  ;; Start profiling
  
;  show "Resizing world"
;  resize-world 0 500 0 3000
  
  show "Setting parameter values"
  setparameters   ;; Set values for all global parameters
  
  read-input-files
  
  setup-reach
  
  setup-cells
  
  setup-population
  
  create-output-files
  
  set MaleSpawnersLength [] set FemaleSpawnersLength [] set SpawningDate [] set MaleSpawnersAge [] 
  set FemaleSpawnersAge [] set EmergenceDate [] set AgeatDeath []
  set MaleBreedGenMinSpawnLength [] set FemaleBreedGenMinSpawnLength [] set GenBreedEmergenceLength [] set GenBreedNeutralTrait []

  show "End of setup"

; profiler:stop          ;; stop profiling
; print profiler:report  ;; view the results
; profiler:reset         ;; clear the data
  
end ;; of setup procedure  


;;;;;;;;;;;;;;;;;;;; READ INPUT FILES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to read-input-files

  show "Reading environmental variables"
  set environmental-time-series time:ts-load "environmentTimeseries.txt" ;; Load environmental time series data from a text input file and store flow and temperature values in a LogoTimeSeries object.
  ;; to define simulation-length
  file-open "environmentTimeseries.txt" let head-string "a string" set head-string file-read-line 
  let column-list [] while [not file-at-end?] [let column1 file-read-line set column-list sentence column-list column1] file-close  set simulation-length length column-list



  show "Reading cell variables"
  ;; Create an array with the cell's 5 fixed habitat properties (3 habitat features + 2 identity features), for every cell, from an text file
  let habitat-list []
  file-open "cellshabitat.txt" 
  while [not file-at-end?]         
  [
    let next-transect file-read
    let next-cell file-read
    let next-shelter file-read
    let next-cover file-read
    let next-gravel file-read
    set habitat-list sentence habitat-list (list(list next-transect next-cell next-shelter next-cover next-gravel))
  ]
  file-close
  set habitat-features array:from-list habitat-list
  if ModelHydraulics?
   [
     ; Create the table linking cell numbers to patches
     set cell-ID-table table:make
     let n n-values (array:length habitat-features) [?]
     foreach n [table:put cell-ID-table (item 1 (array:item habitat-features ?)) (item 1 (array:item habitat-features ?))]
   ]

  
  show "Reading hydraulics"
  ifelse ModelHydraulics?
   [
    ;; If ON, inSTREAM-Gen calculates values for depth and velocity from lookup tables defined in input files (Code provided by Steve Railsback)
    ;; Depth first
    
    ;; First, make sure file name and file exist
    if empty? depth-file-name [ error "Missing depth file name" ]
    if not file-exists? depth-file-name [ error (word "Depth file " depth-file-name " not found") ]
    
    ; Open file and skip the 3 header lines
    file-open depth-file-name
    let header-string "a string"
    repeat 3 [set header-string file-read-line ]
    
    ; Read in the number of flows
    let num-depth-flows first csv:from-row file-read-line
    if not is-number? num-depth-flows [ error "Error reading number of flows in depth file" ]
    
    ; Read in the flow list, and strip off any non-numbers at the beginning
    ; (Non-numbers result from blank spreadsheet cells)
    set depth-flow-list csv:from-row file-read-line
    while [not is-number? first depth-flow-list] [ set depth-flow-list but-first depth-flow-list ]
    ; show depth-flow-list
    
;    ; Read in the depth table for each cell
;    while [not file-at-end?]
;    [
;      let next-row csv:from-row file-read-line
;      let next-cell first next-row
;      ask (table:get cell-ID-table next-cell) 
;      [  set depth-lookup but-first next-row
;        ; show depth-lookup
;      ]      
;     ]
    file-close
    
    ; Velocity second
    
    ; First, make sure file name and file exist
    if empty? velocity-file-name [ error "Missing velocity file name" ]
    if not file-exists? velocity-file-name [ error (word "Velocity file " velocity-file-name " not found") ]
    
    ; Open file and skip the 3 header lines
    file-open velocity-file-name
    repeat 3 [set header-string file-read-line ]
    
    ; Read in the number of flows
    let num-vel-flows first csv:from-row file-read-line
    if not is-number? num-vel-flows [ error "Error reading number of flows in velocity file" ]
    
    ; Read in the flow list, and strip off any non-numbers at the beginning
    ; (Non-numbers result from blank spreadsheet cells)
    set velocity-flow-list csv:from-row file-read-line
    while [not is-number? first velocity-flow-list] [ set velocity-flow-list but-first velocity-flow-list ]
    ; show velocity-flow-list
    
;    ; Read in the velocity table for each cell
;    while [not file-at-end?]
;    [
;      let next-row csv:from-row file-read-line
;      let next-cell first next-row
;      ask (table:get cell-ID-table next-cell) 
;      [  set velocity-lookup but-first next-row
;        ; show velocity-lookup
;      ]     
;    ]
    file-close
   ]
   [
    ;; Otherwise, hydraulic values are read from input files       
    file-open "depthmatrix.txt"      ;; Import matrix for depth data (for every cell for the whole simulation). 
    let depth-matrix file-read 
    set depth-data matrix:from-row-list depth-matrix
    file-close
    
    file-open "velocitymatrix.txt"   ;; Import matrix for velocity data (for every cell for the whole simulation). 
    let velocity-matrix file-read 
    set velocity-data matrix:from-row-list velocity-matrix
    file-close
   ]
  
  
  
end  
  
  
;;;;;;;;;;;;;;;;;;;; SETUP REACH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

to setup-reach
   
  show "Setting up reach"
  
  ;; First it was load an environmental time series data from a text input file and store flow and temperature values in a LogoTimeSeries object.
  ;; Then, report a LogoTime object which is "anchored" to the native time tracking mechanism in NetLogo.
  ;; Once anchored, this LogoTime object will always hold the value of the current time as tracked by ticks. 
  ;; Set the initial values from the LogoTimeSeries object and calculate day length.                                                                                         
  set tick-date time:anchor-to-ticks (time:create initial-date) (timestep-scale + (remainder 365 timestep-scale / int (365 / timestep-scale))) "days"                                                                                                                                                         
  set flow time:ts-get environmental-time-series tick-date  "flow"  
  set temp time:ts-get environmental-time-series tick-date  "temp"
  let juliandate time:ts-get environmental-time-series tick-date  "juliandate"  set initial-Juliandate juliandate
  set dayLength (24 - 2 * ((12 / 180) * acos (tan (siteLatitude) * tan (23.45 * cos ((2 * 180 / 365) * (173 - juliandate))))))  ;; Calculate day length as a function of the Julian date and latitude.
                                                                                                                                ;; Formula in inSTREAM,   
                                                                                                                                ;; but here trigonometric numbers are in degrees instead of radians
end                                                                                                                               
                                                                                                                     
       
;;;;;;;;;;;;;;;;;;;; SETUP CELLS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
to setup-cells

  show "Setting up cells"
  
  ;; Assign habitat features to each cell:
  
  ;; First, an array with the cell's 5 fixed habitat properties (3 habitat features + 2 identity features), for every cell, was created from a text file
  ;; Then, resize the world as a function of cells coordinates
  let arraycoord cell-coord
  let arraylegth (array:length arraycoord)
  if item 2 (array:item arraycoord (arraylegth - 1)) <= max-pxcor or item 1 (array:item arraycoord (arraylegth - 1)) <= max-pycor
   [resize-world 0 item 2 (array:item arraycoord (arraylegth - 1)) 0 item 1 (array:item arraycoord (arraylegth - 1))] 
  
  ask patches [set patchArea (space-scale ^ 2)]   ;; each patch is 10 x 10 cm = 100 square centimeters  
  
;  ;; Finally, link each cell coordinate with its habitat and identity features. Depth and velocity are set up from their corresponding matrices.
;  ;; Shelter and cover area, and food availabiblity are calculated. Shelter, cover and area are calculated as the proportion of shelter and cover within the cell times its area.
;  ;; Finally, set color of cells (grey if depth is 0).              
;  let n n-values (array:length arraycoord) [?] 
;  foreach n
;   [ask (new-cell array:item arraycoord ?) 
;     [
;      set transect first (array:item habitat-features ?)  
;      set cellNumber item 1 (array:item habitat-features ?) 
;      set cellDepth (matrix:get depth-data 0 ?) * 100        ;;Imported in m and m/s to keep conventions from PHABSIM outputs. Then, converted to cm and cm/s to follow model conventions.
;      set cellVelocity (matrix:get velocity-data 0 ?) * 100
;      set cellFracShelter item 2 (array:item habitat-features ?)  set cellAreaShelter (cellFracShelter * cellArea) 
;      set cellFracCover item 3 (array:item habitat-features ?)  set cellAreaCover (cellFracCover * cellArea)
;      set cellFracGravel last (array:item habitat-features ?)  
;      set driftHourlyCellTotal (3600 * cellArea * cellVelocity * cellDepth * habDriftConc / habDriftRegenDist)
;      set searchHourlyCellTotal (habSearchProd * cellArea)
;      ifelse PlotCellDepth?
;       [ifelse cellDepth = 0 [set-color grey] [set-color scale-color blue cellDepth (max [cellDepth] of cells) (min [cellDepth] of cells)]]
;       [set-color scale-color blue cellDepth (max ([cellDepth] of cells) + 1) (min ([cellDepth] of cells) + 1)]
;     ]
;    ]
   
  
  ;; Link each cell coordinate with its habitat and identity features. Shelter and cover area are calculated as the proportion of shelter and cover within the cell times its area.
  let n n-values (array:length arraycoord) [?] 
  foreach n
   [ask (new-cell array:item arraycoord ?) 
     [
      set transect first (array:item habitat-features ?)  
      set cellNumber item 1 (array:item habitat-features ?)          
      set cellFracShelter item 2 (array:item habitat-features ?)  
      set cellAreaShelter (cellFracShelter * cellArea) 
      set cellFracCover item 3 (array:item habitat-features ?)  
      set cellAreaCover (cellFracCover * cellArea)
      set cellFracGravel last (array:item habitat-features ?)  
     ]
    ]
   
   ;; Hydraulic variables are set. The way it's done depends on whether they are calculated within the model (provided by S. Railsback) or imported from input files.
   ifelse ModelHydraulics? 
     [
       ;; Read in the depth table for each cell
       file-open depth-file-name
       let header-string "a string"
       repeat 5 [set header-string file-read-line ]
       while [not file-at-end?]
         [
           let next-row csv:from-row file-read-line
           let next-cell first next-row
           ask cell (table:get cell-ID-table next-cell) 
             [set depth-lookup but-first next-row]
         ]  
       file-close
       
       ;; Read in the velocity table for each cell
       file-open velocity-file-name    
       repeat 5 [set header-string file-read-line ]    
       while [not file-at-end?]
         [
          let next-row2 csv:from-row file-read-line
          let next-cell2 first next-row2
          ask cell (table:get cell-ID-table next-cell2) 
           [set velocity-lookup but-first next-row2]
          ]
       file-close
       
       ;; Set the minimum flow at which cell is wet. Finally, calculate the depth and velocity from the lookup tables.
       ask cells 
        [
          min-flow-wetted
          update-hydraulics-for flow
        ]         
     ]
     [
       ;; Depth and velocity are set up from their corresponding matrices
       ask cells
        [
         set cellDepth (matrix:get depth-data 0 who) * 100       ;;Imported in m and m/s to keep conventions from PHABSIM outputs. Then, converted to cm and cm/s to follow model conventions.
         set cellVelocity (matrix:get velocity-data 0 who) * 100
        ]
     ]
      
   ;; Food availabiblity is calculated. Finally, set color of cells (grey if depth is 0)
   ask cells
    [
     set driftHourlyCellTotal (3600 * cellArea * cellVelocity * cellDepth * habDriftConc / habDriftRegenDist)
     set searchHourlyCellTotal (habSearchProd * cellArea)
     ifelse PlotCellDepth?
       [ifelse cellDepth = 0 [set-color grey] [set-color scale-color blue cellDepth (max [cellDepth] of cells) (min [cellDepth] of cells)]]
       [set-color scale-color blue cellDepth (max ([cellDepth] of cells) + 1) (min ([cellDepth] of cells) + 1)]
    ]    
          
         
        
   ;; Assign to every cell the average distance a fish located in the cell would need to move to find hiding cover. 
   ;; In a cell with cover, it is the average distance within the area with cover (0) plus the average distance within the area without cover (represented as a circunference).
   ;; In a cell without cover is the sum of the distance to the closest covered cell plus the average distance within the area without cover of the closest covered cell.
   ;; Cover is only available if the covered cell is not dry.
   ask cells [set cellDistanceToHide 1000]   ;; At this distance, the increase in survival probability for terrestrial predation is zero
   let cellAreaNoCover 0
   let cellsWithCover cells with [cellFracCover > 0 and cellDepth > 0]
   ask cellsWithCover [set cellAreaNoCover (1 - cellFracCover) * cellArea  set cellDistanceToHide (1 - cellFracCover) * sqrt (cellAreaNoCover / pi)]
   let distanceCellsWithCover 0
   let myClosestCoveredCell []
   ask cells with [cellFracCover = 0 and cellDepth > 0] 
    [
     set myClosestCoveredCell min-one-of cellsWithCover [distance myself]
     if myClosestCoveredCell != nobody
     [
       set distanceCellsWithCover (distance myClosestCoveredCell) * space-scale 
       set cellDistanceToHide (distanceCellsWithCover + [cellDistanceToHide] of myClosestCoveredCell)
     ]    
    ]    
   
   ;; Calculate the adjacent cells for every cell
   ask cells 
    [
     let my-SideadjacentCells other cells with [transect = [transect] of myself and cellNumber >= ([cellNumber] of myself - 1) and cellNumber <= ([cellNumber] of myself + 1)]
     let my-LoweradjacentCells cells with [transect = ([transect] of myself - 1) and x-min-for self <= (x-max-for myself + 1) and x-max-for self >= (x-min-for myself - 1)]
     let my-UpperadjacentCells cells with [transect = ([transect] of myself + 1) and x-min-for self <= (x-max-for myself + 1) and x-max-for self >= (x-min-for myself - 1)]  
     set my-adjacentCells (turtle-set my-SideadjacentCells my-LoweradjacentCells my-UpperadjacentCells)
    ]      
    
   set wettedArea sum [cellArea] of cells with [cellDepth > 0]    ;; Calculate the reach's wetted area

end


;; Set the minimum flow at which the cell is wet (Code provided by Steve Railsback)
to min-flow-wetted
  
  ifelse first depth-lookup > 0.0  ; Depth is non-zero at lowest lookup table flow
    [
      set flow-at-wetting 0.0
    ]
    [ ; Depth is zero at lowest lookup-table flow
      ifelse last depth-lookup <= 0.0  ; The last value in lookup table is zero
      [
        set flow-at-wetting 999999
      ]
      [ ; Last value in table is non-zero
        ifelse item (length depth-flow-list - 2) depth-lookup > 0.0 ; The second-to-last value is also non-zero
        [ ; The second-to-last value is also non-zero, so do interpolation
          let d-index-low 0
          while [ item d-index-low depth-lookup <= 0 ] ; Find the first non-zero depth
          [ set d-index-low d-index-low + 1 ]
          
          let d-index-high d-index-low + 1
          let Q1 item d-index-low depth-flow-list
          let Q2 item d-index-high depth-flow-list
          let D1 item d-index-low depth-lookup
          let D2 item d-index-high depth-lookup
          
          let slope (D2 - D1) / (Q2 - Q1)
          let y-intercept D2 - (slope * Q2)
          ifelse y-intercept < 0
          [ ; y-intercept is negative, so depth goes to zero before flow does
            set flow-at-wetting (-1 * y-intercept / slope)
          ]
          [ ; y-intercept is positive so cell is extrapolated to have depth > 0 at zero flow.
            set flow-at-wetting 0.0
          ]
          
          ; Now check whether flow-at-wetting is less than the highest lookup-table flow with non-zero depth
          let Q0 item (d-index-low - 1) depth-flow-list
          if flow-at-wetting < Q0 [ set flow-at-wetting Q0 ]
        ] ; Do interpolation 
        [ ; If there is depth only at highest flow, flow-at-wetting is halfway between it and next-lowest flow
          set flow-at-wetting ((last depth-flow-list) + (item (length depth-flow-list - 2) depth-flow-list)) / 2
        ]
        
      ] ; Last value in table is non-zero
    ] ; Depth is zero at lowest lookup-table flow
    
end


;;;;;;;;;;;;;;;;;;;; SETUP TROUT POPULATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-population

  show "Creating population"
  
  ;; Create the trout and locate them in a random wetted cell with low probability of high velocity mortality 
  create-trout init-N 
   [
    ifelse random-float 1 > 0.5 [set sex "M"] [set sex "F"]   
    
    ;; Attribute trout age according to the defined proportion [copied from DemGenTrout 1.0 (Frank 2012)]. 
    ;; Date of birth is randomly set within ± 10 days around mean date of emergence. 
    let maxCell max [who] of cells    
    let juliandate time:ts-get environmental-time-series tick-date  "juliandate"
    ifelse who <= (maxCell + init-N * prop-Age0) [set age-class 0 set age (juliandate - (fishEmergenceDateMean + (-10 + random 21)))]  
    [ifelse who <= (maxCell + init-N * (prop-Age0 + prop-Age1)) [set age-class 1 set age (365 + juliandate - (fishEmergenceDateMean + (-10 + random 21)))]
    [ifelse who <= (maxCell + init-N * (prop-Age0 + prop-Age1 + prop-Age2)) [set age-class 2 set age ((2 * 365) + (juliandate - (fishEmergenceDateMean + (-10 + random 21))))]
    [ifelse who <= (maxCell + init-N * (prop-Age0 + prop-Age1 + prop-Age2 + prop-Age3)) [set age-class 3 set age ((3 * 365) + (juliandate - (fishEmergenceDateMean + (-10 + random 21))))]
    [ifelse who <= (maxCell + init-N * (prop-Age0 + prop-Age1 + prop-Age2 + prop-Age3 + prop-Age4)) [set age-class 4 set age ((4 * 365) + (juliandate - (fishEmergenceDateMean + (-10 + random 21))))]
    [set age-class 5 set age (((5 + random 3) * 365) + (juliandate - (fishEmergenceDateMean + (-10 + random 21))))]]]]]
    
    ;; Initial length is randomly drawn from a normal distribution, though it is truncated at 4 standard deviations (following Frank & Baret 2013) 
    if age-class = 0 
     [set fishLength (random-normal fishLengthMeanAge0 fishLengthSdAge0) 
      if fishLength < (fishLengthMeanAge0 - 4 * fishLengthSdAge0) [set fishLength (fishLengthMeanAge0 - 4 * fishLengthSdAge0)]
      if fishLength > (fishLengthMeanAge0 + 4 * fishLengthSdAge0) [set fishLength (fishLengthMeanAge0 + 4 * fishLengthSdAge0)]
      if fishLength < fishMinNewLength [set fishLength fishMinNewLength]]
    if age-class = 1 
     [set fishLength (random-normal fishLengthMeanAge1 fishLengthSdAge1) 
      if fishLength < (fishLengthMeanAge1 - 4 * fishLengthSdAge1) [set fishLength (fishLengthMeanAge1 - 4 * fishLengthSdAge1)]
      if fishLength > (fishLengthMeanAge1 + 4 * fishLengthSdAge1) [set fishLength (fishLengthMeanAge1 + 4 * fishLengthSdAge1)]] 
    if age-class = 2 
     [set fishLength (random-normal fishLengthMeanAge2 fishLengthSdAge2) 
      if fishLength < (fishLengthMeanAge2 - 4 * fishLengthSdAge2) [set fishLength (fishLengthMeanAge2 - 4 * fishLengthSdAge2)]
      if fishLength > (fishLengthMeanAge2 + 4 * fishLengthSdAge2) [set fishLength (fishLengthMeanAge2 + 4 * fishLengthSdAge2)]]
    if age-class = 3 
     [set fishLength (random-normal fishLengthMeanAge3 fishLengthSdAge3) 
      if fishLength < (fishLengthMeanAge3 - 4 * fishLengthSdAge3) [set fishLength (fishLengthMeanAge3 - 4 * fishLengthSdAge3)]
      if fishLength > (fishLengthMeanAge3 + 4 * fishLengthSdAge3) [set fishLength (fishLengthMeanAge3 + 4 * fishLengthSdAge3)]]
    if age-class = 4 
     [set fishLength (random-normal fishLengthMeanAge4 fishLengthSdAge4) 
      if fishLength < (fishLengthMeanAge4 - 4 * fishLengthSdAge4) [set fishLength (fishLengthMeanAge4 - 4 * fishLengthSdAge4)]
      if fishLength > (fishLengthMeanAge4 + 4 * fishLengthSdAge4) [set fishLength (fishLengthMeanAge4 + 4 * fishLengthSdAge4)]]
    if age-class = 5 
     [set fishLength (random-normal fishLengthMeanAge5Plus fishLengthSdAge5Plus) 
      if fishLength < (fishLengthMeanAge5Plus - 4 * fishLengthSdAge5Plus) [set fishLength (fishLengthMeanAge5Plus - 4 * fishLengthSdAge5Plus)]
      if fishLength > (fishLengthMeanAge5Plus + 4 * fishLengthSdAge5Plus) [set fishLength (fishLengthMeanAge5Plus + 4 * fishLengthSdAge5Plus)]]
    set fishWeight (fishWeightParamA * fishLength ^ fishWeightParamB)
    set fishCondition (fishWeight / (fishWeightParamA * fishLength ^ fishWeightParamB))  if fishCondition > 1 [set fishCondition 1]
    set fishMaxSwimSpeed (((fishMaxSwimParamA * fishLength) + fishMaxSwimParamB) * ((fishMaxSwimParamC * temp ^ 2) + (fishMaxSwimParamD * temp) + fishMaxSwimParamE))
    set status "alive"
    set is-sheltered? false 
   
    ;; Set heritable traits: 
    ;; the genotypic value is drawn from a normal distribution centered on the initial population mean phenotypic value and variance equal to the additive genetic variance at the population level.
    ;; The additive genetic variance is calculated as the product between the initial population phenotypic variance and the narrow-sense heritability of the trait.
    ;; Genotypic values are truncated at 4 standard deviations from the mean value defining the normal distribution (genotypic length at emergence cannot be below a user-defined threshold).
    ;; Finally, the individual's phenotypic value of the trait is defined as the sum of its genotypic value and a statistically independent random environmental effect.
    ;; If there is no genetic transmission of traits, then heritability is set to 0.
    if GeneticTransmission? = false [set fishNewLengthHeritability 0 set fishNeutralTraitHeritability 0 set fishSpawnMinLengthHeritability 0]
    set genNewLength random-normal fishNewLengthMean sqrt (fishNewLengthVar * fishNewLengthHeritability)
    if genNewLength < (fishNewLengthMean - 4 * (sqrt (fishNewLengthVar * fishNewLengthHeritability))) [set genNewLength (fishNewLengthMean - 4 * (sqrt (fishNewLengthVar * fishNewLengthHeritability)))]
    if genNewLength > (fishNewLengthMean + 4 * (sqrt (fishNewLengthVar * fishNewLengthHeritability))) [set genNewLength (fishNewLengthMean + 4 * (sqrt (fishNewLengthVar * fishNewLengthHeritability)))]
    if genNewLength < fishMinNewLength [set genNewLength fishMinNewLength]
    set fishNewLength (genNewLength + (environ-value-for fishNewLengthVar fishNewLengthHeritability))
    set genNeutralTrait random-normal fishNeutralTraitMean sqrt (fishNeutralTraitVar * fishNeutralTraitHeritability)
    if genNeutralTrait < (fishNeutralTraitMean - 4 * (sqrt (fishNeutralTraitVar * fishNeutralTraitHeritability))) [set genNeutralTrait (fishNeutralTraitMean - 4 * (sqrt (fishNeutralTraitVar * fishNeutralTraitHeritability)))]
    if genNeutralTrait > (fishNeutralTraitMean + 4 * (sqrt (fishNeutralTraitVar * fishNeutralTraitHeritability))) [set genNeutralTrait (fishNeutralTraitMean + 4 * (sqrt (fishNeutralTraitVar * fishNeutralTraitHeritability)))]
    set fishNeutralTrait (genNeutralTrait + (environ-value-for fishNeutralTraitVar fishNeutralTraitHeritability))
    ifelse sex = "M"
     [
      set genSpawnMinLength random-normal fishSpawnMinLengthMeanM sqrt (fishSpawnMinLengthVarM * fishSpawnMinLengthHeritability)
      if genSpawnMinLength < (fishSpawnMinLengthMeanM - 4 * (sqrt (fishSpawnMinLengthVarM * fishSpawnMinLengthHeritability))) [set genSpawnMinLength (fishSpawnMinLengthMeanM - 4 * (sqrt (fishSpawnMinLengthVarM * fishSpawnMinLengthHeritability)))]
      if genSpawnMinLength > (fishSpawnMinLengthMeanM + 4 * (sqrt (fishSpawnMinLengthVarM * fishSpawnMinLengthHeritability))) [set genSpawnMinLength (fishSpawnMinLengthMeanM + 4 * (sqrt (fishSpawnMinLengthVarM * fishSpawnMinLengthHeritability)))]
      set fishSpawnMinLength (genSpawnMinLength + (environ-value-for fishSpawnMinLengthVarM fishSpawnMinLengthHeritability))
     ]
     [
      set genSpawnMinLength random-normal fishSpawnMinLengthMeanF sqrt (fishSpawnMinLengthVarF * fishSpawnMinLengthHeritability)
      if genSpawnMinLength < (fishSpawnMinLengthMeanF - 4 * (sqrt (fishSpawnMinLengthVarF * fishSpawnMinLengthHeritability))) [set genSpawnMinLength (fishSpawnMinLengthMeanF - 4 * (sqrt (fishSpawnMinLengthVarF * fishSpawnMinLengthHeritability)))]
      if genSpawnMinLength > (fishSpawnMinLengthMeanF + 4 * (sqrt (fishSpawnMinLengthVarF * fishSpawnMinLengthHeritability))) [set genSpawnMinLength (fishSpawnMinLengthMeanF + 4 * (sqrt (fishSpawnMinLengthVarF * fishSpawnMinLengthHeritability)))]
      set fishSpawnMinLength (genSpawnMinLength + (environ-value-for fishSpawnMinLengthVarF fishSpawnMinLengthHeritability))
     ]     
    
    ;; Assign maturity status based on body size and age
    ifelse fishLength >= fishSpawnMinLength and age >= fishSpawnMinAge [set maturityStatus "mature"] [set maturityStatus "non-mature"] 
    set spawnedThisSeason? false
    
    ;; Locate the trout in a low risk habitat
    let lowriskcells cells with [cellDepth > 0 and (cellVelocity / [fishMaxSwimSpeed] of myself) < mortFishVelocityV9] 
    ifelse any? lowriskcells
     [set my-cell one-of lowriskcells move-to one-of [my-patches] of my-cell]
     [set my-cell one-of cells with [cellDepth > 0] move-to one-of [my-patches] of my-cell] 
    ifelse age-class = 0 [set color yellow set size 7] [set color red set size 10]   
   ]   

end   


;;;;;;;;;;;;;;;;;;;; SETUP OUTPUT FILES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to create-output-files

  show "Creating output files"

  if LiveFishOutput?
   [
    let output-file-name (word "LiveFishOutput-" behaviorspace-run-number ".csv")
    if (file-exists? output-file-name) [carefully [file-delete output-file-name] [print error-message]]
    file-open output-file-name
     file-type "Date,"  
     file-type "Year," 
     file-type "Age-class,"   
     file-type "Count,"
     file-type "CountMature,"   
     file-type "MeanAge,"
     file-type "StdAge,"     
     file-type "MeanLength,"
     file-type "StdLength,"
     file-type "TotalWeight,"
     file-type "MeanWeight,"
     file-type "StdWeight,"    
     file-type "MeanCondition,"
     file-type "StdCondition,"
     file-type "MeanPhenMinSpawnLengthMales,"
     file-type "StdPhenMinSpawnLengthMales,"
     file-type "MeanPhenMinSpawnLengthFemales,"
     file-type "StdPhenMinSpawnLengthFemales,"     
     file-type "MeanPhenEmergenceLength,"
     file-type "StdPhenEmergenceLength,"   
     file-type "MeanPhenNeutralTrait,"                                          
     file-type "StdPhenNeutralTrait," 
     file-type "MeanGenMinSpawnLengthMales,"
     file-type "StdGenMinSpawnLengthMales,"
     file-type "MeanGenMinSpawnLengthFemales,"
     file-type "StdPGenMinSpawnLengthFemales,"     
     file-type "MeanGenEmergenceLength,"
     file-type "StdPGenEmergenceLength,"   
     file-type "MeanGenNeutralTrait,"                                          
     file-print "StdGennNeutralTrait"
    file-close 
   ]         
  
  if DeadFishOutput?
   [
    let output-file-name (word "DeadFishOutput-" behaviorspace-run-number ".csv")
    if (file-exists? output-file-name) [carefully [file-delete output-file-name] [print error-message]]
    file-open output-file-name
     file-type "Date,"   
     file-type "Year,"
     file-type "Age-class,"   
     file-type "CauseOfDeath,"                                                       
     file-print "Count" 
    file-close        
   ]    

  if ReddOutput?
   [
    let output-file-name (word "ReddOutput-" behaviorspace-run-number ".csv")
    if (file-exists? output-file-name) [carefully [file-delete output-file-name] [print error-message]]
    file-open output-file-name
     file-type "ReddID,"   
     file-type "CellID,"   
     file-type "CreateDate,"   
     file-type "InitialViableEggs,"   
     file-type "EmptyDate,"   
     file-type "EmptyYear,"
     file-type "LowTemp,"   
     file-type "HighTemp,"   
     file-type "Scouring,"   
     file-type "Dewatering,"   
     file-type "Superimp,"                                                    
     file-print "FryEmerged" 
    file-close    
   ]        

  if BreedersOutput?
   [
    let output-file-name (word "BreedersPopOutput-" behaviorspace-run-number ".csv")
    if (file-exists? output-file-name) [carefully [file-delete output-file-name] [print error-message]]
    file-open output-file-name
     file-type "Date,"
     file-type "SpawningYear,"
     file-type "NFemaleSpawners,"
     file-type "NMaleSpawners,"
     file-type "SpawningLengthFemMean,"
     file-type "SpawningLengthFemSd,"
     file-type "SpawningLengthFemMin,"
     file-type "SpawningLengthMalMean,"
     file-type "SpawningLengthMalSd,"
     file-type "SpawningLengthMalMin,"
     file-type "SpawningAgeFemMean,"
     file-type "SpawningAgeFemSd,"
     file-type "SpawningAgeFemMin,"
     file-type "SpawningAgeMalMean,"
     file-type "SpawningAgeMalSd,"
     file-type "SpawningAgeMalMin,"
     file-type "SpawningDateMean,"
     file-type "SpawningDateSd,"
     file-type "SpawningDateMin,"
     file-type "GenMinSpawnLengthFemMean,"
     file-type "GenMinSpawnLengthFemSd,"
     file-type "GenMinSpawnLengthFemMin,"
     file-type "GenMinSpawnLengthMalMean,"
     file-type "GenMinSpawnLengthMalSd,"
     file-type "GenMinSpawnLengthMalMin,"
     file-type "GenEmergenceLengthMean,"
     file-type "GenEmergenceLengthSd,"
     file-type "GenEmergenceLengthMin,"
     file-type "GenNeutralTraitMean,"
     file-type "GenNeutralTraitSd,"
     file-type "GenNeutralTraitMin,"
     file-type "OffspringEmergDateMean,"
     file-type "OffspringEmergDateSd,"
     file-print "OffspringEmergDateMin"                                                  
    file-close  
   ]

  if BreedersOutput?
   [
    let output-file-name (word "BreedersIndOutput-" behaviorspace-run-number ".csv")
    if (file-exists? output-file-name) [carefully [file-delete output-file-name] [print error-message]]
    file-open output-file-name
     file-type "SpawningDate,"   
     file-type "SpawningYear,"
     file-type "SpawningJulianDate,"
     file-type "ReddID,"  
     file-type "TroutID," 
     file-type "Sex,"   
     file-type "Age-class,"   
     file-type "Age,"
     file-type "Length,"   
     file-type "Weight,"   
     file-type "PhenMinSpawnLength,"  
     file-type "PhenEmergenceLength,"   
     file-type "PhenNeutralTrait,"   
     file-type "GenMinSpawnLength,"   
     file-type "GenEmergenceLength,"                                                 
     file-print "GenNeutralTrait" 
    file-close  
   ]
    
  if HabSelecOutput?
   [
    let output-file-name (word "HabSelecOutput-" behaviorspace-run-number ".csv")
    if (file-exists? output-file-name) [carefully [file-delete output-file-name] [print error-message]]
    file-open output-file-name
     file-type "Date,"   
     file-type "Year,"
     file-type "JulianDate,"
     file-type "Reachflow,"
     file-type "ReachTemp,"
     file-type "CellID,"  
     file-type "Area," 
     file-type "Depth,"   
     file-type "Velocity,"   
     file-type "FracShelter,"
     file-type "FracCover,"   
     file-type "DistanceToHide,"   
     file-type "DriftFoodProduction,"   
     file-type "SearchFoodProduction,"   
     file-type "CountAge0,"   
     file-type "CountAge1,"   
     file-type "CountAge2,"
     file-type "CountAge3Plus,"                                                 
     file-print "CountReachTot" 
    file-close             
   ]            
   
  if MortAnglingHooking and AnglingOutput?
   [
    let output-file-name (word "AnglingStatsOutput-" behaviorspace-run-number ".csv")
    if (file-exists? output-file-name) [carefully [file-delete output-file-name] [print error-message]]
    file-open output-file-name
     file-type "Date,"   
     file-type "Year,"
     file-type "FishingPressure,"
     file-type "ExploitationRate,"
     file-type "FishingStock,"
     file-type "ProjectedHarvest,"  
     file-type "HarvestTotal," 
     file-type "HarvestAge0,"   
     file-type "HarvestAge1,"   
     file-type "HarvestAge2,"
     file-type "HarvestAge3Plus,"   
     file-type "RealExploitRate,"   
     file-type "DeadHookingTotal,"   
     file-type "DeadHookingAge0,"   
     file-type "DeadHookingAge1,"
     file-type "DeadHookingAge2,"                                                 
     file-print "DeadHookingAge3Plus" 
    file-close            
    
    
    let output-file-name-2 (word "AnglingPopulationOutput-" behaviorspace-run-number ".csv")
    if (file-exists? output-file-name-2) [carefully [file-delete output-file-name-2] [print error-message]]
    file-open output-file-name-2
     file-type "Date,"   
     file-type "Year,"
     file-type "CountAge0,"   
     file-type "CountAge1,"   
     file-type "CountAge2,"
     file-type "CountAge3Plus,"  
     file-type "MeanLengthAge0,"
     file-type "MeanLengthAge1,"
     file-type "MeanLengthAge2,"
     file-type "MeanLengthAge3Plus,"
     file-type "MeanWeightAge0,"
     file-type "MeanWeightAge1,"
     file-type "MeanWeightAge2,"
     file-type "MeanWeightAge3Plus,"
     file-type "TotalWeightAge0,"
     file-type "TotalWeightAge1,"
     file-type "TotalWeightAge2,"
     file-type "TotalWeightAge3Plus,"
     file-type "WeightReachTot,"                                               
     file-type "CountReachTot,"
     file-type "MeanAgeAge3Plus,"
     file-type "YearSpawningSeason,"
     file-type "Nspawners,"
     file-type "SpawningLengthFemMean,"
     file-type "SpawningLengthMalMean,"
     file-type "SpawningAgeFemMean,"
     file-type "SpawningAgeMalMean,"
     file-type "SpawningDateMean,"
     file-type "GenMinSpawnLengthFemMean,"
     file-type "GenMinSpawnLengthMalMean,"
     file-type "GenEmergenceLengthMean,"
     file-type "GenNeutralTraitMean,"
     file-type "OffspringEmergDateMean,"
     file-type "ReachInitialNeggs,"                                                    
     file-print "ReachNfryEmerged"      
    file-close                 
   ]             

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RESET PROCEDURE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

to reset
  
  clear-turtles
  clear-output
  clear-plot
  
  reset-ticks
  reset-timer
;  profiler:start  ;; Start profiling

  file-close ; Just in case a file got left open
  
  show "Setting parameter values"
  setparameters   ;; Set values for all global parameters
  
  setup-reach
  
  setup-cells
  
  setup-population
  
  create-output-files
  
  clear-counters

  show "End of reset"

; profiler:stop          ;; stop profiling
; print profiler:report  ;; view the results
; profiler:reset         ;; clear the data
  
end ;; of reset procedure  


to clear-counters

  set fishDeadHighTempTot 0 set fishDeadHighVelTot 0 set fishDeadStrandTot 0 set fishDeadPoorCondTot 0               
  set fishDeadTerrPredTot 0 set fishDeadAqPredTot 0 set fishDeadAnglingTot 0 set fishDeadHookingTot 0
  set fishDeadHighTempAge0 0 set fishDeadHighVelAge0 0 set fishDeadStrandAge0 0 set fishDeadPoorCondAge0 0             
  set fishDeadTerrPredAge0 0 set fishDeadAqPredAge0 0 set fishDeadAnglingAge0 0 set fishDeadHookingAge0 0
  set fishDeadHighTempAge1 0 set fishDeadHighVelAge1 0 set fishDeadStrandAge1 0 set fishDeadPoorCondAge1 0           
  set fishDeadTerrPredAge1 0 set fishDeadAqPredAge1 0 set fishDeadAnglingAge1 0 set fishDeadHookingAge1 0
  set fishDeadHighTempAge2 0 set fishDeadHighVelAge2 0 set fishDeadStrandAge2 0 set fishDeadPoorCondAge2 0           
  set fishDeadTerrPredAge2 0 set fishDeadAqPredAge2 0 set fishDeadAnglingAge2 0 set fishDeadHookingAge2 0
  set fishDeadHighTempAge3Plus 0 set fishDeadHighVelAge3Plus 0 set fishDeadStrandAge3Plus 0 set fishDeadPoorCondAge3Plus 0           
  set fishDeadTerrPredAge3Plus 0 set fishDeadAqPredAge3Plus 0 set fishDeadAnglingAge3Plus 0 set fishDeadHookingAge3Plus 0
  set fishDeadAnglingSeason 0 set fishDeadHookingSeason 0 set fishDeadAnglingSeasonAge0 0 set fishDeadHookingSeasonAge0 0           
  set fishDeadAnglingSeasonAge1 0 set fishDeadHookingSeasonAge1 0 set fishDeadAnglingSeasonAge2 0 set fishDeadHookingSeasonAge2 0
  set fishDeadAnglingSeasonAge3Plus 0 set fishDeadHookingSeasonAge3Plus 0
  
  set ReachNumberOfEggs 0 set ReachIniNumberOfEggs 0 set ReachNumberFryHatched 0
  
  set MaleSpawnersLength [] set FemaleSpawnersLength [] set SpawningDate [] set MaleSpawnersAge [] 
  set FemaleSpawnersAge [] set EmergenceDate [] set AgeatDeath []
  set MaleBreedGenMinSpawnLength [] set FemaleBreedGenMinSpawnLength [] set GenBreedEmergenceLength [] set GenBreedNeutralTrait []
  
end


;;;;;;;;;;;;;;;;;;;; CALIBRATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to calibrate
  
  setup
  
  show "Setting calibrate procedure"
  set calibration?  true 
  set TScountAge0 [] set TScountAge1 [] set TScountAge2 []  set TScountAge3Plus []
  set TSbiomassAge0 [] set TSbiomassAge1 [] set TSbiomassAge2 []  set TSbiomassAge3Plus []
  set TSlengthAge0 [] set TSlengthAge1 [] set TSlengthAge2 []  set TSlengthAge3Plus []
  set SSSEcountAge0 [] set SSSEcountAge1 [] set SSSEcountAge2 [] set SSSEcountAge3Plus []                                 
  set SSSEbiomassAge0 [] set SSSEbiomassAge1 [] set SSSEbiomassAge2 [] set SSSEbiomassAge3Plus []                         
  set SSSElengthAge0 [] set SSSElengthAge1 [] set SSSElengthAge2 [] set SSSElengthAge3Plus []         
  
  file-open "CalDataTS.txt"               ;; Import time series observed population patterns 
  while [not file-at-end?]
  [
    let countAge0 file-read
    let countAge1 file-read
    let countAge2 file-read
    let countAge3Plus file-read
    let biomassAge0 file-read
    let biomassAge1 file-read
    let biomassAge2 file-read
    let biomassAge3Plus file-read
    let lengthAge0 file-read
    let lengthAge1 file-read
    let lengthAge2 file-read
    let lengthAge3Plus file-read
    set TScountAge0 lput countAge0 TScountAge0  
    set TScountAge1 lput countAge1 TScountAge1
    set TScountAge2 lput countAge2 TScountAge2
    set TScountAge3Plus lput countAge3Plus TScountAge3Plus
    set TSbiomassAge0 lput biomassAge0 TSbiomassAge0
    set TSbiomassAge1 lput biomassAge1 TSbiomassAge1
    set TSbiomassAge2 lput biomassAge2 TSbiomassAge2
    set TSbiomassAge3Plus lput biomassAge3Plus TSbiomassAge3Plus
    set TSlengthAge0 lput lengthAge0 TSlengthAge0 
    set TSlengthAge1 lput lengthAge1 TSlengthAge1
    set TSlengthAge2 lput lengthAge2 TSlengthAge2
    set TSlengthAge3Plus lput lengthAge3Plus TSlengthAge3Plus
   ]
  file-close
  
  file-open "experiment-plan-calibration.txt"                           ;; Import experimental design 
  let calibration-matrix file-read 
  set calibration-data matrix:from-row-list calibration-matrix
  file-close
  
  set habDriftConc         (matrix:get calibration-data expindex 0)
  set habDriftRegenDist    (matrix:get calibration-data expindex 1)
  set habSearchProd        (matrix:get calibration-data expindex 2)
  set habPreyEnergyDensity (matrix:get calibration-data expindex 3)
  set mortFishTerrPredMin  (matrix:get calibration-data expindex 4)
  set mortFishAqPredMin    (matrix:get calibration-data expindex 5)
  
  set Plotting? false  set PlotCellDepth? false
  set AnnualFishOutput? false  set LiveFishOutput? false  set DeadFishOutput? false  set ReddOutput? false  set BreedersOutput? false  set HabSelecOutput? false
  
  show "End of calibrate"
  
end


 
;;;;;;;;;;;;;;;;;;;; ANGLING SIMULATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-angling-simulation
  
  show "Setting up angling simulation"

  setup
  

  file-open "experiment-plan-angling-simulation.txt"                            ;; Import experimental design  
  let angling-simulation-matrix file-read 
  set angling-simulation-data matrix:from-row-list angling-simulation-matrix
  file-close
  
  set FixedAnglePressure?     (matrix:get angling-simulation-data expindex 0)
  set anglePressure           (matrix:get angling-simulation-data expindex 1)
  set exploitationRate        (matrix:get angling-simulation-data expindex 2)
  set mortFishAngleSlotLower  (matrix:get angling-simulation-data expindex 3)
  set mortFishAngleSlotUpper  (matrix:get angling-simulation-data expindex 4)
  
  set Plotting? false  set PlotCellDepth? false
  set AnnualFishOutput? true  set LiveFishOutput? true  set DeadFishOutput? true  set ReddOutput? true  set BreedersOutput? true  set HabSelecOutput? false  
  set AnglingOutput? true  set MortAnglingHooking true
  
  show "End of setup of angling simulation"
  
end


 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SCHEDULING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to go
  tick              
  if ticks = simulation-length [print timer show (word "Execution finished in "  timer " seconds") stop]
  
;  profiler:start  ;; Start profiling

; Once a year:

  ;; Update angling pressure or exploitation rate based on the year's projected legally catchable stock 
  if MortAnglingHooking
   [
    let juliandate time:ts-get environmental-time-series tick-date  "juliandate"
    if juliandate = startAnglingSeason 
    [
      let year time:get "year" tick-date
      ifelse FixedAnglePressure? 
      [
        projectAnglingStock
        output-print (word "Legal angling stock year " year ": " anglingStock) 
        let seasonLength (endAnglingSeason - startAnglingSeason)
        ifelse anglingStock != 0
         [set exploitationRate precision ((anglePressure * reachLength * (10 ^ -3) * seasonLength) / (anglingStock * anglingEfficiency)) 1]
         [set exploitationRate 0]                 
        output-print (word "Projected Exploitation rate year " year ": " exploitationRate)
      ] 
      [
        projectAnglingStock
        output-print (word "Legal angling stock year " year ": " anglingStock) 
        let seasonLength (endAnglingSeason - startAnglingSeason)
        set anglePressure precision (((exploitationRate / 100) * anglingStock * anglingEfficiency) / (reachLength * (10 ^ -5) * seasonLength)) 2
        output-print (word "Projected Angling pressure year " year ": " anglePressure)
      ] 
    ]    
   ]
  

; Each time step:

;-- 1. Update reach's environmental and biological conditions and cell's habitat features ------------------------------------  
   
  update-reach-environment
  
  update-hydraulics-for flow
    
  update-cell-habitat
  
  update-reach-biology
  
  ;; List of alive fish in the reach, sorted by decreasing length, to implement later hierarchy by size
  let sorted-fish-list sort-on [(- fishLength)] trout with [status = "alive"] 
  
; Each time step during spawning season:

;-- 2. Reproduce trout ------------------------------------------------------------------------------------------------------- 

  let juliandate time:ts-get environmental-time-series tick-date  "juliandate"
  let spawningtime? false
  ;; When spawning season only spans over one natural year: 
  if fishSpawnEndDate > fishSpawnStartDate and juliandate >= fishSpawnStartDate and juliandate <= fishSpawnEndDate [set spawningtime? true] 
  ;; When spawning season spans over two different natural years:
  if fishSpawnEndDate < fishSpawnStartDate and juliandate >= fishSpawnStartDate [set spawningtime? true]  
  if fishSpawnEndDate < fishSpawnStartDate and juliandate <= fishSpawnEndDate [set spawningtime? true]
  if spawningtime?
   [
    ;--- 2.1. Identify candidate spawners ---;
    ask trout with [status = "alive"] [become-spawner]  
    
    ;--- 2.2. Spawn selected spawners -------;
    foreach sorted-fish-list [ask ? [spawn]]                   ;; Implement hierarchy by size
   ]  
  

; Each time step:

  ;; Irrespective of the defined time step, trout select habitat, feed, grow, and survive/die on a daily basis
  repeat timestep-scale
   [
    ;; If the time step is longer than one day, then every new daily sub-timestep: 
    if timestep-scale > 1
     [
      ;; the food available in the cell is re-generated  
      ask cells 
       [
        set driftHourlyCellTotal (3600 * cellArea * cellVelocity * cellDepth * habDriftConc / habDriftRegenDist)
        set searchHourlyCellTotal (habSearchProd * cellArea)
       ] 
      ;; the sorted list of alive fish is updated
      set sorted-fish-list sort-on [(- fishLength)] trout with [status = "alive"]
     ] 

;-- 3. Select habitat for trout -----------------------------------------------------------------------------------------------  
             
    foreach sorted-fish-list [ask ? [move]]                    ;; Implement hierarchy by size
  
;-- 4. Update trout length, weight, and condition factor ----------------------------------------------------------------------  

    foreach sorted-fish-list [ask ? [feed-and-grow]]           ;; Implement hierarchy by size
  
;-- 5. Kill trout ------------------------------------------------------------------------------------------------------------- 
  
    ask trout with [status = "alive"] [survive-or-die]   
  
   ]
  

; Each time step between spawning season and trout emergence:

;-- 6. Kill eggs in the redd -------------------------------------------------------------------------------------------------- 
  
  ask redds [eggs-survive-or-die]
  
;-- 7. Develop eggs in the redd -----------------------------------------------------------------------------------------------   
  
  ask redds [eggs-develop]
  
;-- 8. Hatch eggs in the redd - Emerge trout - Genetic transmission of traits -------------------------------------------------   
  
  ask redds [emerge-transmit]
  
  if not any? redds [ask trout with [status = "dead"] [die]]  ;; When genetic transmission is done, dead breeders are removed from the system
  

; Each time step and once a year:

;-- 9. Update trout age and age-class -----------------------------------------------------------------------------------------

  ask trout with [status = "alive"] [update-age]  
  
  
; Each time step:

;-- 10. Observer actions ------------------------------------------------------------------------------------------------------

  ;-- 10.1. Plot model graphical outputs --------------------------------------------------------------------------------------

  plot-modelOutputs

  ;-- 10.2. Write model output files ------------------------------------------------------------------------------------------

  write-modelOutputs        
    
  ;-- 10.3. Reset time-step counters ------------------------------------------------------------------------------------------
  
  reset-counters

; Once a year:
  
  ;-- 10.4. Record simulation data for calibration ----------------------------------------------------------------------------
  
  if calibration? and juliandate = OutputDate [record-calibrationData]

  
  
 
; profiler:stop          ;; stop profiling
; print profiler:report  ;; view the results
; profiler:reset         ;; clear the data
  
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;; REACH PROCEDURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-reach-environment
   set flow time:ts-get environmental-time-series tick-date  "flow"  
   set temp time:ts-get environmental-time-series tick-date  "temp"
   let juliandate time:ts-get environmental-time-series tick-date  "juliandate"
   set dayLength (24 - 2 * ((12 / 180) * acos (tan (siteLatitude) * tan (23.45 * cos ((2 * 180 / 365) * (173 - juliandate))))))     
end


to update-reach-biology
  set PiscivFishDens (count trout with [status = "alive" and fishLength > fishPiscivoryLength]) / wettedArea
  
  ifelse temp <= fishCmaxTempT2 [set cmaxTempFunction (fishCmaxTempF1 + ((temp - fishCmaxTempT1) * (fishCmaxTempF2 - fishCmaxTempF1) / (fishCmaxTempT2 - fishCmaxTempT1)))]
  [ifelse temp > fishCmaxTempT2 and temp <= fishCmaxTempT3 [set cmaxTempFunction (fishCmaxTempF2 + ((temp - fishCmaxTempT2) * (fishCmaxTempF3 - fishCmaxTempF2) / (fishCmaxTempT3 - fishCmaxTempT2)))]
  [ifelse temp > fishCmaxTempT3 and temp <= fishCmaxTempT4 [set cmaxTempFunction (fishCmaxTempF3 + ((temp - fishCmaxTempT3) * (fishCmaxTempF4 - fishCmaxTempF3) / (fishCmaxTempT4 - fishCmaxTempT3)))]
  [ifelse temp > fishCmaxTempT4 and temp <= fishCmaxTempT5 [set cmaxTempFunction (fishCmaxTempF4 + ((temp - fishCmaxTempT4) * (fishCmaxTempF5 - fishCmaxTempF4) / (fishCmaxTempT5 - fishCmaxTempT4)))]
  [ifelse temp > fishCmaxTempT5 and temp <= fishCmaxTempT6 [set cmaxTempFunction (fishCmaxTempF5 + ((temp - fishCmaxTempT5) * (fishCmaxTempF6 - fishCmaxTempF5) / (fishCmaxTempT6 - fishCmaxTempT5)))]
  [ifelse temp > fishCmaxTempT6 and temp <= fishCmaxTempT7 [set cmaxTempFunction (fishCmaxTempF6 + ((temp - fishCmaxTempT6) * (fishCmaxTempF7 - fishCmaxTempF6) / (fishCmaxTempT7 - fishCmaxTempT6)))]
  [set cmaxTempFunction (fishCmaxTempF6 + ((temp - fishCmaxTempT6) * (fishCmaxTempF7 - fishCmaxTempF6) / (fishCmaxTempT7 - fishCmaxTempT6)))]]]]]]
  if cmaxTempFunction < 0 [set cmaxTempFunction 0]      
  
  set SurvProbHighTemp SurvProbHighTemp-for-all
  set terrPredFeedTimeF terrPredFeedTimeF-for-all
  set aqPredDensF aqPredDensF-for-all
  set aqPredFeedTimeF aqPredFeedTimeF-for-all
  set aqPredTempF aqPredTempF-for-all
end


to projectAnglingStock
  ;; First, calculate the number of trout already within legal size limits
  let currentAnglingStock count trout with [status = "alive" and fishLength >= mortFishAngleSlotLower and fishLength <= mortFishAngleSlotUpper]
  
  ;; Then, trout below the legal size (other than age0 trout) calculate whether they will be within the legal stock along the angling season:
  let additionalAnglingStock 0
  ask trout with [status = "alive" and fishLength < mortFishAngleSlotLower and age-class != 0]
   [
    let expectedGrowth (energyAvailableforGrowth / fishEnergyDensity) * (endAnglingSeason - startAnglingSeason)  ;; Trout project growth over the whole angling season based on current conditions
    let projectedFishWeight (fishWeight + expectedGrowth)                                                        ;; Trout project weight 
    if projectedFishWeight < 0 [set projectedFishWeight 0]                                                       ;; Projected weight cannot be negative
    let projectedFishLength (projectedFishWeight / fishWeightParamA) ^ (1 / fishWeightParamB)                    ;; Calculate potential fish length based on projected weight
    if fishLength > projectedFishLength [set projectedFishLength fishLength]                                     ;; Trout update length only if projected length is greater than current length
    if projectedFishLength >= mortFishAngleSlotLower [set additionalAnglingStock (additionalAnglingStock + 1)]   ;; If projected length will be over the minimum legal size,
   ]                                                                                                             ;; the trout is included within the angling stock.
  
  set anglingStock (currentAnglingStock + additionalAnglingStock)  ;; Estimate the projected angling stock over the angling season
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;; CELL PROCEDURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-cell-habitat
  
  ask cells 
   [
    ;; First, update hydraulics. Either calculated by the model from lookup tables (in update-hydraulics-for procedure)  or imported from text file (here). 
    if not ModelHydraulics? 
     [
       set cellDepth (matrix:get depth-data ticks who) * 100 
       set cellVelocity (matrix:get velocity-data ticks who) * 100 
     ]      
    if PlotCellDepth?
     [ifelse cellDepth = 0 [set-color grey] [set-color scale-color blue cellDepth (max [cellDepth] of cells) (min [cellDepth] of cells)]]    
  
    ;; Then, update drift and search food production rate
    set driftHourlyCellTotal (3600 * cellArea * cellVelocity * cellDepth * habDriftConc / habDriftRegenDist)
    set searchHourlyCellTotal (habSearchProd * cellArea)
  
    ;; All shelter area of the cell is free
    set cellAreaShelter (cellFracShelter * cellArea) 
   ]
  
  ;; Assign to every cell the average distance a fish located in the cell would need to move to find hiding cover 
  ask cells [set cellDistanceToHide 1000]   
  let cellAreaNoCover 0
  let cellsWithCover cells with [cellFracCover > 0 and cellDepth > 0]
  ask cellsWithCover [set cellAreaNoCover (1 - cellFracCover) * cellArea  set cellDistanceToHide (1 - cellFracCover) * sqrt (cellAreaNoCover / pi)]
  let distanceCellsWithCover 0
  let myClosestCoveredCell []
  ask cells with [cellFracCover = 0 and cellDepth > 0] 
   [
    set myClosestCoveredCell min-one-of cellsWithCover [distance myself]
    if myClosestCoveredCell != nobody
     [
      set distanceCellsWithCover (distance myClosestCoveredCell) * space-scale 
      set cellDistanceToHide (distanceCellsWithCover + [cellDistanceToHide] of myClosestCoveredCell)
     ]    
    ]      
   
  set wettedArea sum [cellArea] of cells with [cellDepth > 0]    ;; Update the reach's wetted area 
end


;; Procedure to interpolate depth and velocity of cells from flow (provided by Steve Railsback)
to update-hydraulics-for [a-flow]

 if ModelHydraulics?
  [
    ; First, interpolate the depth index from flow
    let d-index-high 1
    ifelse a-flow < last depth-flow-list
     [
      while [ a-flow > item d-index-high depth-flow-list]
        [set d-index-high d-index-high + 1]
     ]
     [
      set d-index-high (length depth-flow-list - 1)
     ]
    let d-index-low d-index-high - 1

    ; Second, interpolate the velocity index from flow
    let v-index-high 1
    ifelse a-flow < last velocity-flow-list
     [
      while [ a-flow > item v-index-high velocity-flow-list]
        [set v-index-high v-index-high + 1]
     ]
     [
      set v-index-high (length velocity-flow-list - 1)
     ]
    let v-index-low v-index-high - 1

    ;show (word "Flow: " flow " high index: " d-index-high " low index: " d-index-low)
    ;show (word "Flow: " flow " high index: " v-index-high " low index: " v-index-low)
    
    ; Now have cells do the interpolation
    ; If flow is less than lowest in table, 
    ; ->extrapolate depth downward but don't allow neg. depth
    ; ->interpolate velocity between zero and lowest in table
    ask cells [
    let cell-depth 0.0
    let cell-velocity 0.0
    
    ; First, see if flow is above the flow at which depth first exceeds zero
    if a-flow > flow-at-wetting
      [
        ; If so, interpolate depth
        let lowQ item d-index-low depth-flow-list
        let highQ item d-index-high depth-flow-list
        let lowD item d-index-low depth-lookup
        let highD item d-index-high depth-lookup
        ; Check for flow-at-wetting and use it for interpolation
        ; if cell is dry at lowQ from interpolation table.
        if lowQ < flow-at-wetting 
          [ 
            set lowQ flow-at-wetting
            if lowD > 0.0 [error "Depth interpolation error: Depth not zero for Q below flow-at-wetting"] 
          ]
        let diff highQ - lowQ
        let ratio (a-flow - lowQ) / diff
        
        set cell-depth lowD + ratio * (highD - lowD)
        if cell-depth < 0 [ set cell-depth 0.0 ]
        
        ; Then velocity
        ifelse a-flow < (item 0 velocity-flow-list)
        [
          ; Flow is less than lowest in table, so interpolate from zero to lowest table flow
          set cell-velocity (a-flow / first velocity-flow-list) * first velocity-lookup
          ; But velocity must be zero if depth is zero
          if cell-depth <= 0 [set cell-velocity 0.0]
        ]
        [
          set lowQ item v-index-low velocity-flow-list
          set highQ item v-index-high velocity-flow-list
          let lowV item v-index-low velocity-lookup
          let highV item v-index-high velocity-lookup
          ; Check for flow-at-wetting and use it for interpolation
          ; if cell is dry at lowQ from interpolation table.
          if lowQ < flow-at-wetting 
            [ 
              set lowQ flow-at-wetting
              set lowV 0.0 ; Velocity *might* not be zero at flow-at-wetting due to var. in hydraulic sims. 
            ]
          set diff highQ - lowQ
          set ratio (a-flow - lowQ) / diff
          
          set cell-velocity lowV + ratio * (highV - lowV)
          if cell-velocity < 0 [ error (word "Velocity interpolated to negative value at cell " cellNumber)]
        ]
      ] ; ifelse a-flow < flow-at-wetting

    set cellDepth cell-depth * 100
    set cellVelocity cell-velocity * 100]
  ]
    
end  
  

to set-color [ #color ]  ;; by cell -- set my patches to given color
  ask my-patches [ set pcolor #color ]
end




;;;;;;;;;;;;;;;;;;;;;;;;;;;; TROUT PROCEDURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to become-spawner         
  if fishLength >= fishSpawnMinLength and age >= fishSpawnMinAge [set maturityStatus "mature"]   ;; When trout reach the the phenotypic length maturity threshold at a proper age they become mature
  let juliandate time:ts-get environmental-time-series tick-date  "juliandate"
  if juliandate >= fishSpawnStartDate and juliandate < (fishSpawnStartDate + timestep-scale) ;; The day starting the spawning season,  
   [set spawnedThisSeason? false]                                                            ;; the parameter "spawnedThisSeason?" is set to "false" for all trout
  if maturityStatus = "mature" and sex = "F" and spawnedThisSeason? = false    ;; Only mature female trout who have not already spawned this season can become a spawner 
  and fishCondition >= fishSpawnMinCond                                        ;; (but just if they have a minimum body condition)
   [
    if temp >= fishSpawnMinTemp and temp <= fishSpawnMaxTemp                   ;; Trout only spawn within the tolerable spawning temperature range
    and flow <= habMaxSpawnFlow                                                ;; Trout will not spawn during high flow events  
    and abs (flow - (time:ts-get environmental-time-series (time:plus tick-date (- timestep-scale) "day")  "flow")) / flow <= fishSpawnMaxFlowChange   ;; Trout only spawn under stable flow conditions
     [if random-float 1 < 1 - ((1 - fishSpawnProb) ^ timestep-scale)           ;; Whether the trout actually spawns is determined stochastically       
       [set maturityStatus "spawner"]                                          ;; If all spawning criteria are met then the trout is ready to spawn
     ]
   ]                                                                                            
    
    if maturityStatus = "mature" and sex = "M"   ;; Only mature male trout with a minimum body condition are potential spawners
    and fishCondition >= fishSpawnMinCond                                      
;    and spawnedThisSeason? = false     
     [set maturityStatus "spawner"]                                   
end



to spawn
  let male-spawners trout with [maturityStatus = "spawner" and sex = "M" and fishCondition >= fishSpawnMinCond] ;; Males available for spawning (males have to regain good condition to spawn again after a spawning event)
  if sex = "F" and maturityStatus = "spawner"                                                                   ;; Female trout:
   [
     ;; Select the spawning cell
     move-to my-cell
     let maxMoveDistance (fishMoveDistParamA * fishLength ^ fishMoveDistParamB)   
     let potential-destination-cells cells with [distance myself <= (maxMoveDistance / space-scale) and cellDepth > 0] 
     if any? potential-destination-cells                                          ;; Trout move only if there is any potential destination. Otherwise, stay put.
      [
        set my-cell max-one-of potential-destination-cells [cellSpawnQuality]     ;; Trout move to the cell with the highest spawning quality
        move-to my-cell                                                           
      ]
      
     ;; Select the spawner males to fertilize the redd
     let number-males 1 + random max-n-males-per-female      ;; Number of males drawn randomly from an uniform distribution from 1 to max-n-males-per-female
     let candidate-spawners male-spawners                    
     let selected-spawners male-spawners                     ;; Probability of a male spawner of being selected to fertilize a redd depends on its weight. Then there is stochastic selection.                                                    
     if any? male-spawners 
      [
       let random-selection random-float (max [fishWeight] of male-spawners / sum [fishWeight] of male-spawners)
       let bestMale male-spawners with-max [fishWeight]  ;; The largest male spawner is always selected
       set candidate-spawners male-spawners with [not member? self bestMale and (fishWeight / sum [fishWeight] of male-spawners) >= random-selection]  
       ifelse (number-males - 1) <= count candidate-spawners   ;; Are there enough candidate spawners to reach the maximum number of males to fertilize the eggs?:                                                  
         [set selected-spawners (turtle-set bestMale n-of (number-males - 1) candidate-spawners)]  ;; If so, randomly take the necessary ones. 
         [ifelse ((number-males - 1) - count candidate-spawners) <= count male-spawners with [not member? self (turtle-set bestMale candidate-spawners)]   ;; Otherwise, stochastic selection within the remaining spawners until all males are selected
           [set selected-spawners (turtle-set bestMale candidate-spawners n-of (number-males - 1 - count candidate-spawners) male-spawners with [not member? self (turtle-set bestMale candidate-spawners)])]
           [set selected-spawners (turtle-set bestMale candidate-spawners male-spawners with [not member? self (turtle-set bestMale candidate-spawners)])]]
      ] 
      
     ;; Selected spawners store their heritable info in transitional local variables 
     let FathersgenNewlength [] 
     let FathersgenSpawnMinLength []
     let FathersgenNeutralTrait []
      
     if any? selected-spawners 
      [ask selected-spawners
       [
        set FathersgenNewlength sentence FathersgenNewlength genNewLength 
        set FathersgenSpawnMinLength sentence FathersgenSpawnMinLength genSpawnMinLength
        set FathersgenNeutralTrait sentence FathersgenNeutralTrait genNeutralTrait
       ]
      ]
               
       
     ;; Create the redd. It is then fertilized by selected spawners, which genetically transmit the heritable traits to the future offspring.  
     ;; But first, female trout calculate the parameter controlling the trade-off between number of eggs and egg size. 
     ;; It is the relationship between number of eggs that would be created by the trout if the offspring had the population mean length at emergence and such number if the offspring had the female spawner's genetic length at emergence. 
     ;; The mathematical development of the parameter's formula is left below intentionally for clarification:
;    let GonadMass (fishFecundParamA * fishLength ^ fishFecundParamB) * (fishWeightParamA * fishNewLengthMean ^ fishWeightParamB)   
;    let eggsize-fecund-tradeoff (GonadMass / (fishWeightParamA * genNewLength ^ fishWeightParamB)) / (fishFecundParamA * fishLength ^ fishFecundParamB)  
;    let eggsize-fecund-tradeoff (fishNewLengthMean / genNewLength) ^ fishWeightParamB
     let eggsize-fecund-tradeoff 1
     if GeneticTransmission? [set eggsize-fecund-tradeoff (fishNewLengthMean / genNewLength) ^ fishWeightParamB] 
     hatch-redds 1                                                               
      [
        set creationDate time:show tick-date "dd-MM-yyyy"
        set SpawningDate fput (time:ts-get environmental-time-series tick-date  "juliandate") SpawningDate  ;; Writing info into global counter for plotting purposes
        set my-cell [my-cell] of myself  move-to one-of [my-patches] of my-cell
        set color white set shape "circle"  set size 4 set ReddID [who] of myself
        set numberOfEggs round ((eggsize-fecund-tradeoff * fishFecundParamA * [fishLength] of myself ^ fishFecundParamB) * fishSpawnEggViability)   ;; Not all laid eggs are viable
        set ininumberOfEggs numberOfEggs
        set ReachIniNumberOfEggs (ReachIniNumberOfEggs + ininumberOfEggs)  set ReachNumberOfEggs (ReachNumberOfEggs + ininumberOfEggs)  ;; Writing info into global counters for plotting purposes
        set reddMothergenNewlength [genNewLength] of myself
        set reddMothergenSpawnMinLength [genSpawnMinLength] of myself
        set reddMothergenNeutralTrait [genNeutralTrait] of myself 
        ifelse any? selected-spawners    ;; If no male meets the criteria as a spawner, the female still produces a fertile redd and the genetic transmission depends just on her
          [
           set reddFathersgenNewlength [] set reddFathersgenNewlength [FathersgenNewlength] of myself
           set reddFathersgenSpawnMinLength [] set reddFathersgenSpawnMinLength [FathersgenSpawnMinLength] of myself
           set reddFathersgenNeutralTrait [] set reddFathersgenNeutralTrait [FathersgenNeutralTrait] of myself
          ]
          [
           set reddFathersgenNewlength (list [genNewLength] of myself)
           set reddFathersgenSpawnMinLength (list [genSpawnMinLength] of myself)
           set reddFathersgenNeutralTrait (list [genNeutralTrait] of myself)
          ]             
        set fracDeveloped 0
        set days-after-hatch 0
      ]  
     
     ;; Write info into model output file and global counters for plotting
     if BreedersOutput? [write-BreedersIndOutput  if any? selected-spawners [ask selected-spawners [write-BreedersIndOutput]]]                                                     ;; Write info into the breeders output file
     set FemaleSpawnersLength fput fishLength FemaleSpawnersLength  if any? selected-spawners [ask selected-spawners [set MaleSpawnersLength fput fishLength MaleSpawnersLength]]  ;; Write info for plotting purposes 
     set FemaleSpawnersAge fput age FemaleSpawnersAge  if any? selected-spawners [ask selected-spawners [set MaleSpawnersAge fput age MaleSpawnersAge]]
     set FemaleBreedGenMinSpawnLength fput genSpawnMinLength FemaleBreedGenMinSpawnLength  
     if any? selected-spawners [ask selected-spawners [set MaleBreedGenMinSpawnLength fput genSpawnMinLength MaleBreedGenMinSpawnLength]]
     set GenBreedEmergenceLength fput genNewLength GenBreedEmergenceLength  if any? selected-spawners [ask selected-spawners [set GenBreedEmergenceLength fput genNewLength GenBreedEmergenceLength]]
     set GenBreedNeutralTrait fput genNeutralTrait GenBreedNeutralTrait  if any? selected-spawners [ask selected-spawners [set GenBreedNeutralTrait fput genNeutralTrait GenBreedNeutralTrait]]
      
     ;; Incur weight loss (both males and female) and change spawning status 
     set fishWeight (fishWeight * (1 - fishSpawnWtLossFraction))                                                                               ;; Weight is reduced according to the parameter fishSpawnWtLossFraction
     set fishCondition (fishWeight / (fishWeightParamA * fishLength ^ fishWeightParamB))                                                       ;; Calculate new body condition
     if any? selected-spawners [ask selected-spawners [set fishWeight (fishWeight * (1 - fishSpawnWtLossFraction))]]
     if any? selected-spawners [ask selected-spawners [set fishCondition (fishWeight / (fishWeightParamA * fishLength ^ fishWeightParamB))]]      
     set spawnedThisSeason? true  if any? selected-spawners [ask selected-spawners [set spawnedThisSeason? true]]                              ;; Female trout are no longer elegible to spawn during this season                                                                   
   ]     
end



to move
  move-to my-cell
  let maxMoveDistance (fishMoveDistParamA * fishLength ^ fishMoveDistParamB)
  let my-inDistanceCells cells with [distance myself <= (maxMoveDistance / space-scale) and cellDepth > 0]  ;;The model performs 8 times faster using distance myself compared to in-radius... 
  let my-wettedAdjacentCells ([my-adjacentCells] of my-cell) with [cellDepth > 0]
  let potential-destination-cells (turtle-set my-wettedAdjacentCells my-inDistanceCells)    
;  let potential-destination-cells cells with [distance myself <= (maxMoveDistance / space-scale) and cellDepth > 0]  ;;The model performs 8 times faster using distance myself compared to in-radius...

  set fishMaxSwimSpeed (((fishMaxSwimParamA * fishLength) + fishMaxSwimParamB) * ((fishMaxSwimParamC * temp ^ 2) + (fishMaxSwimParamD * temp) + fishMaxSwimParamE))
  set cMax (fishCmaxParamA * (fishWeight ^ (1 + fishCmaxParamB)) * cmaxTempFunction)
  
  if any? potential-destination-cells                                                   ;; Trout move only if there is any potential destination. Otherwise, stay put.
    [set my-cell max-one-of potential-destination-cells [expectedMaturity-for myself]]  ;; Trout select the cell maximizing expected maturity (fitness measure)
  set energyAvailableforGrowth [bestNetEnergy-for myself] of my-cell                    ;; Set the food energy that the trout will consume in the cell during the time step
  let cMax-rate (cMax / dayLength + 2)   ;; Hourly maximum consumption rate
  ifelse [dailyDriftNetEnergy-for myself] of my-cell > [dailySearchNetEnergy-for myself] of my-cell ;; Implement hierarchical competition for food via the food availability rate in the cell
    [
     ask my-cell [set driftHourlyCellTotal (driftHourlyCellTotal - min (list driftHourlyCellTotal driftintake-for myself [cMax-rate] of myself))]
     
     let fishShelterArea (fishLength ^ 2)      ;; Each trout can use an area of shelter equal to the square of its length.
     if [cellAreaShelter] of my-cell > 0       ;; Trout has access to shelter if the total shelter area of its cell is greater than the shelter area already occupied by more dominant fish.                                        
      [                                        ;; This means that a fish has access to shelter if there is ANY unused shelter space available for it in the cell.
       set is-sheltered? true                 
       ifelse ([cellAreaShelter] of my-cell - fishShelterArea) > 0 
         [ask my-cell [set cellAreaShelter (cellAreaShelter - fishShelterArea)]] 
         [ask my-cell [set cellAreaShelter 0]]
      ]
    ]      
    [
     ask my-cell [set searchHourlyCellTotal (searchHourlyCellTotal - min (list searchHourlyCellTotal searchintake-for myself [cMax-rate] of myself))]
     set is-sheltered? false
    ]   
  move-to one-of [my-patches] of my-cell
end



to feed-and-grow
  let dailyGrowth (energyAvailableforGrowth / fishEnergyDensity)
  set fishWeight (fishWeight + dailyGrowth)                                              ;; Trout update weight after feeding
  if fishWeight < 0 [set fishWeight 0]                                                   ;; Weight cannot be negative
  let fishWannabeLength (fishWeight / fishWeightParamA) ^ (1 / fishWeightParamB)         ;; Calculate potential fish length based on new weight
  if fishLength < fishWannabeLength [set fishLength fishWannabeLength]                   ;; Trout update length only if potential length is greater than current length
  set fishCondition (fishWeight / (fishWeightParamA * fishLength ^ fishWeightParamB))    ;; Calculate new body condition
  if fishCondition < 0 [set fishCondition 0] if fishCondition > 1 [set fishCondition 1]  ;; To avoid over and under flow errors
end


to survive-or-die
  let rand-HighTemp random-float 1 if rand-HighTemp > (SurvProbHighTemp) [set CauseOfDeath "HighTemp" die! stop]
  let SurvivingHighVel [SurvProbHighVel-for myself] of my-cell let rand-HighVel random-float 1 if rand-HighVel > (SurvivingHighVel) [set CauseOfDeath "HighVel" die! stop]
  let SurvivingStrand [SurvProbStrand-for myself] of my-cell let rand-Strand random-float 1 if rand-Strand > (SurvivingStrand) [set CauseOfDeath "Strand" die! stop]
  let SurvivingPoorCond [SurvProbPoorCond-for myself] of my-cell let rand-PoorCond random-float 1 if rand-PoorCond > (SurvivingPoorCond) [set CauseOfDeath "PoorCond" die! stop]
  let SurvivingTerrPred [SurvProbTerrPred-for myself] of my-cell let rand-TerrPred random-float 1 if rand-TerrPred > (SurvivingTerrPred) [set CauseOfDeath "TerrPred" die! stop]
  let SurvivingAqPred [SurvProbAqPred-for myself] of my-cell let rand-AqPred random-float 1 if rand-AqPred > (SurvivingAqPred) [set CauseOfDeath "AqPred" die! stop]  

  ;; Angling and hooking mortality (based on inSTREAM-SD, described in Railsback et al. 2013)
  if MortAnglingHooking
   [ 
    let juliandate time:ts-get environmental-time-series tick-date  "juliandate"
    if juliandate >= startAnglingSeason and juliandate <= endAnglingSeason        ;; Angling mortality happens only during the angling season
     [
      let captureRate (mortFishAngleSuccess * anglePressure * reachLength * (10 ^ -5) * AngleCaptureLengthF)   ;; Calculate fish caught per catchable fish per day
      let timesHooked random-poisson (captureRate * timestep-scale)   ;; A Poisson distribution models the (integer) number of times a trout is hooked during a time step
      if timesHooked > 0     ;; If the trout is hooked at least once
       [
        repeat timesHooked   ;; Survival of angling and hooking mortality is tested once for each time the trout is hooked
         [ 
          ifelse fishLength >= mortFishAngleSlotLower and fishLength <= mortFishAngleSlotUpper                             ;; Determine if the trout is of legal size.
           [ifelse random-float 1 < mortFishAngleFracKeptLegal                                                             ;; If legal, 
             [set CauseOfDeath "Angling" die! stop]                                                                        ;; first face death by angling (angler decides whether keeping the trout),
             [if random-float 1 > (mortFishAngleHookSurvRate ^ timesHooked) [set CauseOfDeath "Hooking" die! stop]]]       ;; and if it is released, then face death by hooking mortality.
                                                                                                                                                
           [ifelse random-float 1 < mortFishAngleFracKeptIllegal                                                           ;; If illegal, first face death by angling and then by hooking.
             [set CauseOfDeath "Angling" die! stop] 
             [if random-float 1 > (mortFishAngleHookSurvRate ^ timesHooked) [set CauseOfDeath "Hooking" die! stop]]] 
         ]
       ]
     ]
   ] 
end



;; Trout update their age (in days) every time-step.
;; When new recruits are born then all older cohorts update their age-class to avoid that some fish have the same age-class while belonging to different cohorts.
;; The same happens when the first fish of older cohorts updates its age-class.
to update-age
  set age (age + timestep-scale)
  if age = timestep-scale [update-cohort-Ageclass stop]
  if floor (age / timestep-scale) = round (365 / timestep-scale) [set age-class 1 set color red set size 10 update-cohort-Ageclass stop]
  if floor (age / timestep-scale) = 2 * round (365 / timestep-scale) [set age-class 2 update-cohort-Ageclass stop]
  if floor (age / timestep-scale) = 3 * round (365 / timestep-scale) [set age-class 3 update-cohort-Ageclass stop]
  if floor (age / timestep-scale) = 4 * round (365 / timestep-scale) [set age-class 4 update-cohort-Ageclass stop]
  if floor (age / timestep-scale) = 5 * round (365 / timestep-scale) [set age-class 5 update-cohort-Ageclass]  
end

to update-cohort-Ageclass
     ask trout with [age-class = 0 and age > 200] [set age-class 1 set color red set size 10]
     ask trout with [age-class = 1 and age > (200 + 365) and status = "alive"] [set age-class 2]
     ask trout with [age-class = 2 and age > (200 + 2 * 365) and status = "alive"] [set age-class 3]
     ask trout with [age-class = 3 and age > (200 + 3 * 365) and status = "alive"] [set age-class 4]
     ask trout with [age-class = 4 and age > (200 + 4 * 365) and status = "alive"] [set age-class 5] 
end


to die!   
  ifelse age-class = 0
     [ifelse CauseOfDeath = "HighTemp" [set fishDeadHighTempAge0 fishDeadHighTempAge0 + 1 set fishDeadHighTempTot fishDeadHighTempTot + 1]
     [ifelse CauseOfDeath = "HighVel" [set fishDeadHighVelAge0 fishDeadHighVelAge0 + 1 set fishDeadHighVelTot fishDeadHighVelTot + 1]
     [ifelse CauseOfDeath = "Strand"  [set fishDeadStrandAge0 fishDeadStrandAge0 + 1 set fishDeadStrandTot fishDeadStrandTot + 1]
     [ifelse CauseOfDeath = "PoorCond" [set fishDeadPoorCondAge0 fishDeadPoorCondAge0 + 1 set fishDeadPoorCondTot fishDeadPoorCondTot + 1]
     [ifelse CauseOfDeath = "TerrPred" [set fishDeadTerrPredAge0 fishDeadTerrPredAge0 + 1 set fishDeadTerrPredTot fishDeadTerrPredTot + 1]
     [ifelse CauseOfDeath = "AqPred"   [set fishDeadAqPredAge0 fishDeadAqPredAge0 + 1 set fishDeadAqPredTot fishDeadAqPredTot + 1]
     [ifelse CauseOfDeath = "Angling"  [set fishDeadAnglingAge0 fishDeadAnglingAge0 + 1 set fishDeadAnglingTot fishDeadAnglingTot + 1 set fishDeadAnglingSeasonAge0 fishDeadAnglingSeasonAge0 + 1 set fishDeadAnglingSeason fishDeadAnglingSeason + 1]
     [set fishDeadHookingAge0 fishDeadHookingAge0 + 1 set fishDeadHookingTot fishDeadHookingTot + 1 set fishDeadHookingSeasonAge0 fishDeadHookingSeasonAge0 + 1 set fishDeadHookingSeason fishDeadHookingSeason + 1]]]]]]]]
     
     [ifelse age-class = 1
         [ifelse CauseOfDeath = "HighTemp" [set fishDeadHighTempAge1 fishDeadHighTempAge1 + 1 set fishDeadHighTempTot fishDeadHighTempTot + 1]
         [ifelse CauseOfDeath = "HighVel"  [set fishDeadHighVelAge1 fishDeadHighVelAge1 + 1 set fishDeadHighVelTot fishDeadHighVelTot + 1]
         [ifelse CauseOfDeath = "Strand"   [set fishDeadStrandAge1 fishDeadStrandAge1 + 1 set fishDeadStrandTot fishDeadStrandTot + 1]
         [ifelse CauseOfDeath = "PoorCond" [set fishDeadPoorCondAge1 fishDeadPoorCondAge1 + 1 set fishDeadPoorCondTot fishDeadPoorCondTot + 1]
         [ifelse CauseOfDeath = "TerrPred" [set fishDeadTerrPredAge1 fishDeadTerrPredAge1 + 1 set fishDeadTerrPredTot fishDeadTerrPredTot + 1]
         [ifelse CauseOfDeath = "AqPred"   [set fishDeadAqPredAge1 fishDeadAqPredAge1 + 1 set fishDeadAqPredTot fishDeadAqPredTot + 1]
         [ifelse CauseOfDeath = "Angling"  [set fishDeadAnglingAge1 fishDeadAnglingAge1 + 1 set fishDeadAnglingTot fishDeadAnglingTot + 1 set fishDeadAnglingSeasonAge1 fishDeadAnglingSeasonAge1 + 1 set fishDeadAnglingSeason fishDeadAnglingSeason + 1]
         [set fishDeadHookingAge1 fishDeadHookingAge1 + 1 set fishDeadHookingTot fishDeadHookingTot + 1 set fishDeadHookingSeasonAge1 fishDeadHookingSeasonAge1 + 1 set fishDeadHookingSeason fishDeadHookingSeason + 1]]]]]]]]
         
         [ifelse age-class = 2
             [ifelse CauseOfDeath = "HighTemp" [set fishDeadHighTempAge2 fishDeadHighTempAge2 + 1 set fishDeadHighTempTot fishDeadHighTempTot + 1]
             [ifelse CauseOfDeath = "HighVel"  [set fishDeadHighVelAge2 fishDeadHighVelAge2 + 1 set fishDeadHighVelTot fishDeadHighVelTot + 1]
             [ifelse CauseOfDeath = "Strand"   [set fishDeadStrandAge2 fishDeadStrandAge2 + 1 set fishDeadStrandTot fishDeadStrandTot + 1]
             [ifelse CauseOfDeath = "PoorCond" [set fishDeadPoorCondAge2 fishDeadPoorCondAge2 + 1 set fishDeadPoorCondTot fishDeadPoorCondTot + 1]
             [ifelse CauseOfDeath = "TerrPred" [set fishDeadTerrPredAge2 fishDeadTerrPredAge2 + 1 set fishDeadTerrPredTot fishDeadTerrPredTot + 1]
             [ifelse CauseOfDeath = "AqPred"   [set fishDeadAqPredAge2 fishDeadAqPredAge2 + 1 set fishDeadAqPredTot fishDeadAqPredTot + 1]
             [ifelse CauseOfDeath = "Angling"  [set fishDeadAnglingAge2 fishDeadAnglingAge2 + 1 set fishDeadAnglingTot fishDeadAnglingTot + 1 set fishDeadAnglingSeasonAge2 fishDeadAnglingSeasonAge2 + 1 set fishDeadAnglingSeason fishDeadAnglingSeason + 1]
             [set fishDeadHookingAge2 fishDeadHookingAge2 + 1 set fishDeadHookingTot fishDeadHookingTot + 1 set fishDeadHookingSeasonAge2 fishDeadHookingSeasonAge2 + 1 set fishDeadHookingSeason fishDeadHookingSeason + 1]]]]]]]]
             
             [ifelse CauseOfDeath = "HighTemp" [set fishDeadHighTempAge3Plus fishDeadHighTempAge3Plus + 1 set fishDeadHighTempTot fishDeadHighTempTot + 1]
             [ifelse CauseOfDeath = "HighVel"  [set fishDeadHighVelAge3Plus fishDeadHighVelAge3Plus + 1 set fishDeadHighVelTot fishDeadHighVelTot + 1]
             [ifelse CauseOfDeath = "Strand"   [set fishDeadStrandAge3Plus fishDeadStrandAge3Plus + 1 set fishDeadStrandTot fishDeadStrandTot + 1]
             [ifelse CauseOfDeath = "PoorCond" [set fishDeadPoorCondAge3Plus fishDeadPoorCondAge3Plus + 1 set fishDeadPoorCondTot fishDeadPoorCondTot + 1]
             [ifelse CauseOfDeath = "TerrPred" [set fishDeadTerrPredAge3Plus fishDeadTerrPredAge3Plus + 1 set fishDeadTerrPredTot fishDeadTerrPredTot + 1]
             [ifelse CauseOfDeath = "AqPred"   [set fishDeadAqPredAge3Plus fishDeadAqPredAge3Plus + 1 set fishDeadAqPredTot fishDeadAqPredTot + 1]
             [ifelse CauseOfDeath = "Angling"  [set fishDeadAnglingAge3Plus fishDeadAnglingAge3Plus + 1 set fishDeadAnglingTot fishDeadAnglingTot + 1 set fishDeadAnglingSeasonAge3Plus fishDeadAnglingSeasonAge3Plus + 1 set fishDeadAnglingSeason fishDeadAnglingSeason + 1]
             [set fishDeadHookingAge3Plus fishDeadHookingAge3Plus + 1 set fishDeadHookingTot fishDeadHookingTot + 1 set fishDeadHookingSeasonAge3Plus fishDeadHookingSeasonAge3Plus + 1 set fishDeadHookingSeason fishDeadHookingSeason + 1]]]]]]]]]]
  
  set AgeatDeath fput age AgeatDeath  ;; Writing info por plotting purposes 
  
  ifelse spawnedThisSeason? 
   [set status "dead" hide-turtle]    ;; Dead breeders are kept in the system for genetic transmission
   [die]    
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;; REDD PROCEDURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to eggs-survive-or-die  
  let eggsLostToLowTempTimestep (eggsLostToLowTemp * timestep-scale) 
  set eggsLostToLowTempTot (eggsLostToLowTempTot + eggsLostToLowTempTimestep)
  set numberOfEggs (numberOfEggs - eggsLostToLowTempTimestep)                     ;; Number of eggs surviving low temperatures
  
  let eggsLostToHighTempTimestep (eggsLostToHighTemp * timestep-scale) 
  set eggsLostToHighTempTot (eggsLostToHighTempTot + eggsLostToHighTempTimestep)    
  set numberOfEggs (numberOfEggs - eggsLostToHighTempTimestep)                    ;; Number of eggs surviving high temperatures  
  
  if flow > (time:ts-get environmental-time-series (time:plus tick-date (- timestep-scale) "day")  "flow") and   ;; Mortality through scouring only happens during peaks of flow events
   flow > (time:ts-get environmental-time-series (time:plus tick-date timestep-scale "day")  "flow") 
    [ifelse random-float 1 > reddScourSurvival                                                                   ;; Stochastic mortality so that all eggs of the redd
      [let eggsLostToScour numberOfEggs set eggsLostToScourTot eggsLostToScour set numberOfEggs 0]               ;; either die                               
      [set numberOfEggs numberOfEggs]                                                                            ;; or survive
    ]                                                                           
    
  let eggsLostToDewateringTimestep 0
  if [cellDepth] of my-cell = 0   ;; A fraction of the eggs die when there is no water in the cell
    [
     set eggsLostToDewateringTimestep (eggsLostToDewatering * timestep-scale)
     set eggsLostToDewateringTot (eggsLostToDewateringTot + eggsLostToDewateringTimestep)
     set numberOfEggs (numberOfEggs - eggsLostToDewateringTimestep)
    ]  
  
  ;; If one or more new redds are created in the same cell where a redd already exist, that redd faces risk of mortality by superimposition.  This risk exists only on the day when the new redds are created.
  ;; Importantly, superimposition only occurs when there is gravel to be disturbed in the cell. Otherwise, survival probability is 1.
  let eggsLostToSuperimpTimestep 0 
  if [cellFracGravel] of my-cell > 0 
   [
    let n-of-superimposing-redds (count redds with [my-cell = [my-cell] of myself and who > [who] of myself and fracDeveloped = 0]) ;; Mortality by superimposition can be caused only by redds which were created
    if n-of-superimposing-redds > 0                                                                                                 ;; 1) in the same cell, and 2) afterwards; and 3) only over the day they were created
      [repeat n-of-superimposing-redds                     ;; The procedure has to be executed once for each new redd placed in the cell on the current day
        [
         let eggsLostToEachSuperimp eggsLostToSuperimp
         set eggsLostToSuperimpTimestep (eggsLostToSuperimpTimestep + eggsLostToEachSuperimp)
         set eggsLostToSuperimpTot (eggsLostToSuperimpTot + eggsLostToEachSuperimp)
         set numberOfEggs (numberOfEggs - eggsLostToEachSuperimp)
        ]
      ]        
   ]
  
  ;; Write info into global counters for plotting purposes
  let eggsLostTimestep (eggsLostToLowTempTimestep + eggsLostToHighTempTimestep + eggsLostToScourTot + eggsLostToDewateringTimestep + eggsLostToSuperimpTimestep)
  set ReachNumberOfEggs (ReachNumberOfEggs - eggsLostTimestep)
   
  if numberOfEggs = 0         ;; When the number of remaining eggs in a redd reaches zero, the redd is dropped from the model and writes its info in the output file
    [
     if ReddOutput? [write-ReddOutput] 
     die
    ]    
end



to eggs-develop  ;; The procedure follows inSTREAM's approach (Railsback et al. 2009)
  let reddDailyDevel (reddDevelParamA + (reddDevelParamB * temp) + (reddDevelParamC * temp ^ 2)) ;; The fractional development that occurs over a day depends just on temperature (based on Van Winkle et al. 1996)
  set fracDeveloped (fracDeveloped + (reddDailyDevel * timestep-scale))                          ;; The developmental status of a redd’s eggs is then updated
end



;; Inheritance rules are based on the infinitesimal model of quantitative genetics (Lynch & Walsh 1998): 
;; the genotypic value of the trait under selection is drawn from a normal distribution centered on the arithmetic mean of the two parental values, 
;; while the variance of this distribution is equal to half the total additive genetic variance for the trait at the population level.
;; The additive genetic variance is calculated as the product between the initial population phenotypic variance and the narrow-sense heritability of the trait, and it may remain constant along time.
;; Or it may be not fixed across generations, and then it is computed as the variance of the breeding values.
;; Finally, the individual's phenotypic expression of the trait is defined as the sum of its genotypic value and a statistically independent random environmental effect.
;; The environmental variance remains constant along time and is set based on the initial population phenotypic variance and the narrow-sense heritability of the trait.

to emerge-transmit
  if fracDeveloped >= 1 
   [ 
    ;; Set number of eggs hatching
    set days-after-hatch (days-after-hatch + 1)
    let number-hatching-eggs round (0.1 * days-after-hatch * timestep-scale * numberOfEggs)  ;; Emergence from the redd is spread along time (10 days in the case of daily time step), following inSTREAM's approach (Railsback et al. 2009)
    if number-hatching-eggs > numberOfEggs [set number-hatching-eggs numberOfEggs]           ;; Number of hatching eggs cannot be greater than number of eggs remaining in the redd (only possible when time step is different from 1)
    set numberOfHatchedEggs (numberOfHatchedEggs + number-hatching-eggs)                     
    set ReachNumberFryHatched (ReachNumberFryHatched + number-hatching-eggs)                 ;; Write info into global counter for plotting purposes 
    
    ;; Compute genetic variables at the population level
    let MPopMean-SpawnMinLength fishSpawnMinLengthMeanM                                     ;; Define mean maturity threshold at the population level
    let FPopMean-SpawnMinLength fishSpawnMinLengthMeanF
    let fishNewLengthAddVar (fishNewLengthVar * fishNewLengthHeritability)                  ;; Define the additive genetic variance at the population level        
    let fishNeutralTraitAddVar (fishNeutralTraitVar * fishNeutralTraitHeritability)
    let fishSpawnMinLengthAddVar 0
    let mutatVar-fishNewLength 0                                                            ;; Unles specified otherwise, mutational variance is set to 0
    let mutatVar-fishNeutralTrait 0
    let mutatVar-fishSpawnMinLength 0
    let breeders trout with [spawnedThisSeason? = true]
    if FixedAdditiveVar? = false                                                            ;; If the total additive variance is not fixed across generations,
        [                                                                                   ;; then it is computed as the variance of the breeding values.
         if count breeders > 1                                                              ;; If there is only one breeder, 
          [                                                                                 ;; then the additive variance takes the value fixed at initialization.
           set fishNewLengthAddVar variance [genNewLength] of breeders
           set fishNeutralTraitAddVar variance [genNeutralTrait] of breeders
          ]    
         if count breeders with [sex = "M"] > 0 [set MPopMean-SpawnMinLength mean [genSpawnMinLength] of breeders with [sex = "M"]]
         if count breeders with [sex = "F"] > 0 [set FPopMean-SpawnMinLength mean [genSpawnMinLength] of breeders with [sex = "F"]] 
         if count breeders with [sex = "M"] > 1 and count breeders with [sex = "F"] > 1
          [set fishSpawnMinLengthAddVar mean list (variance [genSpawnMinLength] of breeders with [sex = "M"]) (variance [genSpawnMinLength] of breeders with [sex = "F"])]
        ]
    
    hatch-trout number-hatching-eggs 
     [
       set my-cell [my-cell] of myself move-to one-of [my-patches] of my-cell set color yellow set size 7  
       ifelse random-float 1 > 0.5 [ set sex "M" ] [ set sex "F" ]
       set age 0 set age-class 0
             
       ifelse sex = "M"                       ;; Additive variance for maturity threshold may vary across sexes 
        [
         ifelse FixedAdditiveVar?  
          [set fishSpawnMinLengthAddVar (fishSpawnMinLengthVarM * fishSpawnMinLengthHeritability)]  
          [if count breeders with [sex = "M"] < 2 or count breeders with [sex = "F"] < 2 [set fishSpawnMinLengthAddVar (fishSpawnMinLengthVarM * fishSpawnMinLengthHeritability)]]
        ]
        [
         ifelse FixedAdditiveVar?  
          [set fishSpawnMinLengthAddVar (fishSpawnMinLengthVarF * fishSpawnMinLengthHeritability)]  
          [if count breeders with [sex = "M"] < 2 or count breeders with [sex = "F"] < 2 [set fishSpawnMinLengthAddVar (fishSpawnMinLengthVarF * fishSpawnMinLengthHeritability)]]
        ]
       
       if mutationFactor > 0              ;; If mutation is taken into account, then the mutational variance is set as a function of the environmental variance of the trait
        [
         set mutatVar-fishNewLength (mutationalVarParam * (environVar-for fishNewLengthVar fishNewLengthHeritability))          
         set mutatVar-fishNeutralTrait (mutationalVarParam * (environVar-for fishNeutralTraitVar fishNeutralTraitHeritability))
         ifelse sex = "M"
          [set mutatVar-fishSpawnMinLength (mutationalVarParam * (environVar-for fishSpawnMinLengthVarM fishSpawnMinLengthHeritability))]
          [set mutatVar-fishSpawnMinLength (mutationalVarParam * (environVar-for fishSpawnMinLengthVarF fishSpawnMinLengthHeritability))] 
        ]
       let fatherId random (length [reddFathersgenNewLength] of myself)          ;; Randomly select the father's genetic inheritance to be transmitted to the new trout. Only one father can fertilize an egg.
       set genNewLength (genotypic-value [reddMothergenNewLength] of myself fatherId [reddFathersgenNewLength] of myself fishNewLengthMean fishNewLengthAddVar mutatVar-fishNewLength) 
       set genNeutralTrait (genotypic-value [reddMothergenNeutralTrait] of myself fatherId [reddFathersgenNeutralTrait] of myself fishNeutralTraitMean fishNeutralTraitAddVar mutatVar-fishNeutralTrait)
       set genSpawnMinLength (genVal-SpawnMinLength sex [reddMothergenSpawnMinLength] of myself fatherId [reddFathersgenSpawnMinLength] of myself FPopMean-SpawnMinLength MPopMean-SpawnMinLength fishSpawnMinLengthAddVar mutatVar-fishSpawnMinLength)
;       if  genNewLength > fishMaxNewLength [set genNewLength fishMaxNewLength]                                        ;; Genotypic length at emergence cannot be over a user-defined threshold
       set fishNewLength (genNewLength + (environ-value-for fishNewLengthVar fishNewLengthHeritability)) 
       if  fishNewLength < fishMinNewLength [set fishNewLength fishMinNewLength]                                      ;; Length at emergence cannot be below a user-defined threshold
       if  fishNewLength > fishMaxNewLength [set fishNewLength fishMaxNewLength]                                      ;; Length at emergence cannot be over a user-defined threshold
       set fishNeutralTrait (genNeutralTrait + (environ-value-for fishNeutralTraitVar fishNeutralTraitHeritability))    
       ifelse sex = "M"
        [set fishSpawnMinLength (genSpawnMinLength + (environ-value-for fishSpawnMinLengthVarM fishSpawnMinLengthHeritability))]
        [set fishSpawnMinLength (genSpawnMinLength + (environ-value-for fishSpawnMinLengthVarF fishSpawnMinLengthHeritability))]          
       
       set maturityStatus "non-mature" 
       set spawnedThisSeason? false        
       
       set fishLength fishNewLength
       set fishWeight (fishWeightParamA * fishLength ^ fishWeightParamB)
       set fishCondition (fishWeight / (fishWeightParamA * fishLength ^ fishWeightParamB)) 
       set status "alive"
     ]
   
    ;; Writing info into global counters por plotting purposes
    set EmergenceDate fput (time:ts-get environmental-time-series tick-date  "juliandate") EmergenceDate   
    set ReachNumberOfEggs (ReachNumberOfEggs - number-hatching-eggs)
    
    set numberOfEggs (numberOfEggs - number-hatching-eggs)  ;; Number of eggs still left in the redd to hatch is updated
    if numberOfEggs = 0         ;; When the number of remaining eggs in a redd reaches zero, the redd is dropped from the model and writes its info in the output file
     [
      if ReddOutput? [write-ReddOutput] 
      die
     ]              
   ]
end




;;;;;;;;;;;;;;;;;;;;;;;;;;;; PLOTS AND OUTPUT FILES PROCEDURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Code for plotting procedures copied from the NetLogo software Beehave (Becher 2013)

to plot-modelOutputs
 if Plotting?
 [
   GenericPlottingProc "Daily dynamics" DemoPlot1 
   
  let juliandate time:ts-get environmental-time-series tick-date  "juliandate"
  if juliandate >= OutputDate and juliandate < OutputDate + timestep-scale 
    [
     GenericPlottingProc "Demographics" DemoPlot2
     GenericPlottingProc "Life History" LifeHistory1
     GenericPlottingProc "Life History 2" LifeHistory2
     GenericPlottingProc "Genetics" Genetics1
     GenericPlottingProc "Genetics 2" Genetics2
    ] 
 ]  
end

to GenericPlottingProc [plotname plotChoice]
  set-current-plot plotname
  
  if plotChoice = "Numbers" 
  [ 
    create-temporary-plot-pen "Total"
    set-plot-pen-color black
    plot count trout with [status = "alive"]
    create-temporary-plot-pen "Age 0"
    set-plot-pen-color yellow
    plot count trout with [age-class = 0 and status = "alive"]
    create-temporary-plot-pen "Age 1+"
    set-plot-pen-color red
    plot count trout with [age-class > 0 and status = "alive"]
  ]
  
  if plotChoice = "Biomass [kg]" 
  [ 
    create-temporary-plot-pen "Total"
    set-plot-pen-color black
    plot sum [fishWeight] of trout with [status = "alive"] / 1000
    create-temporary-plot-pen "Age 0"
    set-plot-pen-color yellow
    plot sum [fishWeight] of trout with [age-class = 0 and status = "alive"] / 1000
    create-temporary-plot-pen "Age 1+"
    set-plot-pen-color red
    plot sum [fishWeight] of trout with [age-class > 0 and status = "alive"] / 1000
  ]
  
  if plotChoice = "Dead fish" 
  [ 
    create-temporary-plot-pen "HighTemp"
    set-plot-pen-color orange
    plot fishDeadHighTempTot
    create-temporary-plot-pen "HighVel"
    set-plot-pen-color sky
    plot fishDeadHighVelTot
    create-temporary-plot-pen "Strand"
    set-plot-pen-color brown
    plot fishDeadStrandTot
    create-temporary-plot-pen "PoorCond"
    set-plot-pen-color red
    plot fishDeadPoorCondTot
    create-temporary-plot-pen "TerrPred"
    set-plot-pen-color black
    plot fishDeadTerrPredTot
    create-temporary-plot-pen "AqPred"
    set-plot-pen-color blue
    plot fishDeadAqPredTot
    create-temporary-plot-pen "Angling"
    set-plot-pen-color green
    plot fishDeadAnglingTot
    create-temporary-plot-pen "Hooking"
    set-plot-pen-color lime
    plot fishDeadHookingTot
  ]
  
   if plotChoice = "Number of eggs" 
  [ 
    create-temporary-plot-pen "Eggs"
    set-plot-pen-color black
    plot ReachNumberOfEggs
  ]  
  
 if plotChoice = "Number of breeders" 
  [ 
    create-temporary-plot-pen "Females"
    set-plot-pen-color red
    ifelse not empty? FemaleSpawnersLength [plot length FemaleSpawnersLength] [plot 0]
    create-temporary-plot-pen "Males"
    set-plot-pen-color blue
    ifelse not empty? MaleSpawnersLength [plot length MaleSpawnersLength] [plot 0]
  ]  
      
  if plotChoice = "Initial number eggs - Hatched fry" 
  [ 
    create-temporary-plot-pen "Initial Eggs"
    set-plot-pen-color black
    plot ReachIniNumberOfEggs
    create-temporary-plot-pen "Hatched fry"
    set-plot-pen-color green
    plot ReachNumberFryHatched
  ]  
  
  if plotChoice = "Length-at-age" 
  [ 
    let aliveTroutAge0 trout with [status = "alive" and age-class = 0]
    let aliveTroutAge1 trout with [status = "alive" and age-class = 1]
    let aliveTroutAge2 trout with [status = "alive" and age-class = 2]
    let aliveTroutAge3Plus trout with [status = "alive" and age > 3 * 365]
    
    create-temporary-plot-pen "Age 0"
    set-plot-pen-color yellow
    ifelse any? aliveTroutAge0 [plot mean [fishLength] of aliveTroutAge0] [plot 0] 
    create-temporary-plot-pen "Age 1"
    set-plot-pen-color orange
    ifelse any? aliveTroutAge1 [plot mean [fishLength] of aliveTroutAge1] [plot 0]
    create-temporary-plot-pen "Age 2"
    set-plot-pen-color red
    ifelse any? aliveTroutAge2 [plot mean [fishLength] of aliveTroutAge2] [plot 0]
    create-temporary-plot-pen "Age 3+"
    set-plot-pen-color brown
    ifelse any? aliveTroutAge3Plus [plot mean [fishLength] of aliveTroutAge3Plus] [plot 0]
  ]  

  if plotChoice = "Length-at-spawning-females" 
  [ 
    create-temporary-plot-pen "Min"
    set-plot-pen-color red
    ifelse not empty? FemaleSpawnersLength [plot min FemaleSpawnersLength] [plot 0] 
    create-temporary-plot-pen "Mean"
    set-plot-pen-color black
    ifelse not empty? FemaleSpawnersLength [plot mean FemaleSpawnersLength] [plot 0] 
    create-temporary-plot-pen "Max"
    set-plot-pen-color green
    ifelse not empty? FemaleSpawnersLength [plot max FemaleSpawnersLength] [plot 0] 
  ]  
  
  if plotChoice = "Length-at-spawning-males" 
  [ 
    create-temporary-plot-pen "Min"
    set-plot-pen-color red
    ifelse not empty? MaleSpawnersLength [plot min MaleSpawnersLength] [plot 0] 
    create-temporary-plot-pen "Mean"
    set-plot-pen-color black
    ifelse not empty? MaleSpawnersLength [plot mean MaleSpawnersLength] [plot 0] 
    create-temporary-plot-pen "Max"
    set-plot-pen-color green
    ifelse not empty? MaleSpawnersLength [plot max MaleSpawnersLength] [plot 0] 
  ]  
  
  if plotChoice = "Age-at-spawning-females" 
  [ 
    create-temporary-plot-pen "Min"
    set-plot-pen-color red
    ifelse not empty? FemaleSpawnersAge [plot min FemaleSpawnersAge] [plot 0] 
    create-temporary-plot-pen "Mean"
    set-plot-pen-color black
    ifelse not empty? FemaleSpawnersAge [plot mean FemaleSpawnersAge] [plot 0] 
    create-temporary-plot-pen "Max"
    set-plot-pen-color green
    ifelse not empty? FemaleSpawnersAge [plot max FemaleSpawnersAge] [plot 0] 
  ]  
  
  if plotChoice = "Age-at-spawning-males" 
  [ 
    create-temporary-plot-pen "Min"
    set-plot-pen-color red
    ifelse not empty? MaleSpawnersAge [plot min MaleSpawnersAge] [plot 0] 
    create-temporary-plot-pen "Mean"
    set-plot-pen-color black
    ifelse not empty? MaleSpawnersAge [plot mean MaleSpawnersAge] [plot 0] 
    create-temporary-plot-pen "Max"
    set-plot-pen-color green
    ifelse not empty? MaleSpawnersAge [plot max MaleSpawnersAge] [plot 0] 
  ]  
  
  if plotChoice = "Spawning date" 
  [ 
    create-temporary-plot-pen "Min"
    set-plot-pen-color red
    ifelse not empty? SpawningDate [plot min SpawningDate] [plot 0] 
    create-temporary-plot-pen "Mean"
    set-plot-pen-color black
    ifelse not empty? SpawningDate [plot mean SpawningDate] [plot 0] 
    create-temporary-plot-pen "Max"
    set-plot-pen-color green
    ifelse not empty? SpawningDate [plot max SpawningDate] [plot 0] 
  ]    
  
  if plotChoice = "Emergence date" 
  [ 
    create-temporary-plot-pen "Min"
    set-plot-pen-color red
    ifelse not empty? EmergenceDate [plot min EmergenceDate] [plot 0] 
    create-temporary-plot-pen "Mean"
    set-plot-pen-color black
    ifelse not empty? EmergenceDate [plot mean EmergenceDate] [plot 0] 
    create-temporary-plot-pen "Max"
    set-plot-pen-color green
    ifelse not empty? EmergenceDate [plot max EmergenceDate] [plot 0] 
  ]      

  if plotChoice = "Age at death" 
  [ 
    create-temporary-plot-pen "Min"
    set-plot-pen-color red
    ifelse not empty? AgeatDeath [plot min AgeatDeath] [plot 0] 
    create-temporary-plot-pen "Mean"
    set-plot-pen-color black
    ifelse not empty? AgeatDeath [plot mean AgeatDeath] [plot 0] 
    create-temporary-plot-pen "Max"
    set-plot-pen-color green
    ifelse not empty? AgeatDeath [plot max AgeatDeath] [plot 0] 
  ]        

  if plotChoice = "Genotypic maturity threshold females" 
  [ 
    create-temporary-plot-pen "Min"
    set-plot-pen-color red
    ifelse not empty? FemaleBreedGenMinSpawnLength [plot min FemaleBreedGenMinSpawnLength] [plot 0] 
    create-temporary-plot-pen "Mean"
    set-plot-pen-color black
    ifelse not empty? FemaleBreedGenMinSpawnLength [plot mean FemaleBreedGenMinSpawnLength] [plot 0] 
    create-temporary-plot-pen "Max"
    set-plot-pen-color green
    ifelse not empty? FemaleBreedGenMinSpawnLength [plot max FemaleBreedGenMinSpawnLength] [plot 0] 
  ]          

  if plotChoice = "Genotypic maturity threshold males" 
  [ 
    create-temporary-plot-pen "Min"
    set-plot-pen-color red
    ifelse not empty? MaleBreedGenMinSpawnLength [plot min MaleBreedGenMinSpawnLength] [plot 0] 
    create-temporary-plot-pen "Mean"
    set-plot-pen-color black
    ifelse not empty? MaleBreedGenMinSpawnLength [plot mean MaleBreedGenMinSpawnLength] [plot 0] 
    create-temporary-plot-pen "Max"
    set-plot-pen-color green
    ifelse not empty? MaleBreedGenMinSpawnLength [plot max MaleBreedGenMinSpawnLength] [plot 0] 
  ]     

  if plotChoice = "Genotypic length-at-emergence" 
  [ 
    create-temporary-plot-pen "Min"
    set-plot-pen-color red
    ifelse not empty? GenBreedEmergenceLength [plot min GenBreedEmergenceLength] [plot 0] 
    create-temporary-plot-pen "Mean"
    set-plot-pen-color black
    ifelse not empty? GenBreedEmergenceLength [plot mean GenBreedEmergenceLength] [plot 0] 
    create-temporary-plot-pen "Max"
    set-plot-pen-color green
    ifelse not empty? GenBreedEmergenceLength [plot max GenBreedEmergenceLength] [plot 0] 
  ]    

  if plotChoice = "Genotypic neutral trait" 
  [ 
    create-temporary-plot-pen "Min"
    set-plot-pen-color red
    ifelse not empty? GenBreedNeutralTrait [plot min GenBreedNeutralTrait] [plot 0] 
    create-temporary-plot-pen "Mean"
    set-plot-pen-color black
    ifelse not empty? GenBreedNeutralTrait [plot mean GenBreedNeutralTrait] [plot 0] 
    create-temporary-plot-pen "Max"
    set-plot-pen-color green
    ifelse not empty? GenBreedNeutralTrait [plot max GenBreedNeutralTrait] [plot 0] 
  ]          
  
end


to write-modelOutputs
;  let printedoutputs n-values item 0 (matrix:dimensions depth-data) [?] 
  let printedoutputs n-values simulation-length [?]
  foreach printedoutputs [if LiveFishOutput? = true and AnnualFishOutput? = false and ticks = fileOutputFreq * (item ? printedoutputs) [write-LiveFishOutput]]
  foreach printedoutputs [if DeadFishOutput? = true and AnnualFishOutput? = false and ticks = fileOutputFreq * (item ? printedoutputs) [write-DeadFishOutput]] 
  foreach printedoutputs [if HabSelecOutput? = true and ticks = fileOutputFreq * (item ? printedoutputs) [ask cells [write-HabSelecOutput]]]  
  
  let juliandate time:ts-get environmental-time-series tick-date  "juliandate"
  if AnnualFishOutput? = true and LiveFishOutput? = true and juliandate >= OutputDate and juliandate < OutputDate + timestep-scale [write-LiveFishOutput]
  if AnnualFishOutput? = true and DeadFishOutput? = true and juliandate >= OutputDate and juliandate < OutputDate + timestep-scale [write-DeadFishOutput] 
  if BreedersOutput? = true and juliandate >= OutputDate and juliandate < OutputDate + timestep-scale [write-BreedersPopOutput]   
  if MortAnglingHooking = true and AnglingOutput? = true and juliandate = endAnglingSeason [write-AnglingStatsOutput]
  if MortAnglingHooking = true and AnglingOutput? = true and juliandate >= OutputDate and juliandate < OutputDate + timestep-scale [write-AnglingPopulationOutput]
end


to write-LiveFishOutput
  let output-file-name (word "LiveFishOutput-" behaviorspace-run-number ".csv")
  let date time:show tick-date "dd-MM-yyyy"
  let year time:get "year" tick-date
  let aliveTroutAge0 trout with [status = "alive" and age-class = 0]
  let aliveTroutAge0M trout with [status = "alive" and age-class = 0 and sex = "M"]
  let aliveTroutAge0F trout with [status = "alive" and age-class = 0 and sex = "F"] 
  let aliveTroutAge1 trout with [status = "alive" and age-class = 1]
  let aliveTroutAge1M trout with [status = "alive" and age-class = 1 and sex = "M"]
  let aliveTroutAge1F trout with [status = "alive" and age-class = 1 and sex = "F"] 
  let aliveTroutAge2 trout with [status = "alive" and age-class = 2]
  let aliveTroutAge2M trout with [status = "alive" and age-class = 2 and sex = "M"]
  let aliveTroutAge2F trout with [status = "alive" and age-class = 2 and sex = "F"] 
  let aliveTroutAge3Plus trout with [status = "alive" and age-class > 2]
  let aliveTroutAge3PlusM trout with [status = "alive" and age-class > 2 and sex = "M"]
  let aliveTroutAge3PlusF trout with [status = "alive" and age-class > 2 and sex = "F"]
  
  
  file-open output-file-name
  file-type date file-type "," file-type year file-type "," file-type "Age0" file-type "," 
  file-type count aliveTroutAge0 file-type ","
  file-type count aliveTroutAge0 with [fishLength >= fishSpawnMinLength] file-type ","
  ifelse any? aliveTroutAge0 [file-type mean [age] of aliveTroutAge0] [file-type "-"] file-type ","
  ifelse count aliveTroutAge0 > 1 [file-type standard-deviation [age] of aliveTroutAge0] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge0 [file-type mean [fishLength] of aliveTroutAge0] [file-type "-"] file-type ","
  ifelse count aliveTroutAge0 > 1 [file-type standard-deviation [fishLength] of aliveTroutAge0] [file-type "-"] file-type ","
  file-type sum [fishWeight] of aliveTroutAge0 file-type ","
  ifelse any? aliveTroutAge0 [file-type mean [fishWeight] of aliveTroutAge0] [file-type "-"] file-type ","
  ifelse count aliveTroutAge0 > 1 [file-type standard-deviation [fishWeight] of aliveTroutAge0] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge0 [file-type mean [fishCondition] of aliveTroutAge0] [file-type "-"] file-type ","
  ifelse count aliveTroutAge0 > 1 [file-type standard-deviation [fishCondition] of aliveTroutAge0] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge0M [file-type mean [fishSpawnMinLength] of aliveTroutAge0M] [file-type "-"] file-type ","
  ifelse count aliveTroutAge0M > 1 [file-type standard-deviation [fishSpawnMinLength] of aliveTroutAge0M] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge0F [file-type mean [fishSpawnMinLength] of aliveTroutAge0F] [file-type "-"] file-type ","
  ifelse count aliveTroutAge0F > 1 [file-type standard-deviation [fishSpawnMinLength] of aliveTroutAge0F] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge0 [file-type mean [fishNewLength] of aliveTroutAge0] [file-type "-"] file-type ","
  ifelse count aliveTroutAge0 > 1 [file-type standard-deviation [fishNewLength] of aliveTroutAge0] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge0 [file-type mean [fishNeutralTrait] of aliveTroutAge0] [file-type "-"] file-type ","
  ifelse count aliveTroutAge0 > 1 [file-type standard-deviation [fishNeutralTrait] of aliveTroutAge0] [file-type "-"] file-type ","  
  ifelse any? aliveTroutAge0M [file-type mean [genSpawnMinLength] of aliveTroutAge0M] [file-type "-"] file-type ","
  ifelse count aliveTroutAge0M > 1 [file-type standard-deviation [genSpawnMinLength] of aliveTroutAge0M] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge0F [file-type mean [genSpawnMinLength] of aliveTroutAge0F] [file-type "-"] file-type ","
  ifelse count aliveTroutAge0F > 1 [file-type standard-deviation [genSpawnMinLength] of aliveTroutAge0F] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge0 [file-type mean [genNewLength] of aliveTroutAge0] [file-type "-"] file-type ","
  ifelse count aliveTroutAge0 > 1 [file-type standard-deviation [genNewLength] of aliveTroutAge0] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge0 [file-type mean [genNeutralTrait] of aliveTroutAge0] [file-type "-"] file-type ","
  ifelse count aliveTroutAge0 > 1 [file-print standard-deviation [genNeutralTrait] of aliveTroutAge0] [file-print "-"]
  
  file-type date file-type "," file-type year file-type "," file-type "Age1" file-type "," 
  file-type count aliveTroutAge1 file-type ","
  file-type count aliveTroutAge1 with [fishLength >= fishSpawnMinLength] file-type ","
  ifelse any? aliveTroutAge1 [file-type mean [age] of aliveTroutAge1] [file-type "-"] file-type ","
  ifelse count aliveTroutAge1 > 1 [file-type standard-deviation [age] of aliveTroutAge1] [file-type "-"] file-type ","  
  ifelse any? aliveTroutAge1 [file-type mean [fishLength] of aliveTroutAge1] [file-type "-"] file-type ","
  ifelse count aliveTroutAge1 > 1 [file-type standard-deviation [fishLength] of aliveTroutAge1] [file-type "-"] file-type ","
  file-type sum [fishWeight] of aliveTroutAge1 file-type ","
  ifelse any? aliveTroutAge1 [file-type mean [fishWeight] of aliveTroutAge1] [file-type "-"] file-type ","
  ifelse count aliveTroutAge1 > 1 [file-type standard-deviation [fishWeight] of aliveTroutAge1] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge1 [file-type mean [fishCondition] of aliveTroutAge1] [file-type "-"] file-type ","
  ifelse count aliveTroutAge1 > 1 [file-type standard-deviation [fishCondition] of aliveTroutAge1] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge1M [file-type mean [fishSpawnMinLength] of aliveTroutAge1M] [file-type "-"] file-type ","
  ifelse count aliveTroutAge1M > 1 [file-type standard-deviation [fishSpawnMinLength] of aliveTroutAge1M] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge1F [file-type mean [fishSpawnMinLength] of aliveTroutAge1F] [file-type "-"] file-type ","
  ifelse count aliveTroutAge1F > 1 [file-type standard-deviation [fishSpawnMinLength] of aliveTroutAge1F] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge1 [file-type mean [fishNewLength] of aliveTroutAge1] [file-type "-"] file-type ","
  ifelse count aliveTroutAge1 > 1 [file-type standard-deviation [fishNewLength] of aliveTroutAge1] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge1 [file-type mean [fishNeutralTrait] of aliveTroutAge1] [file-type "-"] file-type ","
  ifelse count aliveTroutAge1 > 1 [file-type standard-deviation [fishNeutralTrait] of aliveTroutAge1] [file-type "-"] file-type ","    
  ifelse any? aliveTroutAge1M [file-type mean [genSpawnMinLength] of aliveTroutAge1M] [file-type "-"] file-type ","
  ifelse count aliveTroutAge1M > 1 [file-type standard-deviation [genSpawnMinLength] of aliveTroutAge1M] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge1F [file-type mean [genSpawnMinLength] of aliveTroutAge1F] [file-type "-"] file-type ","
  ifelse count aliveTroutAge1F > 1 [file-type standard-deviation [genSpawnMinLength] of aliveTroutAge1F] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge1 [file-type mean [genNewLength] of aliveTroutAge1] [file-type "-"] file-type ","
  ifelse count aliveTroutAge1 > 1 [file-type standard-deviation [genNewLength] of aliveTroutAge1] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge1 [file-type mean [genNeutralTrait] of aliveTroutAge1] [file-type "-"] file-type ","
  ifelse count aliveTroutAge1 > 1 [file-print standard-deviation [genNeutralTrait] of aliveTroutAge1] [file-print "-"]
  
  file-type date file-type "," file-type year file-type "," file-type "Age2" file-type "," 
  file-type count aliveTroutAge2 file-type ","
  file-type count aliveTroutAge2 with [fishLength >= fishSpawnMinLength] file-type ","
  ifelse any? aliveTroutAge2 [file-type mean [age] of aliveTroutAge2] [file-type "-"] file-type ","
  ifelse count aliveTroutAge2 > 1 [file-type standard-deviation [age] of aliveTroutAge2] [file-type "-"] file-type ","  
  ifelse any? aliveTroutAge2 [file-type mean [fishLength] of aliveTroutAge2] [file-type "-"] file-type ","
  ifelse count aliveTroutAge2 > 1 [file-type standard-deviation [fishLength] of aliveTroutAge2] [file-type "-"] file-type ","
  file-type sum [fishWeight] of aliveTroutAge2 file-type ","
  ifelse any? aliveTroutAge2 [file-type mean [fishWeight] of aliveTroutAge2] [file-type "-"] file-type ","
  ifelse count aliveTroutAge2 > 1 [file-type standard-deviation [fishWeight] of aliveTroutAge2] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge2 [file-type mean [fishCondition] of aliveTroutAge2] [file-type "-"] file-type ","
  ifelse count aliveTroutAge2 > 1 [file-type standard-deviation [fishCondition] of aliveTroutAge2] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge2M [file-type mean [fishSpawnMinLength] of aliveTroutAge2M] [file-type "-"] file-type ","
  ifelse count aliveTroutAge2M > 1 [file-type standard-deviation [fishSpawnMinLength] of aliveTroutAge2M] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge2F [file-type mean [fishSpawnMinLength] of aliveTroutAge2F] [file-type "-"] file-type ","
  ifelse count aliveTroutAge2F > 1 [file-type standard-deviation [fishSpawnMinLength] of aliveTroutAge2F] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge2 [file-type mean [fishNewLength] of aliveTroutAge2] [file-type "-"] file-type ","
  ifelse count aliveTroutAge2 > 1 [file-type standard-deviation [fishNewLength] of aliveTroutAge2] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge2 [file-type mean [fishNeutralTrait] of aliveTroutAge2] [file-type "-"] file-type ","
  ifelse count aliveTroutAge2 > 1 [file-type standard-deviation [fishNeutralTrait] of aliveTroutAge2] [file-type "-"] file-type ","  
  ifelse any? aliveTroutAge2M [file-type mean [genSpawnMinLength] of aliveTroutAge2M] [file-type "-"] file-type ","
  ifelse count aliveTroutAge2M > 1 [file-type standard-deviation [genSpawnMinLength] of aliveTroutAge2M] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge2F [file-type mean [genSpawnMinLength] of aliveTroutAge2F] [file-type "-"] file-type ","
  ifelse count aliveTroutAge2F > 1 [file-type standard-deviation [genSpawnMinLength] of aliveTroutAge2F] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge2 [file-type mean [genNewLength] of aliveTroutAge2] [file-type "-"] file-type ","
  ifelse count aliveTroutAge2 > 1 [file-type standard-deviation [genNewLength] of aliveTroutAge2] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge2 [file-type mean [genNeutralTrait] of aliveTroutAge2] [file-type "-"] file-type ","
  ifelse count aliveTroutAge2 > 1 [file-print standard-deviation [genNeutralTrait] of aliveTroutAge2] [file-print "-"]
  
 file-type date file-type "," file-type year file-type "," file-type "Age3Plus" file-type "," 
  file-type count aliveTroutAge3Plus file-type ","
  file-type count aliveTroutAge3Plus with [fishLength >= fishSpawnMinLength] file-type ","
  ifelse any? aliveTroutAge3Plus [file-type mean [age] of aliveTroutAge3Plus] [file-type "-"] file-type ","
  ifelse count aliveTroutAge3Plus > 1 [file-type standard-deviation [age] of aliveTroutAge3Plus] [file-type "-"] file-type ","  
  ifelse any? aliveTroutAge3Plus [file-type mean [fishLength] of aliveTroutAge3Plus] [file-type "-"] file-type ","
  ifelse count aliveTroutAge3Plus > 1 [file-type standard-deviation [fishLength] of aliveTroutAge3Plus] [file-type "-"] file-type ","
  file-type sum [fishWeight] of aliveTroutAge3Plus file-type ","
  ifelse any? aliveTroutAge3Plus [file-type mean [fishWeight] of aliveTroutAge3Plus] [file-type "-"] file-type ","
  ifelse count aliveTroutAge3Plus > 1 [file-type standard-deviation [fishWeight] of aliveTroutAge3Plus] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge3Plus [file-type mean [fishCondition] of aliveTroutAge3Plus] [file-type "-"] file-type ","
  ifelse count aliveTroutAge3Plus > 1 [file-type standard-deviation [fishCondition] of aliveTroutAge3Plus] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge3PlusM [file-type mean [fishSpawnMinLength] of aliveTroutAge3PlusM] [file-type "-"] file-type ","
  ifelse count aliveTroutAge3PlusM > 1 [file-type standard-deviation [fishSpawnMinLength] of aliveTroutAge3PlusM] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge3PlusF [file-type mean [fishSpawnMinLength] of aliveTroutAge3PlusF] [file-type "-"] file-type ","
  ifelse count aliveTroutAge3PlusF > 1 [file-type standard-deviation [fishSpawnMinLength] of aliveTroutAge3PlusF] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge3Plus [file-type mean [fishNewLength] of aliveTroutAge3Plus] [file-type "-"] file-type ","
  ifelse count aliveTroutAge3Plus > 1 [file-type standard-deviation [fishNewLength] of aliveTroutAge3Plus] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge3Plus [file-type mean [fishNeutralTrait] of aliveTroutAge3Plus] [file-type "-"] file-type ","
  ifelse count aliveTroutAge3Plus > 1 [file-type standard-deviation [fishNeutralTrait] of aliveTroutAge3Plus] [file-type "-"] file-type "," 
  ifelse any? aliveTroutAge3PlusM [file-type mean [genSpawnMinLength] of aliveTroutAge3PlusM] [file-type "-"] file-type ","
  ifelse count aliveTroutAge3PlusM > 1 [file-type standard-deviation [genSpawnMinLength] of aliveTroutAge3PlusM] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge3PlusF [file-type mean [genSpawnMinLength] of aliveTroutAge3PlusF] [file-type "-"] file-type ","
  ifelse count aliveTroutAge3PlusF > 1 [file-type standard-deviation [genSpawnMinLength] of aliveTroutAge3PlusF] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge3Plus [file-type mean [genNewLength] of aliveTroutAge3Plus] [file-type "-"] file-type ","
  ifelse count aliveTroutAge3Plus > 1 [file-type standard-deviation [genNewLength] of aliveTroutAge3Plus] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge3Plus [file-type mean [genNeutralTrait] of aliveTroutAge3Plus] [file-type "-"] file-type ","
  ifelse count aliveTroutAge3Plus > 1 [file-print standard-deviation [genNeutralTrait] of aliveTroutAge3Plus] [file-print "-"]  
  file-close 
end


to write-DeadFishOutput  
  let output-file-name (word "DeadFishOutput-" behaviorspace-run-number ".csv")
  let date time:show tick-date "dd-MM-yyyy"
  let year time:get "year" tick-date
  
  file-open output-file-name
  file-print (word date "," year "," "Age0" "," "HighTemp" "," fishDeadHighTempAge0)
  file-print (word date "," year "," "Age0" "," "HighVel" "," fishDeadHighVelAge0)
  file-print (word date "," year "," "Age0" "," "Strand" "," fishDeadStrandAge0)
  file-print (word date "," year "," "Age0" "," "PoorCond" "," fishDeadPoorCondAge0)
  file-print (word date "," year "," "Age0" "," "TerrPred" "," fishDeadTerrPredAge0)
  file-print (word date "," year "," "Age0" "," "AqPred" "," fishDeadAqPredAge0)
  file-print (word date "," year "," "Age0" "," "Angling" "," fishDeadAnglingAge0)
  file-print (word date "," year "," "Age0" "," "Hooking" "," fishDeadHookingAge0)
  file-print (word date "," year "," "Age1" "," "HighTemp" "," fishDeadHighTempAge1)
  file-print (word date "," year "," "Age1" "," "HighVel" "," fishDeadHighVelAge1)
  file-print (word date "," year "," "Age1" "," "Strand" "," fishDeadStrandAge1)
  file-print (word date "," year "," "Age1" "," "PoorCond" "," fishDeadPoorCondAge1)
  file-print (word date "," year "," "Age1" "," "TerrPred" "," fishDeadTerrPredAge1)
  file-print (word date "," year "," "Age1" "," "AqPred" "," fishDeadAqPredAge1)
  file-print (word date "," year "," "Age1" "," "Angling" "," fishDeadAnglingAge1)
  file-print (word date "," year "," "Age1" "," "Hooking" "," fishDeadHookingAge1)
  file-print (word date "," year "," "Age2" "," "HighTemp" "," fishDeadHighTempAge2)
  file-print (word date "," year "," "Age2" "," "HighVel" "," fishDeadHighVelAge2)
  file-print (word date "," year "," "Age2" "," "Strand" "," fishDeadStrandAge2)
  file-print (word date "," year "," "Age2" "," "PoorCond" "," fishDeadPoorCondAge2)
  file-print (word date "," year "," "Age2" "," "TerrPred" "," fishDeadTerrPredAge2)
  file-print (word date "," year "," "Age2" "," "AqPred" "," fishDeadAqPredAge2)
  file-print (word date "," year "," "Age2" "," "Angling" "," fishDeadAnglingAge2)
  file-print (word date "," year "," "Age2" "," "Hooking" "," fishDeadHookingAge2)
  file-print (word date "," year "," "Age3Plus" "," "HighTemp" "," fishDeadHighTempAge3Plus)
  file-print (word date "," year "," "Age3Plus" "," "HighVel" "," fishDeadHighVelAge3Plus)
  file-print (word date "," year "," "Age3Plus" "," "Strand" "," fishDeadStrandAge3Plus)
  file-print (word date "," year "," "Age3Plus" "," "PoorCond" "," fishDeadPoorCondAge3Plus)
  file-print (word date "," year "," "Age3Plus" "," "TerrPred" "," fishDeadTerrPredAge3Plus)
  file-print (word date "," year "," "Age3Plus" "," "AqPred" "," fishDeadAqPredAge3Plus)
  file-print (word date "," year "," "Age3Plus" "," "Angling" "," fishDeadAnglingAge3Plus)
  file-print (word date "," year "," "Age3Plus" "," "Hooking" "," fishDeadHookingAge3Plus)
  file-close   
end

to write-ReddOutput
  let output-file-name (word "ReddOutput-" behaviorspace-run-number ".csv")
  let date time:show tick-date "dd-MM-yyyy"
  let year time:get "year" tick-date
  
  file-open output-file-name
  file-type ReddID                    file-type ","
  file-type [who] of my-cell          file-type ","
  file-type creationDate              file-type ","
  file-type ininumberOfEggs           file-type ","
  file-type date                      file-type ","
  file-type year                      file-type ","
  file-type eggsLostToLowTempTot      file-type ","
  file-type eggsLostToHighTempTot     file-type ","
  file-type eggsLostToScourTot        file-type ","
  file-type eggsLostToDewateringTot   file-type ","
  file-type eggsLostToSuperimpTot     file-type ","
  file-print numberOfHatchedEggs
  file-close
end

to write-BreedersPopOutput
  let output-file-name (word "BreedersPopOutput-" behaviorspace-run-number ".csv")
  let date time:show tick-date "dd-MM-yyyy"
  let year time:get "year" tick-date set year (year - 1)
  
  file-open output-file-name
  file-type date file-type "," file-type year file-type ","
  ifelse not empty? FemaleSpawnersLength [file-type length FemaleSpawnersLength] [file-type 0] file-type ","
  ifelse not empty? MaleSpawnersLength [file-type length MaleSpawnersLength] [file-type 0] file-type ","
  ifelse not empty? FemaleSpawnersLength [file-type mean FemaleSpawnersLength] [file-type "-"] file-type ","
  ifelse not empty? FemaleSpawnersLength [ifelse length FemaleSpawnersLength > 1 [file-type standard-deviation FemaleSpawnersLength] [file-type 0]] [file-type "-"] file-type ","
  ifelse not empty? FemaleSpawnersLength [file-type min FemaleSpawnersLength] [file-type "-"] file-type ","
  ifelse not empty? MaleSpawnersLength [file-type mean MaleSpawnersLength] [file-type "-"] file-type ","
  ifelse not empty? MaleSpawnersLength [ifelse length MaleSpawnersLength > 1 [file-type standard-deviation MaleSpawnersLength] [file-type 0]] [file-type "-"] file-type ","
  ifelse not empty? MaleSpawnersLength [file-type min MaleSpawnersLength] [file-type "-"] file-type ","
  ifelse not empty? FemaleSpawnersAge [file-type mean FemaleSpawnersAge] [file-type "-"] file-type ","
  ifelse not empty? FemaleSpawnersAge [ifelse length FemaleSpawnersAge > 1 [file-type standard-deviation FemaleSpawnersAge] [file-type 0]] [file-type "-"] file-type ","
  ifelse not empty? FemaleSpawnersAge [file-type min FemaleSpawnersAge] [file-type "-"] file-type ","
  ifelse not empty? MaleSpawnersAge [file-type mean MaleSpawnersAge] [file-type "-"] file-type ","
  ifelse not empty? MaleSpawnersAge [ifelse length MaleSpawnersAge > 1 [file-type standard-deviation MaleSpawnersAge] [file-type 0]] [file-type "-"] file-type ","
  ifelse not empty? MaleSpawnersAge [file-type min MaleSpawnersAge] [file-type "-"] file-type ","
  ifelse not empty? SpawningDate [file-type mean SpawningDate] [file-type "-"] file-type ","
  ifelse not empty? SpawningDate [ifelse length SpawningDate > 1 [file-type standard-deviation SpawningDate] [file-type 0]] [file-type "-"] file-type ","
  ifelse not empty? SpawningDate [file-type min SpawningDate] [file-type "-"] file-type ","
  ifelse not empty? FemaleBreedGenMinSpawnLength [file-type mean FemaleBreedGenMinSpawnLength] [file-type "-"] file-type ","
  ifelse not empty? FemaleBreedGenMinSpawnLength [ifelse length FemaleBreedGenMinSpawnLength > 1 [file-type standard-deviation FemaleBreedGenMinSpawnLength] [file-type 0]] [file-type "-"] file-type ","
  ifelse not empty? FemaleBreedGenMinSpawnLength [file-type min FemaleBreedGenMinSpawnLength] [file-type "-"] file-type ","
  ifelse not empty? MaleBreedGenMinSpawnLength [file-type mean MaleBreedGenMinSpawnLength] [file-type "-"] file-type ","
  ifelse not empty? MaleBreedGenMinSpawnLength [ifelse length MaleBreedGenMinSpawnLength > 1 [file-type standard-deviation MaleBreedGenMinSpawnLength] [file-type 0]] [file-type "-"] file-type ","
  ifelse not empty? MaleBreedGenMinSpawnLength [file-type min MaleBreedGenMinSpawnLength] [file-type "-"] file-type ","
  ifelse not empty? GenBreedEmergenceLength [file-type mean GenBreedEmergenceLength] [file-type "-"] file-type ","
  ifelse not empty? GenBreedEmergenceLength [ifelse length GenBreedEmergenceLength > 1 [file-type standard-deviation GenBreedEmergenceLength] [file-type 0]] [file-type "-"] file-type ","
  ifelse not empty? GenBreedEmergenceLength [file-type min GenBreedEmergenceLength] [file-type "-"] file-type ","
  ifelse not empty? GenBreedNeutralTrait [file-type mean GenBreedNeutralTrait] [file-type "-"] file-type ","
  ifelse not empty? GenBreedNeutralTrait [ifelse length GenBreedNeutralTrait > 1 [file-type standard-deviation GenBreedNeutralTrait] [file-type 0]] [file-type "-"] file-type ","
  ifelse not empty? GenBreedNeutralTrait [file-type min GenBreedNeutralTrait] [file-type "-"] file-type ","
  ifelse not empty? EmergenceDate [file-type mean EmergenceDate] [file-type "-"] file-type ","
  ifelse not empty? EmergenceDate [ifelse length EmergenceDate > 1 [file-type standard-deviation EmergenceDate] [file-type 0]] [file-type "-"] file-type ","
  ifelse not empty? EmergenceDate [file-print min EmergenceDate] [file-print "-"] 
  file-close   
end
  
to write-BreedersIndOutput
  let output-file-name (word "BreedersIndOutput-" behaviorspace-run-number ".csv")
  let date time:show tick-date "dd-MM-yyyy"
  let year time:get "year" tick-date
  let juliandate time:ts-get environmental-time-series tick-date  "juliandate"
  
  file-open output-file-name
  file-type date                                               file-type ","
  file-type year                                               file-type ","
  file-type juliandate                                         file-type ","
  ifelse sex = "F" [file-type who] [file-type [who] of myself] file-type ","
  file-type who                                                file-type ","
  file-type sex                                                file-type ","
  file-type age-class                                          file-type ","
  file-type age                                                file-type ","
  file-type fishLength                                         file-type ","
  file-type fishWeight                                         file-type ","
  file-type fishSpawnMinLength                                 file-type "," 
  file-type fishNewLength                                      file-type ","
  file-type fishNeutralTrait                                   file-type ","  
  file-type genSpawnMinLength                                  file-type ","
  file-type genNewLength                                       file-type ","
  file-print genNeutralTrait 
  file-close   
end

to write-HabSelecOutput
  let output-file-name (word "HabSelecOutput-" behaviorspace-run-number ".csv")
  let date time:show tick-date "dd-MM-yyyy"
  let year time:get "year" tick-date
  let juliandate time:ts-get environmental-time-series tick-date  "juliandate"
  let aliveTroutAge0 trout with [status = "alive" and age-class = 0 and [who] of my-cell = [who] of myself] 
  let aliveTroutAge1 trout with [status = "alive" and age-class = 1 and [who] of my-cell = [who] of myself]
  let aliveTroutAge2 trout with [status = "alive" and age-class = 2 and [who] of my-cell = [who] of myself]
  let aliveTroutAge3Plus trout with [status = "alive" and age-class > 2 and [who] of my-cell = [who] of myself]  
  
  file-open output-file-name
  file-type date                                 file-type ","
  file-type year                                 file-type ","
  file-type juliandate                           file-type ","
  file-type flow                                 file-type ","
  file-type temp                                 file-type ","
  file-type who                                  file-type ","
  file-type cellArea                             file-type ","
  file-type cellDepth                            file-type ","
  file-type cellVelocity                         file-type ","
  file-type cellFracShelter                      file-type ","
  file-type cellFracCover                        file-type ","
  file-type cellDistanceToHide                   file-type "," 
  file-type driftHourlyCellTotal                 file-type ","
  file-type searchHourlyCellTotal                file-type ","  
  file-type count aliveTroutAge0                 file-type ","
  file-type count aliveTroutAge1                 file-type ","
  file-type count aliveTroutAge2                 file-type ","
  file-type count aliveTroutAge3Plus             file-type ","
  file-print count trout with [status = "alive"] 
  file-close     
end

to write-AnglingStatsOutput
  let output-file-name (word "AnglingStatsOutput-" behaviorspace-run-number ".csv")
  let date time:show tick-date "dd-MM-yyyy"
  let year time:get "year" tick-date
  let RealExploitationRate precision ((fishDeadAnglingSeason / anglingStock) * 100) 1
  let projectedAnglingHarvest round ((exploitationRate / 100) * anglingStock)
  
  file-open output-file-name
  file-type date                                               file-type ","
  file-type year                                               file-type ","
  file-type anglePressure                                      file-type ","
  file-type exploitationRate                                   file-type ","
  file-type anglingStock                                       file-type ","
  file-type projectedAnglingHarvest                            file-type ","   
  file-type fishDeadAnglingSeason                              file-type ","
  file-type fishDeadAnglingSeasonAge0                          file-type ","
  file-type fishDeadAnglingSeasonAge1                          file-type ","
  file-type fishDeadAnglingSeasonAge2                          file-type ","
  file-type fishDeadAnglingSeasonAge3Plus                      file-type ","
  file-type RealExploitationRate                               file-type ","
  file-type fishDeadHookingSeason                              file-type ","
  file-type fishDeadHookingSeasonAge0                          file-type ","  
  file-type fishDeadHookingSeasonAge1                          file-type ","
  file-type fishDeadHookingSeasonAge2                          file-type ","
  file-print fishDeadHookingSeasonAge3Plus 
  file-close     
end
  
to write-AnglingPopulationOutput  
  let output-file-name (word "AnglingPopulationOutput-" behaviorspace-run-number ".csv")  
  let date time:show tick-date "dd-MM-yyyy"  
  let year time:get "year" tick-date 
  let previousyear (year - 1)
  let juliandate time:ts-get environmental-time-series tick-date  "juliandate"
  let aliveTroutAge0 trout with [status = "alive" and age-class = 0]
  let aliveTroutAge1 trout with [status = "alive" and age-class = 1]
  let aliveTroutAge2 trout with [status = "alive" and age-class = 2]
  let aliveTroutAge3Plus trout with [status = "alive" and age-class > 2]
  let aliveTrout trout with [status = "alive"]
  let NfemaleSpawners 0 if not empty? FemaleSpawnersLength [set NfemaleSpawners length FemaleSpawnersLength] 
  let NmaleSpawners 0 if not empty? MaleSpawnersLength [set NmaleSpawners length MaleSpawnersLength]
  let Nspawners (NfemaleSpawners + NmaleSpawners)
  
  file-open output-file-name
    
  ;; Write population data
  file-type date file-type "," file-type year file-type ","  
  file-type count aliveTroutAge0 file-type ","
  file-type count aliveTroutAge1 file-type ","
  file-type count aliveTroutAge2 file-type ","
  file-type count aliveTroutAge3Plus file-type ","
  ifelse any? aliveTroutAge0 [file-type mean [fishLength] of aliveTroutAge0] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge1 [file-type mean [fishLength] of aliveTroutAge1] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge2 [file-type mean [fishLength] of aliveTroutAge2] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge3Plus [file-type mean [fishLength] of aliveTroutAge3Plus] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge0 [file-type mean [fishWeight] of aliveTroutAge0] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge1 [file-type mean [fishWeight] of aliveTroutAge1] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge2 [file-type mean [fishWeight] of aliveTroutAge2] [file-type "-"] file-type ","
  ifelse any? aliveTroutAge3Plus [file-type mean [fishWeight] of aliveTroutAge3Plus] [file-type "-"] file-type ","  
  file-type sum [fishWeight] of aliveTroutAge0 file-type ","
  file-type sum [fishWeight] of aliveTroutAge1 file-type ","
  file-type sum [fishWeight] of aliveTroutAge2 file-type ","
  file-type sum [fishWeight] of aliveTroutAge3Plus file-type ","
  file-type sum [fishWeight] of aliveTrout file-type ","
  file-type count aliveTrout file-type ","
  ifelse any? aliveTroutAge3Plus [file-type mean [age] of aliveTroutAge3Plus] [file-type "-"] file-type ","
  
  ;; Write breeders data
  ifelse initial-Juliandate > juliandate [file-type previousyear file-type ","] [file-type year file-type ","]  ;; To differentiate fall-spawners from spring/summer-spawners
  file-type Nspawners file-type ","  
  ifelse not empty? FemaleSpawnersLength [file-type mean FemaleSpawnersLength] [file-type "-"] file-type ","
  ifelse not empty? MaleSpawnersLength [file-type mean MaleSpawnersLength] [file-type "-"] file-type ","
  ifelse not empty? FemaleSpawnersAge [file-type mean FemaleSpawnersAge] [file-type "-"] file-type ","
  ifelse not empty? MaleSpawnersAge [file-type mean MaleSpawnersAge] [file-type "-"] file-type ","
  ifelse not empty? SpawningDate [file-type mean SpawningDate] [file-type "-"] file-type ","
  ifelse not empty? FemaleBreedGenMinSpawnLength [file-type mean FemaleBreedGenMinSpawnLength] [file-type "-"] file-type ","
  ifelse not empty? MaleBreedGenMinSpawnLength [file-type mean MaleBreedGenMinSpawnLength] [file-type "-"] file-type ","
  ifelse not empty? GenBreedEmergenceLength [file-type mean GenBreedEmergenceLength] [file-type "-"] file-type ","
  ifelse not empty? GenBreedNeutralTrait [file-type mean GenBreedNeutralTrait] [file-type "-"] file-type ","  
  ifelse not empty? EmergenceDate [file-type mean EmergenceDate] [file-type "-"] file-type ","
  file-type ReachIniNumberOfEggs file-type "," 
  file-print ReachNumberFryHatched

  file-close       
end
                            

to reset-counters
  let juliandate time:ts-get environmental-time-series tick-date  "juliandate"
  if juliandate >= OutputDate and juliandate < OutputDate + timestep-scale 
   [
    set MaleSpawnersLength [] set FemaleSpawnersLength [] set SpawningDate [] set MaleSpawnersAge [] 
    set FemaleSpawnersAge [] set EmergenceDate [] set AgeatDeath []
    set MaleBreedGenMinSpawnLength [] set FemaleBreedGenMinSpawnLength [] set GenBreedEmergenceLength [] set GenBreedNeutralTrait []
    set ReachIniNumberOfEggs 0
    set ReachNumberFryHatched 0
   ]
  
  if juliandate = endAnglingSeason 
   [ 
    set fishDeadAnglingSeason 0 set fishDeadHookingSeason 0 set fishDeadAnglingSeasonAge0 0 set fishDeadHookingSeasonAge0 0          
    set fishDeadAnglingSeasonAge1 0 set fishDeadHookingSeasonAge1 0 set fishDeadAnglingSeasonAge2 0 set fishDeadHookingSeasonAge2 0
    set fishDeadAnglingSeasonAge3Plus 0 set fishDeadHookingSeasonAge3Plus 0
   ]
end


;; Calculate the sum of standardized square errors between observed and simulated values of tested patterns for a specific age-class
to record-calibrationData
  let printoutputs n-values (length TScountAge0) [?]
  foreach printoutputs 
   [
    let year (item ? printoutputs)
    if ticks >= (365 - initial-Juliandate) + OutputDate + (365 * year) and ticks < (365 - initial-Juliandate) + OutputDate + (365 * (year + 1))
     [
      set SSSEcountAge0 lput (SSSE-for "abundance" TScountAge0 0 year) SSSEcountAge0
      set SSSEcountAge1 lput (SSSE-for "abundance" TScountAge1 1 year) SSSEcountAge1  
      set SSSEcountAge2 lput (SSSE-for "abundance" TScountAge2 2 year) SSSEcountAge2  
      set SSSEcountAge3Plus lput (SSSE-for "abundance" TScountAge3Plus 3 year) SSSEcountAge3Plus                                  
      set SSSEbiomassAge0 lput (SSSE-for "biomass" TSbiomassAge0 0 year) SSSEbiomassAge0 
      set SSSEbiomassAge1 lput (SSSE-for "biomass" TSbiomassAge1 1 year) SSSEbiomassAge1  
      set SSSEbiomassAge2 lput (SSSE-for "biomass" TSbiomassAge2 2 year) SSSEbiomassAge2 
      set SSSEbiomassAge3Plus lput (SSSE-for "biomass" TSbiomassAge3Plus 3 year) SSSEbiomassAge3Plus                          
      set SSSElengthAge0 lput (SSSE-for "length" TSlengthAge0 0 year) SSSElengthAge0
      set SSSElengthAge1 lput (SSSE-for "length" TSlengthAge1 1 year) SSSElengthAge1 
      set SSSElengthAge2 lput (SSSE-for "length" TSlengthAge2 2 year) SSSElengthAge2  
      set SSSElengthAge3Plus lput (SSSE-for "length" TSlengthAge3Plus 3 year) SSSElengthAge3Plus  
     ]
   ]        
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REPORTERS TO CREATE CELLS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report new-cell [ #ltrb ]  ;; by observer -- make new cell, assign patches and return reference. Code provided by Jim Lyons at the NetLogo Users Yahoo Group.
  let ref nobody
  create-cells 1
   [ 
     set my-patches patches with [pxcor >= (item 0 #ltrb) and pxcor <= (item 2 #ltrb) and pycor >= (item 3 #ltrb) and pycor <= (item 1 #ltrb)]
     set cellArea sum [patchArea] of my-patches  
     
     setxy ((item 0 #ltrb + item 2 #ltrb)/ 2) ((item 3 #ltrb + item 1 #ltrb)/ 2)  ;; move to center of cell
     set ref self
     set size 0  
     set shape "circle" set color black     
   ]
  report ref
end

  
to-report cell-coord   ;; Report an array storing the four coordinates delimiting every cell, imported from a text file
 let next-coord []
 file-open "cellscoord.txt"
 while [not file-at-end?]
  [
    let next-transect file-read
    let next-cell file-read
    let next-left file-read
    let next-top file-read
    let next-right file-read
    let next-bottom file-read
    set next-coord sentence next-coord (list(list next-left next-top next-right next-bottom))
  ]
  file-close
  let array-coord array:from-list next-coord 
  report array-coord
end


;; Report the minimum and maximum x coordinates of the patches conforming the cell (to calculate the adjacent cells of each cell)
to-report x-min-for [a-cell]
   let x-min min [pxcor] of [my-patches] of a-cell
   report x-min
end

to-report x-max-for [a-cell]
   let x-max max [pxcor] of [my-patches] of a-cell
   report x-max
end 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REPORTERS FOR FEEDING & GROWTH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report respTotal-for [a-trout feeding-strategy]    ;; A cell reporter. Reports the total daily respiration costs for a trout in the cell (j/d) as a function of feeding strategy. 
                                                      ;; Follows the Wisconsin Model equation 1 for respiration (Hanson et al. 1997), as modified by Van Winkle et al. (1996).
  let swimspeed cellVelocity
  if cellAreaShelter > 0 and feeding-strategy = "drift-feeding" [set swimspeed (cellVelocity * habShelterSpeedFrac)]  ;; Swimming speed is reduced by velocity shelters 
                                                                                                                      ;; only if the trout is in a drift-feeding strategy
  let trout-weight [fishWeight] of a-trout
  let feedTime dayLength + 2
  let respStandard (fishRespParamA * trout-weight ^ fishRespParamB) * exp (fishRespParamC * temp)
  let respActivity (feedTime / 24) * (exp (fishRespParamD * swimspeed) - 1) * respStandard
  let respTotal (respStandard + respActivity)
  report respTotal 
end


to-report driftintake-for [a-trout]         ;; A cell reporter. Reports the hourly drift intake rate for a trout in the cell (g/h). 
                                            ;; Follows the inSTREAM drift-feeding bioenergetics model (Railsback et al. 2009).
  let trout-length [fishLength] of a-trout
  let detectDistance fishDetectDistParamA + (fishDetectDistParamB * trout-length)
  let captureArea (2 * detectDistance) * (min (list detectDistance cellDepth))
  let maxSwimSpeed [fishMaxSwimSpeed] of a-trout
  let velocity-ratio (cellVelocity / maxSwimSpeed)
  let captureSuccess 0  
  let Z (ln(1 / 9) + ((ln(81) / (fishCaptureParam1 - fishCaptureParam9)) * (fishCaptureParam1 - velocity-ratio)))
  ifelse Z > 20 
   [set captureSuccess 1]
   [if Z >= -20 [let exp-Z exp(Z) set captureSuccess (exp-Z / (1 + exp-Z))]]
  let driftIntake  (captureSuccess * habDriftConc * cellVelocity * captureArea * 3600)
  report driftIntake
end


to-report dailyDriftNetEnergy-for [a-trout]      ;; A cell reporter. Reports the daily net energy intake for drift feeding (j/d). 
  let trout-cMax [cMax] of a-trout
  let trout-driftIntake driftintake-for a-trout
  let feedTime dayLength + 2
  let dailyPotentialDriftFood (trout-driftIntake * feedTime)                                     ;; Daily drift intake that would be obtained in the absence of more dominant fish in the cell
  let dailyAvailableDriftFood (driftHourlyCellTotal * feedTime)                                  ;; Drift intake rate available after more dominant fish in the cell have consumed their intake
  let dailyDriftFoodIntake min (list dailyPotentialDriftFood dailyAvailableDriftFood trout-cMax) ;; Actual drift intake rate
  let dailyDriftEnergyIntake (dailyDriftFoodIntake * habPreyEnergyDensity)
  
  let respTotal respTotal-for a-trout "drift-feeding"
  let dailyDriftNetEnergy (dailyDriftEnergyIntake - respTotal)
  report dailyDriftNetEnergy
end


to-report searchIntake-for [a-trout]  ;; A cell reporter. Reports the hourly search intake rate for a trout in the cell (g/h). Follows inSTREAM's approach (Railsback et al. 2009). 
  let maxSwimSpeed [fishMaxSwimSpeed] of a-trout
  let searchIntake (habSearchProd * fishSearchArea * max (list ((maxSwimSpeed - cellVelocity) / maxSwimSpeed) 0)) 
  report searchIntake
end


to-report dailySearchNetEnergy-for [a-trout]     ;; A cell reporter. Reports the daily net energy intake for search feeding (j/d).
  let trout-cMax [cMax] of a-trout
  let trout-searchIntake searchIntake-for a-trout
  let feedTime dayLength + 2  
  let dailyPotentialSearchFood (trout-searchIntake * feedTime)                                      ;; Daily search intake that would be obtained in the absence of more dominant fish in the cell
  let dailyAvailableSearchFood (SearchHourlyCellTotal * feedTime)                                   ;; Search intake rate available after more dominant fish in the cell have consumed their intake
  let dailySearchFoodIntake min (list dailyPotentialSearchFood dailyAvailableSearchFood trout-cMax) ;; Actual search intake rate
  let dailySearchEnergyIntake (dailySearchFoodIntake * habPreyEnergyDensity)
  
  let respTotal respTotal-for a-trout "search-feeding"
  let dailySearchNetEnergy (dailySearchEnergyIntake - respTotal)
  report dailySearchNetEnergy  
end


to-report bestNetEnergy-for [a-trout]            ;; A cell reporter. Reports the daily net energy intake for the best feeding strategy (j/d).
  let bestNetEnergy 0
  let trout-dailyDriftNetEnergy dailyDriftNetEnergy-for a-trout
  let trout-dailySearchNetEnergy dailySearchNetEnergy-for a-trout
  ifelse trout-dailyDriftNetEnergy > trout-dailySearchNetEnergy 
   [set bestNetEnergy trout-dailyDriftNetEnergy
    ifelse cellAreaShelter > 0 
     [ask a-trout [set is-sheltered? true]] 
     [ask a-trout [set is-sheltered? false]]] 
   [set bestNetEnergy trout-dailySearchNetEnergy
    ask a-trout [set is-sheltered? false]]
  report bestNetEnergy
end




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REPORTERS FOR SURVIVAL FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

;; Logistic survival probability functions following inSTREAM's approach (Railsback et al. 2009). Code provided by S. Railsback. 


to-report SurvProbHighTemp-for-all              ;; Survival probability function for high temperature
  let ProbHighTemp 1
  if MortHighTemp 
   [
    let Z (ln(1 / 9) + ((ln(81) / (mortFishHiTT1 - mortFishHiTT9)) * (mortFishHiTT1 - temp)))    
    ifelse Z > 20 
     [set ProbHighTemp 1]
     [
      ifelse Z < -20 
       [set ProbHighTemp 0]
       [let exp-Z exp(Z) set ProbHighTemp (exp-Z / (1 + exp-Z))]
     ]
   ]
  report ProbHighTemp
end
  

to-report SurvProbHighVel-for [a-trout]          ;; Survival probability function for high velocity
  let ProbHighVel 1
  if MortHighVel
   [
    let maxSwimSpeed [fishMaxSwimSpeed] of a-trout
    let trout-is-sheltered? [is-sheltered?] of a-trout
    let swimspeed cellVelocity
    if trout-is-sheltered? [set swimspeed (cellVelocity * habShelterSpeedFrac)]  ;; Swimming speed is reduced by velocity shelters 
                                                                                      
    let velocity-ratio (swimspeed / maxSwimSpeed)
    let Z (ln(1 / 9) + ((ln(81) / (mortFishVelocityV1 - mortFishVelocityV9)) * (mortFishVelocityV1 - velocity-ratio)))
    ifelse Z > 20 
     [set ProbHighVel 1]
     [
      ifelse Z < -20 
       [set ProbHighVel 0]
       [let exp-Z exp(Z) set ProbHighVel (exp-Z / (1 + exp-Z))]
     ]
   ]
  report ProbHighVel
end


to-report SurvProbStrand-for [a-trout]          ;; Survival probability function for stranding
  let ProbStrand 1
  if MortStrand
   [
    let trout-length [fishLength] of a-trout
    let depth-ratio (cellDepth / trout-length)
    let Z (ln(1 / 9) + ((ln(81) / (mortFishStrandD1 - mortFishStrandD9)) * (mortFishStrandD1 - depth-ratio)))
    ifelse Z > 20 
     [set ProbStrand 1]
     [
      ifelse Z < -20 
       [set ProbStrand 0]
       [let exp-Z exp(Z) set ProbStrand (exp-Z / (1 + exp-Z))]
     ]
   ]
  report ProbStrand
end


to-report SurvProbPoorCond-for [a-trout]        ;; Survival probability function for poor condition
  let ProbPoorCond 1
  if MortPoorCond 
   [
    let trout-fishCondition [fishCondition] of a-trout
    let Z (ln(1 / 9) + ((ln(81) / (mortFishConditionK1 - mortFishConditionK9)) * (mortFishConditionK1 - trout-fishCondition)))
    ifelse Z > 20 
     [set ProbPoorCond 1]
     [
      ifelse Z < -20 
       [set ProbPoorCond 0]
       [let exp-Z exp(Z) set ProbPoorCond (exp-Z / (1 + exp-Z))]
     ]
   ]
  report ProbPoorCond
end


to-report terrPredDepthF                        ;; Depth Survival probability function for terrestrial predation
  let ProbterrPredDepth 0
  if MortTerrPredDepth
   [ 
    let Z (ln(1 / 9) + ((ln(81) / (mortFishTerrPredD1 - mortFishTerrPredD9)) * (mortFishTerrPredD1 - cellDepth)))
    ifelse Z > 20 
     [set ProbterrPredDepth 1]
     [
      ifelse Z < -20 
       [set ProbterrPredDepth 0]
       [let exp-Z exp(Z) set ProbterrPredDepth (exp-Z / (1 + exp-Z))]
     ]
   ]
  report ProbterrPredDepth
end


to-report terrPredLengthF-for [a-trout]         ;; Length Survival probability function for terrestrial predation
  let ProbterrPredLength 0
  if MortTerrPredLength
   [ 
    let trout-length [fishLength] of a-trout
    let Z (ln(1 / 9) + ((ln(81) / (mortFishTerrPredL1 - mortFishTerrPredL9)) * (mortFishTerrPredL1 - trout-length)))
    ifelse Z > 20 
     [set ProbterrPredLength 1]
     [
      ifelse Z < -20 
       [set ProbterrPredLength 0]
       [let exp-Z exp(Z) set ProbterrPredLength (exp-Z / (1 + exp-Z))]
     ]
   ]
  report ProbterrPredLength
end


to-report terrPredFeedTimeF-for-all              ;; Feeding time Survival probability function for terrestrial predation
  let ProbterrPredFeedTime 0
  if MortTerrPredFeedTime 
   [
    let feedTime (dayLength + 2)  
    let Z (ln(1 / 9) + ((ln(81) / (mortFishTerrPredF1 - mortFishTerrPredF9)) * (mortFishTerrPredF1 - feedTime)))
    ifelse Z > 20 
     [set ProbterrPredFeedTime 1]
     [
      ifelse Z < -20 
       [set ProbterrPredFeedTime 0]
       [let exp-Z exp(Z) set ProbterrPredFeedTime (exp-Z / (1 + exp-Z))]
     ]
   ]
  report ProbterrPredFeedTime
end


to-report terrPredVelF                           ;; Velocity Survival probability function for terrestrial predation
  let ProbterrPredVel 0
  if MortTerrPredVel 
   [
    let Z (ln(1 / 9) + ((ln(81) / (mortFishTerrPredV1 - mortFishTerrPredV9)) * (mortFishTerrPredV1 - cellVelocity)))
    ifelse Z > 20 
     [set ProbterrPredVel 1]
     [
      ifelse Z < -20 
       [set ProbterrPredVel 0]
       [let exp-Z exp(Z) set ProbterrPredVel (exp-Z / (1 + exp-Z))]
     ]
   ]
  report ProbterrPredVel
end


to-report terrPredCoverF                         ;; Distance to hiding cover Survival probability function for terrestrial predation
  let ProbterrPredCover 0
  if MortTerrPredCover 
   [
    let Z (ln(1 / 9) + ((ln(81) / (mortFishTerrPredH1 - mortFishTerrPredH9)) * (mortFishTerrPredH1 - cellDistanceToHide)))
    ifelse Z > 20 
     [set ProbterrPredCover 1]
     [
      ifelse Z < -20 
       [set ProbterrPredCover 0]
       [let exp-Z exp(Z) set ProbterrPredCover (exp-Z / (1 + exp-Z))]
     ]
   ]
  report ProbterrPredCover
end


to-report SurvProbTerrPred-for [a-trout]         ;; Survival probability function for terrestrial predation
  let trout-terrPredLengthF terrPredLengthF-for a-trout
  let ProbTerrPred (mortFishTerrPredMin + ((1 - mortFishTerrPredMin) * (max (list terrPredDepthF trout-terrPredLengthF terrPredFeedTimeF terrPredVelF terrPredCoverF))))
  report ProbTerrPred
end


to-report aqPredDensF-for-all                    ;; Piscivorous fish density Survival probability function for aquatic predation
  let ProbaqPredDens 0
  if MortAqPredDens 
   [ 
    let Z (ln(1 / 9) + ((ln(81) / (mortFishAqPredP1 - mortFishAqPredP9)) * (mortFishAqPredP1 - PiscivFishDens)))
    ifelse Z > 20 
     [set ProbaqPredDens 1]
     [
      ifelse Z < -20 
       [set ProbaqPredDens 0]
       [let exp-Z exp(Z) set ProbaqPredDens (exp-Z / (1 + exp-Z))]
     ]
   ]
  report ProbaqPredDens
end


to-report aqPredDepthF                           ;; Depth Survival probability function for aquatic predation
  let ProbaqPredDepth 0
  if MortAqPredDepth 
   [
    let Z (ln(1 / 9) + ((ln(81) / (mortFishAqPredD1 - mortFishAqPredD9)) * (mortFishAqPredD1 - cellDepth)))
    ifelse Z > 20 
     [set ProbaqPredDepth 1]
     [
      ifelse Z < -20 
       [set ProbaqPredDepth 0]
       [let exp-Z exp(Z) set ProbaqPredDepth (exp-Z / (1 + exp-Z))]
     ]
   ]
  report ProbaqPredDepth
end


to-report aqPredLengthF-for [a-trout]            ;; Length Survival probability function for aquatic predation
  let ProbaqPredLength 0
  if MortAqPredLength 
   [
    let trout-length [fishLength] of a-trout
    let Z (ln(1 / 9) + ((ln(81) / (mortFishAqPredL1 - mortFishAqPredL9)) * (mortFishAqPredL1 - trout-length)))
    ifelse Z > 20 
     [set ProbaqPredLength 1]
     [
      ifelse Z < -20 
       [set ProbaqPredLength 0]
       [let exp-Z exp(Z) set ProbaqPredLength (exp-Z / (1 + exp-Z))]
     ]
   ]
  report ProbaqPredLength
end


to-report aqPredFeedTimeF-for-all                ;; Feeding time Survival probability function for aquatic predation
  let ProbaqPredFeedTime 0
  if MortAqPredFeedTime 
   [
    let feedTime (dayLength + 2)  
    let Z (ln(1 / 9) + ((ln(81) / (mortFishAqPredF1 - mortFishAqPredF9)) * (mortFishAqPredF1 - feedTime)))
    ifelse Z > 20 
     [set ProbaqPredFeedTime 1]
     [
      ifelse Z < -20 
       [set ProbaqPredFeedTime 0]
       [let exp-Z exp(Z) set ProbaqPredFeedTime (exp-Z / (1 + exp-Z))]
     ]
   ]
  report ProbaqPredFeedTime
end


to-report aqPredTempF-for-all                    ;; Temperature Survival probability function for aquatic predation
  let ProbaqPredTemp 0
  if MortAqPredTemp 
   [
    let Z (ln(1 / 9) + ((ln(81) / (mortFishAqPredT1 - mortFishAqPredT9)) * (mortFishAqPredT1 - temp)))
    ifelse Z > 20 
     [set ProbaqPredTemp 1]
     [
      ifelse Z < -20 
       [set ProbaqPredTemp 0]
       [let exp-Z exp(Z) set ProbaqPredTemp (exp-Z / (1 + exp-Z))]
     ]
   ]
  report ProbaqPredTemp
end


to-report SurvProbAqPred-for [a-trout]           ;; Survival probability function for aquatic predation
  let trout-aqPredLengthF aqPredLengthF-for a-trout
  let ProbAqPred (mortFishAqPredMin + ((1 - mortFishAqPredMin) * (max (list aqPredDensF aqPredDepthF trout-aqPredLengthF aqPredFeedTimeF aqPredTempF))))
  report ProbAqPred
end


to-report AngleCaptureLengthF                    ;; Fish length function for angler mortality probability of capture
  let ProbAngleCaptureLength 0
  let Z (ln(1 / 9) + ((ln(81) / (mortFishAngleL1 - mortFishAngleL9)) * (mortFishAngleL1 - fishLength)))
    ifelse Z > 20 
     [set ProbAngleCaptureLength 1]
     [
      ifelse Z < -20 
       [set ProbAngleCaptureLength 0]
       [let exp-Z exp(Z) set ProbAngleCaptureLength (exp-Z / (1 + exp-Z))]
     ]
  report ProbAngleCaptureLength
end




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REPORTERS FOR HABITAT SELECTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report nonstarvSurvival-for [a-trout]         ;; Probability of survival for all mortality sources except poor condition and angling, over a specified time horizon given by the parameter fishFitnessHorizon
  let trout-SurvProbHighTemp SurvProbHighTemp
  let trout-SurvProbHighVel SurvProbHighVel-for a-trout
  let trout-SurvProbStrand SurvProbStrand-for a-trout
  let trout-SurvProbTerrPred SurvProbTerrPred-for a-trout
  let trout-SurvProbAqPred SurvProbAqPred-for a-trout
  let nonstarvSurvival (trout-SurvProbHighTemp * trout-SurvProbHighVel * trout-SurvProbStrand * trout-SurvProbTerrPred * trout-SurvProbAqPred) ^ fishFitnessHorizon
  report nonstarvSurvival
end


to-report projectedfishWeight-for [a-trout]      ;; Project the fish weight that would result if the current day’s growth persisted over the fitness time horizon
  let trout-weight [fishWeight] of a-trout
  let trout-bestNetEnergy bestNetEnergy-for a-trout                                                                 ;; Trout select best feeding strategy
  
  let dailyGrowth (trout-bestNetEnergy / fishEnergyDensity)                                                         ;; Trout calculate daily growth
  let projectedfishWeight (trout-weight + dailyGrowth * fishFitnessHorizon)                                         ;; Trout project weight over the fitness time horizon
  if projectedfishWeight < 0 [set projectedfishWeight 0]                                                            ;; Weight cannot be negative  
  report projectedfishWeight  
end  


to-report projectedfishLength-for [a-trout]      ;; Project the fish length that would result if the current day’s growth persisted over the fitness time horizon 
  let trout-length [fishLength] of a-trout
  let projectedfishWeight projectedfishWeight-for a-trout                                                          
  
  let projectedfishLength (projectedfishWeight / fishWeightParamA) ^ (1 / fishWeightParamB)                         ;; Project fish length based on projected weight
  if trout-length > projectedfishLength [set projectedfishLength trout-length]                                      ;; Projected length cannot be smaller than current length
  report projectedfishLength  
end


to-report projectedfishCondition-for [a-trout]   ;; Project the fish condition that would result if the current day’s growth persisted over the fitness time horizon 
  let trout-projectedfishWeight projectedfishWeight-for a-trout 
  let trout-projectedfishLength projectedfishLength-for a-trout 
  
  let projectedfishCondition (trout-projectedfishWeight / (fishWeightParamA * trout-projectedfishLength ^ fishWeightParamB))   ;; Project body condition over the fitness time horizon 
                                                                                                                               ;; based on projected fish weight and length. 
  report projectedfishCondition                                                                                                                             
end


to-report starvSurvival-for [a-trout]            ;; Probability of surviving starvation over a specified time horizon
  let trout-fishCondition [fishCondition] of a-trout
  let trout-projectedfishCondition projectedfishCondition-for a-trout                                                        
  let trout-SurvProbPoorCond SurvProbPoorCond-for a-trout                          ;; Probability of surviving poor condition for trout's current condition
  
  
  let LogistB (ln(1 / 81) / (mortFishConditionK1 - mortFishConditionK9))
  let LogistA (ln(1 / 9) - (LogistB * mortFishConditionK1))
  let currentProb 1 + exp(LogistA + (LogistB * trout-fishCondition))
  let projectedProb 1 + exp(LogistA + (LogistB * trout-projectedfishCondition))
  let starvSurvival 1
  ifelse abs(trout-projectedfishCondition - trout-fishCondition) < 0.001 ;; If current and projected fish condition are very similar
    [set starvSurvival trout-SurvProbPoorCond ^ fishFitnessHorizon]      ;; then trout just calculate the survival probability over the fitness time horizon based on current condition.
                                                                         ;; Otherwise, the probability of surviving starvation is estimated as the first moment of the logistic function of poor condition survival vs. K
    [set starvSurvival (((1 / LogistB) * ln(projectedProb / currentProb)) / (trout-projectedfishCondition - trout-fishCondition)) ^ fishFitnessHorizon] 
  report starvSurvival                                                                       
end


to-report fracMature-for [a-trout]       ;; A trout reporter representing how close to the size of sexual maturity a fish would be at the end of the fitness time horizon
  let fracMature 1
  let trout-length [fishLength] of a-trout
  let trout-fishSpawnMinLength [fishSpawnMinLength] of a-trout
  if trout-length < trout-fishSpawnMinLength
   [
    let projectedfishLength projectedfishLength-for a-trout
    set fracMature (projectedfishLength / trout-fishSpawnMinLength)
    if projectedfishLength > trout-fishSpawnMinLength [set fracMature 1]     ;; FracMature is limited to a maximum of 1.0
   ]
  report fracMature
end


to-report expectedMaturity-for [a-trout]  ;; Reporter to calculate the "expected maturity" fitness measure (Railsback et al. 1999)
  let trout-starvSurvival starvSurvival-for a-trout
  let trout-fracMature fracMature-for a-trout
  let trout-nonstarvSurvival nonstarvSurvival-for a-trout
  let expectedMaturity (trout-nonstarvSurvival * trout-starvSurvival * trout-fracMature)
  report expectedMaturity
end




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REPORTERS FOR SPAWNING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report spawnDepthSuit  ;; A cell reporter defining the spawning suitability factor for depth 
  let DepthSuit 1
  ifelse cellDepth <= fishSpawnDSuitD2 [set DepthSuit (fishSpawnDSuitS1 + ((cellDepth - fishSpawnDSuitD1) * (fishSpawnDSuitS2 - fishSpawnDSuitS1) / (fishSpawnDSuitD2 - fishSpawnDSuitD1)))]
  [ifelse cellDepth > fishSpawnDSuitD2 and cellDepth <= fishSpawnDSuitD3 [set DepthSuit (fishSpawnDSuitS2 + ((cellDepth - fishSpawnDSuitD2) * (fishSpawnDSuitS3 - fishSpawnDSuitS2) / (fishSpawnDSuitD3 - fishSpawnDSuitD2)))]
  [ifelse cellDepth > fishSpawnDSuitD3 and cellDepth <= fishSpawnDSuitD4 [set DepthSuit (fishSpawnDSuitS3 + ((cellDepth - fishSpawnDSuitD3) * (fishSpawnDSuitS4 - fishSpawnDSuitS3) / (fishSpawnDSuitD4 - fishSpawnDSuitD3)))]
  [ifelse cellDepth > fishSpawnDSuitD4 and cellDepth <= fishSpawnDSuitD5 [set DepthSuit (fishSpawnDSuitS4 + ((cellDepth - fishSpawnDSuitD4) * (fishSpawnDSuitS5 - fishSpawnDSuitS4) / (fishSpawnDSuitD5 - fishSpawnDSuitD4)))]  
  [set DepthSuit (fishSpawnDSuitS4 + ((cellDepth - fishSpawnDSuitD4) * (fishSpawnDSuitS5 - fishSpawnDSuitS4) / (fishSpawnDSuitD5 - fishSpawnDSuitD4)))]]]]
  if DepthSuit < 0 [set DepthSuit 0]
  report DepthSuit
end


to-report spawnVelocitySuit  ;; A cell reporter defining the spawning suitability factor for velocity 
  let VelocitySuit 1
  ifelse cellVelocity <= fishSpawnVSuitV2 [set VelocitySuit (fishSpawnVSuitS1 + ((cellVelocity - fishSpawnVSuitV1) * (fishSpawnVSuitS2 - fishSpawnVSuitS1) / (fishSpawnVSuitV2 - fishSpawnVSuitV1)))]
  [ifelse cellVelocity > fishSpawnVSuitV2 and cellVelocity <= fishSpawnVSuitV3 [set VelocitySuit (fishSpawnVSuitS2 + ((cellVelocity - fishSpawnVSuitV2) * (fishSpawnVSuitS3 - fishSpawnVSuitS2) / (fishSpawnVSuitV3 - fishSpawnVSuitV2)))]
  [ifelse cellVelocity > fishSpawnVSuitV3 and cellVelocity <= fishSpawnVSuitV4 [set VelocitySuit (fishSpawnVSuitS3 + ((cellVelocity - fishSpawnVSuitV3) * (fishSpawnVSuitS4 - fishSpawnVSuitS3) / (fishSpawnVSuitV4 - fishSpawnVSuitV3)))]
  [ifelse cellVelocity > fishSpawnVSuitV4 and cellVelocity <= fishSpawnVSuitV5 [set VelocitySuit (fishSpawnVSuitS4 + ((cellVelocity - fishSpawnVSuitV4) * (fishSpawnVSuitS5 - fishSpawnVSuitS4) / (fishSpawnVSuitV5 - fishSpawnVSuitV4)))]
  [ifelse cellVelocity > fishSpawnVSuitV5 and cellVelocity <= fishSpawnVSuitV6 [set VelocitySuit (fishSpawnVSuitS5 + ((cellVelocity - fishSpawnVSuitV5) * (fishSpawnVSuitS6 - fishSpawnVSuitS5) / (fishSpawnVSuitV6 - fishSpawnVSuitV5)))]
  [set VelocitySuit (fishSpawnVSuitS5 + ((cellVelocity - fishSpawnVSuitV5) * (fishSpawnVSuitS6 - fishSpawnVSuitS5) / (fishSpawnVSuitV6 - fishSpawnVSuitV5)))]]]]]
  if VelocitySuit < 0 [set VelocitySuit 0]
  report VelocitySuit
end


to-report cellSpawnQuality   ;; A cell reporter defining the quality of a cell for spawning, following inSTREAM's approach (Railsback et al. 2009)
  let spawnQuality 1
  ifelse cellFracGravel > 0
   [set spawnQuality (spawnDepthSuit * spawnVelocitySuit * (cellFracGravel * cellArea))]  ;; Spawning quality depends on depth and velocity conditions and on the quantity of gravel available in the cell
   [set spawnQuality (spawnDepthSuit * spawnVelocitySuit)]                                ;; If there is no gravel in the cell, the suitability index depends only on hydraulic conditions
  report spawnQuality   
end
  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REPORTERS FOR REDD MORTALITY FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

;; Following the approach of Railsback et al. (2009), redd survival is modelled using redd “survival functions”, 
;; which determine, for each redd on each day, the probability of each egg surviving one particular kind of mortality. 
;; Then, a random draw is made on a binomial distribution to determine how many eggs survive each redd mortality source.

to-report eggsLostToDewatering   ;; Number of eggs dying by dewatering
  let redd-eggsLostToDewatering 0  
  ifelse numberOfEggs > 100 and (1 - mortReddDewaterSurv) < 0.9                       ;; If so, then the number of dead eggs  
   [set redd-eggsLostToDewatering round ((1 - mortReddDewaterSurv) * numberOfEggs)]   ;; approximates the mortality probability of an egg multiplied by the number of live eggs.
   [repeat numberOfEggs                                                                                               ;; Otherwise, it is modelled through a binomial distribution wherein                                                                                              
     [if (random-bernoulli (1 - mortReddDewaterSurv)) [set redd-eggsLostToDewatering redd-eggsLostToDewatering + 1]]  ;; the event is death of one egg, the number of trials is the number of eggs in the redd,
   ]                                                                                                                  ;; and the probability of occurrence is one minus the survival function value.
  report redd-eggsLostToDewatering
end


to-report reddScourSurvival     ;; Reporter to calculate the probability of a redd of not being destroyed by scouring
  let shearStress (habShearParamA * flow ^ habShearParamB)     ;; ShearStress is the peak Shields stress (dimensionless indicator of scour potential measured at a reach scale) occurring during the high-flow event  
  let scourParam (3.33 * exp (-1.52 * (shearStress / 0.045)))  ;; From Haschenburger (1999)
  let redd-scourSurvival 1                                     ;; Probability of surviving scouring is equal to the proportion of the stream scouring or filling to a depth less than the egg burial depth
  if scourParam * mortReddScourDepth <= 100 [set redd-scourSurvival (1 - exp (- scourParam * mortReddScourDepth))] ;; From Haschenburger (1999) 
  report redd-scourSurvival                                                                                                     
end


to-report reddSurvProbLowTemp   ;; Survival probability function for low temperature (daily fraction of eggs surviving low temperatures)
  let ProbLowTemp 1
  let Z (ln(1 / 9) + ((ln(81) / (mortReddLoTT1 - mortReddLoTT9)) * (mortReddLoTT1 - temp)))
    ifelse Z > 20 
     [set ProbLowTemp 1]
     [
      ifelse Z < -20 
       [set ProbLowTemp 0]
       [let exp-Z exp(Z) set ProbLowTemp (exp-Z / (1 + exp-Z))]
     ]
  report ProbLowTemp
end


to-report eggsLostToLowTemp   ;; Number of eggs dying due to low water temperatures
  let redd-eggsLostToLowTemp 0  
  ifelse numberOfEggs > 100 and (1 - reddSurvProbLowTemp) < 0.9                    ;; If so, then the number of dead eggs  
   [set redd-eggsLostToLowTemp round ((1 - reddSurvProbLowTemp) * numberOfEggs)]   ;; approximates the mortality probability of an egg multiplied by the number of live eggs.
   [repeat numberOfEggs                                                                                         ;; Otherwise, it is modelled through a binomial distribution wherein                                                                                              
     [if (random-bernoulli (1 - reddSurvProbLowTemp)) [set redd-eggsLostToLowTemp redd-eggsLostToLowTemp + 1]]  ;; the event is death of one egg, the number of trials is the number of eggs in the redd,
   ]                                                                                                            ;; and the probability of occurrence is one minus the survival function value.
  report redd-eggsLostToLowTemp
end


to-report reddSurvProbHighTemp   ;; Survival probability function for high temperature (daily fraction of eggs surviving high temperatures)
  let ProbHighTemp 1
  let Z (ln(1 / 9) + ((ln(81) / (mortReddHiTT1 - mortReddHiTT9)) * (mortReddHiTT1 - temp)))
    ifelse Z > 20 
     [set ProbHighTemp 1]
     [
      ifelse Z < -20 
       [set ProbHighTemp 0]
       [let exp-Z exp(Z) set ProbHighTemp (exp-Z / (1 + exp-Z))]
     ]
  report ProbHighTemp
end


to-report eggsLostToHighTemp   ;; Number of eggs dying due to high water temperatures
  let redd-eggsLostToHighTemp 0  
  ifelse numberOfEggs > 100 and (1 - reddSurvProbHighTemp) < 0.9                     ;; If so, then the number of dead eggs  
   [set redd-eggsLostToHighTemp round ((1 - reddSurvProbHighTemp) * numberOfEggs)]   ;; approximates the mortality probability of an egg multiplied by the number of live eggs.
   [repeat numberOfEggs                                                                                            ;; Otherwise, it is modelled through a binomial distribution wherein                                                                                              
     [if (random-bernoulli (1 - reddSurvProbHighTemp)) [set redd-eggsLostToHighTemp redd-eggsLostToHighTemp + 1]]  ;; the event is death of one egg, the number of trials is the number of eggs in the redd,
   ]                                                                                                               ;; and the probability of occurrence is one minus the survival function value.
  report redd-eggsLostToHighTemp
end


to-report eggsLostToSuperimp   ;; Number of eggs dying due to superimposition
  let redd-eggsLostToSuperimp 0                                                     
  let reddSuperImpRisk (reddSize / ([cellArea] of my-cell * [cellFracGravel] of my-cell)) ;; Probability of each new redd causing superimposition is just the area of the redd divided by total gravel area
  let reddSurvProbSuperimp random-float 1
  if reddSurvProbSuperimp < reddSuperImpRisk    ;; Stochastic superimposition mortality
  [                                            
   let reddSurvSuperimp random-float 1                                           ;; Stochastic number of eggs surviving superimposition
   ifelse numberOfEggs > 100 and (1 - reddSurvSuperimp) < 0.9                    ;; If so, then the number of dead eggs  
    [set redd-eggsLostToSuperimp round ((1 - reddSurvSuperimp) * numberOfEggs)]  ;; approximates the mortality probability of an egg multiplied by the number of live eggs.
    [repeat numberOfEggs                                                                                        ;; Otherwise, it is modelled through a binomial distribution wherein                                                                                              
     [if (random-bernoulli (1 - reddSurvSuperimp)) [set redd-eggsLostToSuperimp redd-eggsLostToSuperimp + 1]]]  ;; the event is death of one egg, the number of trials is the number of eggs in the redd,
  ]                                                                                                             ;; and the probability of occurrence is one minus the survival function value.
  report redd-eggsLostToSuperimp  
end


to-report random-bernoulli [probability-true]   ;; Returns either a true or false value, with the probability of returning “true” equal to "probability-true". Code copied from Railsback & Grimm (2012).
  if (probability-true < 0 or probability-true > 1)  ;; First, make sure "that probability-true" has a sensible value
    [user-message (word "Warning in random-bernoulli: probability-true " probability-true)]
  report random-float 1 < probability-true 
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REPORTERS FOR GENETIC TRANSMISSION OF TRAITS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

;; Inheritance rules are based on the infinitesimal model of quantitative genetics (Lynch & Walsh 1998) as modified by Vincenzi et al. (2012): 


;; the genotypic value of the trait under selection is drawn from a normal distribution centered on the arithmetic mean of the two parental values, 
;; while the variance of this distribution is equal to half the total additive genetic variance for the trait at the population level
;; plus the variation due to mutation.
;; The genotypic value is truncated at 4 standard deviations.

to-report genotypic-value [mothervalue fatherId fathervalue popmean additivevar mutationalVar-for]
  let trout-gen-value popmean
  let mutationFactor-X mutationFactor
  if additivevar = 0 [set mutationFactor-X 0]
  if GeneticTransmission? 
   [
    let parental-mean mean list (mothervalue) (item fatherId fathervalue)
    let distribVar ((additivevar + mutationalVar-for * mutationFactor-X) / 2)
    set trout-gen-value random-normal parental-mean (sqrt distribVar) 
    if trout-gen-value < (parental-mean - 4 * (sqrt distribVar)) [set trout-gen-value (parental-mean - 4 * (sqrt distribVar))]
    if trout-gen-value > (parental-mean + 4 * (sqrt distribVar)) [set trout-gen-value (parental-mean + 4 * (sqrt distribVar))]
   ]
  report trout-gen-value
end


;; specific reporter for genotypic value of the maturity threshold, which changes with trout sex.
to-report genVal-SpawnMinLength [troutSex mothervalue fatherId fathervalue femalepopmean malepopmean additivevar mutationalVar-for]
  let trout-gen-value malepopmean 
  let mutationFactor-X mutationFactor
  if additivevar = 0 [set mutationFactor-X 0]
  if troutSex = "F" [set trout-gen-value femalepopmean]  
  if GeneticTransmission? 
   [ 
    let standard-gen-value 0
    let distribVar (additivevar + mutationalVar-for * mutationFactor-X)
    if distribVar > 0 
     [
      let standardgenMother (mothervalue - femalepopmean) / (sqrt distribVar)
      let standardgenFather (item fatherId fathervalue - malepopmean) / (sqrt distribVar)
      if mothervalue = fathervalue and fatherId = 0 [set standardgenFather standardgenMother]  ;; To account for the event when a female spawner spawned and no male spawners were available for spawning
      let parental-mean mean list (standardgenMother) (standardgenFather)
      set standard-gen-value random-normal parental-mean sqrt 0.5
     ]
    set trout-gen-value malepopmean + standard-gen-value * (sqrt distribVar)
    if trout-gen-value < (malepopmean - 4 * (sqrt distribVar)) [set trout-gen-value (malepopmean - 4 * (sqrt distribVar))]
    if trout-gen-value > (malepopmean + 4 * (sqrt distribVar)) [set trout-gen-value (malepopmean + 4 * (sqrt distribVar))]
    if troutSex = "F" [set trout-gen-value femalepopmean + standard-gen-value * (sqrt distribVar)]
    if trout-gen-value < (femalepopmean - 4 * (sqrt distribVar)) [set trout-gen-value (femalepopmean - 4 * (sqrt distribVar))]
    if trout-gen-value > (femalepopmean + 4 * (sqrt distribVar)) [set trout-gen-value (femalepopmean + 4 * (sqrt distribVar))]
   ]
  report trout-gen-value
end



;; The random environmental effect is drawn from a normal distribution with mean 0 and
;; a variance which is set based on the population phenotypic variance at the population level and the narrow-sense heritability of the trait.

to-report environVar-for [phenotypicvar heritability]        ;; Reporter to calculate the environmental variance of the trait
  let traitheritability heritability
  if GeneticTransmission? = false [set traitheritability 0]  ;; If the genetic transmission submodel is turned off, then heritability is set to 0 irrespective of its value at initialization
  let environVar (1 - traitheritability) * phenotypicvar     ;; Environmental variance = phenotypic variance - additive genetic variance
  report environVar
end

to-report environ-value-for [phenotypicvar heritability]         ;; Reporter to calculate the environmental component of the trait. It is truncated at 4 standard deviations.
  let trout-environ-value 0
  let environVar environVar-for phenotypicvar heritability
  set trout-environ-value random-normal 0 sqrt environVar 
  if trout-environ-value < (- 4 * (sqrt environVar)) [set trout-environ-value (- 4 * (sqrt environVar))]
  if trout-environ-value > 4 * (sqrt environVar) [set trout-environ-value 4 * (sqrt environVar)]
  report trout-environ-value
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REPORTERS FOR MODEL CALIBRATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to-report SSSE-for [variable pattern Ageclass year]  ;; Reporter to calculate the sum of standardized square errors between observed and simulated values of tested patterns for a specific age-class
  let simulated-pattern 0
  let observed-pattern 0
  
  if variable = "abundance"
   [
     ifelse Ageclass = 3
      [
       set simulated-pattern count trout with [status = "alive" and age-class >= Ageclass]
       set observed-pattern item year pattern
      ]
      [
       set simulated-pattern count trout with [status = "alive" and age-class = Ageclass]
       set observed-pattern item year pattern
      ]  
   ]
   
  if variable = "biomass"
   [
     ifelse Ageclass = 3
      [
       set simulated-pattern (sum [fishWeight] of trout with [status = "alive" and age-class >= Ageclass] / 1000)
       set observed-pattern item year pattern
      ]
      [
       set simulated-pattern (sum [fishWeight] of trout with [status = "alive" and age-class = Ageclass] / 1000)
       set observed-pattern item year pattern
      ]  
   ]
   
  if variable = "length"
   [
     let alivetrout trout with [status = "alive" and age-class = Ageclass]
     if Ageclass = 3 [set alivetrout trout with [status = "alive" and age-class >= Ageclass]]
     ifelse any? alivetrout [set simulated-pattern mean [fishLength] of alivetrout] [set simulated-pattern 0]
     set observed-pattern item year pattern
   ]
   
  
  if observed-pattern = 0 [set observed-pattern 1]
  let ssse ((simulated-pattern - observed-pattern) ^ 2) / observed-pattern
  report ssse
end
@#$#@#$#@
GRAPHICS-WINDOW
892
54
1147
3849
-1
-1
1.2323
1
10
1
1
1
0
0
0
1
0
198
0
3054
1
1
1
ticks
30.0

BUTTON
9
11
72
44
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
79
12
142
45
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
9
49
72
82
reset
reset
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
892
10
971
55
Date
time:show tick-date \"dd-MM-yyyy\"
17
1
11

SWITCH
6
270
167
303
MortHighTemp
MortHighTemp
0
1
-1000

SWITCH
6
303
167
336
MortHighVel
MortHighVel
0
1
-1000

SWITCH
6
336
167
369
MortStrand
MortStrand
0
1
-1000

SWITCH
6
369
167
402
MortPoorCond
MortPoorCond
0
1
-1000

SWITCH
6
401
167
434
MortTerrPredDepth
MortTerrPredDepth
0
1
-1000

SWITCH
6
434
167
467
MortTerrPredLength
MortTerrPredLength
0
1
-1000

SWITCH
6
467
167
500
MortTerrPredFeedTime
MortTerrPredFeedTime
0
1
-1000

SWITCH
6
533
167
566
MortTerrPredCover
MortTerrPredCover
0
1
-1000

SWITCH
6
599
167
632
MortAqPredDepth
MortAqPredDepth
0
1
-1000

SWITCH
6
632
167
665
MortAqPredLength
MortAqPredLength
0
1
-1000

SWITCH
6
665
167
698
MortAqPredFeedTime
MortAqPredFeedTime
0
1
-1000

SWITCH
6
698
167
731
MortAqPredTemp
MortAqPredTemp
0
1
-1000

SWITCH
462
923
610
956
MortAnglingHooking
MortAnglingHooking
0
1
-1000

TEXTBOX
22
244
148
265
Trout Mortality Functions
11
0.0
1

SWITCH
6
500
167
533
MortTerrPredVel
MortTerrPredVel
0
1
-1000

SWITCH
6
566
167
599
MortAqPredDens
MortAqPredDens
0
1
-1000

SWITCH
9
924
164
957
FixedAdditiveVar?
FixedAdditiveVar?
0
1
-1000

SWITCH
690
47
841
80
DeadFishOutput?
DeadFishOutput?
0
1
-1000

INPUTBOX
761
124
840
184
fileOutputFreq
1
1
0
Number

SWITCH
539
80
690
113
ReddOutput?
ReddOutput?
0
1
-1000

SWITCH
539
47
690
80
LiveFishOutput?
LiveFishOutput?
0
1
-1000

SWITCH
690
80
841
113
BreedersOutput?
BreedersOutput?
0
1
-1000

SWITCH
690
14
841
47
HabSelecOutput?
HabSelecOutput?
1
1
-1000

SWITCH
539
14
690
47
AnnualFishOutput?
AnnualFishOutput?
0
1
-1000

INPUTBOX
538
124
610
184
OutputDate
240
1
0
Number

PLOT
184
13
473
192
Number of trout in the reach
Age-class
Number
0.0
6.0
0.0
300.0
true
false
"set-plot-y-range 0 (count trout) + 1\nset-histogram-num-bars 6" "set-plot-y-range 0 (count trout with [status = \"alive\"]) + 1"
PENS
"trout" 1.0 1 -13345367 true "" "histogram [age-class] of trout with [status = \"alive\"]"

MONITOR
472
13
522
58
Age 0
count trout with [age-class = 0 and status = \"alive\"]
17
1
11

MONITOR
472
57
522
102
Age 1
count trout with [age-class = 1 and status = \"alive\"]
17
1
11

MONITOR
472
102
522
147
Age 2
count trout with [age-class = 2 and status = \"alive\"]
17
1
11

MONITOR
472
147
522
192
Age 3+
count trout with [age-class >= 3 and status = \"alive\"]
17
1
11

CHOOSER
184
192
522
237
DemoPlot1
DemoPlot1
"Numbers" "Biomass [kg]" "Dead fish" "Number of eggs"
0

PLOT
184
237
522
420
Daily dynamics
Tick
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS

PLOT
522
238
861
420
Demographics
Year
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS

CHOOSER
522
193
861
238
Demoplot2
Demoplot2
"Numbers" "Biomass" "Dead fish" "Number of breeders" "Initial number eggs - Hatched fry"
3

CHOOSER
184
420
523
465
LifeHistory1
LifeHistory1
"Length-at-age" "Length-at-spawning-females" "Length-at-spawning-males" "Age-at-spawning-females" "Age-at-spawning-males" "Spawning date" "Emergence date" "Age at death"
0

PLOT
184
465
523
647
Life History
Year
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS

CHOOSER
523
420
861
465
LifeHistory2
LifeHistory2
"Length-at-age" "Length-at-spawning-females" "Length-at-spawning-males" "Age-at-spawning-females" "Age-at-spawning-males" "Spawning date" "Emergence date" "Age at death"
2

PLOT
523
466
861
646
Life History 2
Year
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS

TEXTBOX
628
122
746
151
Model outputs
18
0.0
1

CHOOSER
184
646
523
691
Genetics1
Genetics1
"Genotypic maturity threshold females" "Genotypic maturity threshold males" "Genotypic length-at-emergence" "Genotypic neutral trait"
0

PLOT
184
690
523
870
Genetics
Year
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS

CHOOSER
523
646
861
691
Genetics2
Genetics2
"Genotypic maturity threshold females" "Genotypic maturity threshold males" "Genotypic length-at-emergence" "Genotypic neutral trait"
2

PLOT
523
691
862
870
Genetics 2
Year
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS

SWITCH
9
890
164
923
GeneticTransmission?
GeneticTransmission?
0
1
-1000

SWITCH
633
153
736
186
Plotting?
Plotting?
0
1
-1000

SWITCH
972
20
1090
53
PlotCellDepth?
PlotCellDepth?
0
1
-1000

INPUTBOX
6
730
167
790
mortFishTerrPredMin
0.996
1
0
Number

INPUTBOX
6
788
167
848
mortFishAqPredMin
0.984
1
0
Number

INPUTBOX
9
957
164
1017
mutationFactor
1
1
0
Number

TEXTBOX
38
865
135
883
Genetic sub-model
11
0.0
1

INPUTBOX
10
1058
165
1118
habDriftConc
2.1E-10
1
0
Number

INPUTBOX
10
1117
165
1177
habDriftRegenDist
600
1
0
Number

INPUTBOX
10
1176
165
1236
habSearchProd
4.8E-7
1
0
Number

INPUTBOX
10
1236
165
1296
habPreyEnergyDensity
5200
1
0
Number

TEXTBOX
42
1033
137
1051
Food productivity
11
0.0
1

BUTTON
79
49
143
82
calibration
calibrate
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
146
12
179
110
expindex
expindex
0
2000
1330
1
1
NIL
VERTICAL

INPUTBOX
321
923
451
983
anglePressure
2
1
0
Number

INPUTBOX
321
983
451
1043
mortFishAngleSlotLower
17
1
0
Number

INPUTBOX
321
1042
451
1102
mortFishAngleSlotUpper
100
1
0
Number

TEXTBOX
421
893
513
911
Angling sub-model
11
0.0
1

SWITCH
462
997
610
1030
FixedAnglePressure?
FixedAnglePressure?
1
1
-1000

SWITCH
462
955
610
988
AnglingOutput?
AnglingOutput?
0
1
-1000

INPUTBOX
462
1030
610
1090
exploitationRate
50
1
0
Number

OUTPUT
304
1134
648
1224
12

SWITCH
13
132
163
165
ModelHydraulics?
ModelHydraulics?
0
1
-1000

@#$#@#$#@
## WHAT IS IT?

This is the first version (1.0) of the InSTREAM-Gen model developed by Daniel Ayllón. InSTREAM-Gen is an eco-genetic version of inSTREAM (Railsback et al. 2009), with genetic transmission of traits to allow evolutionary changes in trout life-history. This model is described in Ayllón et al. (2015).

## HOW IT WORKS

The model description follows the ODD (Overview, Design concepts, Details) protocol for describing individual-based models (Grimm et al. 2006, 2010).

### OVERVIEW

#### PURPOSE

InSTREAM-Gen was designed to understand how environmental conditions and anthropogenic disturbances drive the evolution of demographics and life-history strategies of stream-dwelling trout populations. Therefore, it is particularly suited to simulate the eco-evolutionary consequences of river management decisions under a climate change context. 

#### ENTITIES, STATE VARIABLES, AND SCALES

_Spatial scales_: The entire model is represented by one spatially-explicit stream reach of a length defined by the user, but never longer than 300 meters nor wider than 50 meters. The stream habitat within the reach is depicted as a grid of cells of variable size. 

_Temporal scale_: The model includes a temporal scaling factor which allows the user to set the time step (never less than one day), so that it is user-specified. At any case, there are three trout actions (habitat selection, feeding and growth, and survival) which are always performed on a daily basis irrespective of the time step defined. The extent (duration) of the simulation is also defined by the user through the length of the environmental and habitat time-series.

_Entities_: This IBM includes three types of entities: Cells, trout and redds. Cells are objects that represent patches of relatively uniform habitat within a reach. Trout are modelled as individuals. Redds are spawning nests made by trout that are modelled as individual objects.

_State variables_: The global (reach) environment is characterized by its environmental and biological conditions. Environmental variables include day length, water temperature, flow and stream’s wetted area. Global biological conditions comprise density of piscivorous fish and the temperature function of the trout’s physiological maximum daily food consumption.

Each cell is characterized both by its physical habitat, including its water depth and velocity, average distance to hiding cover, the fraction of the cell with velocity shelters, cover from predation and gravel, its total area, and area with velocity shelters and cover from predation; and also by its production rate of two different kinds of food, drift and search (stationary) food.

Each trout has 21 state variables including its current cell, sex, age (days), age-class (from 0+ to >4+), status (alive/dead), body length, body weight, body condition factor, maximum sustainable swimming speed, physiological maximum daily food consumption, energy available for growth in its current cell, phenotypic length maturity threshold, genotypic length maturity threshold, phenotypic length at emergence, genotypic length at emergence, phenotypic value of neutral trait, genotypic value of neutral trait, and three Boolean variables indicating whether the trout has access to a velocity shelter, is already matured or has already spawned this spawning season. Finally, each trout records its cause of death.

Redds are described by its Redd Id, current cell, creation date, number of eggs, developmental status of the eggs, genotypic value of heritable traits of the mother and all fathers, number of days after eggs start to hatch, number of hatched eggs and the total number of dead eggs owing to each source of egg mortality.
 

#### PROCESS OVERVIEW AND SCHEDULING

_Processes_: The model is developed to cover the whole life-cycle of a stream-dwelling trout species. It is structured in nine processes: one related to the reach and cells (update of environmental and habitat conditions), five concerning trout (habitat selection, feeding and growth, survival, reproduction, and ageing) and three performed by redds (development, survival, and hatching of eggs and genetic transmission of traits to new trout). 

The reach and cells update their state variables every time step over the whole simulation; trout perform each process every time step of the simulation, but for reproduction, which only occurs during the spawning season (every time step), and angling and hooking mortality, which is restricted to the angling season (every time step); trout age every time step but change their age-class once a year (the Julian day they were born); redd’s development and survival processes occur on a time-step basis since redd creation until all eggs have hatched; transmission of heritable traits occurs just when the egg hatches and the new trout is created.

_Schedule_: The simulation starts at an initial date set by the user through the input parameter initial-date. Environmental and habitat updates are scheduled first because subsequent trout and redd actions depend on the time step’s environmental and habitat conditions. Trout actions occur before redd’s because one trout action (reproduction) can cause redd mortality via superimposition. Reproduction is the first trout action because spawning can be assumed the primary activity of a fish on the day it spawns. Spawning also affects habitat selection because 1) spawners move to the spawning habitat when a redd is created and fertilized, and 2) spawners incur on weight, and thus body condition, loss after spawning, which affects their choice of habitat. Habitat selection is the second trout action each time step because it is the way that trout adapt to the new habitat conditions; habitat selection strongly affects both growth and survival. Feeding and growth precedes survival because changes in a trout’s length or condition factor affect its probability of survival. Survival has its own sub-schedule because the order in which survival probabilities for the different mortality sources are evaluated strongly affects the number of trout killed by each mortality source. Widespread, less random mortality sources are scheduled first: 1) high temperature, 2) high water velocity, 3) stranding, 4) poor condition, 5) predation by terrestrial animals, 6) predation by piscivorous fish, and 7) angling and hooking. The user has the possibility of choosing which mortality sources can kill trout during the simulation and which ones are not taken into account. Redd actions occur after cell and most trout actions because redds do not affect either habitat or fish, with the exception of creating new trout, which do not execute therefore their first actions until the day after their emergence. Redd survival is the first redd action to be executed. It includes five separate egg mortality sources that follow their own sub-schedule, from least to most random: 1) low temperature, 2) high temperature, 3) scouring, 4) dewatering, 5) superimposition. Trout emergence and genetic transmission of heritable traits is the last redd action. Since survival is scheduled before emergence, trout within redds are subject to redd mortality on the day they emerge (but not to trout mortality). Trout ageing is the last agent’s executed action each time step so that both pre-existent and new created trout can increase their age. Finally, observer actions (plotting graphs and writing output files) take place at the end of the time step. All actions occur in the same predetermined order:

 1. Reach updates environmental and biological conditions. Cells update depth and velocity as a function of flow, and drift/search food production rate.
 2. Trout reproduce: 
  2.1. Trout become spawners.
  2.2. Trout spawn and create redds. 
 3. Trout select habitat.
 4. Trout feed and grow: update length, weight and body condition factor.
 5. Trout survive or die.
 6. Redds’ eggs survive or die. 
 7. Redds’ eggs develop.
 8. Redds’ eggs hatch, new trout are created and heritable traits are transmitted.
 9. Trout age.
 10. Observer plots model graphical outputs and write model output files.


### DESIGN CONCEPTS

**Basic principles:** The model was designed with an eco-genetic structure to analyze both ecological and genetic effects on population dynamics and life-history evolution on contemporary timescales.  Accounting for inheritance of quantitative genetic traits allows the study of the eco-evolutionary responses of populations to changing environmental conditions, extreme climate events and strong anthropogenic selection pressures. InSTREAM-Gen is therefore a spatial dynamics model, which integrates the demographic, genetic and spatial dimensions of individual variability through its underlying spatially explicit bioenergetics model and its quantitative genetic model of inheritance of genetic traits.

InSTREAM-Gen is underpinned by "State and prediction-based theory", a new approach that combines existing trade-off methods with routine updating: individuals make a prediction of the future growth and risk conditions over an entire time horizon under different alternative behaviours, but each time they update their decision by considering how their internal state and external conditions have changed, so that they can select the alternative optimizing a fitness measure (see review by Railsback and Harvey 2013).

In inSTREAM-Gen, population abundance and structure can be influenced by density-dependent or density-independent processes. Direct density dependence is only represented through the aquatic predation mortality function, which partly depends on density of piscivorous fish in the reach. Indirect density-dependent mortality occurs during the spawning season, since increasing number of spawners increases the probability of redds dying by superimposition. Density-independent mortality factors include terrestrial predation, flow and temperature extreme events and recreational fishing.

**Emergence:** Dynamics of population demographics (abundance, biomass, production, age- and size-structure) and genetics (evolutionary changes in life-history traits such as size-at-emergence, size maturity threshold, age-at-first-reproduction, time of spawning and emergence) emerge from the growth, survival, and reproduction of individuals, individual-level processes which are driven by complex interactions between individuals and their spatio-temporally heterogeneous habitat. Likewise, other population-level responses, like density-dependent mortality and growth, and habitat selection patterns, are emergent properties of the modelled systems.

**Adaptation:** Habitat selection (i.e., the decision of which cell to occupy each time step) is the primary adaptive trait of trout, strongly driving trout growth and survival. Other adaptive trait is the selection of the feeding strategy (drift-feeding vs. search-feeding) a fish uses each time step, since it directly affects growth and, indirectly, survival. Trout are able to adapt some of their reproductive behaviors to environmental conditions and their own state: The decision by female spawners of when and where to spawn affects offspring production as well as recruitment survival and growth; selection of male spawners by female spawners is based on the male’s body condition factor, and offspring’s genotypic body size and size maturity threshold are inherited from their parents.

**Objectives:** Habitat selection is modelled as a fitness-seeking process, by which trout select the cell that maximizes “Expected Reproductive Maturity”, a fitness measure developed by Railsback et al. (1999) that represents the expected probability of surviving and reaching reproductive size over a future time horizon.
Prediction: Trout are able to predict the probability of both surviving starvation and other mortality sources (except fishing mortality), and approaching maturity size over a future time horizon defined by the user.

**Learning:** This concept is not explicitly considered in the model.

**Sensing:** Trout sense water temperature, which influences growth and survival, and consequently, habitat selection. Redds sense water temperature too, affecting survival, development and the timing of hatching. Trout perceive the cell’s habitat conditions, both hydraulic conditions and structural features (cover and substrate). This is a main driver of habitat selection. In the case of redds, they also sense their hydraulic environment, which determines the probability of survival of eggs. Trout are aware of all mortality sources in the model and are able to estimate the risk posed by each of them (but for fishing mortality, whose risk is not sensed). 

**Interaction:** Competition for food and feeding habitat (velocity shelters) are modelled explicitly, at the cell scale, according to a size-based dominance hierarchy. Each habitat cell contains a limited daily food supply and a fixed area of velocity shelter, so that the food consumed and the sheltered area once used by larger trout are not available for smaller fish. Sexual selection is simulated by indirect interactions of males through their relative weight and condition factor.

**Stochasticity:** InSTREAM-Gen is not a highly stochastic model. The most important process represented as stochastic is trout and redd mortality. While mortality is modelled by calculating the daily probability of each individual agent’s survival through deterministic logistic functions, whether the agent actually lives or dies is a stochastic event. Stochasticity is also used in the reproduction process for setting the timing of redd creation, and for the selection of the number and identity of males fertilizing the eggs of a redd, as well as of the identity of the male spawner transmitting its genetic inheritance to each egg. The genotypic and phenotypic values of heritable traits of new created trout are drawn from empirical probability distributions. Likewise, position, sex, age (in days), body size, as well as the genotypic value and its phenotypic expression of heritable life-history traits of trout at initialization are stochastic (drawn from probabilistic functions).

**Collectives:** Collectives are not included in the model.

**Observation:** The model produces both graphical displays and output files.
The model provides a graphical display of habitat cells and the location of fish and redds as the model executes. In addition, the model provides several graphical displays of model outputs: population structure updated on a tick basis; fish numbers and biomass, dead fish numbers broken out by mortality source, and total number of eggs in the reach, all updated on a tick basis; yearly demographic outputs (written on the Julian date set by OutputDate parameter) including fish numbers and biomass, dead fish numbers broken out by mortality source, number of breeders, and number of initial eggs and fry hatched; yearly life-history outputs including minimum, mean and maximum values of  length-at-age, length and age at spawning broken out by sex, spawning date, emergence date, and age at death; yearly genetic outputs including minimum, mean and maximum values of  genotypic length at emergence, neutral trait and length maturity threshold, the latter broken out by sex.

The following demographic and genetic outputs can be recorded at the population level to follow the changes through time of the population ecogenetic structure: 1) Summary population statistics (LiveFishOutput file): These statistics include abundance, abundance of mature fish, total fish biomass, and mean and variability (standard deviation) of fish length, weight, and phenotypic values of length maturity threshold, length at emergence and neutral trait, broken out by age-class; 2) Summary breeder population statistics (BreedersPopOutput file): These statistics include abundance, and mean, minimum value and variability (standard deviation) of age and fish length at spawning, all broken out by sex. It includes also both phenotypic and genotypic values of length at emergence, neutral trait and length maturity threshold, the latter broken out by sex. Finally, the output file records the mean, minimum value and standard deviation of spawning date and date of emergence of the offspring; 3) Fish mortality (DeadFishOutput file): It records the number of fish that have died of each mortality source during a time step, broken out by age class; 4) Redd status and mortality (ReddOutput file): It reports when a redd was created, how many viable eggs were created, and when the redd was removed from the model because all its eggs had died or emerged, together with  the number of eggs died from each redd mortality source and the number of emerged new trout.

The model also allows the possibility of recording life-history features of breeders at the individual level (BreedersIndOutput file): It records the trout and fertilized redd’s IDs, the sex, age-class, age, length and weight at spawning, as well as both the phenotypic and genotypic values of length maturity threshold, length at emergence and neutral trait. Habitat use and availability can be recorded through the HabSelecOutput file: It reports, for every cell, its area, depth, velocity, fraction with velocity shelters, fraction with cover from predation, average distance to hiding cover, and food availability (drift and search food production rates, as well as the number of trout in the cell broken out by age-class. The output file also provides the flow, temperature and total trout abundance in the reach.

Both demographic fish output files (LiveFishOutput and DeadFishOutput) can be either written on a yearly or tick basis (set through the AnnualFishOutput? global parameter). When written on a tick basis, the parameter fileOutputFreq sets the frequency. The OutputDate parameter defines the Julian date when the yearly population outputs are written, but for the ReddOutput file, which is updated every time a redd is dead or emptied.



### DETAILS

#### INITIALISATION

At initializing a model run, the user must specify the initial date of simulation and the duration of a time step. State of reach’s environmental variables, as well as cells’ hydraulic and habitat variables are input data. Trout population numbers, age-structure and length-distribution are input data. Population-level distributions of heritable traits are also input data. 

Each individual’s state variable (sex, age, length, and genotypic and phenotypic values of heritable traits) is initialized by drawing from probability distributions describing their variability. Length and genotypic values of heritable traits are truncated at 4 standard deviations from the center of the probability distributions. Length of 0+ trout cannot be lower than a minimum user-defined value fishMinNewLength. Trout weight is calculated as a function of length:

fishWeight = fishWeightParamA × (fishLength)^fishWeightParamB				  

and condition factor is subsequently calculated as a function of body length and weight. The condition factor variable used in the model (fishCondition) can be considered the fraction of “healthy” weight a fish is, given its length (approach adopted from Van Winkle et al. 1996). The value of fishCondition is 1.0 when a fish has a “healthy” weight for its length, according to the length-weight relationship. Trout maximum sustainable swimming speed is a function of the fish’s length and water temperature. It is modelled as a two-term function, where the first term represents how it varies linearly with fish length, while the second modifies maximum swimming speed with a non-linear function of temperature:
											
fishMaxSwimSpeed [cm/s] = [fishMaxSwimParamA × fishLength + fishMaxSwimParamB]
× [fishMaxSwimParamC × (temp)^2 + fishMaxSwimParamD × temp + fishMaxSwimParamE)] 

Status is set to “alive”. Maturity status is set to either “mature” or “non-mature” depending on whether trout’s initial length is over or under the phenotypic value of the length maturity threshold (fishSpawnMinLength). The spawnedThisSeason? variable is set to “NO”.

Each trout’s location is assigned stochastically while avoiding extremely risky habitat. The model limits the random distribution of trout to cells where the trout are not immediately at high risk of mortality due to high velocity or stranding. Therefore, each trout is located in a random wetted cell (cellDepth > 0) with a ratio of cell velocity to the trout’s maximum swimming speed (cellVelocity / fishMaxSwimSpeed) lower than the parameter mortFishVelocityV9, the value at which the probability of surviving high velocity mortality equals 0.9.
 


#### INPUT DATA

Times series of three reach environmental variables (temperature, flow and Julian date) are input data. Temporal series of cells’ hydraulics (water depth and velocity) are input data too. Fixed physical habitat features of cells (spatial location, and fraction of the cell’s area having velocity shelters, elements providing cover from predators and gravels) are specified by means of input files. 


#### SUBMODELS

Here, it is only given an overview of the submodel's overall structure and processes. A detailed description of all submodels is available in the TRACE document ("TRAnsparent and Comprehensive model Evaludation"; Schmolke et al. 2010, Grimm et al. 2014, Augusiak et al. 2014), which was provided as Supplementary Material (Appendix A) by Ayllón et al. (2015).  

**Habitat update:** Reach's daily flow and temperature values, as well as the depth and velocity of each cell, are updated from input files. Food availability in each cell is calculated from its area, depth, velocity, and the reach's food parameters.

**Trout spawning:** Adult female trout determine whether they are ready to spawn, which depends on environmental conditions (date, temperature, and flow magnitude and steadiness) and on their own state (age, phenotypic maturity length threshold, length, and condition). If ready, female trout select and move to the cell having the best spawning quality and create a redd. The number of eggs increases with female length and is traded off with egg size, which increases with the genotypic value of size at emergence (see emergence and genetic transmission of traits below). That is, females that have a larger genotypic value of size at emergence produce larger eggs. Female trout spawn in descending order of length and the largest available male spawner plus a random number (between zero and three) of smaller males fertilize the eggs. The genotypic value of heritable traits of the mother and all fathers are stored in the genetic trait map of the redd. Spawners incur a weight loss to represent the energetic cost of spawning. While female trout can only spawn once per season, males can spawn several times.

**Trout habitat selection:** This is the key adaptive behaviour in the model, which is modelled according to “state- and prediction-based” foraging theory (Railsback and Harvey 2013). Each trout selects a cell for feeding from all wetted cells within a radius that increases with trout length. The trout predicts the growth and predation risk it would incur at each potential cell and moves to the cell offering the highest value of a fitness measure termed "Expected Maturity" (EM; Railsback et al. 1999). EM represents the expected probability of surviving both predation and starvation over a specified time horizon, multiplied by a term representing how growth affects fitness. This growth term represents the fitness benefits of reaching reproductive size for immature individuals. The habitat selection trait assumes a size-based dominance hierarchy: trout can only use resources (food and velocity shelters) that have not been consumed by larger trout. 

**Trout feeding and growth:** The daily weight gain or loss of each trout is calculated using standard bioenergetics approaches. The feeding and growth submodel calculates the potential food intake and metabolic costs a fish would experience in a cell, for both drift and search feeding. Fish growth is modelled as net energy intake, the difference between energy intake from food and metabolic costs. The rate of drift intake by a fish is modelled using a conventional drift-feeding approach, adapted from the models of Hughes (1992) and Hill and Grossman (1993). A fish is assumed to capture food only within a "reactive distance" that increases with body length but decreases with water velocity, while the amount of food passing within the reactive distance increases with velocity and drift concentration. Search food intake increases linearly with food production and decreases linearly to zero as water velocity increases to the fish's maximum sustainable swim speed. Metabolic costs increase with length, swimming speed, and temperature. Velocity shelters reduce swimming costs for drift-feeding fish. Length increases when trout in good condition gain weight; weight loss decreases condition.

**Trout survival:** Each trout determines stochastically whether it survives seven sources of mortality: starvation, high temperatures, high velocity, stranding, predation by terrestrial animals, predation by piscivorous trout, and angling; mortality sources are depicted as daily survival probabilities that depend on characteristics of the fish and its habitat. The risk of predation by terrestrial animals can be reduced by smaller size, high velocity or depth, and proximity of hiding cover; the risk of predation by other trout can be reduced by larger size, shallow depth, low temperature, and low density of large trout. The risk of starvation increases as body condition decreases. Angling and hooking mortality depends on fishing pressure, capture rate, and survival probability, which is a function of how many times a trout is hooked and whether it is kept vs. released each time hooked. Although none of these mortality risks are directly density-dependent, density dependent survival often emerges from competition for safe feeding habitat.

**Redd survival and development:** Redds are subject to egg mortality (loss of some or all of their remaining viable eggs) owing to excessively cold or warm temperatures, dewatering in low flows, scouring in high flows, and superimposition of a new redd. Remaining viable eggs develop at a rate dependent on temperature.

**Emergence and genetic transmission of traits:** When a redd's eggs are fully developed, they “emerge” (hatch into new trout) over several days. Only length at emergence and the maturity size threshold are modelled as transmissible traits, although we additionally included a neutral trait (not affecting the fitness of individuals) used in evaluating whether genetic changes in heritable traits result from selection or from genetic drift. Heritable traits are independent of each other. We assume that each egg is fertilized by just one male spawner, so that each new trout inherits its traits from the mother and a father randomly assigned from the males contributing to the redd (the probability of fertilization is the same for all males). We model the phenotype of an individual as the sum of an inherited additive genetic effect and a non-heritable residual effect. Inheritance rules are based on the infinitesimal model of quantitative genetics theory (Lynch and Walsh 1998). Each offspring's genotypic value for a trait under selection is drawn from a normal distribution centred on the arithmetic mean of the two parental values, while the variance of this distribution is equal to half the total additive genetic variance for the trait at the population level plus the variance potentially introduced by mutation (mutational variance). Theoretically, natural selection and genetic drift tend to decrease genetic variance, which is concurrently replenished by mutation. However, with moderate selection strength and sufficient population size, additive genetic variance for a polygenic trait to which a large number of loci are contributing tends to stay relatively constant over contemporary times (Barton and Keightley 2002, Vincenzi et al. 2012, Vincenzi 2014). Thus, as we do not explicitly model alleles and loci in our genetic module, we assumed the additive genetic variance remains constant across generations.

**Trout Ageing:** Trout update every time step their state variable age, which track the number of days since a trout was born. Trout update their age-class state variable the Julian date where they were born.  When new trout are born, then all trout from the older cohorts update their age-class to avoid that some fish could have the same age-class while belonging to different cohorts. The same action happens likewise when the first fish of older cohorts updates its age-class and new recruits have not been born yet.



### REFERENCES

Augusiak, J., Van den Brink, P.J., Grimm, V., 2014. Merging validation and evaluation of ecological models to ‘evaludation’: A review of terminology and a practical approach. Ecological Modelling 280, 117-128.

Ayllón, D., Railsback, S.F., Vincenzi, S., Groeneveld, J., Almodóvar, A., Grimm, V., 2015. InSTREAM-Gen: modelling eco-evolutionary dynamics of trout populations under anthropogenic environmental change. Ecological Modelling.

Barton, N.H., Keightley, P.D., 2002. Understanding quantitative genetic variation. Nature reviews Genetics 3, 11–21.

Grimm, V., Berger, U., Bastiansen, F., Eliassen, S., Ginot, V., Giske, J., Goss-Custard, J., Grand, T., Heinz, S.K., Huse, G., Huth, A., Jepsen, J.U., Jorgensen, C., Mooij, W.M., Muller, B., Pe'er, G., Piou, C., Railsback, S.F., Robbins, A.M., Robbins, M.M., Rossmanith, E., Ruger, N., Strand, E., Souissi, S., Stillman, R.A., Vabo, R., Visser, U., DeAngelis, D.L., 2006. A standard protocol for describing individual-based and agent-based models. Ecological Modelling 198, 115-126.

Grimm, V., Berger, U., DeAngelis, D.L., Polhill, J.G., Giske, J., Railsback, S.F., 2010. The ODD protocol A review and first update. Ecological Modelling 221, 2760-2768.

Grimm, V., Augusiak, J., Focks, A., Frank, B.M., Gabsi, F., Johnston, A.S.A., Liu, C., Martin, B.T., Meli, M., Radchuk, V., Thorbek, P., Railsback, S.F., 2014. Towards better modelling and decision support: Documenting model development, testing, and analysis using TRACE. Ecological Modelling 280, 129-139.

Hill, J., Grossman, G. D., 1993. An energetic model of microhabitat use for rainbow trout and rosyside dace. Ecology 74, 685-698.

Hughes, N. F., 1992. Selection of positions by drift-feeding salmonids in dominance hierarchies:  Model and test for arctic grayling (Thymallus arcticus) in subarctic mountain streams, interior Alaska. Canadian Journal of Fisheries and Aquatic Sciences 49, 1999-2008.

Lynch, M., Walsh, J.B., 1998. Genetics and Analysis of Quantitative Traits. Sinauer Associates Inc., Sunderland, USA.

Railsback, S.F., Lamberson, R.H., Harvey, B.C., Duffy, W.E., 1999. Movement rules for individual-based models of stream fish. Ecological Modelling 123, 73-89.

Railsback, S.F., Harvey, B.C., 2013. Trait-mediated trophic interactions: is foraging theory keeping up? Trends in Ecology and Evolution 28, 119-125.

Railsback, S.F., Harvey, B.C., Jackson, S.K., Lamberson, R.H., 2009. InSTREAM: the individual-based stream trout research and environmental assessment model. U.S. Department of Agriculture, Forest Service, Pacific Southwest Research Station, Albany, CA.

Schmolke, A., Thorbek, P., DeAngelis, D.L., Grimm, V., 2010. Ecological models supporting environmental decision making: a strategy for the future. Trends in Ecology and Evolution 25, 479-486.

Vincenzi, S., 2014. Extinction risk and eco-evolutionary dynamics in a variable environment with increasing frequency of extreme events. Journal of the Royal Society Interface 11.

Vincenzi, S., De Leo, G.A., Bellingeri, M., 2012. Consequences of extreme events on population persistence and evolution of a quantitative trait. Ecological Informatics 8, 20-28.




## HOW TO USE IT

Click the "setup" button to initialise the model. Input data are loaded, the stream, cells and trout agents are created, and the outputs are initialised. The values of each parameter are set. Parameter values can be changed at the "_Parameters setup_" file that is uploaded to the model when clicking the "includes" button in the "Code" tab. Only the six parameters suited for calibration plus the parameter defining the mutation factor can be set from the interface.The switches on the left side ("Trout Mortality Functions") allow the user to select the mortality sources that can actually kill the trout. By means of the switches of the "Genetic sub-model", the user decides whether the genetic transmission of heritable traits will be modelled or not, and if so, whether the total additive genetic variance fixed at initialization remains constant across generations or it changes otherwise, being then computed as the variance of the breeding genotypic values. In the "Model outputs" section of the interface, the user can choose the output files to be written during the simulation. The different model outputs written in each output file are described in the concept "Observation" of the section "Design concepts" of the ODD. The switch _AnnualFishOutput?_ defines whether the "_LiveFishOutput_" and "_DeadFishOutput_" files are recorded on an annual (the date of recording is set at the _OutputDate_ input box) or a periodic basis (then the frequency is set at the _fileOutputFreq_ input box). While the "_HabSelecOutput_" file is always written in a periodic basis, the "_ReddOutput_" and "_BreedersOutput_" files are always recorded on an annual basis.

Click the "reset" button to initialise the model without loading again the input files.

Click the "calibrate" button only if the simulation run is aimed at calibrating the model. The slider _expindex_ sets the combination of parameter values defined in the "_experiment-plan-calibration_" input file that will be used in the simulation run.  

Click the "go" button to start the simulation (click "go" again to stop it). The age distribution of trout (number of alive trout broken out by age-classes) is always shown both in a plot ("Number of trout in the reach") and in the adjacent monitors.The user can choose whether to plot additional demographic and genetic population patterns through the switch _Plotting?_. The patterns to be displayed in each plot during the simulation run can be selected by the user by means of the choosers. The selection must be made before the simulation starts and cannot be changed afterwards. The first line of plots display demographic outputs; the one on the left can display the daily dynamics of four model outputs (numbers of alive and dead trout, biomass, and number of eggs), while the one on the right can display the annual dynamics of five model outputs (numbers of alive and dead trout, biomass, number of breeders, and number of both initial eggs and hatched trout). The plots in the second line show life-history annual patterns; the same model outputs (length-at-age by age-classes, both length and age at spawning of either male or female breeders, spawning and emergence date, and age at death) can be choosen on either plot. The plots in the third line display the values of four genetic outputs (genotypic values of length-at-emergence, minimum length for spawning of either males or females, and the neutral trait). Finally, the model provides a graphical display of habitat cells and the location of fish and redds as the model executes; the user can decide whether cells display their updated water depth through the _PlotCellDepth?_ switch; the actual date of simulation is always shown.  

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

During the development of the model, Daniel Ayllón was funded by a Marie Curie Intraeuropean Fellowship (PIEF-GA-2012-329264) for the project EcoEvolClim. 
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
