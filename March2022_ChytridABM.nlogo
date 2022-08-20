;Chytridiomycosis ABM

extensions [rnd]

globals
[
  zspn-inc                                       ;incidence of zoosporangia per day
  bd-mortality                                   ;number of frogs dying of Bd
  baseline-mortality                             ;number of metamorphs dying due to background mortality
  lambda-zsp                                     ;growth rate of zoospores
  n-zspn                                         ;sum of zoosporangium
  shoreline                                      ;shoreline perimeter
  k                                              ;max number of frogs
  num-metamorphosis                              ;number of tadpoles that undergo metamorphosis (number of metamorphs prior to baseline mortality or bd death)
  var_spn_abun_tadpoles                          ;variance of tadpole parasite abundances
  avg_spn_abun_tadpoles                          ;mean parasite abundance of tadpoles
  aggregation_abun_tadpoles                      ;parasite abundance aggregation
  prop_not_s_k_abun                              ;proportion of tadpoles with spn less than s_k
  avg_spn_inten_tadpoles                       ;mean infection intensity of tadpoles
  var_spn_inten_tadpoles                       ;variance of tadpole infection intensities
  aggregation_inten_tadpoles                   ;infection intensity aggregation
  prop_not_s_k_inten                           ;proportion of tadpoles with infection intensities greater than zero and less than carrying capacity
  tad_prev                                     ;prevalence of bd in tadpoles
  avg_spn_inten_metas
  var_spn_inten_metas
  aggregation_inten_metas
  metas_prev
  prop_deaths_due_to_bd                       ;proportion of metamorph deaths attributable to Bd = bd-mortality / (bd-mortality + baseline-mortality)
]

breed [ tadpoles tadpole ]
breed [ metamorphs metamorph ]

patches-own
[
  border                                          ;this patch variable is set to 1 if a patch is located on the border of the model landscape, 0 if a non-border patch
  pond                                            ;this patch variable is set to 1 if a patch has pond, 0 if otherwise
  nspn                                            ;sum of zoosporangia on all frogs in this patch
  zsp                                             ;number of zoospores in a pond patch
  prev-zsp                                        ;number of zoospores in the previous time step (to calculate lambda)
  pondid                                          ;pond number
  pp                                              ;pond perimeter
  new-infections                                  ;output of multinomial draws, represents a list where for each time a host appears in the list it has a new zoospore successfully infecting it
  perimeter-land                                  ;perimeter land patch
]

metamorphs-own
[
  aid                                            ;age in days
  bd                                             ;1 if infected, 0 if uninfected
  spn                                            ;sporangia load
  imm                                            ;immunity
  smax                                           ;maximum sporangia load - mortality occurs when sporangia load exceeds smax
  est
  expo
  infprob
  new-pz0                                        ;temporary new pz0 for each patch
  pz0                                            ;prezoosporangium (pz0-pz7 are used to create a 7 day lag period from zoospore establishment till the maturation of an infectious sporangia)
  pz1
  pz2
  pz3
  pz4
  pz5
  pz6
  pz7
  vac
  b_cohort                                       ;specifies birth cohort
  on-land                                        ;move to a land patch
]

tadpoles-own
[
  aid                                           ;age in days
  bd                                            ;1 if infected, 0 if uninfected
  spn                                           ;sporangia load
  imm                                           ;immunity
  s_k                                           ;maximum number of zoospores on tadpoles due to space limitation (sporangia carrying capacity)
  est
  expo
  infprob
  new-pz0
  pz0                                           ;prezoosporangium (pz0-pz7 are used to create a 7 day lag period from zoospore establishment till the maturation of an infectious sporangia)
  pz1
  pz2
  pz3
  pz4
  pz5
  pz6
  pz7
  vac
  b_cohort                                      ;specifies birth cohort if modeling birth pulses
]

to setup
  ca
  ifelse SimplePond = TRUE
  [ resize-world 0 3 0 3
    set-patch-size 80 ]
  [ set-patch-size 17     ;5
    resize-world 0 20 0 20 ]

  ask patches [
    set pcolor brown
    set pond 0
    ]
  ;defines border patches
  let maxx max [ pxcor ] of patches
  let maxy max [ pycor ] of patches
  let minx min [ pxcor ] of patches
  let miny min [ pycor ] of patches
  ask patches [
   ifelse pxcor = maxx or pxcor = minx or pycor = maxy or pycor = miny
    [ set border 1 ]
    [ set border 0 ]
  ]
  ;define non-border patches (nbpatches) because we don't want pond patches on the border
  let nbpatches patches with [ border = 0 ]
  ifelse SimplePond = TRUE
  [ ask one-of nbpatches [
    set pond 1
    set pondid 1
    set pcolor blue
    initialize-tadpole-pop
    initialize-Bd-tadpoles
    ]
    ]
  [
  create-pond
  ]
    ;below code specifies which patches are on the perimeter.
  ask patches with [ pond = 1 ] [
     if count neighbors4 with [ pond = 0 ] > 0 [
        set pp 1
        set pcolor 107
      ]
  ]
   ask patches with [ pond = 0 ] [
     if count neighbors4 with [ pond = 1 ] > 0 [
        set perimeter-land 1
    ]
  ]
    ask patches with [pp = 1][
    ;reports the shoreline in the command center (if I want that)
  ;  calculate-shoreline
    ]
   ; print shoreline  ;in case I want to know what the shoreline/perimeter is
    let perimeterp patches with [ pp = 1 ]
    ask perimeterp [
     initialize-tadpole-pop
    if inf-ponds = 1 [
      initialize-Bd-tadpoles
    ]
  ]

  ; if only wanting 1 pond patch to be infected, comment out lines 145-147 and use lines 151-155
;  if inf-ponds = 1 [
;      ask n-of 1 perimeterp [  ;if only wanting to initialize 1 patch with Bd tadpole
;      initialize-Bd-tadpoles
;    ]
;  ]

 ;create-data-file
  reset-ticks
end

to go
;  random-seed behaviorspace-run-number
  ;if ticks = 90 [ stop ]
  if ticks = last-day [ stop ]
  ;if ticks = 104 [ stop ]    ; for scenario in which all tadpoles have the same amount of time to metamorphose regardless of birth cohort
  ask tadpoles [
    set aid aid + 1                  ;adding a day to the tadpole's age
   ; if random-float 1 < 0.06 [       ;tadpole daily mortality probability estimated from Govindarajulu 2006
   if random-float 1 < tad-mort [
    die
    ]
  ]
  if birth_pulses = 2 [
  if ticks = 7 [
    ask patches with [ pp = 1 ] [
    initialize-tadpole-pop_2
    ]
  ]
  ]
  if birth_pulses = 3 [
  if ticks = 7 [
   ask patches with [ pp = 1 ] [
 ;  initialize-tadpole-pop
   initialize-tadpole-pop_2
    ]
  ]
  if ticks = 14 [
    ask patches with [ pp = 1 ] [
   ; initialize-tadpole-pop
     initialize-tadpole-pop_3
    ]
  ]
  ]
  ask metamorphs [
    set aid aid + 1                   ;adding a day to the metamorph's age
    ;if random-float 1 < 0.09 [        ;baseline daily metamorph mortality - 0.09 is default as of 7.12.22
    if random-float 1 < meta-mort [    ;meta_mort is a scale of mortality probabilities
      set baseline-mortality baseline-mortality + 1  ;keep track of baseline mortality
      die
    ]
  ]

  if any? tadpoles with [ aid > 55 ] [  ;initiate metamorphosis in (some) tadpoles greater than 55
  ask tadpoles with [ aid > 55 ] [
  if random-float 1 < 0.11 [
  metamorphosis
    ]
    ]
  ]
;  if ticks = 75 [                  ;tadpoles in a later birth pulse have less time as tadpoles before metamorphosis
;    if any? tadpoles [
;    ask tadpoles[
;    metamorphosis
;      ]
;    ]
;  ]
    if any? tadpoles with [ aid > 74 ] [   ;regardless of birth cohort, all tadpoles have the same amount of time to metamorphose
    ask tadpoles[                          ;metamorphosis ranges from 56-75 days in tadpole age
    metamorphosis
      ]
    ]

 ;tadpole movement submodel
  if SimplePond = FALSE [
    tadpoles-move
   ; metamorphs-move
   ; metamorphs-move-simple
    metamorphs-move-complex
    ;ask metamorphs [                                                             ;all metamorphs move to another perimeter patch
   ; move-to one-of patches with [ pond = 1  and pp = 1]
   ; ]
  ]
  let pondppatches patches with [ pond = 1 and pp = 1]
  ask pondppatches [
    set prev-zsp zsp                                                            ;store the last tick's zoospores as previous zoospores
  ;  set pcolor scale-color red zsp 1000000 0
    ]
    let bd-tadpoles tadpoles with [ bd = 1  and spn < s_k ]

;REMINDER: double check that bd-max-tadpoles is implemented correctly - do pz0 infections continue to progress?

    ask bd-tadpoles
    [
      update-infections
      if spn >= 8000 [
        set color red
        ]
      tadpole-zsp-shedding-and-reinfection
      if spn = 0 [
      set bd 0
      set color green
        ]
        ]
    let maxbd-tadpoles tadpoles with [ bd = 1  and spn >= s_k ]      ;maxbd-tadpoles are tadpoles that have met or exceeded sporangia carrying capacity
    ask maxbd-tadpoles [                                             ;maxbd-tadpoles cannot get reinfected but can still contribute to the zoospore pool and clear sporangia
      if spn >= 8000 [                                               ;set these tadpoles as red if their sporangia load is above 8000
        set color red
        set est 0  ;this prevents maxbd-tadpoles from getting reinfected, but they can still be exposed to Bd
        ]
        max-bd-tadpole-shedding
    ]
    let bd-metamorphs metamorphs with [ bd = 1 ]
    ask bd-metamorphs
    [
     update-infections
     if spn >= smax [
        set bd-mortality bd-mortality + 1
       ;set color red
        die
        ]
      ;if spn >= 8000 [      ;metamorphs nearing bd-induced mortality appear red
        ;set color red
        ;]
      ;set spn spn - round ((0.148 + random-float 0.006) * spn)   ;sporangia loss rate 0.148 – 0.153 per day as of 8.19.22
      set spn spn - round ((0.19 + random-float 0.01) * spn)    ;value from Briggs 2010 paper
      ;set spn spn - round (0.2 * spn) - NO VARIATION
      if spn = 0 [            ;uninfected metamorphs appear green
        set bd 0
        set color green
        ]
      let zsp-release round (spn * 17.8)                         ;zoospore release rate at 23 degrees C (Woodhams et al., 2008; Briggs 2010 SI)
      let f-selfinfect round (0.05 * zsp-release)                ;fraction of the released zoospores that immediately encounter the host - SELF EXPOSURE
      set pz0 f-selfinfect * est ;pz0 + 1                        ;SELF-INFECTION
     ask patch-here [
        set zsp zsp + (zsp-release - f-selfinfect)
        ]
      ]
 ; ask pondppatches with [zsp > 0] [
    ;asking infected pond perimeter patches
   ask patches with [pond = 1] [
    set pcolor scale-color red zsp 1000000 0
    ]
  ;make netlogo plot here
 plot-zsp
; ask patches with [zsp > 0] [  ;both land and pond patches are infectious
  ask patches with [zsp > 0 and pond = 1] [  ;only pond patches are infectious
  infection-step
  ]
  ; ask turtles with [spn > 0] [   ;WRONG PLACE?
  ask turtles with [pz0 > 0] [
    set bd 1
    set color white
  ]
  if ticks > 1 [
    if sum [ prev-zsp ] of patches > 0 [
      set lambda-zsp precision (sum [ zsp ] of patches / sum [ prev-zsp ] of patches) 2
      ]
    ]
  set n-zspn sum [ spn ] of tadpoles
  set zspn-inc 0
  ;write-data-file
;  if ticks = 53 [
;  summary-stats-tad-intensity
;  ]
  ask metamorphs with [bd = 0] [
    set spn 0
  ]
  let total-bd-metas count metamorphs with [spn > 0]
  ;if ticks = 103 and total-bd-metas >= 2 [
  if ticks = last-day - 1 [
  set prop_deaths_due_to_bd bd-mortality / (bd-mortality + baseline-mortality)
  if total-bd-metas >= 2 [
  summary-stats-meta-intensity
  ]
  ]
  tick
end

;procedures below

to summary-stats-tad-intensity
  set avg_spn_inten_tadpoles mean [ spn ] of tadpoles with [spn > 0]
  set var_spn_inten_tadpoles variance [ spn ] of tadpoles with [spn > 0]
  set aggregation_inten_tadpoles (variance [spn] of tadpoles with [spn > 0]) / mean [spn] of tadpoles with [spn > 0]
  set prop_not_s_k_inten (count tadpoles with [spn < s_k and spn > 0]) / count tadpoles with [spn > 0]
  set tad_prev (count tadpoles with [ bd = 1 ]) / count tadpoles
end

to summary-stats-meta-intensity
  set avg_spn_inten_metas mean [ spn ] of metamorphs with [spn > 0]
  set var_spn_inten_metas variance [ spn ] of metamorphs with [spn > 0]
  set aggregation_inten_metas (variance [spn] of metamorphs with [spn > 0]) / mean [spn] of metamorphs with [spn > 0]
;  set prop_not_smax_inten (count metamorphs with [spn < smax and spn > 0]) / count metamorphs with [spn > 0]
  set metas_prev (count metamorphs with [ bd = 1 ]) / count metamorphs
end

to plot-zsp
  set-current-plot "Zsp Count"
  set-current-plot-pen "zsp"
  plot sum [ zsp ] of patches
end

;populate patches with tadpoles
to initialize-tadpole-pop

  if birth_pulses = 1 [
  sprout-tadpoles ini-tadpoles-per-pondpatch [
    set b_cohort 1        ;specify birth cohort
    set shape "frog top"
    set size 0.2
    set spn 0
    set imm random 100
   ; set s_k (9890 + random 231)
    set s_k (9395 + random 989) ; + or - 5% of 9890
    set est baseline_est + random-float 0.1 ;variation in establishment
    set expo 0.019 + random-float 0.001 ;0.019-0.021, exposure rate: amount of the environmental untis per host per day (units = liters per host per day), like a search term
                                  ;functions as a removal rate of parasites from the environemnt due to contact process
    set infprob est * expo
    set color 65
    let dir-neighbor min-one-of patches with [ pond = 0 ] [ distance myself ]
    let face-dir-x [ pxcor ] of dir-neighbor
    let face-dir-y [ pycor ] of dir-neighbor
    facexy face-dir-x face-dir-y
    rt random 40
    ;rt random 270  ;patch-at-heading-and-distance -90 1
    fd (0.32 + random-float 0.13) ;(0.25 + random-float 0.19)
    ]
  ]

    if birth_pulses = 2 [
    let new-tadpoles round ((ini-tadpoles-per-pondpatch) / 2)
    sprout-tadpoles new-tadpoles [
 ; sprout-tadpoles ini-tadpoles-per-pondpatch [
    set b_cohort 1        ;specify birth cohort
    set shape "frog top"
    set size 0.2
    set spn 0
    set imm random 100
    set s_k (9395 + random 989) ; + or - 5% of 9890
    set est baseline_est + random-float 0.1 ;variation in establishment
    set expo 0.019 + random-float 0.001 ;0.019-0.021 ;exposure rate: amount of the environmental untis per host per day (units = liters per host per day), like a search term
                                  ;functions as a removal rate of parasites from the environemnt due to contact process
    set infprob est * expo
    set color 65
    let dir-neighbor min-one-of patches with [ pond = 0 ] [ distance myself ]
    let face-dir-x [ pxcor ] of dir-neighbor
    let face-dir-y [ pycor ] of dir-neighbor
    facexy face-dir-x face-dir-y
    rt random 40
    ;rt random 270  ;patch-at-heading-and-distance -90 1
    fd (0.32 + random-float 0.13) ;(0.25 + random-float 0.19)
    ]
  ]

   if birth_pulses = 3 [
   let new-tadpoles round ((ini-tadpoles-per-pondpatch) / 3)
  sprout-tadpoles new-tadpoles [
 ; sprout-tadpoles ini-tadpoles-per-pondpatch [
    set b_cohort 1        ;specify birth cohort
    set shape "frog top"
    set size 0.2
    set spn 0
    set imm random 100
    set s_k (9395 + random 989)
    set est baseline_est + random-float 0.1 ;variation in establishment
    set expo 0.019 + random-float 0.001; exposure rate: amount of the environmental untis per host per day (units = liters per host per day), like a search term
                                  ;functions as a removal rate of parasites from the environemnt due to contact process
    set infprob est * expo
    set color 65
    let dir-neighbor min-one-of patches with [ pond = 0 ] [ distance myself ]
    let face-dir-x [ pxcor ] of dir-neighbor
    let face-dir-y [ pycor ] of dir-neighbor
    facexy face-dir-x face-dir-y
    rt random 40
    ;rt random 270  ;patch-at-heading-and-distance -90 1
    fd (0.32 + random-float 0.13) ;(0.25 + random-float 0.19)
    ]
  ]
end

;select a certain number of tadpoles in each patch to have Bd
to initialize-Bd-tadpoles
  if Bd-inf-tadpoles-per-infpondpatch > 0 [
    ask n-of Bd-inf-tadpoles-per-infpondpatch tadpoles-here [
      set bd 1
      set color white
      set spn 100
      ;set pcolor yellow
      ]
    ]
end

;populate patches with tadpoles with birth cohort 2
;there's probably an easier way to do this than creating multiple submodels but will do this for now
to initialize-tadpole-pop_2
  if birth_pulses = 2 [   ;divide initial tadpoles per patch by three because there are 3 birth pulses, this way total initial tadpoles is the same
    let new-tadpoles round (ini-tadpoles-per-pondpatch) / 2
   sprout-tadpoles new-tadpoles [
  ;sprout-tadpoles ini-tadpoles-per-pondpatch [
    set b_cohort 2                  ;specify birth cohort
    set shape "frog top"
    set size 0.2
    set spn 0
    set imm random 100
    set s_k (9395 + random 989)
   ; set est 0.1 + random-float 0.1 ;variation in establishment
    set est baseline_est + random-float 0.1
    set expo 0.019 + random-float 0.001 ;exposure rate: amount of the environmental untis per host per day (units = liters per host per day), like a search term
                                  ;functions as a removal rate of parasites from the environemnt due to contact process
    set infprob est * expo
    set color 115
    let dir-neighbor min-one-of patches with [ pond = 0 ] [ distance myself ]
    let face-dir-x [ pxcor ] of dir-neighbor
    let face-dir-y [ pycor ] of dir-neighbor
    facexy face-dir-x face-dir-y
    rt random 40
    ;rt random 270  ;patch-at-heading-and-distance -90 1
    fd (0.32 + random-float 0.13) ;(0.25 + random-float 0.19)
    ]
  ]

    if birth_pulses = 3 [
    let new-tadpoles round ((ini-tadpoles-per-pondpatch) / 3)    ;divide initial tadpoles per patch by three because there are 3 birth pulses, this way total initial tadpoles is the same
    sprout-tadpoles new-tadpoles [
    ;sprout-tadpoles ini-tadpoles-per-pondpatch [
    set b_cohort 2                  ;specify birth cohort
    set shape "frog top"
    set size 0.2
    set spn 0
    set imm random 100
    set s_k (9395 + random 989)
    set est baseline_est + random-float 0.1 ;variation in establishment
    set expo 0.019 + random-float 0.001 ;exposure rate: amount of the environmental untis per host per day (units = liters per host per day), like a search term
                                  ;functions as a removal rate of parasites from the environemnt due to contact process
    set infprob est * expo
    set color 115
    let dir-neighbor min-one-of patches with [ pond = 0 ] [ distance myself ]
    let face-dir-x [ pxcor ] of dir-neighbor
    let face-dir-y [ pycor ] of dir-neighbor
    facexy face-dir-x face-dir-y
    rt random 40
    ;rt random 270  ;patch-at-heading-and-distance -90 1
    fd (0.32 + random-float 0.13) ;(0.25 + random-float 0.19)
  ]
  ]
end

;populate patches with tadpoles with birth cohort 3
;there's probably an easier way to do this than creating multiple submodels but will do this for now
to initialize-tadpole-pop_3
  let new-tadpoles3 round ((ini-tadpoles-per-pondpatch) / 3)
  sprout-tadpoles new-tadpoles3 [
 ; sprout-tadpoles ini-tadpoles-per-pondpatch [
    set b_cohort 3                  ;specify birth cohort
    set shape "frog top"
    set size 0.2
    set spn 0
    set imm random 100
    set s_k (9395 + random 989)
    set est baseline_est + random-float 0.1 ;variation in establishment
    set expo 0.019 + random-float 0.001 ;exposure rate: amount of the environmental untis per host per day (units = liters per host per day), like a search term
                                  ;functions as a removal rate of parasites from the environemnt due to contact process
    set infprob est * expo
    set color 125
    let dir-neighbor min-one-of patches with [ pond = 0 ] [ distance myself ]
    let face-dir-x [ pxcor ] of dir-neighbor
    let face-dir-y [ pycor ] of dir-neighbor
    facexy face-dir-x face-dir-y
    rt random 40
    ;rt random 270  ;patch-at-heading-and-distance -90 1
    fd (0.32 + random-float 0.13) ;(0.25 + random-float 0.19)
    ]
end

to tadpoles-move
  let n-mobile-tadpoles round (t-movement * count tadpoles)                           ;10% of tadpoles move to a different perimeter patch each tick
    ask n-of n-mobile-tadpoles tadpoles [
      let nextpatch patches with [pp = 1]
      move-to one-of nextpatch
      let dir-neighbor min-one-of patches with [ pond = 0 ] [ distance myself ]    ;check with ani about why direction matters
     ; let dir-neighbor min-n-of patches with [ pond = 0 ] [ distance myself ]
      let face-dir-x [ pxcor ] of dir-neighbor
      let face-dir-y [ pycor ] of dir-neighbor
      facexy face-dir-x face-dir-y
      rt random 40
      fd (0.32 + random-float 0.13)
  ]
end

;to metamorphs-move
;let n-land-metamorphs round (0.20 * count metamorphs)
;ask n-of n-land-metamorphs metamorphs [
;    ;move-to one-of patches with [ pond = 1 and pp = 0] ;metamorphs in deep pond
;    move-to one-of patches with [ perimeter-land = 1]
;  ]
;end

to metamorphs-move-complex
ask metamorphs[
    set on-land 0          ;re-set it so that all metamorphs are defaulted to be equally likely to move between land and water, regardless of what type of patch currently on
  ]
let n-land-metamorphs round (m-land * count metamorphs)
ask n-of n-land-metamorphs metamorphs[
set on-land 1             ;select a subset of metamorphs to move to a land patch
  ]
ask metamorphs [
    ifelse on-land = 1
    [move-to one-of patches with [ perimeter-land = 1]]
    [move-to one-of patches with [ pond = 1  and pp = 1]]
  ]
end

to metamorphs-move-simple
ask metamorphs [                                                             ;all metamorphs move to another perimeter patch
    move-to one-of patches with [ pond = 1  and pp = 1]
    ]
end

to update-infections
  let z1 pz0             ;upon establishment, a zoospore takes ~7 days to mature into a sporangia
      let z2 pz1
      let z3 pz2
      let z4 pz3
      let z5 pz4
      let z6 pz5
      let z7 pz6
      set pz1 z1
      set pz2 z2
      set pz3 z3
      set pz4 z4
      set pz5 z5
      set pz6 z6
      set pz7 z7
      set pz0 0
      set spn spn + pz7
      set zspn-inc zspn-inc + pz7
end

to metamorphosis
        hatch-metamorphs 1 [
          set aid 1
          set b_cohort [ b_cohort ] of myself     ;keep track of birth cohort identity
          set bd [ bd ] of myself       ;maintain bd infection through metamorphosis (McMahon & Rohr, 2015)
          set expo [expo] of myself
          set est [est] of myself
          set infprob [infprob] of myself
;          set new-pz0 [new-pz0] of myself
          if bd = 1 [
;previously all pz0-pz7 was commented out so prezoosporangia were not carried through metamorphosis
;             set pz0 0.5 * ([ pz0 ] of myself)
;             set pz1 0.5 * ([ pz1 ] of myself)
;             set pz2 0.5 * ([ pz2 ] of myself)
;             set pz3 0.5 * ([ pz3 ] of myself)
;             set pz4 0.5 * ([ pz4 ] of myself)
;             set pz5 0.5 * ([ pz5 ] of myself)
;             set pz6 0.5 * ([ pz6 ] of myself)
;             set pz7 0.5 * ([ pz7 ] of myself)
;             set spn 0.5 * ([ spn ] of myself)   ;**look up conversion factor from McMahon & Rohr 2015 for this but starting with 50% for now** maintain spn load proportional to tadpole infection intensity
             set pz0 [ pz0 ] of myself   ;trying 100% carryover of Bd infections through metamorphosis
             set pz1 [ pz1 ] of myself
             set pz2 [ pz2 ] of myself
             set pz3 [ pz3 ] of myself
             set pz4 [ pz4 ] of myself
             set pz5 [ pz5 ] of myself
             set pz6 [ pz6 ] of myself
             set pz7 [ pz7 ] of myself
             set spn [ spn ] of myself   ;**look up conversion factor from McMahon & Rohr 2015 for this but starting with 50% for now** maintain spn load proportional to tadpole infection intensity

    ]
          ;set smax (16000 + random 231)
         ; set smax (baseline_smax + random 231)
          set smax baseline_smax
          set shape "frog top"
          set size 0.4
          set color brown
          ]
        set num-metamorphosis num-metamorphosis + 1
        die                             ;remove tadpole from population once it's metamorphosed
end

to tadpole-zsp-shedding-and-reinfection
     ; set spn spn - round ((0.148 + random-float 0.006) * spn)   ;sporangia loss rate 0.148 – 0.153 per day - Briggs 2010 uses 0.2 lost per day
      set spn spn - round ((0.19 + random-float 0.01) * spn)     ;Briggs 2010 uses 0.2 lost per day
      let zsp-release round (spn * 17.8)                         ;zoospore release rate at 23 degrees C (Woodhams et al., 2008; Briggs 2010 SI)
      let f-selfinfect round (0.05 * zsp-release)                ;fraction of the released zoospores that immediately self-infect the host
      set pz0 f-selfinfect ;pz0 + 1
      let same-patch-zsp round (0.4 * (zsp-release - f-selfinfect)) ;40% of zoospores in pool deposited into the patch the tadpole is currently on
      ask patch-here [
      set zsp zsp + same-patch-zsp
        ]
      let near-shallow-patch-zsp round (zsp-release - f-selfinfect - same-patch-zsp) ;10% of zoospores in pool deposited onto neighbor patch
      ask one-of patches in-radius 1 [
      set zsp zsp + near-shallow-patch-zsp
        ]
end

to max-bd-tadpole-shedding
      set spn spn - round ((0.19 + random-float 0.01) * spn)         ;Briggs 2010 uses 0.2 lost per day
      let zsp-release round (spn * 17.8)                             ;zoospore release rate at 23 degrees C (Woodhams et al., 2008; Briggs 2010 SI)

      let f-selfinfect round (0.05 * zsp-release)                   ;fraction of the released zoospores that immediately self-infect the host
                                                                    ;because these tadpoles have maxxed out their infection loads, these f-selfinfect zoospores do not actually establish pz0
                                                                    ;but they are prevented from re-entering the zoospore pool
  ;     set pz0 f-selfinfect ;pz0 + 1                               ;no self-reinfection since they are at their max so all self-reinfections fail
      let same-patch-zsp round (0.4 * (zsp-release - f-selfinfect)) ;40% of zoospores in pool deposited into the patch the tadpole is currently on
      ask patch-here [
      set zsp zsp + same-patch-zsp
        ]
      let near-shallow-patch-zsp round (zsp-release - f-selfinfect - same-patch-zsp) ;10% of zoospores in pool deposited onto neighbor patch
      ask one-of patches in-radius 1 [
      set zsp zsp + near-shallow-patch-zsp
        ]
end

to infection-step
    set nspn (sum [ spn ] of tadpoles-here + sum [ spn ] of metamorphs-here)                           ;count total number of zoosporangia on all infected frogs
    ;print nspn
    ;let z-mort 0.284 + random-float 0.5
    let z-mort 0.2698 + random-float 0.0284
    let frogs count tadpoles-here + count metamorphs-here
    let total-expo sum [expo] of turtles ;contact process (exposure) is dependent on the number of hosts - i.e. more hosts, more likely a zoospore will find one
    let p-left-in-water exp ( - (z-mort + total-expo) )
    let surviving-in_water round(zsp * p-left-in-water) ;deterministic how many survive in water
    let successful-infections round( zsp * (1 - p-left-in-water) * (sum [expo * est] of turtles) / (z-mort + total-expo)) ;assumed number of zoospores that have established
    set new-infections rnd:weighted-n-of-with-repeats successful-infections turtles [infprob]
   ; show new-infections ;new infections is a list, not an agentset, because it can contain the same agent multiple times within the list
   ;show rnd:weighted-n-of-with-repeats successful-infections turtles [infprob]
   ;show length new-infections
     ask turtles-here [
     ; show filter [i -> i = self] new-infections
      let my-new-infections filter [i -> i = self] new-infections ;subset new-infections list to create a list of only myself
      if length my-new-infections > 0[
      set new-pz0 length my-new-infections  ;the length of the subsetted list is how many new zoospores were established in this specific host
     ; show new-pz0
      set pz0 pz0 + new-pz0 ;add new infections to previous pz0
    ]
  ]
    set zsp surviving-in_water
end

to create-data-file
   if not file-exists? (word "results/no_Bd_baseline_bddynamics.csv") [
   ; file-open (word "results/baseline_bddynamics" date-and-time ".csv")
    file-open (word "results/no_Bd_baseline_bddynamics.csv")
    file-print "day, N_tadpoles, INF_tadpoles, dead_frogs, zspn, zsp, prevzsp, lambda, zspn-inc, N_metamorphs, INF_metamorphs"  ;dead_frogs includes all life stages (tadpoles, metas, frogs) dead with Bd but really only metas are dying of Bd
    file-close
    ]
end

to write-data-file
  file-open (word "results/no_Bd_baseline_bddynamics.csv")
  file-type ticks file-type "," file-type count tadpoles file-type "," file-type count tadpoles with [ bd = 1 ] file-type "," file-type bd-mortality file-type "," file-type n-zspn file-type ","file-type sum [ zsp ] of patches file-type "," file-type sum [ prev-zsp ] of patches file-type "," file-type lambda-zsp file-type "," file-type zspn-inc file-type "," file-type count metamorphs file-type "," file-type count metamorphs with [ bd = 1 ] file-type ","
  file-print ""
  file-close
end

to create-pond
  let nbpatches patches with [ border = 0 ]
  let counter 0
    ask one-of nbpatches [
      set pond 1
      set counter counter + 1
      set pcolor blue
      ;below lines specify the size of the pond (mean and variance)
      let pondsize round random-normal 135 18
      repeat pondsize [
        ask min-one-of patches with [ pond = 0 and border = 0 ] [ distance myself ] [
          set pond 1
          set pcolor blue
          ]
        ]
      ]
end

to calculate-shoreline
        if count neighbors4 with [ pond = 0 ] = 4 [
        set shoreline shoreline + 4
      ]
      if count neighbors4 with [ pond = 0] = 3 [
        set shoreline shoreline + 3
      ]
      if count neighbors4 with [ pond = 0] = 2 [
        set shoreline shoreline + 2
      ]
      if count neighbors4 with [ pond = 0] = 1 [
        set shoreline shoreline + 1
      ]
end
@#$#@#$#@
GRAPHICS-WINDOW
330
10
695
376
-1
-1
17.0
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
20
0
20
1
1
1
ticks
30.0

BUTTON
19
38
83
71
Setup
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
121
39
184
72
Go
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

MONITOR
703
14
760
59
day
ticks + 1
17
1
11

SLIDER
14
213
234
246
ini-tadpoles-per-pondpatch
ini-tadpoles-per-pondpatch
0
1000
500.0
10
1
NIL
HORIZONTAL

SLIDER
15
339
276
372
Bd-inf-tadpoles-per-infpondpatch
Bd-inf-tadpoles-per-infpondpatch
0
10
1.0
1
1
NIL
HORIZONTAL

MONITOR
702
66
844
111
Tadpole population size
count tadpoles
17
1
11

PLOT
700
118
934
277
Sporangia per infected tadpole
Sporangia/host
No. of hosts
0.0
9000.0
0.0
50.0
false
false
"" ""
PENS
"default" 100.0 1 -16777216 true "" "histogram [ spn ] of tadpoles with [bd = 1]"

MONITOR
947
276
1018
321
Zoospores
sum  [ zsp ] of patches with [ pond = 1 ]
17
1
11

MONITOR
937
119
1027
164
Total sporangia
sum [ spn ] of tadpoles with [ bd = 1 ]
17
1
11

MONITOR
851
65
967
110
Infected tadpoles
count tadpoles with [ bd = 1 ]
17
1
11

PLOT
1045
281
1245
431
Zoospore count
days
Zoospores
0.0
90.0
0.0
200000.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot sum [ zsp ] of patches"

SLIDER
15
410
187
443
v-coverage
v-coverage
0
1
0.0
0.1
1
NIL
HORIZONTAL

SLIDER
14
458
186
491
v-efficacy
v-efficacy
0
1
0.0
0.1
1
NIL
HORIZONTAL

MONITOR
764
14
892
59
NIL
round lambda-zsp
17
1
11

PLOT
1041
122
1241
272
per capita sporangia
day
mean number of sporangia
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"pen-1" 1.0 0 -13345367 true "" "if any? tadpoles [ plot ( sum [ spn ] of tadpoles) / count tadpoles ]"
"pen-2" 1.0 0 -8053223 true "" "plot (sum [ spn ] of metamorphs) / (count metamorphs + 1)"

SLIDER
15
290
187
323
inf-ponds
inf-ponds
0
1
1.0
1
1
NIL
HORIZONTAL

SWITCH
15
95
133
128
SimplePond
SimplePond
1
1
-1000

MONITOR
977
66
1138
111
Metamorph population size
count metamorphs
17
1
11

MONITOR
1158
66
1290
111
Infected metamorphs
count metamorphs with [ bd = 1 ]
17
1
11

MONITOR
941
171
1024
216
spn tadpoles
sum [ spn ] of tadpoles
17
1
11

MONITOR
944
223
1026
268
spn meta
round sum [ spn ] of metamorphs
17
1
11

SLIDER
14
173
186
206
birth_pulses
birth_pulses
1
3
1.0
1
1
NIL
HORIZONTAL

PLOT
701
284
936
449
Sporangia per infected metamorph
Sporangia/host
No. of hosts
0.0
4000.0
0.0
20.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [ spn ] of metamorphs with [bd = 1]"

PLOT
221
388
421
538
pz0 per metamorph
pz0 per meta
no. metamorphs
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [ pz0 ] of metamorphs"

PLOT
435
388
635
538
pz0 per tadpole
pz0 per tadpole
no. tadpoles
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [ pz0 ] of tadpoles"

PLOT
1049
439
1249
589
Zsp Count
day
zoospores
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"zsp" 1.0 0 -16777216 true "" "plot sum [ zsp ] of patches"

SLIDER
702
466
874
499
t-movement
t-movement
0
1
0.25
0.25
1
NIL
HORIZONTAL

SLIDER
703
511
875
544
m-land
m-land
0
1
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
702
556
874
589
meta-mort
meta-mort
0
0.09
0.02
0.01
1
NIL
HORIZONTAL

SLIDER
514
556
686
589
tad-mort
tad-mort
0
0.06
0.06
0.1
1
NIL
HORIZONTAL

MONITOR
885
506
995
551
% metas on land
count metamorphs with [on-land = 1] / count metamorphs
2
1
11

MONITOR
1073
11
1242
56
prop. of deaths due to Bd
bd-mortality / (bd-mortality + baseline-mortality)
2
1
11

MONITOR
899
10
1068
55
Number of perimeter patches
count patches with [pp = 1]
17
1
11

MONITOR
1245
124
1372
169
NIL
num-metamorphosis
17
1
11

MONITOR
1247
177
1339
222
NIL
bd-mortality
17
1
11

MONITOR
1246
227
1372
272
NIL
baseline-mortality
17
1
11

MONITOR
1256
286
1343
331
meta pop size
num-metamorphosis - (baseline-mortality + bd-mortality)
17
1
11

SLIDER
17
508
189
541
baseline_smax
baseline_smax
0
1000000
562.0
1
1
NIL
HORIZONTAL

SLIDER
14
137
186
170
last-day
last-day
0
250
90.0
1
1
NIL
HORIZONTAL

SLIDER
14
251
186
284
baseline_est
baseline_est
0
1
0.25
0.01
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

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

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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

frog top
true
0
Polygon -7500403 true true 146 18 135 30 119 42 105 90 90 150 105 195 135 225 165 225 195 195 210 150 195 90 180 41 165 30 155 18
Polygon -7500403 true true 91 176 67 148 70 121 66 119 61 133 59 111 53 111 52 131 47 115 42 120 46 146 55 187 80 237 106 269 116 268 114 214 131 222
Polygon -7500403 true true 185 62 234 84 223 51 226 48 234 61 235 38 240 38 243 60 252 46 255 49 244 95 188 92
Polygon -7500403 true true 115 62 66 84 77 51 74 48 66 61 65 38 60 38 57 60 48 46 45 49 56 95 112 92
Polygon -7500403 true true 200 186 233 148 230 121 234 119 239 133 241 111 247 111 248 131 253 115 258 120 254 146 245 187 220 237 194 269 184 268 186 214 169 222
Circle -16777216 true false 157 38 18
Circle -16777216 true false 125 38 18

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
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="tadpole_movement1" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="54"/>
    <metric>avg_spn_tadpoles</metric>
    <metric>var_spn_tadpoles</metric>
    <metric>aggregation_tadpoles</metric>
    <metric>prop_not_s_k</metric>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="t-movement" first="0" step="0.25" last="1"/>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="200"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="tadpole_movement3" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="54"/>
    <metric>var_spn_abun_tadpoles</metric>
    <metric>avg_spn_abun_tadpoles</metric>
    <metric>aggregation_abun_tadpoles</metric>
    <metric>prop_not_s_k_abun</metric>
    <metric>avg_spn_inten_tadpoles</metric>
    <metric>var_spn_inten_tadpoles</metric>
    <metric>aggregation_inten_tadpoles</metric>
    <metric>prop_not_s_k_inten</metric>
    <metric>tad_prev</metric>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="t-movement" first="0" step="0.25" last="1"/>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="200"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="meta_pop_size" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="90"/>
    <metric>count metamorphs</metric>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t-movement">
      <value value="0.25"/>
    </enumeratedValueSet>
    <steppedValueSet variable="meta-mort" first="0" step="0.02" last="0.09"/>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="200"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="tad_meta_mortality" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="90"/>
    <metric>count metamorphs</metric>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t-movement">
      <value value="0.25"/>
    </enumeratedValueSet>
    <steppedValueSet variable="meta-mort" first="0.03" step="0.03" last="0.09"/>
    <steppedValueSet variable="tad-mort" first="0.03" step="0.015" last="0.06"/>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="200"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="tadpole_movement4" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="54"/>
    <metric>var_spn_abun_tadpoles</metric>
    <metric>avg_spn_abun_tadpoles</metric>
    <metric>aggregation_abun_tadpoles</metric>
    <metric>prop_not_s_k_abun</metric>
    <metric>avg_spn_inten_tadpoles</metric>
    <metric>var_spn_inten_tadpoles</metric>
    <metric>aggregation_inten_tadpoles</metric>
    <metric>prop_not_s_k_inten</metric>
    <metric>tad_prev</metric>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="t-movement" first="0" step="0.25" last="1"/>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tad-mort">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="tadpole_movement3" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="54"/>
    <metric>var_spn_abun_tadpoles</metric>
    <metric>avg_spn_abun_tadpoles</metric>
    <metric>aggregation_abun_tadpoles</metric>
    <metric>prop_not_s_k_abun</metric>
    <metric>avg_spn_inten_tadpoles</metric>
    <metric>var_spn_inten_tadpoles</metric>
    <metric>aggregation_inten_tadpoles</metric>
    <metric>prop_not_s_k_inten</metric>
    <metric>tad_prev</metric>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="t-movement" first="0" step="0.25" last="1"/>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="200"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="meta_land_movement1" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="90"/>
    <metric>count metamorphs</metric>
    <metric>avg_spn_inten_metas</metric>
    <metric>var_spn_inten_metas</metric>
    <metric>aggregation_inten_metas</metric>
    <metric>metas_prev</metric>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t-movement">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="m-land" first="0" step="0.25" last="1"/>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="200"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="tadpole_movement5" repetitions="25" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="54"/>
    <metric>var_spn_abun_tadpoles</metric>
    <metric>avg_spn_abun_tadpoles</metric>
    <metric>aggregation_abun_tadpoles</metric>
    <metric>prop_not_s_k_abun</metric>
    <metric>avg_spn_inten_tadpoles</metric>
    <metric>var_spn_inten_tadpoles</metric>
    <metric>aggregation_inten_tadpoles</metric>
    <metric>prop_not_s_k_inten</metric>
    <metric>tad_prev</metric>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="t-movement" first="0" step="0.25" last="1"/>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tad-mort">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="birth_pulses" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="90"/>
    <metric>count metamorphs</metric>
    <metric>avg_spn_inten_metas</metric>
    <metric>var_spn_inten_metas</metric>
    <metric>aggregation_inten_metas</metric>
    <metric>metas_prev</metric>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t-movement">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tad-mort">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="meta-mort">
      <value value="0.04"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="birth_pulses" first="1" step="1" last="3"/>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="200"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="meta_pop_size_with_and_without_bd" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="90"/>
    <metric>count metamorphs</metric>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_smax">
      <value value="16000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t-movement">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="meta-mort">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tad-mort">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="last-day">
      <value value="150"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="birth_pulses2" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="90"/>
    <metric>count metamorphs</metric>
    <metric>avg_spn_inten_metas</metric>
    <metric>var_spn_inten_metas</metric>
    <metric>aggregation_inten_metas</metric>
    <metric>metas_prev</metric>
    <metric>bd-mortality / num-metamorphosis</metric>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t-movement">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tad-mort">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="meta-mort">
      <value value="0.04"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="birth_pulses" first="1" step="1" last="3"/>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="300"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="baseline_smax" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="90"/>
    <metric>count metamorphs</metric>
    <metric>bd-mortality / num-metamorphosis</metric>
    <metric>avg_spn_inten_metas</metric>
    <metric>metas_prev</metric>
    <enumeratedValueSet variable="baseline_smax">
      <value value="100"/>
      <value value="500"/>
      <value value="1000"/>
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t-movement">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="meta-mort">
      <value value="0.02"/>
      <value value="0.04"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tad-mort">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="noBd_meta_pop_size" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="90"/>
    <metric>count metamorphs</metric>
    <metric>avg_spn_inten_metas</metric>
    <metric>metas_prev</metric>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t-movement">
      <value value="0.25"/>
    </enumeratedValueSet>
    <steppedValueSet variable="meta-mort" first="0" step="0.02" last="0.1"/>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="200"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="diff_initial_meta_pop_size_with_without_Bd" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="90"/>
    <metric>count metamorphs</metric>
    <metric>avg_spn_inten_metas</metric>
    <metric>aggregation_inten_metas</metric>
    <metric>metas_prev</metric>
    <metric>prop_deaths_due_to_bd</metric>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t-movement">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="meta-mort">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_smax">
      <value value="16000"/>
    </enumeratedValueSet>
    <steppedValueSet variable="ini-tadpoles-per-pondpatch" first="200" step="200" last="800"/>
  </experiment>
  <experiment name="smax_diff_starting_pop" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="90"/>
    <metric>count metamorphs</metric>
    <metric>prop_deaths_due_to_bd</metric>
    <metric>avg_spn_inten_metas</metric>
    <metric>metas_prev</metric>
    <enumeratedValueSet variable="baseline_smax">
      <value value="100"/>
      <value value="500"/>
      <value value="1000"/>
      <value value="16000"/>
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t-movement">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="meta-mort">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tad-mort">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="400"/>
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="smax3" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="90"/>
    <metric>count metamorphs</metric>
    <metric>prop_deaths_due_to_bd</metric>
    <metric>avg_spn_inten_metas</metric>
    <metric>metas_prev</metric>
    <enumeratedValueSet variable="baseline_smax">
      <value value="1"/>
      <value value="1000"/>
      <value value="16000"/>
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t-movement">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="meta-mort">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tad-mort">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="smax_extended_time" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count metamorphs</metric>
    <metric>prop_deaths_due_to_bd</metric>
    <metric>avg_spn_inten_metas</metric>
    <metric>metas_prev</metric>
    <enumeratedValueSet variable="baseline_smax">
      <value value="1"/>
      <value value="1000"/>
      <value value="16000"/>
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="last-day">
      <value value="90"/>
      <value value="150"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t-movement">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="meta-mort">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tad-mort">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="est0.45_meta_pop_size_with_and_without_bd" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="90"/>
    <metric>count metamorphs</metric>
    <metric>prop_deaths_due_to_bd</metric>
    <metric>avg_spn_inten_metas</metric>
    <metric>metas_prev</metric>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_smax">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t-movement">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="meta-mort">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tad-mort">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="last-day">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_est">
      <value value="0.45"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="vary_est" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count metamorphs</metric>
    <metric>prop_deaths_due_to_bd</metric>
    <metric>avg_spn_inten_metas</metric>
    <metric>metas_prev</metric>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_smax">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_est">
      <value value="0"/>
      <value value="0.15"/>
      <value value="0.3"/>
      <value value="0.45"/>
      <value value="0.6"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="meta-mort">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tad-mort">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t-movement">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="last-day">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="562smax_meta_pop_size_with_and_without_bd2" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="90"/>
    <metric>count metamorphs</metric>
    <metric>prop_deaths_due_to_bd</metric>
    <metric>avg_spn_inten_metas</metric>
    <metric>metas_prev</metric>
    <metric>aggregation_inten_metas</metric>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_smax">
      <value value="562"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t-movement">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="meta-mort">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tad-mort">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="last-day">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_est">
      <value value="0.1"/>
      <value value="0.15"/>
      <value value="0.2"/>
      <value value="0.25"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
