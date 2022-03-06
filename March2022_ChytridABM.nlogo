;Chytridiomycosis ABM

globals
[
  zspn-inc                                       ;incidence of zoosporangia per day
  bd-mortality                                   ;number of frogs dying of Bd
  lambda-zsp                                     ;growth rate of zoospores
  n-zspn                                         ;sum of zoosporangium
  shoreline                                      ;shoreline perimeter
  k                                              ;max number of frogs
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

]

metamorphs-own
[
  aid                                            ;age in days
  bd                                              ;1 if infected, 0 if uninfected
  spn
  imm
  smax
  pz0
  pz1
  pz2
  pz3
  pz4
  pz5
  pz6
  pz7
  vac
]

tadpoles-own
[
  aid                                           ;age in days
  bd
  spn
  imm
  smax
  pz0
  pz1
  pz2
  pz3
  pz4
  pz5
  pz6
  pz7
  vac

]
;prezsp-own
;[
  ;host                                            ;who of host infected
  ;counter                                         ;7 days until sporangia develops
;]
;
to setup
  ca
  ifelse SimplePond = TRUE
  [ resize-world 0 3 0 3
    set-patch-size 80 ]
  [ set-patch-size 17     ;5
    resize-world 0 20 0 20 ]   ;0 99 0 99

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
  [ let counter 0
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
  ]
    ;below code specifies which patches are on the perimeter.
    ask patches with [ pond = 1 ] [
     if count neighbors4 with [ pond = 0 ] > 0 [
        set pp 1
        set pcolor 107
      ]
  ]
   ;reports the shoreline in the command center (if I want that)
    ask patches with [pp = 1][
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
    ]
   ; print shoreline  ;in case I want to know what the shoreline/perimeter is

    let perimeterp patches with [ pp = 1 ]
    ask perimeterp [
     initialize-tadpole-pop
    if inf-ponds = 1 [
      initialize-Bd-tadpoles
    ]
  ]


  ;#####Vaccination coverage 100%: all susceptible (imm > 66 to imm < 66 and imm > 33 to imm < 33)
;  ask frogs with [ imm > 66 and vac = 0 ] [
;    set imm random 67
;    set vac 1
;    ]
;  ask frogs with [ imm > 33 and imm < 67 and vac = 0 ] [
;    set imm random 34
;    set vac 1
;    ]
    ;]

;  if Bd-inf-frogs > 0 [
;    ask n-of Bd-inf-frogs frogs [
;      set bd 1
;      set color white
;      set spn 100
;    ]
;  ]

  ;if not file-exists? (word "results/baseline_bddynamics" date-and-time ".csv") [
  if not file-exists? (word "results/no_Bd_baseline_bddynamics.csv") [
   ; file-open (word "results/baseline_bddynamics" date-and-time ".csv")
    file-open (word "results/no_Bd_baseline_bddynamics.csv")
    file-print "day, N_tadpoles, INF_tadpoles, dead_frogs, zspn, zsp, prevzsp, lambda, zspn-inc, N_metamorphs, INF_metamorphs"  ;dead_frogs includes all life stages (tadpoles, metas, frogs) dead with Bd but really only metas are dying of Bd
    file-close
    ]
  reset-ticks
end

to go
  random-seed behaviorspace-run-number
  if ticks = 90 [ stop ]
  ask tadpoles [
    set aid aid + 1
    if random-float 1 < 0.06 [       ;tadpole daily mortality probability estimated from Govindarajulu 2006
      die
    ]
  ]
  ask metamorphs [
    set aid aid + 1
    if random-float 1 < 0.09 [
      die                                   ;#####14May21
    ]
  ]

  if any? tadpoles with [ aid > 55 ] [
    ask tadpoles with [ aid > 55 ] [
      if random-float 1 < 0.11 [
        hatch-metamorphs 1 [
          set aid 1
          set bd [ bd ] of myself
          if bd = 1 [
            set spn 100
          ]
          set smax (16000 + random 231)
          set shape "frog top"
          set size 0.4
          set color brown
          ]
        die
        ]
    ]
    ]
  if ticks = 75 [
    if any? tadpoles [
      ask tadpoles [
        hatch-metamorphs 1 [
          set aid 1
          set bd [ bd ] of myself
          if bd = 1 [
            set spn 100
          ]
          set smax (40000 + random 231)
          set shape "frog top"
          set size 0.4
          set color brown
          ]
        die
      ]
    ]
  ]

  if SimplePond = FALSE [
    let n-mobile-tadpoles round (0.10 * count tadpoles)
    ask n-of n-mobile-tadpoles tadpoles [
      let my-pond-id [ pondid ] of patch-here
      let nextpatch patches with [ pondid = my-pond-id and pp = 1 ]
      move-to one-of nextpatch ;patches with [ pondid = my-pond-id ]
      let dir-neighbor min-one-of patches with [ pond = 0 ] [ distance myself ]
      let face-dir-x [ pxcor ] of dir-neighbor
      let face-dir-y [ pycor ] of dir-neighbor
      facexy face-dir-x face-dir-y
      rt random 40
      fd (0.32 + random-float 0.13)
      ]
    ask metamorphs [
      move-to one-of patches with [ pond = 1  and pp = 1]
    ]
    ]
  let pondppatches patches with [ pond = 1 and pp = 1]
  ask pondppatches [
    set prev-zsp zsp
    set pcolor scale-color red zsp 1000000 0
    ]
  if ticks > 0 [
;    if not any? tadpoles [
;      stop
;      ]
    let bd-tadpoles tadpoles with [ bd = 1  and spn < 8000]
    ask bd-tadpoles
    [
      let z1 pz0
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

      ;if spn >= smax [
        ;set bd-mortality bd-mortality + 1
        ;die
        ;]
      ;if spn >= 8000 [
        ;set color red
        ;]
      if spn = 0 [
        set bd 0
        set color green
        ;stop
        ]
      set spn spn - round ((0.148 + random-float 0.006) * spn)   ;sporangia loss rate 0.148 – 0.153 per day
      let zsp-release round (spn * 17.8)                         ;zoospore release rate at 23 degrees C (Woodhams et al., 2008; Briggs 2010 SI)

      let f-selfinfect round (0.05 * zsp-release)                ;fraction of the released zoospores that immediately self-infect the host
      ;;#####                                                    ;simulating heterogenous susceptibility in frogs
;      let f-selfinfect 0
;      ifelse imm > 66
;      [ set f-selfinfect round (0.05 * zsp-release) ]                    ;fully susceptible
;      [ ifelse imm > 33
;        [ set f-selfinfect round (0.5 * 0.05 * zsp-release) ]
;        [ set f-selfinfect round (0.25 * 0.05 * zsp-release) ]
;      ]
      ;####
      ;let hostwho who
      ;hatch-prezsp f-selfinfect [
        ;ht
        ;set counter -7
        ;set host hostwho
        ;]
      set pz0 f-selfinfect ;pz0 + 1
      let same-patch-zsp round (0.4 * (zsp-release - f-selfinfect)) ;40% of zoospores in pool deposited into the patch the tadpole is currently on
;      ask patch-here [
;      set zsp zsp + (zsp-release - f-selfinfect)
;        ]
      ask patch-here [
      set zsp zsp + same-patch-zsp
        ]
      let near-shallow-patch-zsp round (0.1 * (zsp-release - f-selfinfect - same-patch-zsp)) ;10% of zoospores in pool deposited onto neighbor patch
      ask one-of patches in-radius 1 [
      set zsp zsp + near-shallow-patch-zsp
        ]
      let nearest-deep-patch-zsp zsp-release - f-selfinfect - same-patch-zsp - near-shallow-patch-zsp ;remaining approx 50% of zoospores deposited onto nearest deepest patch
      ;ask min-one-of patches [distance myself with [pond = 1 and pp = 0]] [
      ask min-one-of patches with [pond = 1 and pp = 0] [distance myself] [
      set zsp zsp + nearest-deep-patch-zsp
        ]
    ]
    let bd-metamorphs metamorphs with [ bd = 1 ]
    ask bd-metamorphs
    [                 ;upon establishment, a zoospore takes ~7 days to mature into a sporangia
      let z1 pz0
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

      if spn >= smax [
        set bd-mortality bd-mortality + 1
        die
        ]
      ;if spn >= 8000 [      ;metamorphs nearing bd-induced mortality appear red
        ;set color red
        ;]
      if spn = 0 [            ;uninfected metamorphs appear green
        set bd 0
        set color green
        ]
      set spn spn - round ((0.148 + random-float 0.006) * spn)   ;sporangia loss rate 0.148 – 0.153 per day
      let zsp-release round (spn * 17.8)                         ;zoospore release rate at 23 degrees C (Woodhams et al., 2008; Briggs 2010 SI)

      let f-selfinfect round (0.05 * zsp-release)                ;fraction of the released zoospores that immediately self-infect the host


      ;;#####                                                    ;simulating heterogenous susceptibility in frogs
;      let f-selfinfect 0
;      ifelse imm > 66
;      [ set f-selfinfect round (0.05 * zsp-release) ]                    ;fully susceptible
;      [ ifelse imm > 33
;        [ set f-selfinfect round (0.5 * 0.05 * zsp-release) ]
;        [ set f-selfinfect round (0.25 * 0.05 * zsp-release) ]
;      ]
      ;####
      ;let hostwho who
      ;hatch-prezsp f-selfinfect [
        ;ht
        ;set counter -7
        ;set host hostwho
        ;]


      set pz0 f-selfinfect ;pz0 + 1
      ask patch-here [
        set zsp zsp + (zsp-release - f-selfinfect)
        ]
      ]
    ]
;  let pondpatches patches with [ pond = 1 ]
;  ask pondpatches [
;    set prev-zsp zsp
;    ]
  ask pondppatches [
    set nspn (sum [ spn ] of tadpoles-here + sum [ spn ] of metamorphs-here)                           ;count total number of zoosporangia on all infected frogs
    ;print nspn
    if zsp > 0 [                                                  ;zoospore-decay  loss rate of zoospore from the zoospore pool 0.248 – 0.252
      let zsp-d round ((0.248 + random-float 0.005) * zsp)
      set zsp (zsp - zsp-d)
    ]
    ;set zsp (zsp + round (nspn * 17.8))                          ;zoospore release rate at 23 degrees C (Woodhams et al., 2008; Briggs 2010 SI) ;
    let prop-zsp round (0.05 * zsp)                               ;proportion of zoospores that encounter hosts   from Farthing et al., 2021; .001
    let prop-zsp-host round (0.5 * prop-zsp)                      ;proportion of zoospores successfully infect host after encounter; .0001
    repeat prop-zsp-host [
      if any? turtles-here [
        ask one-of turtles-here [
          set pz0 pz0 + 1
          set bd 1                                               ;simulating heterogenous susceptibility
          set color white
          ;####
;          ifelse imm > 66
;          [ set bd 1 set pz0 pz0 + 1 ]
;          [ ifelse imm > 33
;            [ if random-float 1 < 0.51 [
;              set bd 1 set pz0 pz0 + 1
;              ]
;            ]
;            [ if random-float 1 < 0.26 [
;              set bd 1 set pz0 pz0 + 1
;              ]
;              ]
;            ]
          ;###
;          let hostwho who
;          hatch-prezsp 1 [
;            ht
;            set counter -7
;            set host hostwho
;            ]
          ]
        ]
    ]
    set zsp (zsp - prop-zsp)
    ]

;  ask prezsp [
;    set counter counter + 1
;    if counter = 0 [
;      let hostid host
;      if any? frogs with [ who = hostid ] [
;        ask frog hostid [
;          set bd 1
;          set color white
;          set spn spn + 1
;          set zspn-inc zspn-inc + 1                            ;update incidence counter
;          ]
;      ]
;      die
;    ]
;  ]

  if ticks > 1 [
    if sum [ prev-zsp ] of patches > 0 [
      set lambda-zsp precision (sum [ zsp ] of patches / sum [ prev-zsp ] of patches) 2
      ]
    ]
  set n-zspn sum [ spn ] of tadpoles
  file-open (word "results/no_Bd_baseline_bddynamics.csv")
  ;file-open (word "results/baseline_bddynamics" date-and-time ".csv")
  file-type ticks file-type "," file-type count tadpoles file-type "," file-type count tadpoles with [ bd = 1 ] file-type "," file-type bd-mortality file-type "," file-type n-zspn file-type ","file-type sum [ zsp ] of patches file-type "," file-type sum [ prev-zsp ] of patches file-type "," file-type lambda-zsp file-type "," file-type zspn-inc file-type "," file-type count metamorphs file-type "," file-type count metamorphs with [ bd = 1 ] file-type ","
  file-print ""
  file-close
  set zspn-inc 0
  tick
end

to initialize-tadpole-pop
  sprout-tadpoles ini-tadpoles-per-pondpatch [
    set shape "frog top"
    set size 0.2
    set imm random 100
    set smax (9890 + random 231)
    set color 65
    let dir-neighbor min-one-of patches with [ pond = 0 ] [ distance myself ]
    let face-dir-x [ pxcor ] of dir-neighbor
    let face-dir-y [ pycor ] of dir-neighbor
    facexy face-dir-x face-dir-y
    rt random 40
    ;rt random 270  ;patch-at-heading-and-distance -90 1
    fd (0.32 + random-float 0.13) ;(0.25 + random-float 0.19)
    ]
end


to initialize-Bd-tadpoles
  if Bd-inf-tadpoles-per-infpond > 0 [
    ask n-of Bd-inf-tadpoles-per-infpond tadpoles-here [
      set bd 1
      set color white
      set spn 100
      ]
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
0
0
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
1006
17
1063
62
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
100000
100.0
100
1
NIL
HORIZONTAL

SLIDER
15
339
211
372
Bd-inf-tadpoles-per-infpond
Bd-inf-tadpoles-per-infpond
0
10
1.0
1
1
NIL
HORIZONTAL

MONITOR
944
73
1086
118
Tadpole population size
count tadpoles
17
1
11

PLOT
942
136
1176
295
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
"default" 100.0 1 -16777216 true "" "histogram [ spn ] of tadpoles"

MONITOR
1158
351
1229
396
Zoospores
sum  [ zsp ] of patches with [ pond = 1 ]
17
1
11

MONITOR
1179
137
1269
182
Total sporangia
sum [ spn ] of tadpoles with [ bd = 1 ]
17
1
11

MONITOR
1076
73
1192
118
Infected tadpoles
count tadpoles with [ bd = 1 ]
17
1
11

PLOT
936
313
1136
463
Zoospore count
days
Zoospores
0.0
10.0
0.0
10.0
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
1325
364
1400
409
NIL
lambda-zsp
17
1
11

PLOT
1283
140
1483
290
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
1219
73
1380
118
Metamorph population size
count metamorphs
17
1
11

MONITOR
1400
73
1532
118
Infected metamorphs
count metamorphs with [ bd = 1 ]
17
1
11

MONITOR
1183
189
1266
234
spn tadpoles
sum [ spn ] of tadpoles
17
1
11

MONITOR
1186
241
1268
286
spn meta
sum [ spn ] of metamorphs
17
1
11

MONITOR
1184
454
1255
499
NIL
shoreline
17
1
11

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
  <experiment name="baseline_100ite" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="frogs-per-pond">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-frogs-per-infpond">
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
    <enumeratedValueSet variable="nponds">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="het_sus_ite100" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="frogs-per-pond">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-frogs-per-infpond">
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
    <enumeratedValueSet variable="nponds">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="vacc100_100ite" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="frogs-per-pond">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-frogs-per-infpond">
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
    <enumeratedValueSet variable="nponds">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="vacccov50%" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="frogs-per-pond">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-frogs-per-infpond">
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
    <enumeratedValueSet variable="nponds">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="hetsus_100ite" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="frogs-per-pond">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-frogs-per-infpond">
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
    <enumeratedValueSet variable="nponds">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="bl_100ite" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="frogs-per-pond">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-frogs-per-infpond">
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
    <enumeratedValueSet variable="nponds">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="baseline_practice_experiment" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpond">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
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
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="1001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nponds">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="baseline_experiment3" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks</metric>
    <metric>count tadpoles</metric>
    <metric>count tadpoles with [ bd = 1 ]</metric>
    <metric>bd-mortality</metric>
    <metric>n-zspn</metric>
    <metric>sum [ zsp ] of patches</metric>
    <metric>sum [ prev-zsp ] of patches</metric>
    <metric>lambda-zsp</metric>
    <metric>zspn-inc</metric>
    <metric>count metamorphs</metric>
    <metric>count metamorphs with [ bd = 1 ]</metric>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpond">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
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
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="1001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nponds">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpond">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
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
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="1001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nponds">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpond">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="1001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nponds">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="no_Bd_baseline" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpond">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="1001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nponds">
      <value value="1"/>
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
