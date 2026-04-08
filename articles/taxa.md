# Freshwater Macroinvertebrate Taxa Lists

## Intro

What taxa lists to use for identification?

According to
[WFD100](https://www.sniffer.org.uk/Handlers/Download.ashx?IDMF=e9b55f14-59cf-46b4-927f-66411e2e02d8)
document, there are five standardised taxa lists. In particular, Taxa
List 2 and 5 (TL2/TL5) have been widely adopted for reporting purposes.
TL2 is a family-level(ish) list used for routine WHPT/RICT reporting.
TL5 is used for species-level(ish) reporting including AWIC.

These taxa lists allow changes in the environment to be detected by
applying consistent levels of analysis - making changes between samples
directly comparable.

## Data Sources

The combine taxa table for freshwater macro-invertebrates table is
called INVERT-TAXON-DICTIONARY and is stored in the
[macroinverebrateMetrics R
package](https://github.com/aquaMetrics/macroinvertebrateMetrics/blob/master/inst/extdat/INVERT-TAXON-DICTIONARY.csv).
This table provides the basis for taxa lists used in analysis including
WFD100 reference lists. Subsets of this table are then provided to the
Labware based LIMS and ESRI’s Survey123 for data entry.

The INVERT-TAXON-DICTIONARY is a copy of previous table held in NEMS
(predecessor to LIMS). The R package is used when calculating metrics
for reporting purposes.

## Implementor’s Trap

We must implement taxonomy, keys, metrics and data sharing standards
that are independently produced by different organisations and a not
maintained and updated in sync.

In an ideal world, we would download a globally managed list of taxa,
identification keys, metrics and reporting tools which are tightly
coordinated with each other and allow sharing of data.

However, there are a number of independent sources of truth. These
include WFD100, National Biodiversity Network (NBN), Environmental
Change Network (ECN), metrics and identification keys.

To solve this implementation dilemma, we combine all sources of truth
into a taxa table (INVERT-TAXON-DICTIONARY), and co-ordinate updates to
keys, data entry, training, auditing and data-sharing as required. This
is repeated in similar way for other lists, for example diatoms,
macrophytes and marine lists.

#### Summary of macroinvertebrate taxa data flow

- (..\>) Dotted line with arrow shows irregular usage.
- (..) Dotted line with no arrow shows planned but not implemented .
- (-\>) Solid line with arrow shows data has previously or currently
  flows but majority of these connections are not automated.

## Metadata

The INVERT-TAXON-DICTIONARY is a single table with 49 columns. Covering
taxonomy, ECN and NBN identifiers, metric scores, taxa lists,
presentation and record-keeping. The columns in this table are described
below:

## Requirements

By filtering on the columns in INVERT-TAXON-DICTIONARY, we can filter
the table to provide taxa lists matching our requirements:

- Taxa lists should limit the level of analysis to keep reporting and
  training consistent.

- LIMS requires three taxa lists, TL2, TL5 (including LAMM) and TL5_ECN
  (including LAMM and ECN).

- LIMS requires a single column/list of unique taxa names for the user
  to pick from.

- Survey123 requires TL2 table with Taxon name, WHPT and BMWP scores.

- Auditing at the TL5 and TL2 must be possible (even if extra taxa are
  included).

- Lists for TL2 and TL5 must be available for external auditors for
  reference.

- The taxa names should match the names in the preferred identification
  keys.

- Taxa names should match with requirements of metric/modelling tools
  (in separate column if required).

- Taxa results should be share-able with the NBN gateway.

- Taxa results should be share-able with ECN.

- Include additional taxa to the TL2 list which are routinely
  identified.

- To calculate LAMM metric, additional taxa must also be added to the
  TL5 list.

- For sites in the ECN network, additional taxa are needed to be added
  to TL5 to match with ECN taxa list.

- Three target lists for analysis at TL2 (including extra SEPA taxa) and
  TL5 (including LAMM) and ECN levels are required for documentation
  providing a reference for analysts.

## Prepare Lists

Below we work through a number of steps to a create target lists for
various levels of analysis.

### TL5 including LAMM

Some duplicate LAMM taxa because LAMM list includes non-standard
groups/aggregates not included in WFD100 taxa lists.

|      | TAXON_NAME             | LAMM_TAXON                             |
|:-----|:-----------------------|:---------------------------------------|
| 2671 | Gammarus duebeni       | Gammarus pulex / Gammarus duebeni      |
| 2675 | Gammarus pulex         | Gammarus pulex / Gammarus duebeni      |
| 3902 | Agapetus delicatulus   | Agapetus sp. (excl. Agapetus ochripes) |
| 3903 | Agapetus fuscipes      | Agapetus sp. (excl. Agapetus ochripes) |
| 4174 | Mystacides longicornis | Mystacides longicornis/nigra           |
| 4175 | Mystacides nigra       | Mystacides longicornis/nigra           |

Combine LAMM and TL5

### TL5_ECN

What’s in TL5 but not in TL_ECN

- Mainly Beetle and Corixidae species

&nbsp;

      [1] "Physa acuta"                     "Oligochaeta"                    
      [3] "Nemurella picteti"               "Mesovelia furcata"              
      [5] "Hydrometra stagnorum"            "Velia"                          
      [7] "Gerris argentatus"               "Gerris lacustris"               
      [9] "Gerris odontogaster"             "Gerris thoracicus"              
     [11] "Nepa cinerea"                    "Ilyocoris cimicoides"           
     [13] "Aphelocheirus aestivalis"        "Notonecta glauca"               
     [15] "Notonecta maculata"              "Notonecta obliqua"              
     [17] "Micronecta"                      "Cymatia coleoptrata"            
     [19] "Callicorixa praeusta"            "Callicorixa wollastoni"         
     [21] "Corixa affinis"                  "Corixa dentipes"                
     [23] "Corixa panzeri"                  "Corixa punctata"                
     [25] "Hesperocorixa linnei"            "Hesperocorixa sahlbergi"        
     [27] "Sigara (Sigara)"                 "Sigara distincta"               
     [29] "Sigara falleni"                  "Sigara fossarum"                
     [31] "Sigara scotti"                   "Sigara lateralis"               
     [33] "Sigara nigrolineata"             "Sigara semistriata"             
     [35] "Sigara venusta"                  "Brychius elevatus"              
     [37] "Haliplus confinis"               "Haliplus flavicollis"           
     [39] "Haliplus fluviatilis"            "Haliplus heydeni"               
     [41] "Haliplus immaculatus"            "Haliplus laminatus"             
     [43] "Haliplus lineatocollis"          "Haliplus lineolatus"            
     [45] "Haliplus ruficollis"             "Haliplus wehnckei"              
     [47] "Noterus clavicornis"             "Laccophilus hyalinus"           
     [49] "Laccophilus minutus"             "Hyphydrus ovatus"               
     [51] "Hygrotus inaequalis"             "Hygrotus versicolor"            
     [53] "Hydroporus angustatus"           "Hydroporus discretus"           
     [55] "Hydroporus ferrugineus"          "Hydroporus memnonius"           
     [57] "Hydroporus nigrita"              "Hydroporus obscurus"            
     [59] "Hydroporus palustris"            "Hydroporus planus"              
     [61] "Hydroporus pubescens"            "Hydroporus tessellatus"         
     [63] "Stictonectes lepidus"            "Graptodytes pictus"             
     [65] "Porhydrus lineatus"              "Deronectes latus"               
     [67] "Nebrioporus assimilis"           "Nebrioporus depressus"          
     [69] "Stictotarsus duodecimpustulatus" "Oreodytes davisii"              
     [71] "Oreodytes sanmarkii"             "Oreodytes septentrionalis"      
     [73] "Scarodytes halensis"             "Platambus maculatus"            
     [75] "Agabus bipustulatus"             "Agabus chalconatus"             
     [77] "Agabus didymus"                  "Agabus guttatus"                
     [79] "Agabus paludosus"                "Agabus sturmii"                 
     [81] "Ilybius"                         "Colymbetes fuscus"              
     [83] "Acilius sulcatus"                "Dytiscus marginalis"            
     [85] "Dytiscus semisulcatus"           "Gyrinus aeratus"                
     [87] "Gyrinus distinctus"              "Gyrinus marinus"                
     [89] "Gyrinus urinator"                "Gyrinus natator group"          
     [91] "Orectochilus villosus"           "Helophorus aequalis"            
     [93] "Helophorus grandis"              "Helophorus arvernicus"          
     [95] "Helophorus brevipalpis"          "Helophorus flavipes"            
     [97] "Helophorus minutus"              "Helophorus obscurus"            
     [99] "Helophorus strigifrons"          "Paracymus scutellaris"          
    [101] "Hydrobius fuscipes"              "Anacaena bipustulata"           
    [103] "Anacaena globulus"               "Anacaena limbata"               
    [105] "Anacaena lutescens"              "Laccobius biguttatus"           
    [107] "Laccobius minutus"               "Laccobius atratus"              
    [109] "Laccobius atrocephalus"          "Laccobius sinuatus"             
    [111] "Laccobius striatulus"            "Enochrus testaceus"             
    [113] "Hydrochus angustatus"            "Ochthebius bicolon"             
    [115] "Ochthebius dilatatus"            "Ochthebius exsculptus"          
    [117] "Ochthebius minimus"              "Hydraena gracilis"              
    [119] "Hydraena nigrita"                "Hydraena pulchella"             
    [121] "Hydraena riparia"                "Hydraena rufipes"               
    [123] "Hydraena testacea"               "Limnebius nitidus"              
    [125] "Limnebius truncatellus"          "Elodes"                         
    [127] "Cyphon"                          "Prionocyphon serricornis"       
    [129] "Hydrocyphon deflexicollis"       "Pomatinus substriatus"          
    [131] "Dryops"                          "Tipulidae"                      
    [133] "Limoniidae"                      "Pediciidae"                     
    [135] "Athericidae"                     "Muscidae"                       
    [137] "Heptagenia lateralis"            "Gerris najas"                   
    [139] "Armiger crista"                 

What’s in ECN and not in TL5

- Mainly Oligochaeta families and Beetle families not found in TL5

&nbsp;

     [1] "Ferrissia wautieri"                      
     [2] "Ferrissia (Pettancylus) clessiniana"     
     [3] "Aeolosomatidae"                          
     [4] "Lumbriculidae"                           
     [5] "Haplotaxidae"                            
     [6] "Naididae"                                
     [7] "Tubificidae"                             
     [8] "Dikerogammarus villosus"                 
     [9] "Mesoveliidae"                            
    [10] "Hydrometridae"                           
    [11] "Veliidae"                                
    [12] "Gerridae"                                
    [13] "Nepidae"                                 
    [14] "Naucoridae"                              
    [15] "Aphelocheiridae"                         
    [16] "Notonectidae"                            
    [17] "Corixidae"                               
    [18] "Haliplidae"                              
    [19] "Noteridae"                               
    [20] "Dytiscidae"                              
    [21] "Gyrinidae"                               
    [22] "Helophoridae"                            
    [23] "Hydrophilidae"                           
    [24] "Hydrochidae"                             
    [25] "Hydraenidae"                             
    [26] "Scirtidae"                               
    [27] "Dryopidae"                               
    [28] "Tipula"                                  
    [29] "Eloeophila"                              
    [30] "Hexatoma"                                
    [31] "Pedicia"                                 
    [32] "Dicranota"                               
    [33] "Atherix"                                 
    [34] "Limnophora"                              
    [35] "Enchytraeidae (including Propappidae)"   
    [36] "Lumbricidae (including Glossoscolecidae)"
    [37] "Cordulegastridae"                        
    [38] "Planorbis crista"                        
    [39] "Nemurella pictetii"                      

### Note on TL4

TL4 is double the length of ECN list (658 Vs 317). TL4 is much more
resource intensive than ECN list. And…yes, unexpectedly TL4 is a larger
list than TL5. So in terms of number of taxa: TL1 \< TL2 \< TL3 \< TL4
\> TL5!

### What to use?

What we need is TL5 + ECN? For a ECN analysis which can be ‘downgraded’
to TL5 and used for AWIC/LAMM or other TL5 level metrics.

However, we need to take away family level options where genus/species
options are available in ECN/TL5, for instance:

- Remove Oligochaeta (TL5) as option and replace with sub families from
  ECN list (Naididae, Lumbriculidae etc)
- Add *Corixa affinis* (TL5) and remove Corixidae (ECN)

ECN families to keep:

    [1] "Aeolosomatidae"   "Lumbriculidae"    "Haplotaxidae"     "Naididae"        
    [5] "Tubificidae"      "Cordulegastridae"

TL5 families to keep

    [1] "Tipulidae"   "Limoniidae"  "Pediciidae"  "Athericidae" "Muscidae"   

### Not Active

Check column `active_data_entry` = FALSE. This column was used to filter
which taxa could be entered in NEMS. Even if particular taxa are part of
an official taxa list, there were sometimes reasons to prevent entry to
use a different (usually NBN preferred / most recent name used in
identification keys) named.

ECN taxa not active:

| TAXON_NAME                               |
|:-----------------------------------------|
| Enchytraeidae (including Propappidae)    |
| Lumbricidae (including Glossoscolecidae) |
| Nemurella pictetii                       |

Nemurella pictetii \> Should be Nemurella picteti - with one ‘i’. So use
correctly spelt version. This can be converted back to ‘pictetii’ for
ECN reporting if required.

Enchytraeidae (including Propappidae) & Lumbricidae (including
Glossoscolecidae) don’t have NBN codes which constrains data sharing,
additionally these superfamilies were not previously recorded in NEMS.
Suggest we include all separate families instead of superfamilies
i.e. Enchytraeidae, Propappidae, Lumbricidae, Glossoscolecidae. When
reporting for ECN we can re-combine into super families if required.

TL5 taxa not active:

| TAXON_NAME           |
|:---------------------|
| Heptagenia lateralis |
| Armiger crista       |

*Heptagenia lateralis* is not active, include *Electrogena lateralis*
instead, as preferred by NBN and used by ECN.

*Armiger crista* not used but this looks like a mistake, include
*Armiger crista*. *Gyraulus (Armiger) crista* is used by ECN. *Armiger
crista* can be converted to *Gyraulus (Armiger) crista* if required for
ECN reporting.

### AWIC taxa

Any AWIC taxa missing from TL5?

    [1] "Caenis"      "Protonemura" "Sialis"     

These missing genus are well covered at species level in TL5, so don’t
need to be added. Therefore the analyst should target these species
rather than the genus level:

     [1] "Caenis horaria"               "Caenis rivulorum"            
     [3] "Caenis robusta"               "Caenis pusilla"              
     [5] "Caenis luctuosa group"        "Caenis pseudorivulorum group"
     [7] "Protonemura meyeri"           "Protonemura montana"         
     [9] "Protonemura praecox"          "Sialis fuliginosa"           
    [11] "Sialis lutaria"               "Sialis nigripes"             

## Taxa Lists

Three completed lists for analysis.

### TL2 + extra taxa/sub-families

For routine ‘family’ level lab and bankside samples.

### TL5 + LAMM

For sampling points with AWIC or LAMM, core surveillance (and perhaps
other purposes).

### TL5 + LAMM + ECN

For sampling points with ECN purpose, a new analysis in LIMS with these
lists.
