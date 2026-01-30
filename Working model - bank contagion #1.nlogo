; Proposal: to determine the bank size based on one of the following:
; -turnover (Cifra afeceri)
; -credits volume
; Basically, public exposed data related to each bank in the analysis, depending on an xls file.

; bank X connects to bank Y & bank Z, each of them having their own size.
; Proposal: weight the links between bankX -> bankY && bankX -> bankZ depending on bankY, bankZ size.
; Eg: bankY size=4, bankZ size=6; bankX has 10 interbank assets => should have 4 to bankY, 6 to bankZ, as more assets should be loaned to bankZ, as bankY has a lower capacity of maybe returning the money back.

extensions [
  nw
  table
  array
]

directed-link-breed [ directed-edges directed-edge ]
undirected-link-breed [ undirected-edges undirected-edge ]

links-own [
  weight         ; Valoarea imprumutului dintre doua banci (agenti)
  link-loan-type ; Short/Long term
  link-interest-rate  ; Rata dobanzii
  is-sellable
]

globals [
  discount-rate
  deposit-withdrawal-rate
  visited-default-banks
  already-default-banks
  buffer
  banks-max-size
  banks-min-size
  started-contagion-liquid-assets
  started-contagion-interbank-assets
  started-contagion-illiquid-assets
  started-contagion-interbank-liabilities
  started-contagion-total-deposits
  started-contagion-bank-size
  rate-of-SME ; Ce procent din totalul depozitelor este din partea SME-urilor. Ipoteza: SME-urile au depozite >100k ; rate-of-sme -> volumul acestor depozite
  rate-of-large-companies ; Ce procent din totalul depozitelor este din partea 'large-companies'. Ipoteza: large-companies au depozite >100k
  ;;Deposits - rate-of-sme * deposits - rate-of-large-companies * deposits = x. Aceste depozite 'x' constituie depozite <100k (ex: 300 depozite <100k)
  ;;Depozitele <100k nu pot fi folosite pentru mecanisme de 'salvare' a bancii (bail-in)
  ;;;bail-in din partea altor banci = se incearca; daca nu, se merge pe nivelurile urmatoare.
  ;;;bail-in lv1 = se incearca bail-in folosind depozitele celor din 'large-companies'
  ;;;bail-in lv2 = se incearca bail-in folosind depozitele celor din SME
  ;;;bail-in lv3 = se incearca bail-in folosind toate depozitele, indiferent ca sunt >100k sau <100k, sau ca vin de la micro, SME, large-companies
  ;;;bailout     = interventia guvernamentala, folosind taxele colectate
  possible-loan-types ; only short-term and long-term allowed
  computation-precision ; how many decimals are after the floating point
  default-max-banks-reached ; how many banks entered the default; if all of them are in default state, stop the execution of the program
  fund-resolution-budget ; Bugetul fondului de rezolutie -> va fi suma dintre 1% din totalul depozitelor ASIGURATE ale fiecarei banci

  ;; Stari ale bancilor, in functie de bilantul acestora - bun, criza de lichiditate sau default.
  STATE-HEALTHY
  STATE-LIQUIDITY-CRISIS
  STATE-DEFAULT
]

;; se va incerca vinderea creditelor acordate, pentru cresterea lichiditatii - in situatie de liquidity-crisis
;; in situatie de solvency-crisis
;; bail-in lv1
;; bail-in lv2
;; bailouut

turtles-own [
  state
  has-started-contagion ; Exprima daca din cauza agentului curent a inceput contagiunea financiara
  will-be-in-default ; Exprima daca banca va intra in stare default in cadrul urmatoarelor tickuri. Daca da, o excludem din partea de imprumuturi etc. (la 'go')
  interbank-assets ; Activele interbancare
  illiquid-assets ; Active bancare cu lichidate redusa
  liquid-assets   ; Active bancare lichide
  interbank-liabilities ; Pasive interbancare
  sme-uninsured-deposits-volume ; Volume of deposits >= 100k, for SMEs, that is not insured (subpart of deposits)
  large-companies-uninsured-deposits-volume ; Volume of deposits >= 100k, for large companies (1st for bail-in), that is not insured (subpart of deposits)
  insured-deposits ; Volume of deposits < 100k, that are insured and cannot be used for bail-in. (subpart of deposits)
  total-deposits ; Depozite bancare
  equity ; Capitalul bancii
  bank-size ; Dimensiunea bancii, luand in considerare anumiti indicatori financiari, precum CA
  reached-max-possible-connectivity ; A/F, daca conectivitatea bancii a fost atinsa
  max-node-connectivity ; Conectivitatea maxima a unei banci
  total-links ; Totalul conexiunilor cu alte banci
  interest-rate-map ; Rata dobanzii in functie de tipul dobanzii, termen scurt / lung
  revenue-from-interest-rate ; Venitul potential generat din dobanzi
  liability-from-interest-rate ; Costurile potentiale generate din dobanzile altor agenti
]

;;Fn setup - initializarea modelului;;
to setup
  set discount-rate (buyer-discount-rate / 100)
  clear-all
  reset-ticks
  ask patches [ set pcolor black ]
  setup-globals
  setup-bank-nonfinancial-states
  setup-bank-distribution
  setup-network
  setup-bank-financial-states

  ask directed-edges [
    set shape "curved"
  ]

  ;; Initializarea fondului de rezolutie dupa ce fiecare banca a fost initializata, pentru a determina bugetul asigurat de suma de 1% din total depozite asigurate
  ;; Conform Legii 312/2015, aceasata suma nu poate depasi 1% din suma depozitelor asigurate
  set fund-resolution-budget (four-decimal (0.01 * sum [insured-deposits] of turtles))
end

;;Fn setup-globals - initializarea var. globale;
to setup-globals
  set possible-loan-types ["short-term" "long-term"]
  set computation-precision 4
  set visited-default-banks []
  set already-default-banks []
  set rate-of-SME .1
  set rate-of-large-companies .01
  set discount-rate (buyer-discount-rate / 100)
  set deposit-withdrawal-rate (deposits-withdrawal-rate / 100)
  set STATE-HEALTHY "HEALTHY"
  set STATE-LIQUIDITY-CRISIS "LIQUIDITY-CRISIS"
  set STATE-DEFAULT "DEFAULT"
end

to setup-bank-nonfinancial-states
  set default-max-banks-reached false
  create-turtles number-of-banks [ set shape "house" ]

  layout-circle turtles (max-pxcor - 1)

  ask turtles [
    set state STATE-HEALTHY
    set has-started-contagion false
    set will-be-in-default false
    set reached-max-possible-connectivity false
    set max-node-connectivity max-connectivity-node-may-have
    set interest-rate-map table:make
    foreach possible-loan-types [
      [loanType] ->
      ifelse loanType = "short-term"[
        table:put interest-rate-map loanType four-decimal ( (1.01 + (precision random-float 0.01 4)) ); interest-rate btw (1%-2%) 4 means it has .xxxx decimal points => random between 0 - 0.01 with precision of 4
      ][
        if loanType = "long-term" [
          table:put interest-rate-map loanType four-decimal ( (1.03 + (precision random-float 0.01 4))) ; interest-rate btw (3%-4%) 4 means it has .xxxx decimal points => random between 0 - 0.01 with precision of 4
        ]
      ]
    ]


  ]

  nw:set-context turtles directed-edges
end

to update-total-links [b]
  ask b [ set total-links (total-links + 1) ]
end

; Set-up network starting from the biggest banks.
; Create randomized directions "to", "from"
to setup-network
  let desc-sorted-b get-turtles-sorted-by-banksize-desc
  print (word "###### NETWORK SETUP START ######")
  foreach desc-sorted-b [ curr-bank ->
    ask curr-bank [
      print ("")
      print (word "START setup for " self " with bank-size: " [bank-size] of self)
      let possible-turtles-to-connect other turtles with [total-links < max-node-connectivity]
      print (word "   Possible nodes it can connect to: " possible-turtles-to-connect)
      ; Check against max. connectivity && if there are others that can be connected
      while [ (total-links < max-node-connectivity) and any? possible-turtles-to-connect ] [
        ask one-of possible-turtles-to-connect [
          let connect-turtle self

          ;; Ratio este utilizabil doar in situatia determinarii probabilitatii cu care link A->B poate exista (daca nu, A<-B)
          let size-ratio ([bank-size] of curr-bank / banks-max-size)

          ;; Intrucat bancile mari (core banks) dispun de un volum mai mare de bani,
             ;; pentru a evita scenarii in care exista doar A(mare)->B(oarecare) in situatia bancilor mari, probabilitatea va fi de 80% sa aiba linkuri A->B.
          ;; La fel si pentru bancile mici, care ar trebui sa contracteze imprumuturi cu o frecventa mai mare decat bancile mari (80% vs 20%)
          ;; Cea mai mica banca are 20% sansa de link A->B
          ;; Cea mai mare banca are 80% sansa de link A->B
          ;; Frecventa cu care 'Core banks' imprumute 'smaller banks' este o situatie apropiata de realitate, decat vice versa ('small banks' sa imprumute mai des 'core banks')
          let lending-probability (0.2 + (size-ratio * 0.6))

          ifelse (random-float 1.0 < lending-probability)[
            ; bank -> connect-turtle
            print (word "    Create " curr-bank " -> " connect-turtle " link")
            ask curr-bank [ create-directed-edge-to connect-turtle ]
          ][
            ; bank <- connect-turtle
            print (word "    Create " curr-bank " <- " connect-turtle " link")
            ask curr-bank [ create-directed-edge-from connect-turtle]
          ]

          update-total-links connect-turtle
          set possible-turtles-to-connect (possible-turtles-to-connect with [self != connect-turtle])
          print(word "    Updated agentset to: " possible-turtles-to-connect)
        ]
        update-total-links curr-bank
      ]
    ]
  ]
  print (word "###### NETWORK END ######")
end

; Sort turtles DESC by bank-size to prepare them for the network prio setup
to-report get-turtles-sorted-by-banksize-desc
  let sorted-list-of-turtles sort-by [[t1 t2] -> [bank-size] of t1 > [bank-size] of t2] turtles
  report sorted-list-of-turtles
end

to setup-bank-distribution
  set banks-max-size 0
  set banks-min-size 99
  ask turtles [
    set color blue
    set size 2
    ; Banks have different sizes, based on log-normal distribution
    ; with mean and standard deviation provided by the user
    ; * 100 only to avoid values such as 1.33, 0.53 etc.
    let raw-number (abs(exp(random-normal mu sigma)) * 100)
    set bank-size round raw-number
    set label bank-size

    if banks-max-size < bank-size
    [set banks-max-size bank-size]

    if banks-min-size > bank-size
    [set banks-min-size bank-size]
  ]
end

to setup-bank-financial-states
  print (word "\n###### FINANCIAL-STATES START ######")
  ask turtles [
    print ("")
    print (word "START setup for " self)

    ;;;;;; Initializarea pasivelor, avand in vedere cate imprumuturi a contractat banca curenta
    ;; self <- bank
    let number-of-ins count (my-in-links)
    set interbank-liabilities (four-decimal sum [weight] of my-in-links)

    ;; Calcularea totalului de pasive (total-deposits + interbank-liabilities)
    ;; Cum pasivele tin de dimensiunea bancii, daca aceasta este prea mica, setam depozitele cu '0' pentru mentinerea ecuatiei active=pasive
    let total-liabilities four-decimal (max (list bank-size interbank-liabilities))
    set total-deposits four-decimal (max (list 0 (total-liabilities - interbank-liabilities)))

    ;; Calcularea capitalului bancii si necesarul de active pentru mentinerea active=pasive
    ;; Bazat pe Basel III - Capital Adequancy Ratio (8%) => 8% din active trebuie sa fie capitalul
    let capital-adequancy-ratio (8 / 100)
    let target-total-assets (four-decimal (total-liabilities / (1 - capital-adequancy-ratio)))

    ;; Initializam capitalul bancii astfel incat total active (interbank-asset + liquid + illiquid) = total pasive (interbank-liabilities + total-deposits + equity)
    set equity (four-decimal (target-total-assets - total-liabilities))

    ;; Initializarea activelor, avand in vedere cate imprumuturi a acordat banca curenta.
    ;; self -> bank
    let number-of-outs count (my-out-links)
    ifelse number-of-outs = 0 [
      set liquid-assets (four-decimal (0.35 * target-total-assets))
      set interbank-assets 0
    ]
    [
      set liquid-assets (four-decimal (0.35 * target-total-assets))
      set interbank-assets (four-decimal (0.20 * target-total-assets))
    ]
    ;; Activele nelichide se vor initializa cu remainder-ul dintre pasive totale - active curente => active nelichide (pentru a mentine active totale=pasive totale).
    ;; Practic, valoarea acestui activ va fi 65% din target-total, sau 45% din target-total
    set illiquid-assets four-decimal ( (equity + total-deposits + interbank-liabilities) - (liquid-assets + interbank-assets) )

    set sme-uninsured-deposits-volume (four-decimal (rate-of-SME * total-deposits))
    set large-companies-uninsured-deposits-volume (four-decimal (rate-of-large-companies * total-deposits))
    set insured-deposits (four-decimal (total-deposits * (1 - (rate-of-SME + rate-of-large-companies))))
    let asset-minus-liabilities (four-decimal ( (illiquid-assets + liquid-assets + interbank-assets) - (interbank-liabilities + total-deposits + equity) ))
    print (word "    Assets - Liabilities: " asset-minus-liabilities)
    print (word "    Iliquid assets: " illiquid-assets)
    print (word "    Liquid assets: " liquid-assets)
    print (word "    Interbank assets: " interbank-assets)
    print (word "    Interbank liabilities: " interbank-liabilities)
    print (word "    Interbank liabilities: " interbank-liabilities)
    print (word "    Sum of my-in-links: " sum [weight] of my-in-links)
    print (word "    Equity: " equity)
    print (word "    Total deposits: " total-deposits)
    print (word "       from which insured: " insured-deposits)
    print (word "       from which SME uninsured: " sme-uninsured-deposits-volume)
    print (word "       from which large-companies uninsured: " large-companies-uninsured-deposits-volume)
    distribute-interbank-assets self
  ]
end

to-report four-decimal [ n ] ; "c" for clean
  report precision n 2
end

; Sets up financial state of each bank. If bank has no links,
; then there are no interbank claims, and everything is
; determined by total-deposits and mortages (illiquid assets).
; Otherwise, make interbank assets 20% of total and distribute
; evenly among randomly generated links (interbank liabilities
; determined endogenously, one bank's asset is another's liability)
to setup-financial-states
  setup-bank-financial-states
end

to distribute-interbank-assets [currentTurtle]
  ask currentTurtle[

    let number-of-outs count (my-out-links)
    let connected-turtles turtle-set [end2] of my-out-links
    let sumWeightsOfConnectedTo sum [bank-size] of connected-turtles

    print(word "      Links-to: " connected-turtles)
    print(word "      Total bank- of banks borrowed: " sumWeightsOfConnectedTo)

    ask my-out-links [
      set is-sellable true

      let connected-turtle [end2] of self ; Get the other turtle the currentTurtle is connected-to in the context of an outgoing directed link
      let loanType one-of possible-loan-types ; randomly choose one loanType from the global initialized list
      print(word "Type of loan: " loanType " to " connected-turtle)

      let howMuchToBorrow four-decimal ( ([bank-size] of connected-turtle * [interbank-assets] of currentTurtle) / sumWeightsOfConnectedTo )
      print (word "Borrowing " howMuchToBorrow " to " connected-turtle)
      set weight howMuchToBorrow

      ask self [
        set weight howMuchToBorrow
        set link-loan-type one-of possible-loan-types
        set link-interest-rate four-decimal (get-interest-rate currentTurtle link-loan-type)
      ]
    ]
  ]
end

to push-to-sell-loans-list [default-bank]
  if not member? default-bank already-default-banks [
    set already-default-banks lput default-bank already-default-banks
  ]
end

;;Fn ce reduce activele interbancare ale altor banci, impactate de catre cea default, in functie de 'suma pe care cea default a contractat-o'
to reduce-interbankassets-of-borrower [default-bank non-default-bank]
  ask non-default-bank [
    let lossWeight [weight] of link-with default-bank
    let initial-interbank-assets interbank-assets
    let updated-with-loss-interbank-assets 0
    ifelse (interbank-assets > lossWeight)[
      set updated-with-loss-interbank-assets (interbank-assets - lossWeight)
    ][
      set updated-with-loss-interbank-assets 0
    ]
    print (word "   NEXT checked: Bank " non-default-bank " reduces its interbank-assets: " initial-interbank-assets " -> " updated-with-loss-interbank-assets)

    set interbank-assets updated-with-loss-interbank-assets
  ]
end

;;Fn ce reduce activele interbancare ale bancii afectate de una default. Practic, in aceasta fn, scadem toate imprumuturile acordate catre banci default - aceste imp. nu mai sunt recuperabile.
to cut-interbankassets-if-lent-towards-default [affected-bank]
  print ("    Reducing the interbank-assets && equity of the affected one depending on the amount lent to defaulted")
  ask affected-bank [
    let initial-interbank-assets interbank-assets
    let initial-equity equity

    ;; Identificam linkurile catre banci default   affected -> default. De asemenea, le marcam ca nefiind bune pentru vanzare + cu rosu
    let toxic-out-links my-out-links with [ [state] of end2 = STATE-DEFAULT ]

    ;; Identificarea bancilor default care au imprumutat bani de la cea afectata; Marcam cu rosu.
    let defaulted-debtor-banks (turtle-set [end2] of toxic-out-links)
    ask defaulted-debtor-banks [
      mark-link-to-default-bank-as-unsellable self
    ]

    let total-asset-loss (four-decimal (sum [weight] of toxic-out-links))

    let updated-with-loss-interbank-assets 0
    if (interbank-assets > total-asset-loss)[
      set updated-with-loss-interbank-assets (four-decimal (interbank-assets - total-asset-loss))
    ]

    set equity four-decimal (equity - total-asset-loss)
    set interbank-assets updated-with-loss-interbank-assets
    print (word "     Interbank-assets: " initial-interbank-assets " -> " updated-with-loss-interbank-assets " | Equity: " initial-equity " -> " equity)
  ]
end

; A default; A -> B
; Mark only if link was not visited already
to color-dbank-out-links [d-bank]
  print (word " #################### MARKING WITH YELLOW OUTGOING FROM: " d-bank)
  ask [my-out-links] of d-bank [
    if color != red [set color yellow] ; mark the fact that the links with the banks that have been borrowed has been affected; The curr.-def.-turtle's lend will be sold to another bank
  ]
end

; A default; A <- B
to color-dbank-in-links [d-bank]
  ask [my-in-links] of d-bank [
    if color != yellow [set color red]
  ]
end

; Check if non-d-bank enters default if  non-d-bank -> d-bank.
to check-if-defaults-other [non-d-bank]
  ask non-d-bank [

    ; Check if there is any bank that borrowed non-d; If so, try to bail-in using them.     non-d-bank <- B
    let banks-that-borrowed-me in-link-neighbors
    print ("")
    if color = blue [
      print(word "                Contaminated bank: " self)

      ifelse (is-under-default-risk non-d-bank)[
        print ("Trying to save the bank using the regulatory mechanisms.")
        try-to-cascade-mitigate-default non-d-bank

        ; Check again if under the risk of being defaulted. If not, mark it.
        if (is-under-default-risk non-d-bank = false) [
          print ("Bank was saved using the regulatory mechanisms.")
          set color orange
        ]
      ][
        print("Bank was affected, but not defaulted!")
      ]

      if (is-under-default-risk non-d-bank)[
        print("Bank will be in default!")
        set will-be-in-default true
        set already-default-banks lput self already-default-banks
      ]
    ]
  ]
end

;;Fn helper ce calculeaza net-worth-ul bancii, facand abstractie de capitalurile proprii (equity).
to-report compute-net-worth [bank]
  let net-worth 0
  ask bank[
    set net-worth (four-decimal (interbank-assets + illiquid-assets + liquid-assets - interbank-liabilities - total-deposits))
  ]
  report net-worth
end

;;Fn helper ce descrie procesul de bail-in / bail-out
;; 1st tier bail-in - bancile care m-au imprumutat vor fi afectate
;; 2nd tier bail-in - depozitele curente vor fi afectate
;; 3rd tier bail-in - fondul de garantare al depozitelor bancare.
to try-to-cascade-mitigate-default [potential-default-bank]
  ask potential-default-bank [
    let borrowers-bail-in true
    let max-amount-covered-by-res-funds 0.05 * (total-deposits + interbank-liabilities)

    let amount-still-required-to-save 0

    if (is-under-default-risk potential-default-bank)[

      if (compute-net-worth potential-default-bank < 0)[

        let current-net-worth (four-decimal (compute-net-worth potential-default-bank))

        let required-bail-in-rate 0
        set amount-still-required-to-save (four-decimal abs(current-net-worth))

        ;; Pentru a nu scadea mai mult decat valoarea arcului, de aceea este necesara aceasta verificare.
        ;; Rata cu care fiecare arc va scadea, in functie de suma necesara. Ne vom raporta doar la interbank-liabilities, intrucat bancile care m-au imprumutat se raporteaza doar la acest param.
        ifelse (amount-still-required-to-save > interbank-liabilities)
        [ set required-bail-in-rate 1 ]
        [ set required-bail-in-rate (four-decimal (amount-still-required-to-save / interbank-liabilities)) ]

        ;; Aplicam primul mecanism anti-default. Bancile care m-au imprumutat vor face bail-in. Se actualizeaza suma care inca este necesara pentru acoperire.
        print (word "       Proceeding to step 1 of bail-in - creditors. Required amount from creditors: " amount-still-required-to-save)
        creditors-bail-in potential-default-bank required-bail-in-rate
        set amount-still-required-to-save (four-decimal abs(interbank-assets + illiquid-assets + liquid-assets - interbank-liabilities - total-deposits) )
        set current-net-worth (four-decimal (compute-net-worth potential-default-bank))

        ;; Daca inca este in risc de default dupa prima primul bail-in, continuam cu al2lea
        if (is-under-default-risk potential-default-bank)[
          print (word "     Proceeding to step 2 of bail-in - deposits. Required amount from uninsured deposits: " amount-still-required-to-save)
          deposits-bail-in potential-default-bank
          set current-net-worth (four-decimal (compute-net-worth potential-default-bank))
          set amount-still-required-to-save (four-decimal abs(current-net-worth) )
        ]

        ;; Daca inca este in risc de default dupa 1) creditors bail-in 2) deposists-bail-in, continuam cu fondul de rezolutie al 'Fondului de Garantare a Depozitelor Bancare'
        if (is-under-default-risk potential-default-bank = true) [
          print (word "     Proceeding to step 3 of bail-in - resolution funds. Required amount from resolution funds: " amount-still-required-to-save)
          apply-resolution-funds potential-default-bank
        ]
      ]
    ]
  ]
end

to apply-resolution-funds [potential-default-bank]
  print ("  Mechanism 3. Bailing-in using the RESOLUTION FUNDS.")

  print (word "     Total available resolution funds: " fund-resolution-budget)
  let initial-equity equity
  let initial-assets (four-decimal (interbank-assets + liquid-assets + illiquid-assets))

  ask potential-default-bank [

    ;; Un capital-adequancy-ratio de minim 5%, pentru ca banca sa nu fie prea fragila odata ce intra din nou in sistem.
    ;; Totusi, aceasta valoare pentru equity nu este neaparat asigurata, intrucat trebuie sa luam in considerare si contributia maxima pe care FGDB o poate avea
    let target-CAR 0.05

    ;; Verificare initiala a capitalurilor. Daca acestea sunt negative, trebuie luate in considerare cand se calculeaza target-equity, ca Fondul de Garantare al Depozitelor sa acopere si aceasta 'gaura'
    let equity-deficit 0
    if (equity < 0)[
      set equity-deficit (four-decimal abs(compute-net-worth potential-default-bank))
    ]

    ;; Calculam care este capitalul necesar pentru a atinge 5% CAR (se va lua in considerare si deficitul, ca 'equity' sa porneasca de la 0)
    ;; Formula: (Rate * Liabilities) / (1 - Rate)
    let target-equity (four-decimal (target-CAR * (interbank-liabilities + total-deposits) / (1 - target-CAR)) + equity-deficit)

    ;; Conform Legii 312/2015, fondul de garantare al depozitelor nu poate interveni cu o suma mai mare de 5% decat totalul pasivelor
    let total-liabilities (four-decimal (interbank-liabilities + total-deposits + equity))
    let bank-specific-cap (four-decimal (0.05 * total-liabilities))

    ;; Interventia este limitata de:
    ;;    - Contributie de max. 5% din total pasive
    ;;    - Suma disponibila in Fondul de Garantare al Depozitelor
    ;;    - Suma de care banca are nevoie pentru a evita default.
    let actual-help-amount (four-decimal min (list target-equity bank-specific-cap fund-resolution-budget) )
    print (word "      Target equity: " (target-equity) " | Bank-specific-cap: " (four-decimal bank-specific-cap) " | Fund resolution budget: " (four-decimal fund-resolution-budget))

    ;; Verificare daca banca ar fi in default si cu rezolutia din fondul de garantare al depozitelor.
    ;;  - Daca da, atunci aplicarea fondurilor de garantare nu are sens, intrucat banca va ramane in continuare in default.
    ;;  - Daca nu, atunci banca va fi ajutata de catre fondurile de garantare al depozitelor.
    let net-worth-with-eventual-help (four-decimal ( (compute-net-worth potential-default-bank) + actual-help-amount))

    ifelse (net-worth-with-eventual-help >= 0)[
      if actual-help-amount > 0 [
        set fund-resolution-budget (four-decimal (fund-resolution-budget - actual-help-amount))
        set equity (four-decimal (equity + actual-help-amount))
        set liquid-assets (four-decimal (liquid-assets + actual-help-amount))
        print (word "     Remaining available resolution funds: " fund-resolution-budget)
      ]
      print (word "     Equity:" initial-equity " -> " equity " | Assets: " initial-assets " -> " (four-decimal (interbank-assets + liquid-assets + illiquid-assets)))
      print (word "     NEW CAR:" (four-decimal (equity / (interbank-assets + liquid-assets + illiquid-assets)) ))
    ][
      print (word "      Resolution funds intervention will still lead to default, so it makes no sense to apply them")
    ]
  ]
end

;; Fn helper ce va scadea 'datoriile'(interbank-assets) ale bancii care risca sa intre in default cu o valoare de bailin-rate%
;; Aceasta fn va actualiza valorile arcelor afectate, equity si interbank-assetsurile bancii care au imprumutat banca in risc de default, dar si bancii care evita defaultul
to creditors-bail-in [potential-default-bank bailin-rate]
  print (word "       Mechanism 1. Bailing-in " potential-default-bank " using the CREDITORS.")
  print (word "        Link loss rate: " bailin-rate)
  ;; Actualizarea 'datoriilor' bancii ce se afla in prag de default, in urma procesului de bail-in
  ask potential-default-bank [
    let amount-cancelled (four-decimal (interbank-liabilities * bailin-rate))
    print (word "        Cancelled amount: " amount-cancelled)
    let initial-interbank-liabilities interbank-liabilities
    let initial-equity equity

    set interbank-liabilities (four-decimal (interbank-liabilities - amount-cancelled))
    set equity (four-decimal (equity + amount-cancelled))
    print (word "         Interbank liabilities: " initial-interbank-liabilities " -> " interbank-liabilities)
    print (word "         Equity: " initial-equity " -> " equity)
  ]

  ;; Actualizarea 'activelor' bancilor (vecinilor) care au imprumutat banca ce se afla in risc de default
  ask in-link-neighbors [
    print (word "        Bank " self " helps to bail:")

    let asset-loss 0
    let initial-interbank-assets interbank-assets
    let initial-equity equity

    ;; Actualizarea arcului cu noua valoare.
    ask out-link-to potential-default-bank [
      set asset-loss (four-decimal (weight * bailin-rate))
      set weight (four-decimal (weight - asset-loss))
      print (word "         Link loss weight: " asset-loss)
    ]

    set interbank-assets (four-decimal (interbank-assets - asset-loss))
    set equity (four-decimal (equity - asset-loss))
    print (word "         Interbank-assets: " initial-interbank-assets " -> " interbank-assets)
    print (word "         Equity: " initial-equity " -> " equity)
  ]
end

;; Fn helper ce va scadea 'depozitele'(total-deposits) in asa fel incat aceasta sa nu mai fie in risc de default
;; Aceasta fn va scadea depozitele totale in urma scaderii depozitelor neasigurate ale firmelor mari -> SME-uri; Aceasta scadere se va reflecta intr-o crestere a equity-ului
to deposits-bail-in [potential-default-bank]
  print ("  Mechanism 2. Bailing-in using the UNINSURED DEPOSITS.")
  ask potential-default-bank [

    ifelse ( (compute-net-worth self) < 0)[

      let required-amount-for-deposit-bailin (four-decimal abs(compute-net-worth self))

      let initial-equity equity
      let initial-total-deposits total-deposits
      let initial-large-companies-uninsured large-companies-uninsured-deposits-volume
      let initial-sme-uninsured sme-uninsured-deposits-volume

      print (word "     Uninsured large-companies amount: " large-companies-uninsured-deposits-volume)
      print (word "     Uninsured SME deposits amount: " sme-uninsured-deposits-volume)

      print ("     Try to bail-in using deposits..")

      ;; Se incepe procesul de bail-in folosind depozitele plecand, prima data, de la companiile care au depozite neasigurate.
      let large-companies-contribution-amount (four-decimal (get-min required-amount-for-deposit-bailin large-companies-uninsured-deposits-volume))
      print (word "      Large companies uninsured deposits contribution amount: " large-companies-contribution-amount)
      set total-deposits (four-decimal (total-deposits - large-companies-contribution-amount))
      print (word "       Total deposits: " initial-total-deposits " -> " total-deposits)
      set large-companies-uninsured-deposits-volume (four-decimal (large-companies-uninsured-deposits-volume - large-companies-contribution-amount))
      set equity (four-decimal (equity + large-companies-contribution-amount))

      set required-amount-for-deposit-bailin (four-decimal (required-amount-for-deposit-bailin - large-companies-contribution-amount))

      ;; Daca banca inca se afla in risc de default, se va continua procesul de bail-in folosind depozitele neasigurate ale SME-urilor
      if (is-under-default-risk potential-default-bank = true) [
        print (word "      Still required: " required-amount-for-deposit-bailin)

        let sme-contribution-amount (four-decimal (get-min required-amount-for-deposit-bailin sme-uninsured-deposits-volume))
        set initial-total-deposits total-deposits
        print (word "      SMEs uninsured deposits contribution amount: " sme-contribution-amount)
        set total-deposits (four-decimal (total-deposits - sme-contribution-amount))
        print (word "       Total deposits: " initial-total-deposits " -> " total-deposits)
        set sme-uninsured-deposits-volume (four-decimal (sme-uninsured-deposits-volume - sme-contribution-amount))
        set equity (four-decimal (equity + sme-contribution-amount))
      ]
      print(word "       Equity: " initial-equity " -> " equity)
    ][
      print ("     No bailin using deposits is required")
    ]
  ]
end


to-report get-min [a b]
  ifelse a < b [ report a ][ report b]
end

to mark-link-to-default-bank-as-unsellable [default-bank]
  ask default-bank [
    ask my-in-links [
      set color red
      set is-sellable false
    ]
  ]
end

to mark-link-to-liquidity-crisis-as-unsellable [liquidity-crisis-bank]
  ask liquidity-crisis-bank[
    ask my-in-links[
      set color orange
      set is-sellable false
    ]
  ]
end

to mark-link-to-self-as-sellable [bank]
  ask bank [
    ask my-in-links [
      set color gray
      set is-sellable true
    ]
  ]
end


to go
  print ("\n TICK \n")
  if default-max-banks-reached = true [ stop ]


  let defaulted-this-iteration []

  ;; Sursa infectiei pentru iteratia curenta. (it1 = exogenous, it2 = cele care au intrat in default pe urma exogenous shock, it3 = ...)
  let current-iteration-default-banks turtles with [state = STATE-DEFAULT and not member? self visited-default-banks]
  print (word "Default banks in this iteration: " [self] of current-iteration-default-banks)

  ;; Ar trebui sa verificam doar vecinii bancii care intra in default in iteratia precedenta. Ulterior, daca exista alte banci ce intra in default dupa ce primii vecini au fost verificati, acestia se vor parcurge
  ;; Verificam cei mai apropiati 'vecini' pentru a observa daca acestia sunt in risc de criza lichididate/default, daca cel 'curent' a intrat in default.
  ;; Se va verifica iterativ. Ex: A->B->C. tick1=vecinii lui B; tick2=vecinii lui C
  let affected-neighbors turtle-set [in-link-neighbors] of current-iteration-default-banks

  print (word "Affected banks in this iteration: " [self] of affected-neighbors)

  ask affected-neighbors [
    print (word "################ VISITING TURTLE AFFECTED BY A DEFAULT ONE: " self)

    let all-neighbors-of-affected-bank link-neighbors
    print (word "   Affected bank " self " is connected to a total of: " all-neighbors-of-affected-bank)

    cut-interbankassets-if-lent-towards-default self

    ;; Verificare initiala impotriva unei eventuale crize de lichiditati + actionare in situatie de criza
    ifelse (is-under-liquidity-risk self)[
      print(word "       Is under liquidity-risk? TRUE")
      print(word "~~~~~~~ Triggering fire-asset-sell ~~~~~~~")
      sell-granted-loans self
    ][
      print(word "       Is under liquidity-risk? FALSE \n")
    ]

    ;; Verificare ulterioara daca banca inca se afla in starea unei crize de lichiditati. Daca da, ii schimbam starea si marcam imprumuturile contractate ca fiind 'nesigure' pentru potentiali cumparatori
    ifelse (is-under-liquidity-risk self)[
      print (word "       Still under liquidity risk? TRUE")
      set-state-for-bank self STATE-LIQUIDITY-CRISIS
      mark-link-to-liquidity-crisis-as-unsellable self
    ][
      ;; Daca banca curenta nu se mai afla in risc de lichiditate, dam revert la imprumuturile contractate - marcand banca curenta ca fiind 'sanatoasa'
      print (word "       Still under liquidity risk? FALSE. Setting state for bank as: " STATE-HEALTHY)
      set-state-for-bank self STATE-HEALTHY
      mark-link-to-self-as-sellable self
    ]

    ;; Verificare initiala impotriva insolventei + actionare in situatie de default.
    ifelse (is-under-default-risk self)[
      print(word "       Is under default-risk? TRUE")
      print(word "~~~~~~~ Triggering regulatory processes ~~~~~~~")
      try-to-cascade-mitigate-default self
    ][
      print(word "       Is under default-risk? FALSE \n")
    ]

    ;; Verificare ulterioara daca banca inca se afla in starea de default. Daca da, ii schimbam starea si marcam ca fiind imprumuturile contractate ca fiind 'nesigure' pentur potentiali cumparatori.
    ifelse (is-under-default-risk self)[
      print (word "       Still under default risk? TRUE")
      set-state-for-bank self STATE-DEFAULT
      mark-link-to-default-bank-as-unsellable self
    ][
      print(word "       Still under default risk? FALSE")
    ]
  ]

  ;; 'Impingem' toate bancile ce au fost in default in iteratia curenta ca fiind deja 'vizitate' de catre vecinii acestora.
  ask current-iteration-default-banks [
    set visited-default-banks lput self visited-default-banks
    set-state-for-bank self STATE-DEFAULT
    mark-link-to-default-bank-as-unsellable self
  ]

  ;; Verificare de siguranta - bilant contabil pentru toate - verificam daca exista altele care trebuie sa intre in default/criza solventa
  ;; Auditare globală asupra tuturor băncilor care sunt încă active
  ask turtles with [state != STATE-DEFAULT] [
    ifelse (is-under-default-risk self)[
      print (word "       Auditing bank " self " as default-state after final audit checks. Its neighbors will be looped through next iteration")
      set-state-for-bank self STATE-DEFAULT
      mark-link-to-default-bank-as-unsellable self
    ][
      ifelse (is-under-liquidity-risk self)[
        print (word "       Auditing bank " self " as liquidity-crisis state after final audit checks")
        set-state-for-bank self STATE-LIQUIDITY-CRISIS
        mark-link-to-liquidity-crisis-as-unsellable self
      ][
        set-state-for-bank self STATE-HEALTHY
        mark-link-to-self-as-sellable self
      ]
    ]
  ]

  print(word "Defaulted-this-iteration: " defaulted-this-iteration)

; if ticks = 20 [stop]
 if ( (count turtles with [state = STATE-DEFAULT]) = number-of-banks) [
    set default-max-banks-reached true
  ]
 tick
end

to set-state-for-bank [bank to-state]
  ifelse (to-state = STATE-HEALTHY or to-state = STATE-LIQUIDITY-CRISIS or to-state = STATE-DEFAULT)[
    ask bank[
      print(word "    Changing bank state to " to-state)
      if (to-state = STATE-HEALTHY)         [ set color blue ]
      if (to-state = STATE-LIQUIDITY-CRISIS)[ set color orange ]
      if (to-state = STATE-DEFAULT)         [ set color red ]

      set state to-state
    ]
  ][
    error "Incorrect state used for bank!"
  ]


end

;; Fn ce verifica bilantul contabil la sfarsitul unui tick, pentru fiecare banca. Daca 'capitalurile proprii' sunt <0, inseamna ca banca nu
to check-balance-sheet-for-all
  print ("@@@@@@@ Checking balance sheet for all at the end of tick @@@@@@@")
  ask turtles with [state != STATE-DEFAULT] [
    ;; Calculam din nout 'net-worth-ul'
    let current-net-worth (four-decimal (interbank-assets + illiquid-assets + liquid-assets - interbank-liabilities - total-deposits))
    set equity (four-decimal (current-net-worth) )

    if (equity < 0) [
      set-state-for-bank self STATE-DEFAULT
      print (word "!!! TERMINATED: Bank " self " finished the tick with negative equity: " equity)
    ]
  ]
end

;;Fn ce verifica daca o banca este in riscul de default;;
to-report is-under-default-risk [bank]
  let maybe-default false
  ask bank [
    let total-assets (four-decimal (interbank-assets + illiquid-assets + liquid-assets) )
    let total-liabilities (four-decimal (interbank-liabilities + total-deposits) )
    if (total-assets < total-liabilities) [
      set maybe-default true
    ]
  ]
  report maybe-default
end

;;Fn ce verifica daca o banca este in criza de lichiditate (in acest nivel, va vinde creditele date pentru cresterea lichiditatii);;
to-report is-under-liquidity-risk [bank]
  let maybe-liquidity-risk false

  ask bank [
    let immediate-deposit-demand (four-decimal (total-deposits * deposit-withdrawal-rate))

    if (liquid-assets < (interbank-liabilities + immediate-deposit-demand) ) [
      set maybe-liquidity-risk true
    ]
  ]

  report maybe-liquidity-risk
end

to-report is-with-negative-equity [bank]
  report [equity] of bank < 0
end

to initial-default-setup-for [ default-agent ]
  ask default-agent [
    set has-started-contagion true
    set started-contagion-interbank-assets interbank-assets
    set started-contagion-liquid-assets liquid-assets
    set started-contagion-illiquid-assets illiquid-assets
    set started-contagion-interbank-liabilities interbank-liabilities
    set started-contagion-total-deposits total-deposits
    set started-contagion-bank-size bank-size
    set liquid-assets 0

    ifelse (is-under-default-risk self = true)[
      set-state-for-bank self STATE-DEFAULT
    ][
      error "The simulation cannot start as the bank won't be in default if liquid-assets=0"
    ]
  ]
end

; Initial exogenous shock, one bank is chosen
; at random and defaults (turns red)
to exogenous-shock
  ifelse (any? turtles with [state != STATE-DEFAULT])[
    ask one-of turtles with [state != STATE-DEFAULT] [
      initial-default-setup-for self
    ]
  ][
    print "No other banks can enter default state"
  ]
end

to biggest-size-exogenous-shock
  ifelse (any? turtles with [bank-size = banks-max-size and state != STATE-DEFAULT])[
    ask one-of turtles with [bank-size = banks-max-size and state != STATE-DEFAULT] [
      initial-default-setup-for self
    ]
  ][
    print "No banks with biggest size remaining that can enter default state"
  ]
end

to smallest-size-exogenous-shock
  ifelse (any? turtles with [bank-size = banks-min-size and state != STATE-DEFAULT])[
    ask one-of turtles with [bank-size = banks-min-size and state != STATE-DEFAULT] [
      initial-default-setup-for self
    ]
  ][
    print "No banks with smallest size remaining that can enter default state"
  ]
end

;; Fn ce cauta cumparator pentru imprumutul dintre BankA -> BankB, pentru o anumita suma. 'loan-to-sell' exprima '->'
to-report find-random-potential-buyer-for-loan [loan-to-sell for-amount]
  print (word "         Potential buyers should have more than " for-amount " liquid assets to buy the loan from " loan-to-sell)
  let bank-that-wants-to-sell [end1] of loan-to-sell
  let bank-that-borrows [end2] of loan-to-sell

  let potential-buyers-agentset (turtles with [
    ;; Cumparatorul nu trebuie sa fie in risc de default
    (is-under-default-risk self = false)
    ;; Cumparatorul nu trebuie sa fie in risc de criza lichiditate
    and (is-under-liquidity-risk self = false)
    and self != bank-that-wants-to-sell
    and self != bank-that-borrows
    and four-decimal liquid-assets >= four-decimal for-amount
  ])

  print (word "         Potential-buyers: " [self] of potential-buyers-agentset)

  ; Use any? to check if the agentset is empty
  ifelse any? potential-buyers-agentset [
    report one-of potential-buyers-agentset
  ] [
    report nobody
  ]
end

;; Fn ce updateaza datele contabile ale cumparatorului unui imprumut (in caz de banca de la care cumpara este in fire-sell assets)
to update-buyer-of-loan [buyer amount-to-buy]
  ask buyer [
    let initial-interbank-assets interbank-assets
    let initial-liquid-assets liquid-assets
    let initial-equity equity

    let price-bought-for (four-decimal (amount-to-buy * (1 - discount-rate) ))
    let equity-gains (four-decimal (amount-to-buy * discount-rate))

    print(word "          New props for buyer" buyer)

    set interbank-assets (four-decimal (interbank-assets + amount-to-buy))
    set liquid-assets (four-decimal (liquid-assets - price-bought-for))
    set equity (four-decimal (equity + equity-gains))

    print (word "             Interbank-assets: " initial-interbank-assets " -> " interbank-assets)
    print (word "             Liquid-assets: " initial-liquid-assets " -> " liquid-assets)
    print (word "             Equity: " initial-equity " -> " equity)

    set total-links total-links + 1
  ]
end

;; Fn ce updateaza datele contabile ale vanzatorului unui imprumut (in caz de fire-sell assets)
to update-seller-of-loan [seller amount-to-sell]
  ask seller [
    let initial-interbank-assets interbank-assets
    let initial-liquid-assets liquid-assets
    let initial-equity equity

    let price-sold-for (four-decimal (amount-to-sell * (1 - discount-rate)))
    let equity-loss (four-decimal (amount-to-sell * discount-rate))


    ;; Scadem capitalul propriu al bancii (equity va acoperi suma care se pierde).
    ;; Daca equity ar deveni negativ, banca este tehnic insolventa, deci va intra in default in 'to go', posibil la mecanismele regulatoare.
    ;; Pierderea este absorbita de capitalul bancii
    set equity (four-decimal (equity - equity-loss))

    print(word "          New props for seller" seller)

    ifelse (four-decimal amount-to-sell > four-decimal interbank-assets)[
      set interbank-assets 0
    ][
      set interbank-assets (four-decimal (interbank-assets - amount-to-sell))
    ]
    set liquid-assets (four-decimal (liquid-assets + price-sold-for))

    print (word "             Interbank-assets: " initial-interbank-assets " -> " interbank-assets)
    print (word "             Liquid-assets: " initial-liquid-assets " -> " liquid-assets)
    print (word "             Equity: " initial-equity " -> " equity)

    set total-links total-links - 1
  ]
end

;; Fn responsabila pentru crearea unui arc nou intre banca ce cumpara imprumutul acordat de catre banca in riscul defaultului ([end1]) altei banci.
to set-and-update-new-link [bank-that-buys-loan old-loan]
  let who-loaned [end2] of old-loan

  ;; Actualizam/initializam arcul nou creat.
  ask bank-that-buys-loan [

    ;; Verificam daca deja exista un imprumut acordat de catre bank-that-buys-loan catre who-loaned
    let existing-link out-link-to who-loaned

    ;; Daca nu exista, creem arc nou cu vechile valori
    ifelse (existing-link = nobody)[
      create-directed-edge-to who-loaned
      ask out-link-to who-loaned [
        set shape "curved"
        set color green
        set weight (four-decimal ([weight] of old-loan))
        set link-interest-rate (four-decimal ([link-interest-rate] of old-loan))
        set link-loan-type [link-loan-type] of old-loan
      ]
    ][
      ;; Daca exista, actualizam arcul curent cu noile valori
      ask existing-link [
        set weight (four-decimal (weight + [weight] of old-loan))
        set color yellow ;; Highlight that this link grew
        ;; Setam o medie a ratelor, pentru usurinta calculelor
        set link-interest-rate (four-decimal ([link-interest-rate] of old-loan / link-interest-rate))
      ]
    ]

  ]
end

;;Fn helper care explica procesul de 'Asset Fire Sale';; Starea pietei este una RISCANTA.
;;;;Aceasta vanzare va fi la o suma mai mica (%discount-rate) decat imprumutul acordat (weight);;
;;Se incearca mitigarea unei eventuale stari de 'criza de lichiditate';;
;;Banca ce risca de a intra in criza de lichiditate va incerca sa vanda imprumuturile pe care le-a acordat altor banci,
      ;;pentru a dispune de lichiditate ca sa nu intre in 'criza de lichiditate';;
;;Intrucat bancile care nu sunt in 'prag' de criza lichiditate isi asuma riscul de a cumpara din imprumuturile bancii ce se afla in prag de criza lichiditate,
      ;;vor fi 'recompensate'
;;Recompensa este descrisa de 'discount-rate'. A(risc default)->B  => C->B (C a cumparat imprumutul la un pret mai 'mic')
;;Recompensa este cauzata de starea pietei, care este RISCANTA, avand in vedere faptul ca una dintre banci este in pragul de 'criza de lichiditati'
to sell-granted-loans [potential-liquidity-crisis-bank]

  ask potential-liquidity-crisis-bank [
    print (word "Total links: " my-out-links " out of which only " my-out-links with [is-sellable = true] " are sellable")
    ;; Verificam doar acele imprumuturi care sunt acordate catre banci 'sigure'. Imprumuturile acordate catre banci 'nesigure' (default/liquidity-crisis) sunt riscante pentru cumparatori
    ask my-out-links with [is-sellable = true] [
      ;; Daca banca curenta inca este in pericol de criza lichiditate/default, continuam sa vindem imprumuturi pentru cresterea lichiditatii
      if (is-under-liquidity-risk potential-liquidity-crisis-bank)[

        if ([state] of end2 = STATE-HEALTHY)[
          let amount-required (four-decimal (weight - weight * discount-rate) )
          print (word "       Trying to sell " self " loan. Weight amount: " weight "; Selling for: :" amount-required)

          let buyer find-random-potential-buyer-for-loan self amount-required

          ;; trebuie updatate valorile pentru cel care a vandut (liquid-assets), dar si cel care a cumparat (liquid-assets + many more)
          ifelse buyer != nobody [
            print (word "         Sold loan between " self " to " buyer)

            ;; Setam culoarea imprumutului care va fi 'vandut' cu galben
            ask self[
              set color yellow
            ]
            set-and-update-new-link buyer self
            update-buyer-of-loan buyer weight
            update-seller-of-loan potential-liquidity-crisis-bank weight
          ][
            print (word "         No buyer found for loan " self)
          ]
        ]
      ]
    ]
  ]
end

to-report extract-weight-of-link [turtle1 linkDirection turtle2]
  let linkWeight 0
  ask turtle1 [
    ifelse linkDirection = "from" [
      ; 'from' means that the 'turtle1' has borrowed money FROM 'turtle2' ==>  turtle1 <- turtle2
      print (word turtle1 " receives money from " turtle2)
      ask in-link-from turtle2 [
        set linkWeight weight
      ]
    ][
      ; 'to' means that the 'turtle1' has borrowed money TO 'turtle2'     ==>  turtle1 -> turtle1
      print (word turtle1 " sends money to " turtle2)

      ask out-link-to turtle2 [
        set linkWeight weight
      ]
    ]
  ]
  report linkWeight
end

to-report weight-between [turtle1 turtle2]
  let lw nobody
  ask turtle1 [
    let existing-link out-link-to turtle2
    ifelse is-link? existing-link [
      ask existing-link [ set lw weight]
    ][
      print (word "Warning! There is no link between " turtle1 " and " turtle2)
    ]
  ]
  report lw
end

to-report get-interest-rate [bank loanType]
  report table:get ([interest-rate-map] of bank) loanType
end
@#$#@#$#@
GRAPHICS-WINDOW
407
10
870
474
-1
-1
13.0
1
10
1
1
1
0
0
0
1
-17
17
-17
17
1
1
1
ticks
30.0

BUTTON
44
167
219
200
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

MONITOR
884
61
973
106
Total vertices
count links
3
1
11

SLIDER
43
52
217
85
number-of-banks
number-of-banks
2
80
30.0
1
1
NIL
HORIZONTAL

BUTTON
44
208
219
241
Default Random Bank
exogenous-shock
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
884
190
975
235
Defaulted Banks
count turtles with [ color = red ]
17
1
11

PLOT
44
366
266
548
Defaulted Banks
t
Defaulted Banks
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"Default" 1.0 0 -2674135 true "" "plot count turtles with [ color = red ]"
"Non-default banks" 1.0 0 -13791810 true "" "plot count turtles with [ color = blue]"
"pen-2" 1.0 0 -817084 true "" "plot count turtles with [ color = orange]"

INPUTBOX
270
226
320
286
mu
0.0
1
0
Number

INPUTBOX
332
226
382
287
sigma
1.0
1
0
Number

MONITOR
883
241
1041
286
Total non-defaulted Banks
count turtles with [ color != red ]
17
1
11

MONITOR
883
292
1038
337
Government-saved banks
count turtles with [color = green]
17
1
11

MONITOR
883
343
1155
388
Non-defaulted banks without gov. intervention
count turtles with [color = blue]
17
1
11

SLIDER
43
94
279
127
max-connectivity-node-may-have
max-connectivity-node-may-have
0
32
11.0
1
1
NIL
HORIZONTAL

BUTTON
201
328
264
361
go
go
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
44
250
194
283
Default Smallest Bank
smallest-size-exogenous-shock
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
44
291
190
324
Default Biggest Bank
biggest-size-exogenous-shock
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
269
186
396
219
buyer-discount-rate
buyer-discount-rate
10
30
13.0
1
1
NIL
HORIZONTAL

SLIDER
268
150
397
183
deposits-withdrawal-rate
deposits-withdrawal-rate
0
100
30.0
5
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

The following program recreates the behavior of financial contagion described by Gai and Kapadia (2010). This model differs from the model described in the paper in that the user has the liberty to choose the number of banks in the network (in the original model, everything is randomly determined). This model assumes there are no fire sales (hence q = 1). Defaulted banks are represented by the color red, solvent banks are represented by the color blue.

This first version runs one round of default simulation only, with banks of different sizes. Note that the main findings of the paper are not affected by allowing bank sizes to vary.

## THINGS TO NOTICE

To run the model, first setup the world (choose number of desired banks in the network, anywhere between 2 and 200). Adjust for mean and standard deviation of bank size to set up random distribution of bank sizes. The number next to each bank indicates its relative size.

Then, default a single random bank by clicking on the button "Default Random Bank" only once. After this, hit the button "Go" to see the contagion chain effect. The model contains a graph that automatically plots the number of defaulted banks per time period. Hit "Go" again to stop the cycle once the plot shows that the number of defaults has come to an equilibrium.

## EXTENDING THE MODEL

Extensions of the model could include: sliders to choose the number of links (in order to adjust the parameter of interconnectedness "z"), making banks have different sizes, allow interbank assets not to be evenly distributed among incoming links. Please see other impemented versions.

## RELATED MODELS

-

## CREDITS AND REFERENCES

* Gai, Prasanna and Kapadia, Sujit, Contagion in Financial Networks (March 23, 2010). Bank of England Working Paper No. 383. Available at SSRN: http://ssrn.com/abstract=1577043 or http://dx.doi.org/10.2139/ssrn.1577043
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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
setup-simple-random
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Random Bank - Contagion" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup
exogenous-shock</setup>
    <go>go</go>
    <timeLimit steps="360"/>
    <metric>count turtles with [color = red]</metric>
    <metric>started-contagion-interbank-assets</metric>
    <metric>started-contagion-illiquid-assets</metric>
    <metric>started-contagion-interbank-liabilities</metric>
    <metric>started-contagion-deposits</metric>
    <metric>started-contagion-bank-size</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="mu">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigma">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Banks">
      <value value="32"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Smallest Bank - Contagion" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup
smallest-size-exogenous-shock</setup>
    <go>go</go>
    <timeLimit steps="360"/>
    <metric>count turtles with [color = red]</metric>
    <metric>started-contagion-interbank-assets</metric>
    <metric>started-contagion-illiquid-assets</metric>
    <metric>started-contagion-interbank-liabilities</metric>
    <metric>started-contagion-deposits</metric>
    <metric>started-contagion-bank-size</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="mu">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigma">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Banks">
      <value value="32"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Biggest Bank - Contagion" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup
biggest-size-exogenous-shock</setup>
    <go>go</go>
    <timeLimit steps="360"/>
    <metric>count turtles with [color = red]</metric>
    <metric>started-contagion-interbank-assets</metric>
    <metric>started-contagion-illiquid-assets</metric>
    <metric>started-contagion-interbank-liabilities</metric>
    <metric>started-contagion-deposits</metric>
    <metric>started-contagion-bank-size</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="mu">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigma">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Banks">
      <value value="32"/>
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

curved
1.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

dashed
0.0
-0.2 0 0.0 1.0
0.0 1 4.0 4.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
