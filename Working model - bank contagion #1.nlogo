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
  panic-deposit-withdrawal-rate
  baseline-deposit-withdrawal-rate
  visited-default-banks
  banks-max-size
  banks-min-size
  rate-of-SME-uninsured-deposits ; Ce procent din totalul depozitelor este din partea SME ca fiind depozit neasigurat. Ipoteza: SME-urile au depozite >100k
  rate-of-large-companies-uninsured-deposits ; Ce procent din totalul depozitelor este din partea 'large-companies'. Ipoteza: large-companies au depozite >100k
  ;;Deposits - rate-of-SME-uninsured-deposits * deposits - rate-of-large-companies-uninsured-deposits * deposits = x. Aceste depozite 'x' constituie depozite <100k (ex: 300 depozite <100k)
  ;;Depozitele <100k nu pot fi folosite pentru mecanisme de 'salvare' a bancii (bail-in)
  ;;;bail-in din partea altor banci = se incearca; daca nu, se merge pe nivelurile urmatoare.
  ;;;bail-in lv1 = se incearca bail-in folosind depozitele celor din 'large-companies'
  ;;;bail-in lv2 = se incearca bail-in folosind depozitele celor din SME
  ;;;bail-in lv3 = se incearca bail-in folosind toate depozitele, indiferent ca sunt >100k sau <100k, sau ca vin de la micro, SME, large-companies
  ;;;bailout     = interventia guvernamentala, folosind fonduri rezolutie bancara cf lege 312/2015
  possible-loan-types ; only short-term and long-term allowed
  default-max-banks-reached ; how many banks entered the default; if all of them are in default state, stop the execution of the program
  fund-resolution-budget ; Bugetul fondului de rezolutie -> va fi suma dintre 1% din totalul depozitelor ASIGURATE ale fiecarei banci

  ;; Stari ale bancilor, in functie de bilantul acestora - bun, criza de lichiditate sau default.
  STATE-HEALTHY
  STATE-LIQUIDITY-CRISIS
  STATE-DEFAULT

  monitor-loans-sold ; Variabila pentru monitorizarea imprumuturilor vandute
]

;; se va incerca vinderea creditelor acordate, pentru cresterea lichiditatii - in situatie de liquidity-crisis
;; in situatie de solvency-crisis
;; bail-in lv1
;; bail-in lv2
;; bailouut

turtles-own [
  state
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

  reset-ticks
end

;;Fn setup-globals - initializarea var. globale;
to setup-globals
  set possible-loan-types ["short-term" "long-term"]
  set visited-default-banks []
  set rate-of-SME-uninsured-deposits .1
  set rate-of-large-companies-uninsured-deposits .01
  set discount-rate (buyer-discount-rate / 100)
  set panic-deposit-withdrawal-rate (four-decimal (panic-deposits-withdrawal-rate / 100))
  set baseline-deposit-withdrawal-rate (four-decimal (baseline-deposits-withdrawal-rate / 100))
  set STATE-HEALTHY "HEALTHY"
  set STATE-LIQUIDITY-CRISIS "LIQUIDITY-CRISIS"
  set STATE-DEFAULT "DEFAULT"

  set monitor-loans-sold 0
end

to setup-bank-nonfinancial-states
  set default-max-banks-reached false
  create-turtles number-of-banks [ set shape "house" ]

  layout-circle turtles (max-pxcor - 1)

  ask turtles [
    set state STATE-HEALTHY
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
      set liquid-assets (four-decimal (0.30 * target-total-assets))
      set interbank-assets 0
    ]
    [
      set liquid-assets (four-decimal (0.30 * target-total-assets))
      set interbank-assets (four-decimal (0.20 * target-total-assets))
    ]
    ;; Activele nelichide se vor initializa cu remainder-ul dintre pasive totale - active curente => active nelichide (pentru a mentine active totale=pasive totale).
    ;; Practic, valoarea acestui activ va fi 70% din target-total, sau 50% din target-total
    set illiquid-assets four-decimal ( (equity + total-deposits + interbank-liabilities) - (liquid-assets + interbank-assets) )

    set sme-uninsured-deposits-volume (four-decimal (rate-of-SME-uninsured-deposits * total-deposits))
    set large-companies-uninsured-deposits-volume (four-decimal (rate-of-large-companies-uninsured-deposits * total-deposits))
    set insured-deposits (four-decimal (total-deposits * (1 - (rate-of-SME-uninsured-deposits + rate-of-large-companies-uninsured-deposits))))
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

to-report four-decimal [ n ]
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

        ;; Initializam loan-type-ul ca fiind
        set link-loan-type ifelse-value (random-float 100 < short-loan-ratio)
        [ "short-term" ]
        [ "long-term" ]
        set link-interest-rate four-decimal (get-interest-rate currentTurtle link-loan-type)
      ]
    ]
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

  ;; Verificare de siguranta - bilant contabil pentru toate - verificam daca exista altele care trebuie sa intre in default/criza lichiditate, in functie de modificarile de pe tickul anterior
  ;; Auditare globală asupra tuturor băncilor care sunt încă active
  ask turtles with [state != STATE-DEFAULT] [
    ifelse (is-under-default-risk self)[
      print (word "       Auditing bank " self " as default-state after final audit checks. Its neighbors will be looped through next iteration")
      set-state-for-bank self STATE-DEFAULT
      mark-link-to-default-bank-as-unsellable self
    ][
      ifelse (is-under-liquidity-risk self)[
        print (word "       Auditing bank " self " as liquidity-crisis after final audit checks. Its neighbors will be looped through next iteration")
        set-state-for-bank self STATE-LIQUIDITY-CRISIS
        mark-link-to-liquidity-crisis-as-unsellable self
      ][
        set-state-for-bank self STATE-HEALTHY
        mark-link-to-self-as-sellable self
      ]
    ]
  ]

  ;; Procedura de mai jos este responsabila pentru
  ;:   - transformarea din active ilichide -> active lichide
  ;;   - retragere depozite de catre clienti - 5% in conditii normale, withdrawal-rate% in conditii de stres (vecin cu o banca afectata)
  ask turtles with [state != STATE-DEFAULT] [

    recover-maturity-from-iliquid-assets self

    let current-rate (four-decimal baseline-deposit-withdrawal-rate) ;; The baseline "healthy" withdrawal rate

    ;; Setam rate-ul cu care depozitele vor fi retrase in functie de vecini - daca unul dintre vecini este DEFAULT, va interveni PANIC WITHDRAWAL
    ifelse any? out-link-neighbors with [state = STATE-DEFAULT] [
      set current-rate panic-deposit-withdrawal-rate
      print (word "!!! PANIC withdrawals for " self ": " (four-decimal (current-rate * 100)) "% run due to neighbor default.")
    ][
      print (word "!!! NORMAL withdrawals for " self ": " (four-decimal (current-rate * 100)) "% run.")
    ]

    ;; Procesul de retragere al depozitelor (daca activele lichide nu sunt suficiente, un prim proces de fire-sell-assets se va declansa aici)
    pay-depositors self current-rate
    print ("")
  ]


  ;; Sursa infectiei pentru iteratia curenta, bazandu-ne pe rezultatul iteratiei precedente. (it1 = exogenous, vf vecini, it2 = vecini + cele care au intrat in default pe urma exogenous shock, it3 = ...)
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

    ; Verificare initiala impotriva insolventei + actionare in situatie de default.
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
      if (is-under-liquidity-risk self)[
        set-state-for-bank self STATE-LIQUIDITY-CRISIS
        mark-link-to-liquidity-crisis-as-unsellable self
      ]
    ]
  ]

  ;; 'Impingem' toate bancile ce au fost in default in iteratia curenta ca fiind deja 'vizitate' de catre vecinii acestora.
  ask current-iteration-default-banks [
    set visited-default-banks lput self visited-default-banks
    set defaulted-this-iteration lput self defaulted-this-iteration
    set-state-for-bank self STATE-DEFAULT
    mark-link-to-default-bank-as-unsellable self
  ]

  print(word "Defaulted-this-iteration: " defaulted-this-iteration)

 if ticks = 50 [stop]
 if ( (count turtles with [state = STATE-DEFAULT]) = number-of-banks) [
    set default-max-banks-reached true
  ]
 tick
end

;; Fn helper ce va transforma 2% active ilichide in active lichide, la fiecare tick. De asemenea, aceasta suma proportionala are o dobanda de 1% pe care banca o incaseaza sub forma de equity.
;; NET INTEREST MARGIN docs
to recover-maturity-from-iliquid-assets [bank]
  let initial-illiquid-assets [illiquid-assets] of bank
  let initial-liquid-assets [liquid-assets] of bank

  ask bank [
    ;; Suma recuperata din activele ilichide, fixata la 2% per fiecare tick.
    let matured-amount (four-decimal (illiquid-assets * 0.02))

    ;; Actualizam activele + equity
    if matured-amount > 0 [
      set illiquid-assets (four-decimal (illiquid-assets - matured-amount))
      set liquid-assets (four-decimal (liquid-assets + matured-amount))

      ;; Dobanda va fi marcata ca fiind o crestere in equity.
      let net-interest-margin 0.01
      set equity (four-decimal (equity + (matured-amount * net-interest-margin)))
    ]

    print (word "   Illiquid-assets have matured. Illiquid-assets: " initial-illiquid-assets " -> " illiquid-assets " | Liquid-assets: " initial-liquid-assets " -> " liquid-assets)
  ]
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

;; Fn helper ce va calcula depozitele care vor fi retrase din cauza fricii civile.
to-report withdrawal-demand [bank]
  let immediate-deposits-withdrawal-required 0
  ask bank [
    set immediate-deposits-withdrawal-required (four-decimal (four-decimal (panic-deposit-withdrawal-rate) * (four-decimal total-deposits) ) )
    print (word "Withdrawal rate: " panic-deposit-withdrawal-rate " | Total-deposits: " total-deposits " | Immediate deposits withdrawal: " immediate-deposits-withdrawal-required " | Available to pay: " liquid-assets)
  ]
  report immediate-deposits-withdrawal-required
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
;; - in situatia in care banca curenta este vecina (current -> default) cu o banca default, aceasta va resimti 'panic-deposit-demand' si nu va putea cumpara din lipsa de lichiditate
;; - in situatia in care banca curenta nu este vecina cu niciuna default, riscul de lichiditate va fi calculat in functie de 5% din total depozit
to-report is-under-liquidity-risk [bank]
  let maybe-liquidity-risk false
  ask bank [
    let withdrawal-rate baseline-deposit-withdrawal-rate
    if any? out-link-neighbors with [state = STATE-DEFAULT] [
      set withdrawal-rate panic-deposit-withdrawal-rate
    ]
    let immediate-deposit-demand (four-decimal (withdrawal-rate * total-deposits))
    let immediate-interbank-liabilities-demand (four-decimal (sum [weight] of my-in-links with [link-loan-type = "short-term"]))

    if (liquid-assets < (four-decimal (immediate-interbank-liabilities-demand + immediate-deposit-demand)) ) [
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
        set color green
        set thickness (thickness + 0.1) ;; Evidentiem faptul ca arcul existent este actualizat cu noua valoare
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
          print (word "       Trying to sell " self " loan. Weight amount: " weight "; Selling for: " amount-required)

          let buyer find-random-potential-buyer-for-loan self amount-required

          ;; trebuie updatate valorile pentru cel care a vandut (liquid-assets), dar si cel care a cumparat (liquid-assets + many more)
          ifelse buyer != nobody [
            print (word "         Sold loan between " self " to " buyer)

            ;; Setam culoarea imprumutului care va fi 'vandut' cu galben
            ask self[
              set color yellow
            ]
            ;; Contorizam imprumutul vandut
            set monitor-loans-sold (monitor-loans-sold + 1)
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

;; Fn helper ce incearca acoperirea (plata) depozitelor. In situatia in care aceasta banca nu are destule active lichide pentru a acoperi depozitele imediate,
;;; aceasta va intra in procesul de fire-sell assets pentru a capata lichiditate. Fire-sell-assets se va intampla la un discount rate de %-discount-rate.
to pay-depositors [potential-unable-to-pay-deposits-bank current-rate]

  ask potential-unable-to-pay-deposits-bank [

    ;; Initializam suma depozitelor care trebuie acoperita
    let immediate-deposit-demand (four-decimal (total-deposits * current-rate))

    ;; Prima data se vor epuiza activele lichide
    let available-liquid-cash-payment (min (list liquid-assets immediate-deposit-demand))

    print (word "   Total demand-amount from deposits: " immediate-deposit-demand " | Total liquidity to pay: " liquid-assets " | Total available in interbank-assets: " interbank-assets)

    if available-liquid-cash-payment > 0 [
      let eff-rate (available-liquid-cash-payment / total-deposits)
      reduce-deposits-proportionally self eff-rate available-liquid-cash-payment
      set liquid-assets (four-decimal (liquid-assets - available-liquid-cash-payment))

      print (word "   Paid amount: " available-liquid-cash-payment " from liquid-assets. New liquid-assets: " liquid-assets)
    ]

    set immediate-deposit-demand (four-decimal (immediate-deposit-demand - available-liquid-cash-payment))

    ;; Ulterior, daca activele lichide nu au fost suficiente, banca curenta va intra in procesul de fire-sell-assets pentru a capata destula lichiditate pentru acoperirea depozitelor
    if (immediate-deposit-demand > 0) [
      print (word "   Selling interbank-assets to cover immediate-demand is required. Total demand-amount from deposits: " immediate-deposit-demand " | Total liquidity to pay: " liquid-assets " | Total available in interbank-assets: " interbank-assets)

      ask my-out-links with [is-sellable = true] [

        ;; Daca banca curenta inca nu poate acoperi depozitele la termen, continuam sa vindem imprumuturi pentru cresterea lichiditatii pana cand valoarea necesara e atinsa.
        if ([liquid-assets] of potential-unable-to-pay-deposits-bank < immediate-deposit-demand)[

          ;; Vindem doar acele imprumuturi care nu sunt riscante
          if ([state] of end2 = STATE-HEALTHY)[
            let amount-for-deposit (four-decimal (weight - weight * discount-rate) )
            print (word "       Trying to sell " self " loan. Weight amount: " weight "; Selling for: " amount-for-deposit)

            let buyer find-random-potential-buyer-for-loan self amount-for-deposit

            ;; Trebuie updatate valorile pentru cel care a vandut (liquid-assets), dar si cel care a cumparat (liquid-assets + many more)
            ifelse buyer != nobody [
              print (word "         Sold loan between " self " to " buyer)

              ;; Setam culoarea imprumutului care va fi 'vandut' cu galben
              ask self[
                set color yellow
              ]
              ;; Contorizam imprumutul vandut
              set monitor-loans-sold (monitor-loans-sold + 1)
              set-and-update-new-link buyer self
              update-buyer-of-loan buyer weight
              update-seller-of-loan potential-unable-to-pay-deposits-bank weight

              ;; Determinam valoarea depozitelor care va fi acoperita.
              let payment-to-depositors (min (list amount-for-deposit immediate-deposit-demand))

              ;; Reducem depozitele cu valoarea pe care banca a obtinut-o in urma vanzarii unui interbank-asset (vanzarea unui imprumut acordat)
              let sale-eff-rate (payment-to-depositors / [total-deposits] of potential-unable-to-pay-deposits-bank)
              reduce-deposits-proportionally potential-unable-to-pay-deposits-bank sale-eff-rate payment-to-depositors

              ;; Reducem activele lichide cu suma pe care banca a platit-o pentru acoperirea depozitelor.
              ask potential-unable-to-pay-deposits-bank [
                print (word "      Liquid-assets after sells: " liquid-assets)
                set liquid-assets (four-decimal (liquid-assets - payment-to-depositors))
                print (word "      Liquid-assets with subtracted: " liquid-assets)
              ]

              ;; Actualizam cererea de depozite scazand suma platita de catre banca din activele lichide, dupa vanzarea unui imprumut
              set immediate-deposit-demand (four-decimal (immediate-deposit-demand - payment-to-depositors))
              print (word "        Deposit gap updated. Remaining immediate deposit demand: " immediate-deposit-demand)
            ][
              print (word "         No buyer found for loan " self)
            ]
          ]
        ]
      ]
    ]
  ]
end

;; Fn helper ce reduce depozitele banci intr-un mod proportional, in functie de 'rate', avand in vedere activele lichide folosite (cash-paid)
to reduce-deposits-proportionally [bank rate cash-paid]
  ask bank [
    print (word "     Initial deposit props: Deposits " total-deposits ", from which: | Insured deposits: " insured-deposits " | SME uninsured: " sme-uninsured-deposits-volume " | Large uninsured: " large-companies-uninsured-deposits-volume)
    set insured-deposits (four-decimal (insured-deposits * (1 - rate)))
    set sme-uninsured-deposits-volume (four-decimal (sme-uninsured-deposits-volume * (1 - rate)))
    set large-companies-uninsured-deposits-volume (four-decimal (large-companies-uninsured-deposits-volume * (1 - rate)))
    set total-deposits (four-decimal (total-deposits - cash-paid))
    print (word "     Updated deposit props: Deposits " total-deposits ", from which: | Insured deposits: " insured-deposits " | SME uninsured: " sme-uninsured-deposits-volume " | Large uninsured: " large-companies-uninsured-deposits-volume)
  ]
end

to reduce-deposits-with [bank amount]
  ask bank [
    print (word "Initial deposit props: Deposits " total-deposits " | Insured deposits: " insured-deposits " | SME uninsured: " sme-uninsured-deposits-volume " | Large uninsured: " large-companies-uninsured-deposits-volume)

    let effective-rate (four-decimal (amount / total-deposits) )
    set insured-deposits (four-decimal (insured-deposits * (1 - effective-rate)))
    set sme-uninsured-deposits-volume (four-decimal (sme-uninsured-deposits-volume * (1 - effective-rate)))
    set large-companies-uninsured-deposits-volume (four-decimal (large-companies-uninsured-deposits-volume * (1 - effective-rate)))
    set total-deposits (four-decimal (total-deposits - amount) )

    print (word "Updated deposit props: Deposits " total-deposits " | Insured deposits: " insured-deposits " | SME uninsured: " sme-uninsured-deposits-volume " | Large uninsured: " large-companies-uninsured-deposits-volume)
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
465
10
1066
612
-1
-1
16.943
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
0
0
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
1068
191
1157
236
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
8.0
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
1070
246
1161
291
Defaulted Banks
count turtles with [ color = red ]
17
1
11

PLOT
44
366
390
548
Defaulted Banks
t
Defaulted Banks
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Default" 1.0 0 -2674135 true "" "plot count turtles with [ color = red ]"
"Healthy" 1.0 0 -13791810 true "" "plot count turtles with [ color = blue]"
"Liquidity-crisis banks" 1.0 0 -817084 true "" "plot count turtles with [ color = orange]"

INPUTBOX
282
295
332
355
mu
0.0
1
0
Number

INPUTBOX
344
295
394
356
sigma
1.0
1
0
Number

MONITOR
1069
302
1227
347
Total non-defaulted Banks
count turtles with [ color != red ]
17
1
11

MONITOR
1069
353
1275
398
Government-saved banks
count turtles with [color = green]
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
6.0
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
273
216
400
249
buyer-discount-rate
buyer-discount-rate
10
30
10.0
1
1
NIL
HORIZONTAL

SLIDER
272
180
453
213
panic-deposits-withdrawal-rate
panic-deposits-withdrawal-rate
0
100
45.0
5
1
NIL
HORIZONTAL

SLIDER
273
255
401
288
short-loan-ratio
short-loan-ratio
0
100
25.0
1
1
NIL
HORIZONTAL

PLOT
1291
461
1486
611
Imprumuturi vandute
NIL
NIL
0.0
30.0
0.0
20.0
true
false
"" ""
PENS
"monitor-loans-sold" 1.0 0 -7500403 true "" "plot monitor-loans-sold"

MONITOR
1071
405
1273
450
Sold loans throughout the simulation
count links with [color = yellow]
17
1
11

PLOT
1074
461
1274
611
Buget FGDB
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot fund-resolution-budget"

SLIDER
271
142
454
175
baseline-deposits-withdrawal-rate
baseline-deposits-withdrawal-rate
0
10
5.0
1
1
NIL
HORIZONTAL

PLOT
1073
10
1502
160
Bilant contabil banci
Banks
Value
0.0
32.0
0.0
10.0
true
false
"" "let i 0\n\nforeach sort turtles [ t ->\n  ask t [\n    ;; --- BAR 1: ASSETS (Stacked & Filled) ---\n    let total-assets (interbank-assets + liquid-assets + illiquid-assets)\n    \n    ;; 1. Draw Total Assets (This fills the bottom to the very top)\n    set-current-plot-pen \"Illiquid\"\n    plotxy i total-assets\n    \n    ;; 2. Draw Liquid + Interbank (Overwrites the bottom part of the bar)\n    set-current-plot-pen \"Interbank\"\n    plotxy i (total-assets - interbank-assets)\n    \n    ;; 3. Draw Liquid only (Overwrites the very bottom)\n    set-current-plot-pen \"Liquid\"\n    plotxy i (total-assets - interbank-assets - illiquid-assets)\n\n    ;; --- BAR 2: DEPOSITS (Adjacent bar) ---\n    set-current-plot-pen \"Deposits\"\n    plotxy (i + 1) total-deposits\n  ]\n  ;; Move the cursor forward for the next bank (leaving a gap)\n  set i (i + 3)\n]\n\n"
PENS
"Illiquid" 1.0 1 -16777216 true "" ""
"Interbank" 1.0 1 -2139308 true "" ""
"Liquid" 1.0 1 -5825686 true "" ""
"Deposits" 1.0 1 -2674135 true "" ""

@#$#@#$#@
# Ce este?

Scopul modelului este acela de a studia propagarea efectului de contagiune financiară în rețeaua bancară specifică României, fiind bazat pe modelul propus de către Gai și Kapadia (2010).
 
Modelul reușește să cuprindă reglementările bancare existente la nivel național, prin actele normative deja în vigoare, cât și la nivel european **_(BRRD)_**, sau la nivel global **_(Basel III)_**. Astfel, pentru o perspectivă holistică a propagării efectului de contagiune financiară, modelul împletește mecanisme responsabile pentru asigurarea rezilienței bancare cu instrumentele de rezoluție bancară, în eventualitatea în care o instituție bancară se regăsește într-o criză de lichiditate sau într-o situație de dificultate majoră. 

Spre deosebire de modelele existente în literatura de specialitate, acest model introduce elemente distinctive, precum: **comportamentul deponenților, rata dobânzii, valoarea creditelor, maturizarea activelor cu lichiditate redusă, depozitele asigurate sau neasigurate, mecanisme de recapitalizare**_(bail-in)_ sau **intervenția Autorității de Rezoluție**_(Banca Națională a României)_.

# Descrierea modelului

## Scop

Scopul modelului este descris de către investigarea gradului de reziliență al sistemului bancar românesc, alături de evaluarea eficienței mecanismelor și instrumentelor de reziliență bancară, implementate de către fiecare actor din rețeaua bancară din România.

## Variabile

Valorile variabilelor modelului dictează testarea ipotezei propuse, întrucât acestea influențează în mod direct valorile proprietăților agenților, dar și modul în care actorii sistemului bancar vor interacționa între ei. Astfel, variabilele modelului bazat pe agenți sunt:

* **numărul de bănci**: Orice valoare în intervalul _[1; 32]_;
* **conectivitatea fiecărui agent de tip instituție bancară**: Orice valoare în intervalul _[1; număr de bănci]_;
* **agentul care declanșează efectul de contagiune financiară**: _Orice instituție financiar bancară, cea cu cifra de afaceri minimă_ sau _cea cu cifra de afaceri maximă_;
* **rata de referință a depozitelor retrase**: Orice valoare în intervalul _[0; 10]_;
* **rata depozitelor retrase în situație de panică**: Orice valoare în intervalul _[0; 100]_;
* **creditele pe termen scurt raportate la creditele pe termen lung**: Orice valoare în intervalul _[0; 100]_;
* **media și abaterea distribuției logaritmice a instituțiilor**: Orice valoare zecimală, _mai mare sau egală decât 0_;
* **inițializarea modelului bazat pe agenți**;
* **declanșarea simulării modelului bazat pe agenți**.

## Tipuri de agenți și proprietățile acestora

### Băncile

Instituțiile bancare reprezintă principalul tip de agenți ai modelului propus. Având în vedere mecanismele și instrumentele impuse de către reglementările naționale, Basel III și BRRD, proprietățile agenților sunt:

* **dimensiunea băncii**: Pune în evidență dimensiunea sau importanța băncii în cadrul rețelei bancare. Valorile posibile sunt descrise de către distribuția logaritmică de _**medie mu**_ și **_abatere sigma_**;
* **stare**: Descrie starea curentă în care instituția financiar bancară se regăsește. Valorile posibile sunt: _stare normală_, _criză de lichiditate_ sau _insolvență_;
* **active interbancare**: Exprimă valoarea creditelor acordate sau a activelor interbancare. Valorile posibile sunt: _0%_ sau _20%_ din **dimensiunea băncii**;
* **active cu lichiditate redusă**: Exprimă valoarea activelor cu durată mare de lichiditate. Valorile posibile sunt: _50%_ sau _70%_ din **dimensiunea băncii**;
* **active lichide**: Exprimă valoarea activelor lichide. Valoarea posibilă este de _30%_ din **dimensiunea băncii**;
* **pasive interbancare**: Exprimă valoarea creditelor contractate a pasivelor interbancare. Valorile posibile sunt cuprinse în intervalul _[0; sumă credite contractate]_;
* **depozite totale**: Marchează totalul depozitelor clienților. Valorile posibile sunt: _0_ sau _dimensiunea băncii – pasive interbancare_;
* **depozite neasigurate ale companiilor de dimensiuni mici și mijlocii**: Valoarea este de _10%_ din **totalul depozitelor clienților**;
* **depozite neasigurate ale companiilor de dimensiuni mari**: Valoarea este de _1%_ din **totalul depozitelor clienților**;
* **depozite asigurate**: Reprezintă totalul valorii depozitelor garantate. Valoarea este de _89%_ din **totalul depozitelor clienților**;
* **capitalurile proprii**: Valoarea este de _8%_ din **_totalul activelor instituției bancare_**;
* **grad maxim de conectivitate**: Reprezintă conectivitatea maximă pe care acest actor bancar o poate atinge. Valorile posibile sunt cuprinse în intervalul _[0; 32]_;
* **grad maxim de conectivitate atins**: Valorile posibile sunt _adevărat_ sau _fals_;
* **total arce de intrare și ieșire**: Valorile posibile sunt cuprinse În intervalul _[0; 32]_;
* **rata dobânzii**: Colecție de tip cheie-valoare cu valori posibile între _short - [1; 3]%_ sau _long - [3; 4]%_;
* **venituri din rata dobânzii**: Marchează cu cât vor crește capitalurile proprii în urma creditelor acordate;
* **cheltuieli cu rata dobânzii**: Descrie cu cât vor scădea capitalurile proprii în urma creditelor contractate.

### Arcele

Al doilea tip de agent este descris de către arcele de intrare și de ieșire ale fiecărei instituții bancare, întrucât acestea sunt responsabile pentru crearea interdependenței dintre actorii rețelei bancare. Așadar, proprietățile arcelor sunt exprimate de către rândurile de mai jos:

* **valoarea creditului**: Valorile posibile sunt cuprinse în intervalul _(0; dimensiunea băncii)_;
* **tipul creditului acordat**: Valorile posibile sunt: _termen scurt_ sau _termen lung_;
* **rata dobânzii**: Valorile posibile sunt cuprinse în intervalul _[1; 4]%_;
* **poate fi vândut**: Semnifică dacă acest portofoliu de active, constituind creditele acordate, poate fi vândut de către banca creditoare. Valorile posibile sunt: _adevărat_ sau _fals_.

## Reguli de interacțiune per iterație

În cadrul fiecărei iterații, actorii sistemului bancar vor acționa în funcție de un set specific de reguli, având în vedere starea în care se află, fiind totodată impactați și de către rata de retragere a depozitelor clienților. Aceste reguli sunt menite să urmărească nu doar reziliența rețelei bancare, ci și reziliența fiecărei instituții bancare parte a acestui sistem. Așadar, regulile de interacțiune sunt descrise de către pașii următori:

  1. Proces de auditare intern orientat asupra fluxurilor financiare, în urma căruia, starea băncii poate fi schimbată;

  2. Tranziția unei valori empirice de _2% din totalul activelor nelichide_ către _active lichide_;

  3. Retragerea depozitelor clienților:

    i) Dacă instituția bancară **nu este în vecinătatea unei bănci ce se află în default**, depozitele vor fi retrase având în vedere _rata de referință a depozitelor retrase_;
    ii) Dacă instituția bancară **este în vecinătatea unei bănci ce se află în default**, depozitele vor fi retrase luând în considerare _rata depozitelor retrase în situație de panică_.

  4. Dacă instituția financiar bancară este expusă unui risc de lichiditate redusă, aceasta va tranzacționa activele interbancare în vederea creșterii lichidității la un preț mai mic decât valoarea arcului, înregistrând pierderile, cât și câștigurile de lichidități. Arcul dintre instituția bancară ce va vinde o parte din portofoliul de active și instituția debitoare va fi marcat prin culoarea **galben**. 
În mod reciproc, bilanțul contabil al instituției bancare dispusă să achiziționeze o parte din portofoliul de active al celei expuse riscului de lichiditate redusă va înregistra pierderi ale activelor lichide, dar o creștere mai mare a capitalurilor proprii. Un nou arc va fi creat între instituția ce a achiziționat o parte din portofoliul de active al băncii ce se află în criză de lichiditate și banca debitoare inițială, acest arc fiind reprezentat prin culoarea **verde**.

  5. Dacă instituția bancară este într-o situație de dificultate majoră:

     i) Aplicarea primului mecanism de recapitalizare - creditorii vor suferi pierderi egal proporționale; Capitalurile proprii vor crește;
    ii) Aplicarea celui de-al doilea mecanism de recapitalizare, dacă primul nu a fost suficient pentru a asigura viabilitatea - depozitele neasigurate vor suferi pierderi:
      a) Depozitele neasigurate ale companiilor de mari dimensiuni vor înregistra pierderi pentru asigurarea unui bilanț contabil pozitiv;
      b) Dacă depozitele neasigurate ale companiilor de mari dimensiuni nu au fost suficiente pentru asigurarea viabilității instituției bancare, depozitele companiilor de dimensiuni mici și mijlocii vor înregistra pierderi în pofida creșterii capitalurilor proprii.
    iii) Intervenția Autorității de Rezoluție bancară prin intervenția Fondului de Rezoluție Bancară, sub următoarele condiții:
      a) Există fonduri suficiente;
      b) Mecanismele de recapitalizare descrise de către punctele i) și ii) au absorbit minim _8%_ din totalul pasivelor;
      c) Maxim _5%_ din totalul pasivelor și al capitalurilor proprii pot fi acoperite.

  6. Re-evaluarea stării instituției bancare:

    i) Dacă instituția bancară este în _stare normală_:
      a) **banca** va fi reprezentată prin culoarea **albastră**; 
      b) **arcele** vor fi marcate prin culoarea **gri**.

    ii) Dacă instituția bancară se regăsește într-o stare de _criză de lichiditate_:
      a) **banca** își va schimba culoarea în **portocaliu**;
      b) **banca** își va schimba starea în **criză de lichiditate**;
      c) **arcele orientate de intrare** vor fi exprimate prin culoarea **portocalie**;
      d) **creanțele interbancare** deținute de către băncile creditoare, descrise prin arcele de intrare, **nu vor mai putea fi tranzacționate** de către acestea.


    iii) Dacă instituția bancară se regăsește într-o stare de _insolvabilitate_:
      a) **banca** își va schimba culoarea în **roșu**;
      b) **banca** își va schimba starea în **defaulte**;
      c) **arcele orientate de intrare** vor fi exprimate prin culoarea **roșie**;
      d) **instituțiile creditoare** vor înregistra **pierderea**.
### Băncile.

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
