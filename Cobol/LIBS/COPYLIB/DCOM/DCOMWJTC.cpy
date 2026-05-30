      ******************************************************************
      * DESCRICAO  : CALCULAR JUROS, TAXAS E TARIFAS DO TITULO         *
      * COPYBOOK   : DCOMWJTC - BOOK DE ACESSO AO PROGRAMA DCOM3JTC    *
      * FUNCIONAL  : DCOM3JTC - CALCULAR JUROS, TAXAS E TARIFAS        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      ******************************************************************
          05 DCOMWJTC-HEADER.
             10 DCOMWJTC-COD-LAYOUT    PIC X(008) VALUE 'DCOMWJTC'.
             10 DCOMWJTC-TAM-LAYOUT    PIC 9(005) VALUE 0500.
      *
          05 DCOMWJTC-REGISTRO.
             10 DCOMWJTC-BLOCO-ENTRADA.
                15 DCOMWJTC-E-CCANAL              PIC  9(003).
                15 DCOMWJTC-E-CPRODT              PIC  9(003).
                15 DCOMWJTC-E-CSPROD-DESC-COML    PIC  9(003).
                15 DCOMWJTC-E-DINIC-OPER-DESC.
                   20 DCOMWJTC-E-DIA-INIC-OPER    PIC  9(002).
                   20 FILLER                      PIC  X(001).
                   20 DCOMWJTC-E-MES-INIC-OPER    PIC  9(002).
                   20 FILLER                      PIC  X(001).
                   20 DCOMWJTC-E-ANO-INIC-OPER    PIC  9(004).
                15 DCOMWJTC-E-DVCTO-OPER-DESC.
                   20 DCOMWJTC-E-DIA-VCTO-OPER    PIC  9(002).
                   20 FILLER                      PIC  X(001).
                   20 DCOMWJTC-E-MES-VCTO-OPER    PIC  9(002).
                   20 FILLER                      PIC  X(001).
                   20 DCOMWJTC-E-ANO-VCTO-OPER    PIC  9(004).
                15 DCOMWJTC-E-PTX-JURO-MES        PIC  9(003)V9(005).
                15 DCOMWJTC-E-PTX-JURO-ANO        PIC  9(003)V9(005).
                15 DCOMWJTC-E-VLR-OPER-DESC       PIC  9(015)V9(002).
                15 DCOMWJTC-E-CINDCD-PSSOA-TOMAD  PIC  X(001).
                15 DCOMWJTC-E-CTPO-RAMO-ATVDD     PIC  9(003).
                15 DCOMWJTC-E-CPORTE-EMPR         PIC  9(003).
                15 DCOMWJTC-E-CNATUZ-JURID        PIC  9(003).
                15 DCOMWJTC-E-CINDCD-INCID-IOF    PIC  X(001).
                15 DCOMWJTC-E-CINDCD-CALC-JURO    PIC  X(001).
                15 DCOMWJTC-E-CINDCD-FREQ-TX      PIC  X(002).
                15 DCOMWJTC-E-FLAG-FUNCAO         PIC  X(001).
                15 DCOMWJTC-E-FLAG-RED-IOF        PIC  X(001).
                15 DCOMWJTC-E-FLAG-ADIC-IOF       PIC  X(001).
                15 DCOMWJTC-E-FLAG-CALC-CUSTO     PIC  X(001).
                15 DCOMWJTC-E-TMED-PONDE-OPER     PIC  9(003).
                15 DCOMWJTC-E-VOPER-DESC-APURA    PIC  9(015)V9(002).
                15 DCOMWJTC-E-PALIQT-IOF-DIA      PIC  9(005)V9(005).
                15 DCOMWJTC-E-CINDCD-ALIQT-REDZD  PIC  X(001).
                15 DCOMWJTC-E-PALIQT-IOF-REDZD    PIC  9(005)V9(005).
                15 DCOMWJTC-E-CINDCD-FREQ-ALIQT   PIC  X(002).
                15 DCOMWJTC-E-PTX-NTAX            PIC  9(008)V9(008).
                15 DCOMWJTC-E-CINDCD-FREQ-NTAX    PIC  X(002).
                15 DCOMWJTC-E-DATA-FIM-OPER       PIC  X(010).
                15 DCOMWJCT-E-FILLER              PIC  X(050).
      *
             10 DCOMWJTC-BLOCO-SAIDA.
                15 DCOMWJTC-S-VJURO-OPER-DESC     PIC  9(015)V9(002).
                15 DCOMWJTC-S-VJDIA-OPER-DESC     PIC  9(015)V9(002).
                15 DCOMWJTC-S-VIOF-OPER-DESC      PIC  9(015)V9(002).
                15 DCOMWJTC-S-PALIQT-IOF-DESC     PIC  9(005)V9(005).
                15 DCOMWJTC-S-CINDCD-ALIQT-REDZD  PIC  X(001).
                15 DCOMWJTC-S-PALIQT-IOF-REDZD    PIC  9(005)V9(005).
                15 DCOMWJTC-S-PTX-CUSTO-MES       PIC  9(005)V9(005).
                15 DCOMWJTC-S-PTX-CUSTO-ANO       PIC  9(005)V9(005).
                15 DCOMWJTC-S-VCUSTO-PCELA-DESC   PIC  9(015)V9(002).
                15 DCOMWJTC-S-PALIQT-IOF-DIA      PIC  9(005)V9(005).
                15 DCOMWJTC-S-CINDCD-FREQ-ALIQT   PIC  X(002).
                15 DCOMWJTC-S-PTX-NTAX            PIC  9(008)V9(008).
                15 DCOMWJTC-S-CINDCD-FREQ-NTAX    PIC  X(002).
                15 DCOMWJTC-S-VCUSTO-DIA          PIC  9(015)V9(002).
                15 DCOMWJCT-S-FILLER              PIC  X(130).
      *
