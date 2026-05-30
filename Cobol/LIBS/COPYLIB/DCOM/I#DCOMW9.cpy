      ******************************************************************
      * SISTEMA : DCOM - I#DCOMW9                                      *
      * LAYOUT  : AREA DO MODULO DCOM4495 - CALCULA VALOR DO CUSTO     *
      * CRIACAO : 13/10/2008              ULTIMA ALT: 13/10/2008       *
      ******************************************************************
       01      ENTRADA-ROT-4495.
            05 ROTE-4495-COD-RETORNO          PIC  9(004).
            05 ROTE-4495-MSG-RETORNO          PIC  X(079).
            05 ROTE-4495-RESTART              PIC  9(005).
            05 ROTE-4495-FLAG                 PIC  X(001).
            05 ROTE-4495-CFUNC-BDSCO          PIC  9(009).
            05 ROTE-4495-CTERM                PIC  X(008).
            05 ROTE-4495-CPRODT               PIC  9(003).
            05 ROTE-4495-CSPROD-DESC-COML     PIC  9(003).
            05 ROTE-4495-DINIC-OPER-DESC      PIC  X(010).
            05 ROTE-4495-DVCTO-OPER-DESC      PIC  X(010).
            05 ROTE-4495-PTX-JURO-MES         PIC  9(003)V9(5).
            05 ROTE-4495-PTX-JURO-ANO         PIC  9(003)V9(5).
            05 ROTE-4495-VLR-OPER-DESC        PIC  9(015)V9(2).
            05 ROTE-4495-CINDCD-PSSOA-TOMAD   PIC  X(001).
            05 ROTE-4495-CTPO-RAMO-ATVDD      PIC  9(003).
            05 ROTE-4495-CPORTE-EMPR          PIC  9(003).
            05 ROTE-4495-CNATUZ-JURID         PIC  9(003).
            05 ROTE-4495-CINDCD-INCID-IOF     PIC  X(001).
            05 ROTE-4495-CINDCD-CALC-JURO     PIC  X(001).
            05 ROTE-4495-CINDCD-FREQ-TX       PIC  X(002).
            05 ROTE-4495-FLAG-FUNCAO          PIC  X(001).
            05 ROTE-4495-FLAG-RED-IOF         PIC  X(001).
            05 ROTE-4495-FLAG-ADIC-IOF        PIC  X(001).
            05 ROTE-4495-FLAG-CALC-CUSTO      PIC  X(001).
            05 ROTE-4495-TMED-PONDE-OPER      PIC  9(003).
            05 ROTE-4495-VOPER-DESC-APURA     PIC  9(015)V99.
            05 ROTE-4495-PALIQT-IOF-DIA       PIC  9(005)V9(5).
            05 ROTE-4495-CINDCD-ALIQT-REDZD   PIC  X(001).
            05 ROTE-4495-PALIQT-IOF-REDZD     PIC  9(005)V9(5).
            05 ROTE-4495-CINDCD-FREQ-ALIQT    PIC  X(002).
            05 ROTE-4495-PTX-NTAX             PIC  9(008)V9(08).
            05 ROTE-4495-CINDCD-FREQ-NTAX     PIC  X(002).
            05 FILLER                         PIC  X(100).

       01   SAIDA-ROT-4495.
            05 ROTS-4495-COD-RETORNO        PIC  9(004).
            05 ROTS-4495-MSG-RETORNO        PIC  X(079).
            05 ROTS-4495-RESTART            PIC  9(005).
            05 ROTS-4495-FLAG               PIC  X(001).
            05 ROTS-4495-VJURO-OPER-DESC    PIC  9(015)V9(2).
            05 ROTS-4495-VJDIA-OPER-DESC    PIC  9(015)V9(2).
            05 ROTS-4495-VIOF-OPER-DESC     PIC  9(015)V9(2).
            05 ROTS-4495-PALIQT-IOF-DESC    PIC  9(005)V9(5).
            05 ROTS-4495-CINDCD-ALIQT-REDZD PIC  X(001).
            05 ROTS-4495-PALIQT-IOF-REDZD   PIC  9(005)V9(5).
            05 ROTS-4495-PTX-CUSTO-MES      PIC  9(005)V9(5).
            05 ROTS-4495-PTX-CUSTO-ANO      PIC  9(005)V9(5).
            05 ROTS-4495-VCUSTO-PCELA-DESC  PIC  9(015)V9(2).
            05 ROTS-4495-PALIQT-IOF-DIA     PIC  9(005)V9(5).
            05 ROTS-4495-CINDCD-FREQ-ALIQT  PIC  X(002).
            05 ROTS-4495-PTX-NTAX           PIC  9(008)V9(08).
            05 ROTS-4495-CINDCD-FREQ-NTAX   PIC  X(002).
            05 FILLER                       PIC  X(100).

