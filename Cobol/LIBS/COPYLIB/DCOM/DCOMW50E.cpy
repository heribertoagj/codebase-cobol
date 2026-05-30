      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB050: DB2PRD.TREGRA_IOF_DESC                         *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM450S                                       *
      *    -> FETCH   - DCOM450F                                       *
      *    -> INSERT  - DCOM450I                                       *
      *    -> UPDATE  - DCOM450U                                       *
      *    -> DELETE  - DCOM450D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW50E-HEADER.
          10 DCOMW50E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW50E'.
          10 DCOMW50E-TAM-LAYOUT       PIC  9(005) VALUE 00600.
      *
       05 DCOMW50E-BLOCO-ENTRADA.
          10 DCOMW50E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMW50E-ENTRADA.
             15 DCOMW50E-CPRODT                    PIC S9(003) COMP-3.
             15 DCOMW50E-CREGRA-IOF-DESC           PIC S9(003) COMP-3.
             15 DCOMW50E-DINIC-REGRA-IOF           PIC  X(010).
             15 DCOMW50E-IREGRA-IOF-DESC           PIC  X(040).
             15 DCOMW50E-CINDCD-PSSOA-TOMAD        PIC  X(001).
             15 DCOMW50E-CFORMA-CNSTT-PJ           PIC S9(002) COMP-3.
             15 DCOMW50E-CTPO-RAMO-ATVDD           PIC S9(003) COMP-3.
             15 DCOMW50E-CPORTE-EMPR               PIC S9(003) COMP-3.
             15 DCOMW50E-CINDCD-LOGCO-VLR          PIC  X(002).
             15 DCOMW50E-VREFT-OPER-DESC           PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMW50E-CINDCD-LOGCO-PRZ          PIC  X(002).
             15 DCOMW50E-TREFT-OPER-DESC           PIC S9(003) COMP-3.
             15 DCOMW50E-PALIQT-IOF-DESC           PIC S9(003)V9(005)
                                                               COMP-3.
             15 DCOMW50E-CINDCD-FREQ-ALIQT         PIC  X(002).
             15 DCOMW50E-DFIM-REGRA-IOF            PIC  X(010).
             15 DCOMW50E-HULT-ATULZ                PIC  X(026).
             15 DCOMW50E-CFUNC-BDSCO               PIC S9(009) COMP-3.
             15 DCOMW50E-CTERM                     PIC  X(008).
             15 DCOMW50E-CINDCD-ALIQT-REDZD        PIC  X(001).
             15 DCOMW50E-PALIQT-IOF-REDZD          PIC S9(003)V9(005)
                                                               COMP-3.
             15 DCOMW50E-CFREQ-ALIQT-REDZD         PIC  X(002).
             15 DCOMW50E-DSIT-REGRA-IOF            PIC  X(010).
             15 DCOMW50E-CSIT-REGRA-IOF            PIC S9(001) COMP-3.
             15 DCOMW50E-CTPO-ACAO-REGRA           PIC  X(001).
             15 DCOMW50E-FILLER                    PIC  X(025).
      *
          10 DCOMW50E-BLOCO-VARIAVEL.
             15 DCOMW50E-V-FILLER                  PIC  X(300).
      *
          10 DCOMW50E-BLOCO-PAGINACAO.
             15 DCOMW50E-MAX-OCORR                 PIC  9(003).
             15 DCOMW50E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW50E-P-INICIAL              VALUE 'I'.
                88 DCOMW50E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW50E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW50E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW50E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMW50E-BLOCO-RESTART.
             15 DCOMW50E-CHAVE-INI.
                20 DCOMW50E-I-CPRODT               PIC S9(003) COMP-3.
                20 DCOMW50E-I-CREGRA-IOF-DESC      PIC S9(003) COMP-3.
                20 DCOMW50E-I-DINIC-REGRA-IOF      PIC  X(010).
                20 DCOMW50E-I-FILLER               PIC  X(014).
             15 DCOMW50E-CHAVE-FIM.
                20 DCOMW50E-F-CPRODT               PIC S9(003) COMP-3.
                20 DCOMW50E-F-CREGRA-IOF-DESC      PIC S9(003) COMP-3.
                20 DCOMW50E-F-DINIC-REGRA-IOF      PIC  X(010).
                20 DCOMW50E-F-FILLER               PIC  X(014).
      *
          10 DCOMW50E-FILLER                       PIC  X(040).
      *
