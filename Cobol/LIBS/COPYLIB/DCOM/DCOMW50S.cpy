      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
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
       05 DCOMW50S-HEADER.
          10 DCOMW50S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW50S'.
          10 DCOMW50S-TAM-LAYOUT       PIC  9(005) VALUE 07650.
      *
          10 DCOMW50S-BLOCO-SAIDA.
             15 DCOMW50S-QTDE-TOTAL                PIC  9(010).
             15 DCOMW50S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMW50S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMW50S-CPRODT                 PIC S9(003) COMP-3.
                20 DCOMW50S-CREGRA-IOF-DESC        PIC S9(003) COMP-3.
                20 DCOMW50S-DINIC-REGRA-IOF        PIC  X(010).
                20 DCOMW50S-IREGRA-IOF-DESC        PIC  X(040).
                20 DCOMW50S-CINDCD-PSSOA-TOMAD     PIC  X(001).
                20 DCOMW50S-CFORMA-CNSTT-PJ        PIC S9(002) COMP-3.
                20 DCOMW50S-CTPO-RAMO-ATVDD        PIC S9(003) COMP-3.
                20 DCOMW50S-CPORTE-EMPR            PIC S9(003) COMP-3.
                20 DCOMW50S-CINDCD-LOGCO-VLR       PIC  X(002).
                20 DCOMW50S-VREFT-OPER-DESC        PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMW50S-CINDCD-LOGCO-PRZ       PIC  X(002).
                20 DCOMW50S-TREFT-OPER-DESC        PIC S9(003) COMP-3.
                20 DCOMW50S-PALIQT-IOF-DESC        PIC S9(003)V9(005)
                                                               COMP-3.
                20 DCOMW50S-CINDCD-FREQ-ALIQT      PIC  X(002).
                20 DCOMW50S-DFIM-REGRA-IOF         PIC  X(010).
                20 DCOMW50S-HULT-ATULZ             PIC  X(026).
                20 DCOMW50S-CFUNC-BDSCO            PIC S9(009) COMP-3.
                20 DCOMW50S-CTERM                  PIC  X(008).
                20 DCOMW50S-CINDCD-ALIQT-REDZD     PIC  X(001).
                20 DCOMW50S-PALIQT-IOF-REDZD       PIC S9(003)V9(005)
                                                               COMP-3.
                20 DCOMW50S-CFREQ-ALIQT-REDZD      PIC  X(002).
                20 DCOMW50S-DSIT-REGRA-IOF         PIC  X(010).
                20 DCOMW50S-CSIT-REGRA-IOF         PIC S9(001) COMP-3.
                20 DCOMW50S-CTPO-ACAO-REGRA        PIC  X(001).
      *                                                                *
          10 DCOMW50S-FILLER                       PIC  X(024).
      *                                                                *
      *================================================================*
