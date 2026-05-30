      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB0E9: DB2PRD.TSPROD_CRITR_REDE                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4E9S                                       *
      *    -> FETCH   - DCOM4E9F                                       *
      *    -> INSERT  - DCOM4E9I                                       *
      *    -> UPDATE  - DCOM4E9U                                       *
      *    -> DELETE  - DCOM4E9D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWE9E-HEADER.
          10 DCOMWE9E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWE9E'.
          10 DCOMWE9E-TAM-LAYOUT       PIC  9(005) VALUE 01000.
      *
       05 DCOMWE9E-BLOCO-ENTRADA.
          10 DCOMWE9E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMWE9E-ENTRADA.
             15 DCOMWE9E-CPRODT                    PIC S9(003) COMP-3.
             15 DCOMWE9E-CSPROD-DESC-COML          PIC S9(003) COMP-3.
             15 DCOMWE9E-CINDCD-PSSOA-TOMAD        PIC  X(001).
             15 DCOMWE9E-CSGMTO-CLI                PIC S9(003) COMP-3.
             15 DCOMWE9E-CRTING                    PIC  X(003).
             15 DCOMWE9E-NNVEL-RTING               PIC S9(002) COMP-3.
             15 DCOMWE9E-HINIC-VGCIA-NVEL          PIC  X(026).
             15 DCOMWE9E-CMEIO-ENTRD-DESC          PIC S9(003) COMP-3.
             15 DCOMWE9E-DINIC-CRITR-REDE          PIC  X(010).
             15 DCOMWE9E-DFIM-CRITR-TX-REDE        PIC  X(010).
             15 DCOMWE9E-CIDTFD-TX-MAX-REDE        PIC S9(005) COMP-3.
             15 DCOMWE9E-CTERM                     PIC  X(008).
             15 DCOMWE9E-CFUNC-BDSCO               PIC S9(009) COMP-3.
             15 DCOMWE9E-HULT-ATULZ                PIC  X(026).

          10 DCOMWE9E-BLOCO-PAGINACAO.
             15 DCOMWE9E-MAX-OCORR                 PIC  9(003).
             15 DCOMWE9E-TOTALIZAR                 PIC  X(001).
             15 DCOMWE9E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWE9E-P-INICIAL              VALUE 'I'.
                88 DCOMWE9E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWE9E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWE9E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWE9E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMWE9E-BLOCO-RESTART.
             15 DCOMWE9E-CHAVE-INI.
                20 DCOMWE9E-I-CPRODT               PIC S9(003) COMP-3.
                20 DCOMWE9E-I-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMWE9E-I-CINDCD-PSSOA-TOMAD   PIC  X(001).
                20 DCOMWE9E-I-CSGMTO-CLI           PIC S9(003) COMP-3.
                20 DCOMWE9E-I-CRTING               PIC  X(003).
                20 DCOMWE9E-I-NNVEL-RTING          PIC S9(002) COMP-3.
                20 DCOMWE9E-I-HINIC-VGCIA-NVEL     PIC  X(026).
                20 DCOMWE9E-I-CMEIO-ENTRD-DESC     PIC S9(003) COMP-3.
                20 DCOMWE9E-I-DINIC-CRITR-REDE     PIC  X(010).
                20 FILLER                          PIC  X(251).
             15 DCOMWE9E-CHAVE-FIM.
                20 DCOMWE9E-F-CPRODT               PIC S9(003) COMP-3.
                20 DCOMWE9E-F-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMWE9E-F-CINDCD-PSSOA-TOMAD   PIC  X(001).
                20 DCOMWE9E-F-CSGMTO-CLI           PIC S9(003) COMP-3.
                20 DCOMWE9E-F-CRTING               PIC  X(003).
                20 DCOMWE9E-F-NNVEL-RTING          PIC S9(002) COMP-3.
                20 DCOMWE9E-F-HINIC-VGCIA-NVEL     PIC  X(026).
                20 DCOMWE9E-F-CMEIO-ENTRD-DESC     PIC S9(003) COMP-3.
                20 DCOMWE9E-F-DINIC-CRITR-REDE     PIC  X(010).
                20 FILLER                          PIC  X(251).
      *
          10 DCOMWE9E-FILLER                       PIC  X(268).
      *
