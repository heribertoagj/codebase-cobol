      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB072: DB2PRD.TSPROD_CRITR_REDE                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4E9S                                       *
      *    -> FETCH   - DCOM4E9F                                       *
      *    -> INSERT  - DCOM4E9I                                       *
      *    -> UPDATE  - DCOM4E9U                                       *
      *    -> DELETE  - DCOM4E9D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWE9S-HEADER.
          10 DCOMWE9S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWE9S'.
          10 DCOMWE9S-TAM-LAYOUT       PIC  9(005) VALUE  05500.
      *
          10 DCOMWE9S-BLOCO-SAIDA.
             15 DCOMWE9S-QTDE-TOTAL                PIC S9(010) COMP-3.
             15 DCOMWE9S-QTDE-RETORNADA            PIC S9(003) COMP-3.
             15 DCOMWE9S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMWE9S-CPRODT                 PIC S9(003) COMP-3.
                20 DCOMWE9S-CSPROD-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWE9S-CINDCD-PSSOA-TOMAD     PIC  X(001).
                20 DCOMWE9S-CSGMTO-CLI             PIC S9(003) COMP-3.
                20 DCOMWE9S-CRTING                 PIC  X(003).
                20 DCOMWE9S-NNVEL-RTING            PIC S9(002) COMP-3.
                20 DCOMWE9S-HINIC-VGCIA-NVEL       PIC  X(026).
                20 DCOMWE9S-CMEIO-ENTRD-DESC       PIC S9(003) COMP-3.
                20 DCOMWE9S-DINIC-CRITR-REDE       PIC  X(010).
                20 DCOMWE9S-DFIM-CRITR-TX-REDE     PIC  X(010).
                20 DCOMWE9S-CIDTFD-TX-MAX-REDE     PIC S9(005) COMP-3.
                20 DCOMWE9S-CTERM                  PIC  X(008).
                20 DCOMWE9S-CFUNC-BDSCO            PIC S9(009) COMP-3.
                20 DCOMWE9S-HULT-ATULZ             PIC  X(026).
      *
             15 DCOMWE9S-FILLER                    PIC  X(379).
