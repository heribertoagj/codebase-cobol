      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB0H1: DB2PRD.TCONTR_LIM_ROTRO                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4H1S                                       *
      *    -> FETCH   - DCOM4H1F                                       *
      *    -> INSERT  - DCOM4H1I                                       *
      *    -> UPDATE  - DCOM4H1U                                       *
      *    -> DELETE  - DCOM4H1D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWH1S-HEADER.
          10 DCOMWH1S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWH1S'.
          10 DCOMWH1S-TAM-LAYOUT       PIC  9(005) VALUE 04526.
      *
          10 DCOMWH1S-BLOCO-SAIDA.
             15 DCOMWH1S-QTDE-TOTAL                PIC  9(010).
             15 DCOMWH1S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMWH1S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMWH1S-CCONTR-LIM-DESC        PIC  S9(009) COMP-3.
                20 DCOMWH1S-CVRSAO-CONTR-LIM       PIC  S9(003) COMP-3.
                20 DCOMWH1S-CTPO-AGPTO-ROTRO       PIC  S9(003) COMP-3.
                20 DCOMWH1S-CINDCD-OBRIG-AGPTO     PIC  X(001).
                20 DCOMWH1S-CINDCD-SIT-AGPTO       PIC  X(001).
                20 DCOMWH1S-CINDCD-CONCL-ROTRO     PIC  X(001).
                20 DCOMWH1S-DVALDC-ROTRO-ELMTO     PIC  X(010).
                20 DCOMWH1S-HULT-ATULZ             PIC  X(026).
                20 DCOMWH1S-CFUNC-BDSCO            PIC  S9(009) COMP-3.
                20 DCOMWH1S-CTERM                  PIC  X(008).
                20 DCOMWH1S-FILLER                 PIC  X(029).
      *                                                                *
      *================================================================*
