      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
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
       05 DCOMWH1E-HEADER.
          10 DCOMWH1E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWH1E'.
          10 DCOMWH1E-TAM-LAYOUT       PIC  9(005) VALUE 00578.
      *
       05 DCOMWH1E-BLOCO-ENTRADA.
          10 DCOMWH1E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMWH1E-ENTRADA.
             15 DCOMWH1E-CCONTR-LIM-DESC           PIC  S9(009) COMP-3.
             15 DCOMWH1E-CVRSAO-CONTR-LIM          PIC  S9(003) COMP-3.
             15 DCOMWH1E-CTPO-AGPTO-ROTRO          PIC  S9(003) COMP-3.
             15 DCOMWH1E-CINDCD-OBRIG-AGPTO        PIC  X(001).
             15 DCOMWH1E-CINDCD-SIT-AGPTO          PIC  X(001).
             15 DCOMWH1E-CINDCD-CONCL-ROTRO        PIC  X(001).
             15 DCOMWH1E-DVALDC-ROTRO-ELMTO        PIC  X(010).
             15 DCOMWH1E-HULT-ATULZ                PIC  X(026).
             15 DCOMWH1E-CFUNC-BDSCO               PIC  S9(009) COMP-3.
             15 DCOMWH1E-CTERM                     PIC  X(008).
             15 DCOMWH1E-FILLER                    PIC  X(029).
      *
          10 DCOMWH1E-BLOCO-VARIAVEL.
             15 DCOMWH1E-V-FILLER                  PIC  X(300).
      *
          10 DCOMWH1E-BLOCO-PAGINACAO.
             15 DCOMWH1E-MAX-OCORR                 PIC  9(003).
             15 DCOMWH1E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWH1E-P-INICIAL              VALUE 'I'.
                88 DCOMWH1E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWH1E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWH1E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWH1E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMWH1E-BLOCO-RESTART.
             15 DCOMWH1E-CHAVE-INI.
                20 DCOMWH1E-I-CCONTR-LIM-DESC      PIC  S9(009) COMP-3.
                20 DCOMWH1E-I-CVRSAO-CONTR-LIM     PIC  S9(003) COMP-3.
                20 DCOMWH1E-I-CTPO-AGPTO-ROTRO     PIC  S9(003) COMP-3.
                20 DCOMWH1E-I-FILLER               PIC  X(015).
             15 DCOMWH1E-CHAVE-FIM.
                20 DCOMWH1E-F-CCONTR-LIM-DESC      PIC  S9(009) COMP-3.
                20 DCOMWH1E-F-CVRSAO-CONTR-LIM     PIC  S9(003) COMP-3.
                20 DCOMWH1E-F-CTPO-AGPTO-ROTRO     PIC  S9(003) COMP-3.
                20 DCOMWH1E-F-FILLER               PIC  X(015).
      *
          10 DCOMWH1E-FILLER                       PIC  X(113).
      *
