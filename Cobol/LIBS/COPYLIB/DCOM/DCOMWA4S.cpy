      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB0A4: DB2PRD.TGARNT_CONTR_LIM                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4A4S                                       *
      *    -> FETCH   - DCOM4A4F                                       *
      *    -> INSERT  - DCOM4A4I                                       *
      *    -> UPDATE  - DCOM4A4U                                       *
      *    -> DELETE  - DCOM4A4D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWA4S-HEADER.
          10 DCOMWA4S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWA4S'.
          10 DCOMWA4S-TAM-LAYOUT       PIC  9(005) VALUE 04526.
      *
          10 DCOMWA4S-BLOCO-SAIDA.
             15 DCOMWA4S-QTDE-TOTAL                PIC  9(010).
             15 DCOMWA4S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMWA4S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMWA4S-CCONTR-LIM-DESC        PIC  S9(009) COMP-3.
                20 DCOMWA4S-CVRSAO-CONTR-LIM       PIC  S9(003) COMP-3.
                20 DCOMWA4S-CTPO-GARNT             PIC  S9(003) COMP-3.
                20 DCOMWA4S-CTERM                  PIC  X(008).
                20 DCOMWA4S-CFUNC-BDSCO            PIC  S9(009) COMP-3.
                20 DCOMWA4S-HULT-ATULZ             PIC  X(026).
                20 DCOMWA4S-FILLER                 PIC  X(042).
      *                                                                *
      *================================================================*
