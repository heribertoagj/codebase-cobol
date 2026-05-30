      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB0J5: DB2PRD.TCTRL_NRO_CORP                          *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4J5S                                       *
      *    -> FETCH   - DCOM4J5F                                       *
      *    -> INSERT  - DCOM4J5I                                       *
      *    -> UPDATE  - DCOM4J5U                                       *
      *    -> DELETE  - DCOM4J5D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWJ5S-HEADER.
          10 DCOMWJ5S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWJ5S'.
          10 DCOMWJ5S-TAM-LAYOUT       PIC  9(005) VALUE 02600.
      *
          10 DCOMWJ5S-BLOCO-SAIDA.
             15 DCOMWJ5S-QTDE-TOTAL                PIC  9(010).
             15 DCOMWJ5S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMWJ5S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMWJ5S-CBCO                   PIC S9(003) COMP-3.
                20 DCOMWJ5S-CAG-BCRIA              PIC S9(005) COMP-3.
                20 DCOMWJ5S-CCTA-BCRIA-CLI         PIC S9(013) COMP-3.
                20 DCOMWJ5S-NULT-SEQ-CONTR         PIC S9(018) COMP-3.
                20 DCOMWJ5S-QDIG-SEQ-CONTR         PIC S9(003) COMP-3.
                20 DCOMWJ5S-HULT-ATULZ             PIC  X(026).
      *                                                                *
          10 DCOMWJ5S-FILLER                       PIC  X(074).
      *                                                                *
      *================================================================*
