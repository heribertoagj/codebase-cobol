      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB025: DB2PRD.TEVNTO_ELMTO_DESC                 *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM425S                                       *
      *    -> FETCH   - DCOM425F                                       *
      *    -> INSERT  - DCOM425I                                       *
      *    -> UPDATE  - DCOM425U                                       *
      *    -> DELETE  - DCOM425D                                       *
      *================================================================*
      *
       05 DCOMW25S-HEADER.
          10 DCOMW25S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW25S'.
          10 DCOMW25S-TAM-LAYOUT       PIC  9(005) VALUE 003300.
          10 DCOMW25S-BLOCO-SAIDA.
             15 DCOMW25S-QTDE-TOTAL                PIC  9(010).
             15 DCOMW25S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMW25S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMW25S-CELMTO-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMW25S-CEVNTO-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMW25S-CINDCD-TPO-EVNTO       PIC  X(001).
                20 DCOMW25S-CINDCD-REG-LOG         PIC  X(001).
                20 DCOMW25S-CTBELA-LOG-DESC        PIC  X(008).
                20 DCOMW25S-CCATLG-COMPS-REG       PIC  X(008).
                20 DCOMW25S-NSEQ-PROCM-EVNTO       PIC S9(003) COMP-3.
                20 DCOMW25S-CTRANS-PROG-ATULZ      PIC  X(008).
                20 DCOMW25S-HULT-ATULZ             PIC  X(026).
             15 DCOMW25S-FILLER                    PIC  X(374).
      *                                                                *
      *================================================================*
