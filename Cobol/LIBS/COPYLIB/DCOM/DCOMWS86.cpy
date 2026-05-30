      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB086: DB2PRD.TTPO_TX_JURO_DESC                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM486S                                       *
      *    -> FETCH   - DCOM486F                                       *
      *    -> INSERT  - DCOM486I                                       *
      *    -> UPDATE  - DCOM486U                                       *
      *    -> DELETE  - DCOM486D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW86S-HEADER.
          10 DCOMW86S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW86S'.
          10 DCOMW86S-TAM-LAYOUT       PIC  9(005) VALUE 04800.
      *
       10 DCOMW86S-BLOCO-SAIDA.
          15 DCOMW86S-QTDE-TOTAL                   PIC  9(010).
          15 DCOMW86S-QTDE-RETORNADA               PIC  9(003).
          15 DCOMW86S-OCORRENCIA                   OCCURS 050 TIMES.
             20 DCOMW86S-CTPO-TX-JURO-DESC         PIC S9(003)V COMP-3.
             20 DCOMW86S-ITPO-TX-JURO-DESC         PIC  X(030).
             20 DCOMW86S-IRSUMO-TPO-TX-JURO        PIC  X(015).
             20 DCOMW86S-CSGL-TPO-TX-JURO          PIC  X(006).
             20 DCOMW86S-CIDTFD-TX-JURO            PIC S9(005)V COMP-3.
             20 DCOMW86S-HULT-ATULZ                PIC  X(026).
             20 DCOMW86S-CFUNC-BDSCO               PIC S9(009)V COMP-3.
             20 DCOMW86S-CTERM                     PIC  X(008).
       10 DCOMW86S-FILLER                          PIC  X(024).
      *
