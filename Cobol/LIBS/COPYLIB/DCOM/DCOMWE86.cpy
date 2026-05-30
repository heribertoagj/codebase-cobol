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
       05 DCOMW86E-HEADER.
          10 DCOMW86E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW86E'.
          10 DCOMW86E-TAM-LAYOUT       PIC  9(005) VALUE 00450.
      *
       05 DCOMW86E-BLOCO-ENTRADA.
          10 DCOMW86E-INSTRUCAO                 PIC  X(010).
      *
          10 DCOMW86E-ENTRADA.
             15 DCOMW86E-CTPO-TX-JURO-DESC      PIC S9(003)V COMP-3.
             15 DCOMW86E-ITPO-TX-JURO-DESC      PIC  X(030).
             15 DCOMW86E-IRSUMO-TPO-TX-JURO     PIC  X(015).
             15 DCOMW86E-CSGL-TPO-TX-JURO       PIC  X(006).
             15 DCOMW86E-CIDTFD-TX-JURO         PIC S9(005)V COMP-3.
             15 DCOMW86E-HULT-ATULZ             PIC  X(026).
             15 DCOMW86E-CFUNC-BDSCO            PIC S9(009)V COMP-3.
             15 DCOMW86E-CTERM                  PIC  X(008).
      *
          10 DCOMW86E-BLOCO-VARIAVEL.
             15 DCOMW86E-V-FILLER               PIC  X(300).
      *
          10 DCOMW86E-BLOCO-PAGINACAO.
             15 DCOMW86E-MAX-OCORR              PIC  9(003).
             15 DCOMW86E-INDICADOR-PAGINACAO    PIC  X(001).
                88 DCOMW86E-P-INICIAL           VALUE 'I'.
                88 DCOMW86E-P-PRIMEIRA          VALUE 'P'.
                88 DCOMW86E-P-SEGUINTE          VALUE 'S'.
                88 DCOMW86E-P-ANTERIOR          VALUE 'A'.
                88 DCOMW86E-P-ULTIMA            VALUE 'U'.
      *
          10 DCOMW86E-BLOCO-RESTART.
             15 DCOMW86E-CHAVE-INI.
                20 DCOMW86E-I-CTPO-TX-JURO-DESC PIC S9(003)V COMP-3.
             15 DCOMW86E-CHAVE-FIM.
                20 DCOMW86E-F-CTPO-TX-JURO-DESC PIC S9(003)V COMP-3.
      *
          10 DCOMW86E-FILLER                    PIC  X(024).
      *
