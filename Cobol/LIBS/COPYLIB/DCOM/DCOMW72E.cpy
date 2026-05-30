      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB072: DB2PRD.TSPROD_TX_JURO                          *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM472S                                       *
      *    -> FETCH   - DCOM472F                                       *
      *    -> INSERT  - DCOM472I                                       *
      *    -> UPDATE  - DCOM472U                                       *
      *    -> DELETE  - DCOM472D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW72E-HEADER.
          10 DCOMW72E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW72E'.
          10 DCOMW72E-TAM-LAYOUT       PIC  9(005) VALUE 01000.
      *
       05 DCOMW72E-BLOCO-ENTRADA.
          10 DCOMW72E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMW72E-ENTRADA.
             15 DCOMW72E-CPRODT              PIC S9(003) COMP-3.
             15 DCOMW72E-CSPROD-DESC-COML    PIC S9(003) COMP-3.
             15 DCOMW72E-HINIC-TX-SPROD      PIC  X(026).
             15 DCOMW72E-CTPO-TX-JURO-DESC   PIC S9(003) COMP-3.
             15 DCOMW72E-HFIM-TX-SPROD       PIC  X(026).
             15 DCOMW72E-HULT-ATULZ          PIC  X(026).
             15 DCOMW72E-CFUNC-BDSCO         PIC S9(009) COMP-3.
             15 DCOMW72E-CTERM               PIC  X(008).
             15 DCOMW72E-QDIA-OPER-ATIVO     PIC S9(004) COMP-3.
             15 DCOMW72E-CINDCD-FREQ-ESTOQ   PIC  X(002).
             15 DCOMW72E-CINDCD-OPER-ATRSO   PIC  X(001).

          10 DCOMW72E-BLOCO-PAGINACAO.
             15 DCOMW72E-MAX-OCORR                 PIC  9(003).
             15 DCOMW72E-TOTALIZAR                 PIC  X(001).
             15 DCOMW72E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW72E-P-INICIAL              VALUE 'I'.
                88 DCOMW72E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW72E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW72E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW72E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMW72E-BLOCO-RESTART.
             15 DCOMW72E-CHAVE-INI.
                20 DCOMW72E-I-CPRODT               PIC S9(003) COMP-3.
                20 DCOMW72E-I-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMW72E-I-HINIC-TX-SPROD       PIC  X(026).
                20 FILLER                          PIC  X(270).
             15 DCOMW72E-CHAVE-FIM.
                20 DCOMW72E-F-CPRODT               PIC S9(003) COMP-3.
                20 DCOMW72E-F-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMW72E-F-HINIC-TX-SPROD       PIC  X(026).
                20 FILLER                          PIC  X(270).
      *
          10 DCOMW72E-FILLER                       PIC  X(269).
      *
