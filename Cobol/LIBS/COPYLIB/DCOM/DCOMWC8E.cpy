      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB0C8: DB2PRD.TOPER_ROTRO_INCL                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4C8S                                       *
      *    -> FETCH   - DCOM4C8F                                       *
      *    -> INSERT  - DCOM4C8I                                       *
      *    -> UPDATE  - DCOM4C8U                                       *
      *    -> DELETE  - DCOM4C8D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWC8E-HEADER.
          10 DCOMWC8E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWC8E'.
          10 DCOMWC8E-TAM-LAYOUT       PIC  9(005) VALUE 00500.
      *
       05 DCOMWC8E-BLOCO-ENTRADA.
          10 DCOMWC8E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMWC8E-ENTRADA.
             15 DCOMWC8E-DANO-OPER-DESC            PIC S9(004) COMP-3.
             15 DCOMWC8E-NSEQ-OPER-DESC            PIC S9(009) COMP-3.
             15 DCOMWC8E-CTPO-AGPTO-ROTRO          PIC S9(003) COMP-3.
             15 DCOMWC8E-CINDCD-OBRIG-AGPTO        PIC  X(001).
             15 DCOMWC8E-CINDCD-SIT-AGPTO          PIC  X(001).
             15 DCOMWC8E-CINDCD-CONCL-ROTRO        PIC  X(001).
             15 DCOMWC8E-HULT-ATULZ                PIC  X(026).
             15 DCOMWC8E-CFUNC-BDSCO               PIC S9(009) COMP-3.
             15 DCOMWC8E-CTERM                     PIC  X(008).
             15 DCOMWC8E-FILLER                    PIC  X(030).
      *
          10 DCOMWC8E-BLOCO-VARIAVEL.
             15 DCOMWC8E-V-FILLER                  PIC  X(300).
      *
          10 DCOMWC8E-BLOCO-PAGINACAO.
             15 DCOMWC8E-MAX-OCORR                 PIC  9(003).
             15 DCOMWC8E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWC8E-P-INICIAL              VALUE 'I'.
                88 DCOMWC8E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWC8E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWC8E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWC8E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMWC8E-BLOCO-RESTART.
             15 DCOMWC8E-CHAVE-INI.
                20 DCOMWC8E-I-DANO-OPER-DESC       PIC S9(004) COMP-3.
                20 DCOMWC8E-I-NSEQ-OPER-DESC       PIC S9(009) COMP-3.
                20 DCOMWC8E-I-CTPO-AGPTO-ROTRO     PIC S9(003) COMP-3.
                20 DCOMWC8E-I-FILLER               PIC  X(015).
             15 DCOMWC8E-CHAVE-FIM.
                20 DCOMWC8E-F-DANO-OPER-DESC       PIC S9(004) COMP-3.
                20 DCOMWC8E-F-NSEQ-OPER-DESC       PIC S9(009) COMP-3.
                20 DCOMWC8E-F-CTPO-AGPTO-ROTRO     PIC S9(003) COMP-3.
                20 DCOMWC8E-F-FILLER               PIC  X(015).
      *
          10 DCOMWC8E-FILLER                       PIC  X(041).
      *
