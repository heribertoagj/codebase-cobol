      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB0G9: DB2PRD.TWORKF_OPER_DESC                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4G9S                                       *
      *    -> FETCH   - DCOM4G9F                                       *
      *    -> INSERT  - DCOM4G9I                                       *
      *    -> UPDATE  - DCOM4G9U                                       *
      *    -> DELETE  - DCOM4G9D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWG9E-HEADER.
          10 DCOMWG9E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWG9E'.
          10 DCOMWG9E-TAM-LAYOUT       PIC  9(005) VALUE 01000.
      *
       05 DCOMWG9E-BLOCO-ENTRADA.
          10 DCOMWG9E-INSTRUCAO        PIC  X(010).
      *
          10 DCOMWG9E-ENTRADA.
             15 DCOMWG9E-DANO-OPER-DESC       PIC S9(004) COMP-3.
             15 DCOMWG9E-NSEQ-OPER-DESC       PIC S9(009) COMP-3.
             15 DCOMWG9E-NPCELA-DESC-COML     PIC S9(005) COMP-3.
             15 DCOMWG9E-CEVNTO-DESC-COML     PIC S9(003) COMP-3.
             15 DCOMWG9E-HWORKF-OPER-DESC     PIC  X(026).
             15 DCOMWG9E-CSIT-INIC-DESC       PIC S9(003) COMP-3.
             15 DCOMWG9E-HSIT-INIC-DESC       PIC  X(026).
             15 DCOMWG9E-CSIT-FNAL-DESC       PIC S9(003) COMP-3.
             15 DCOMWG9E-HSIT-FNAL-DESC       PIC  X(026).
             15 DCOMWG9E-RWORKF-OPER-DESC.
                20 DCOMWG9E-RWORKF-OPER-DESC-LEN
                                              PIC S9(004) COMP-3.
                20 DCOMWG9E-RWORKF-OPER-DESC-TEXT
                                              PIC  X(240).
             15 DCOMWG9E-CFUNC-BDSCO          PIC S9(009) COMP-3.
             15 DCOMWG9E-CTERM                PIC  X(008).
             15 DCOMWG9E-FILLER               PIC  X(100).
      *
          10 DCOMWG9E-BLOCO-VARIAVEL.
             15 DCOMWG9E-V-FILLER             PIC  X(100).
      *
          10 DCOMWG9E-BLOCO-PAGINACAO.
             15 DCOMWG9E-MAX-OCORR            PIC  9(003).
             15 DCOMWG9E-TOTALIZAR            PIC  X(001).
             15 DCOMWG9E-INDICADOR-PAGINACAO  PIC  X(001).
                88 DCOMWG9E-P-INICIAL         VALUE 'I'.
                88 DCOMWG9E-P-PRIMEIRA        VALUE 'P'.
                88 DCOMWG9E-P-SEGUINTE        VALUE 'S'.
                88 DCOMWG9E-P-ANTERIOR        VALUE 'A'.
                88 DCOMWG9E-P-ULTIMA          VALUE 'U'.
      *
          10 DCOMWG9E-BLOCO-RESTART.
             15 DCOMWG9E-CHAVE-INI.
                20 DCOMWG9E-I-DANO-OPER-DESC  PIC S9(004) COMP-3.
                20 DCOMWG9E-I-NSEQ-OPER-DESC  PIC S9(009) COMP-3.
                20 DCOMWG9E-I-NPCELA-DESC-COML
                                              PIC S9(005) COMP-3.
                20 DCOMWG9E-I-CEVNTO-DESC-COML
                                              PIC S9(003) COMP-3.
                20 DCOMWG9E-I-HWORKF-OPER-DESC
                                              PIC  X(026).
             15 DCOMWG9E-CHAVE-FIM.
                20 DCOMWG9E-F-DANO-OPER-DESC  PIC S9(004) COMP-3.
                20 DCOMWG9E-F-NSEQ-OPER-DESC  PIC S9(009) COMP-3.
                20 DCOMWG9E-F-NPCELA-DESC-COML
                                              PIC S9(005) COMP-3.
                20 DCOMWG9E-F-CEVNTO-DESC-COML
                                              PIC S9(003) COMP-3.
                20 DCOMWG9E-F-HWORKF-OPER-DESC
                                              PIC  X(026).
      *
          10 DCOMWG9E-FILLER                  PIC  X(343).
      *
