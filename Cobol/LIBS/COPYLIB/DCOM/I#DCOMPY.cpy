      ******************************************************************
      * SISTEMA : DCOM - I#DCOMPY                                      *
      * TAMANHO : 0074 BYTES                                           *
      * ARQUIVO : TRANSITORIA CARTAO ANTECIPAVEL (SELECAO DE BANDEIRAS)*
      * INCLUDE DA TABELA DCOMB0M6 - DB2PRD.TTRASI_CATAO_ANTVL         *
      ******************************************************************
       01  PY-M6-REGISTRO.
  1  3     05  PY-M6-DANO-OPER-DESC     PIC S9(004)    COMP-3.
  4  5     05  PY-M6-NSEQ-OPER-DESC     PIC S9(009)    COMP-3.
  9  5     05  PY-M6-CADM-CATAO-CREDT   PIC S9(009)    COMP-3.
 14  1     05  PY-M6-CINDCD-VDA-RCBVL   PIC  X(001).
 15  9     05  PY-M6-VDISPN-CATAO-ANTVL PIC S9(015)V99 COMP-3.
 24  1     05  PY-M6-CINDCD-SELEC-DESC  PIC  X(001).
 25  1     05  PY-M6-CSELEC-CATAO-CREDT PIC  X(001).
 26 26     05  PY-M6-HULT-ATULZ         PIC  X(026).
 52  5     05  PY-M6-CFUNC-BDSCO        PIC S9(009)    COMP-3.
 57  8     05  PY-M6-CTERM              PIC  X(008).
 65 10     05  PY-M6-DINCL-REG          PIC  X(010).
