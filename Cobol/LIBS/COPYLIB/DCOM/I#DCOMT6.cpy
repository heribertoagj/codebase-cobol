      ******************************************************************
      * SISTEMA : DCOM - I#DCOMT6                                      *
      * TAMANHO : 0150 BYTES                                           *
      * ARQUIVO : TABELA DE-PARA DE OPERACOES HSBC MIGRADAS PARA DCOM  *
      * INCLUDE DA TABELA DCOMB0M5 - DB2PRD.TOPER_DESC_MGRAD           *
      ******************************************************************
       01  T6-M5-REGISTRO.
 01 03     05  T6-M5-DANO-OPER-DESC    PIC S9(004)V USAGE COMP-3.
 04 05     05  T6-M5-NSEQ-OPER-DESC    PIC S9(009)V USAGE COMP-3.
 09 10     05  T6-M5-NOPER-DESC-MGRAD  PIC S9(018)V USAGE COMP-3.
 19 26     05  T6-M5-HULT-ATULZ        PIC  X(026).
      *
      **** ESPACO PARA NOVOS CAMPOS DA TABELA
      *
 45 50     05  T6-M5-FILLER-TABELA     PIC  X(050).
      *
      **** ESPACO PARA DADOS COMPLEMENTARES DO ARQUIVO
      *
 95 02     05  T6-M5-CTPO-DESC-COML    PIC S9(003)V USAGE COMP-3.
 97 03     05  T6-M5-CAG-BCRIA         PIC S9(005)V USAGE COMP-3.
100 07     05  T6-M5-CCTA-BCRIA-CLI    PIC S9(013)V USAGE COMP-3.
107 02     05  T6-M5-CPRODT            PIC S9(003)V USAGE COMP-3.
109 02     05  T6-M5-CSPROD-DESC-COML  PIC S9(003)V USAGE COMP-3.
111 40     05  T6-M5-FILLER-ARQUIVO    PIC  X(040).
