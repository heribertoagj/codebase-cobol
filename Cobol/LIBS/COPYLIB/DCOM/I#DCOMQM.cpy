      ******************************************************************
      * SISTEMA : DCOM - I#DCOMQM                                      *
      * TAMANHO : 0049 BYTES                                           *
      * ARQUIVO : HISTORICO GARANTIA OPERACAO DESCONTO                 *
      * INCLUDE DA TABELA DCOMB0B2 - DB2PRD.THIST_GARNT_OPER           *
      ******************************************************************
       01  QM-B2-REGISTRO.
01 03      05 QM-B2-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
04 05      05 QM-B2-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
09 02      05 QM-B2-CTPO-GARNT           PIC S9(3)V USAGE COMP-3.
11 26      05 QM-B2-HULT-ATULZ           PIC X(26).
37 05      05 QM-B2-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
42 08      05 QM-B2-CTERM                PIC X(8).
