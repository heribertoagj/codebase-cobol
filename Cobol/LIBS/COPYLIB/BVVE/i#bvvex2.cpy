      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : CARGA/BAIXA DA TABELA DE MOTIVOS DE RECUSA          *
      *  LRECL   : 72(FB)                                              *
      *  NOME INC: I#BVVEX2                                            *
      *  DATA    : JAN 2010                                            *
      *----------------------------------------------------------------*
       01  BVVEX2-REG.
           05  BVVEX2-CMOTVO-RCUSA-AFLIA     PIC S9(5)V USAGE COMP-3.
           05  BVVEX2-RMOTVO-RCUSA-AFLIA     PIC X(60).
           05  BVVEX2-RMOTVO-RCUSA-AFLIA-N   PIC X(01).
           05  BVVEX2-CUSUAR-MANUT           PIC X(07).
           05  BVVEX2-CUSUAR-MANUT-N         PIC X(01).

