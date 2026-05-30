      *================================================================*
      * BOOK - I#BVVERS                                                *
      * DADOS DA TABELA BACCV040  - INFORMACOES DE LIMITES             *
      *                                                                *
      * TAMANHO: 062                                                   *
      *================================================================*
       01  PARM-REG-IPARMMOV.
           05  CHAVE-IPARMMOV.
               10  PARM-AGENCIA-I               PIC S9(005)    COMP-3.
               10  PARM-CONTA-I                 PIC S9(007)    COMP-3.
           05  PARM-PRODUTO-I                   PIC S9(005)    COMP-3.
           05  PARM-PERCENTUAL-I                PIC S9(003)V99 COMP-3.
           05  PARM-VLRNOMINAL-I                PIC S9(013)V99 COMP-3.
           05  PARM-TOTCRED-I                   PIC S9(013)V99 COMP-3.
           05  PARM-TOTDEB-I                    PIC S9(013)V99 COMP-3.
           05  PARM-VLRDISP-I                   PIC S9(013)V99 COMP-3.
           05  PARM-TOTMOVDIA-I                 PIC S9(013)V99 COMP-3.
           05  PARM-TOTMOVGRAN-I                PIC S9(013)V99 COMP-3.
           05  PARM-TOTMOVGRAN-NULL             PIC  X(001).
