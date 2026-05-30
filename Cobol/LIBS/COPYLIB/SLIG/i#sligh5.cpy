      ***===========================================================***
      *** NOME INC                                     LENGTH=0036  ***
      *** I#SLIGH5                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO DE INDICES DOS CLIENTES/EMITENTES      ***
      ***            PARA TRANSMISSAO PARA O SISTEMA GATI           ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-DADOS.
           03 SLIGH5-CLIENTE-CARTEIRA.
              05 SLIGH5-CGC-CPF-CLI              PIC S9(009)   COMP-3.
              05 SLIGH5-INDCD-PSSOA-CLI          PIC  X(001).
              05 SLIGH5-CIDTFD-CART-LIQDZ        PIC S9(002)   COMP-3.
              05 SLIGH5-CGC-CPF-CLI-ST           PIC X(009).
           03 SLIGH5-INDICES.
              05 SLIGH5-PINDIC-LIQDZ-FNAL        PIC S9(003)V99 COMP-3.
              05 SLIGH5-PINDIC-LIQDZ-FNAL-NULL   PIC  X(001).
              05 SLIGH5-PINDIC-PRIM-DEP          PIC S9(003)V99 COMP-3.
              05 SLIGH5-PINDIC-PRIM-DEP-NULL     PIC  X(001).
              05 SLIGH5-PINDIC-LIQDZ-REAP        PIC S9(003)V99 COMP-3.
              05 SLIGH5-PINDIC-LIQDZ-REAP-NULL   PIC  X(001).
              05 SLIGH5-PINDIC-VCTO-PER          PIC S9(003)V99 COMP-3.
              05 SLIGH5-PINDIC-VCTO-PER-NULL     PIC  X(001).
              05 SLIGH5-PINDIC-CHEQ-DEP          PIC S9(003)V99 COMP-3.
              05 SLIGH5-PINDIC-CHEQ-DEP-NULL     PIC X(001).
              05 SLIGH5-PINDIC-BAIXA-ANTCP       PIC S9(003)V99 COMP-3.
              05 SLIGH5-PINDIC-BAIXA-ANTCP-NULL  PIC X(001).
              05 SLIGH5-PINDIC-CHEQ-REAP         PIC S9(003)V99 COMP-3.
              05 SLIGH5-PINDIC-CHEQ-REAP-NULL    PIC  X(001).
      *
