      ***===========================================================***
      *** NOME INC                                     LENGTH=00130 ***
      *** I#SLIGH6                                                 ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO DE INDICES DOS CLIENTES/SACADO         ***
      ***            PARA TRANSMISSAO PARA O SISTEMA GATI           ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-DADOS.
           03 SLIGH6-CLIENTE-CARTEIRA.
              05 SLIGH6-CGC-CPF                  PIC S9(009)   COMP-3.
              05 SLIGH6-INDCD-PSSOA              PIC  X(001).
              05 SLIGH6-CIDTFD-CART-LIQDZ        PIC S9(002)   COMP-3.
              05 SLIGH6-CGC-CPF-ST               PIC X(009).
           03 SLIGH6-INDICES.
              05 SLIGH6-PINDIC-LIQDZ-FNAL        PIC S9(003)V99 COMP-3.
              05 SLIGH6-PINDIC-LIQDZ-FNAL-NULL   PIC  X(001).
              05 SLIGH6-PINDIC-VCTO-PER          PIC S9(003)V99 COMP-3.
              05 SLIGH6-PINDIC-VCTO-PER-NULL     PIC X(001).
              05 SLIGH6-PINDIC-VCTO-PER-AN       PIC S9(003)V99 COMP-3.
              05 SLIGH6-PINDIC-VCTO-PER-AN-NULL  PIC X(001).
              05 SLIGH6-PINDIC-BAIXA-ANTCP       PIC S9(003)V99 COMP-3.
              05 SLIGH6-PINDIC-BAIXA-ANTCP-NULL  PIC X(001).
              05 SLIGH6-PINDIC-LIQDZ-BAIXA       PIC S9(003)V99 COMP-3.
              05 SLIGH6-PINDIC-LIQDZ-BAIXA-NULL  PIC X(001).
              05 SLIGH6-PINDIC-PRROG             PIC S9(003)V99 COMP-3.
              05 SLIGH6-PINDIC-PRROG-NULL        PIC X(001).
              05 SLIGH6-PINDIC-ABTMT             PIC S9(003)V99 COMP-3.
              05 SLIGH6-PINDIC-ABTMT-NULL        PIC X(001).
              05 SLIGH6-PINDIC-PGTO-ANTCP        PIC S9(003)V99 COMP-3.
              05 SLIGH6-PINDIC-PGTO-ANTCP-NULL   PIC X(001).
