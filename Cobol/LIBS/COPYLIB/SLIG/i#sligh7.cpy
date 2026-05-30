      ***===========================================================***
      *** NOME INC                                     LENGTH=00010 ***
      *** I#SLIGH7                                                 ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO DE INDICES DOS CLIENTES/SACADO         ***
      ***            PARA TRANSMISSAO PARA O SISTEMA GATI           ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-DADOS.
           03 SLIGH7-CLIENTE-CARTEIRA.
              05 SLIGH7-CGC-CPF-CLI              PIC S9(009)   COMP-3.
              05 SLIGH7-INDCD-PSSOA-CLI          PIC  X(001).
           03 SLIGH7-INDICES.
              05 SLIGH7-PINDIC-LIQDZ-FNAL        PIC S9(003)V99 COMP-3.
              05 SLIGH7-PINDIC-LIQDZ-FNAL-NULL   PIC  X(001).
              05 SLIGH7-CGC-CPF-CLI-ST           PIC X(009).
      *
