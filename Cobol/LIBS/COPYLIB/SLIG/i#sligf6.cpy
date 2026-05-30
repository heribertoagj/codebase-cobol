      ***===========================================================***
CNPJ7C*** NOME INC                            LENGTH=(0107)  116    ***
      *** I#SLIGF6                                                  ***
      ***-----------------------------------------------------------***
      **                                                             **
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVO DE UNLOAD DA TABELA SLIGB001            **
      **                                                             **
      ***===========================================================***
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
       05  SLIGF6-REGISTRO.
           10 SLIGF6-CCGC-CPF-CLI        PIC S9(09)      COMP-3.
           10 SLIGF6-CINDCD-PSSOA-CLI    PIC  X(01).
           10 SLIGF6-CIDTFD-CART-LIQDZ   PIC S9(02)      COMP-3.
           10 SLIGF6-VVCTO-PER-PG        PIC S9(13)V99   COMP-3.
           10 SLIGF6-VVCTO-PER-PG-N      PIC  X(01).
           10 SLIGF6-VVCTO-PER-NAO-PG    PIC S9(13)V99   COMP-3.
           10 SLIGF6-VVCTO-PER-NAO-PG-N  PIC  X(01).
           10 SLIGF6-VVCTO-PER-BXADO     PIC S9(13)V99   COMP-3.
           10 SLIGF6-VVCTO-PER-BXADO-N   PIC  X(01).
           10 SLIGF6-VVCTO-APOS-PER-PG   PIC S9(13)V99   COMP-3.
           10 SLIGF6-VVCTO-APOS-PER-PG-N PIC  X(01).
           10 SLIGF6-VAPOS-PER-NAO-PG    PIC S9(13)V99   COMP-3.
           10 SLIGF6-VAPOS-PER-NAO-PG-N  PIC  X(01).
           10 SLIGF6-VAPOS-PER-BXADO     PIC S9(13)V99   COMP-3.
           10 SLIGF6-VAPOS-PER-BXADO-N   PIC  X(01).
           10 SLIGF6-VCHEQ-PRIM-DEVLC    PIC S9(13)V99   COMP-3.
           10 SLIGF6-VCHEQ-PRIM-DEVLC-N  PIC  X(01).
           10 SLIGF6-VCHEQ-REAP          PIC S9(13)V99   COMP-3.
           10 SLIGF6-VCHEQ-REAP-N        PIC  X(01).
           10 SLIGF6-VCHEQ-REAP-DEVOL    PIC S9(13)V99   COMP-3.
           10 SLIGF6-VCHEQ-REAP-DEVOL-N  PIC  X(01).
           10 SLIGF6-VCHEQ-REAP-PG       PIC S9(13)V99   COMP-3.
           10 SLIGF6-VCHEQ-REAP-PG-N     PIC  X(01).
           10 SLIGF6-VCHEQ-DEVOL-ANTER   PIC S9(13)V99   COMP-3.
           10 SLIGF6-VCHEQ-DEVOL-ANTER-N PIC  X(01).
CNPJ7C     10 SLIGF6-CCGC-CPF-CLI-N      PIC  X(09).
