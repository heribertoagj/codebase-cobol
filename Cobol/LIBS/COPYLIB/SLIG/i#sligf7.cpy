      ***===========================================================***
CNPJ7C*** NOME INC                            LENGTH=(0117)  126    ***
      *** I#SLIGF7                                                  ***
      ***-----------------------------------------------------------***
      **                                                             **
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVO DE UNLOAD DA TABELA SLIGB017            **
      **                                                             **
      ***===========================================================***
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
       05  SLIGF7-REGISTRO.
           10 SLIGF7-CCGC-CPF-CLI        PIC S9(09)      COMP-3.
           10 SLIGF7-CINDCD-PSSOA-CLI    PIC  X(01).
           10 SLIGF7-CIDTFD-CART-LIQDZ   PIC S9(02)      COMP-3.
           10 SLIGF7-DPER-ABRGC          PIC  X(10).
           10 SLIGF7-VVCTO-PER-PG        PIC S9(13)V99   COMP-3.
           10 SLIGF7-VVCTO-PER-PG-N      PIC  X(01).
           10 SLIGF7-VVCTO-PER-NAO-PG    PIC S9(13)V99   COMP-3.
           10 SLIGF7-VVCTO-PER-NAO-PG-N  PIC  X(01).
           10 SLIGF7-VVCTO-PER-BXADO     PIC S9(13)V99   COMP-3.
           10 SLIGF7-VVCTO-PER-BXADO-N   PIC  X(01).
           10 SLIGF7-VVCTO-APOS-PER-PG   PIC S9(13)V99   COMP-3.
           10 SLIGF7-VVCTO-APOS-PER-PG-N PIC  X(01).
           10 SLIGF7-VAPOS-PER-NAO-PG    PIC S9(13)V99   COMP-3.
           10 SLIGF7-VAPOS-PER-NAO-PG-N  PIC  X(01).
           10 SLIGF7-VAPOS-PER-BXADO     PIC S9(13)V99   COMP-3.
           10 SLIGF7-VAPOS-PER-BXADO-N   PIC  X(01).
           10 SLIGF7-VCHEQ-PRIM-DEVLC    PIC S9(13)V99   COMP-3.
           10 SLIGF7-VCHEQ-PRIM-DEVLC-N  PIC  X(01).
           10 SLIGF7-VCHEQ-REAP          PIC S9(13)V99   COMP-3.
           10 SLIGF7-VCHEQ-REAP-N        PIC  X(01).
           10 SLIGF7-VCHEQ-REAP-DEVOL    PIC S9(13)V99   COMP-3.
           10 SLIGF7-VCHEQ-REAP-DEVOL-N  PIC  X(01).
           10 SLIGF7-VCHEQ-REAP-PG       PIC S9(13)V99   COMP-3.
           10 SLIGF7-VCHEQ-REAP-PG-N     PIC  X(01).
           10 SLIGF7-VCHEQ-DEVOL-ANTER   PIC S9(13)V99   COMP-3.
           10 SLIGF7-VCHEQ-DEVOL-ANTER-N PIC  X(01).
CNPJ7C     10 SLIGF7-CCGC-CPF-CLI-X      PIC  X(09).
