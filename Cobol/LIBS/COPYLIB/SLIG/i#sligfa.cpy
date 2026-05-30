      ***===========================================================***
CNPJ7C*** NOME INC                             LENGTH= (0107) 116   ***
      *** I#SLIGFA                                                  ***
      ***-----------------------------------------------------------***
      **                                                             **
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVO DE UNLOAD DA TABELA SLIGB031            **
      **                                                             **
      ***===========================================================***
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
       05  SLIGFA-REGISTRO.
           10 SLIGFA-CCGC-CPF-CLI        PIC S9(09)      COMP-3.
           10 SLIGFA-CINDCD-PSSOA-CLI    PIC  X(01).
           10 SLIGFA-CIDTFD-CART-LIQDZ   PIC S9(02)      COMP-3.
           10 SLIGFA-VVCTO-PER-PG        PIC S9(13)V99   COMP-3.
           10 SLIGFA-VVCTO-PER-PG-N      PIC  X(01).
           10 SLIGFA-VVCTO-PER-NAO-PG    PIC S9(13)V99   COMP-3.
           10 SLIGFA-VVCTO-PER-NAO-PG-N  PIC  X(01).
           10 SLIGFA-VVCTO-PER-BXADO     PIC S9(13)V99   COMP-3.
           10 SLIGFA-VVCTO-PER-BXADO-N   PIC  X(01).
           10 SLIGFA-VVCTO-APOS-PER-PG   PIC S9(13)V99   COMP-3.
           10 SLIGFA-VVCTO-APOS-PER-PG-N PIC  X(01).
           10 SLIGFA-VAPOS-PER-NAO-PG    PIC S9(13)V99   COMP-3.
           10 SLIGFA-VAPOS-PER-NAO-PG-N  PIC  X(01).
           10 SLIGFA-VAPOS-PER-BXADO     PIC S9(13)V99   COMP-3.
           10 SLIGFA-VAPOS-PER-BXADO-N   PIC  X(01).
           10 SLIGFA-VCHEQ-PRIM-DEVLC    PIC S9(13)V99   COMP-3.
           10 SLIGFA-VCHEQ-PRIM-DEVLC-N  PIC  X(01).
           10 SLIGFA-VCHEQ-REAP          PIC S9(13)V99   COMP-3.
           10 SLIGFA-VCHEQ-REAP-N        PIC  X(01).
           10 SLIGFA-VCHEQ-REAP-DEVOL    PIC S9(13)V99   COMP-3.
           10 SLIGFA-VCHEQ-REAP-DEVOL-N  PIC  X(01).
           10 SLIGFA-VCHEQ-REAP-PG       PIC S9(13)V99   COMP-3.
           10 SLIGFA-VCHEQ-REAP-PG-N     PIC  X(01).
           10 SLIGFA-VCHEQ-DEVOL-ANTER   PIC S9(13)V99   COMP-3.
           10 SLIGFA-VCHEQ-DEVOL-ANTER-N PIC  X(01).
CNPJ7C     10 SLIGFA-CCGC-CPF-CLI-N      PIC  X(09).
