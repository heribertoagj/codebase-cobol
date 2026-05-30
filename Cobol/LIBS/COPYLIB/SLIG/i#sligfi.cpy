      ***===========================================================***
CNPJ7C*** NOME INC                           LENGTH=(0117) 135      ***
      *** I#SLIGFI                                                  ***
      ***-----------------------------------------------------------***
      **                                                             **
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVO DE UNLOAD DA TABELA SLIGB091            **
      **                                                             **
      ***===========================================================***
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
       05  SLIGFI-REGISTRO.
           10 SLIGFI-CCGC-CPF-CLI        PIC S9(09)      COMP-3.
           10 SLIGFI-CINDCD-PSSOA-CLI    PIC  X(01).
           10 SLIGFI-CCGC-CPF-SACDO      PIC S9(09)      COMP-3.
           10 SLIGFI-CINDCD-PSSOA-SACDO  PIC  X(01).
           10 SLIGFI-CIDTFD-CART-LIQDZ   PIC S9(02)      COMP-3.
           10 SLIGFI-DPER-ABRGC          PIC  X(10).
           10 SLIGFI-VVCTO-PER-PG        PIC S9(13)V99   COMP-3.
           10 SLIGFI-VVCTO-PER-PG-N      PIC  X(01).
           10 SLIGFI-VVCTO-PER-NAO-PG    PIC S9(13)V99   COMP-3.
           10 SLIGFI-VVCTO-PER-NAO-PG-N  PIC  X(01).
           10 SLIGFI-VVCTO-PER-BXADO     PIC S9(13)V99   COMP-3.
           10 SLIGFI-VVCTO-PER-BXADO-N   PIC  X(01).
           10 SLIGFI-VVCTO-APOS-PER-PG   PIC S9(13)V99   COMP-3.
           10 SLIGFI-VVCTO-APOS-PER-PG-N PIC  X(01).
           10 SLIGFI-VAPOS-PER-NAO-PG    PIC S9(13)V99   COMP-3.
           10 SLIGFI-VAPOS-PER-NAO-PG-N  PIC  X(01).
           10 SLIGFI-VAPOS-PER-BXADO     PIC S9(13)V99   COMP-3.
           10 SLIGFI-VAPOS-PER-BXADO-N   PIC  X(01).
           10 SLIGFI-VCHEQ-PRIM-DEVLC    PIC S9(13)V99   COMP-3.
           10 SLIGFI-VCHEQ-PRIM-DEVLC-N  PIC  X(01).
           10 SLIGFI-VCHEQ-REAP          PIC S9(13)V99   COMP-3.
           10 SLIGFI-VCHEQ-REAP-N        PIC  X(01).
           10 SLIGFI-VCHEQ-REAP-DEVOL    PIC S9(13)V99   COMP-3.
           10 SLIGFI-VCHEQ-REAP-DEVOL-N  PIC  X(01).
           10 SLIGFI-VCHEQ-REAP-PG       PIC S9(13)V99   COMP-3.
           10 SLIGFI-VCHEQ-REAP-PG-N     PIC  X(01).
           10 SLIGFI-VCHEQ-DEVOL-ANTER   PIC S9(13)V99   COMP-3.
           10 SLIGFI-VCHEQ-DEVOL-ANTER-N PIC  X(01).
CNPJ7C     10 SLIGFI-CCGC-CPF-CLI-N      PIC  X(09).
CNPJ7C     10 SLIGFI-CCGC-CPF-SACDO-N    PIC  X(09).
