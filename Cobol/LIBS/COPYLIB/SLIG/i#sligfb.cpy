      ***===========================================================***
CNPJ7C*** NOME INC                             LENGTH=(0117) 126    ***
      *** I#SLIGFB                                                  ***
      ***-----------------------------------------------------------***
      **                                                             **
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVO DE UNLOAD DA TABELA SLIGB027            **
      **                                                             **
      ***===========================================================***
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
       05  SLIGFB-REGISTRO.
           10 SLIGFB-CCGC-CPF-CLI        PIC S9(09)      COMP-3.
           10 SLIGFB-CINDCD-PSSOA-SACDO  PIC  X(01).
           10 SLIGFB-CIDTFD-CART-LIQDZ   PIC S9(02)      COMP-3.
           10 SLIGFB-DPER-ABRGC          PIC  X(10).
           10 SLIGFB-VVCTO-PER-PG        PIC S9(13)V99   COMP-3.
           10 SLIGFB-VVCTO-PER-PG-N      PIC  X(01).
           10 SLIGFB-VVCTO-PER-NAO-PG    PIC S9(13)V99   COMP-3.
           10 SLIGFB-VVCTO-PER-NAO-PG-N  PIC  X(01).
           10 SLIGFB-VVCTO-PER-BXADO     PIC S9(13)V99   COMP-3.
           10 SLIGFB-VVCTO-PER-BXADO-N   PIC  X(01).
           10 SLIGFB-VVCTO-APOS-PER-PG   PIC S9(13)V99   COMP-3.
           10 SLIGFB-VVCTO-APOS-PER-PG-N PIC  X(01).
           10 SLIGFB-VAPOS-PER-NAO-PG    PIC S9(13)V99   COMP-3.
           10 SLIGFB-VAPOS-PER-NAO-PG-N  PIC  X(01).
           10 SLIGFB-VAPOS-PER-BXADO     PIC S9(13)V99   COMP-3.
           10 SLIGFB-VAPOS-PER-BXADO-N   PIC  X(01).
           10 SLIGFB-VCHEQ-PRIM-DEVLC    PIC S9(13)V99   COMP-3.
           10 SLIGFB-VCHEQ-PRIM-DEVLC-N  PIC  X(01).
           10 SLIGFB-VCHEQ-REAP          PIC S9(13)V99   COMP-3.
           10 SLIGFB-VCHEQ-REAP-N        PIC  X(01).
           10 SLIGFB-VCHEQ-REAP-DEVOL    PIC S9(13)V99   COMP-3.
           10 SLIGFB-VCHEQ-REAP-DEVOL-N  PIC  X(01).
           10 SLIGFB-VCHEQ-REAP-PG       PIC S9(13)V99   COMP-3.
           10 SLIGFB-VCHEQ-REAP-PG-N     PIC  X(01).
           10 SLIGFB-VCHEQ-DEVOL-ANTER   PIC S9(13)V99   COMP-3.
           10 SLIGFB-VCHEQ-DEVOL-ANTER-N PIC  X(01).
CNPJ7C     10 SLIGFB-CCGC-CPF-CLI-N      PIC  X(09).
