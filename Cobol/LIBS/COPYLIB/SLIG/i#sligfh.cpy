      ***===========================================================***
CNPJ7C*** NOME INC                         LENGTH=(0099) 117        ***
      *** I#SLIGFH                                                  ***
      ***-----------------------------------------------------------***
      **                                                             **
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVO DE UNLOAD DA TABELA SLIGB092            **
      **                                                             **
      ***===========================================================***
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
       05  SLIGFH-REGISTRO.
           10 SLIGFH-CCGC-CPF-CLI        PIC S9(09)    COMP-3.
           10 SLIGFH-CINDCD-PSSOA-CLI    PIC  X(01).
           10 SLIGFH-CCGC-CPF-SACDO      PIC S9(09)      COMP-3.
           10 SLIGFH-CINDCD-PSSOA-SACDO  PIC  X(01).
           10 SLIGFH-CIDTFD-CART-LIQDZ   PIC S9(02)    COMP-3.
           10 SLIGFH-DPER-ABRGC          PIC  X(10).
           10 SLIGFH-VANTES-PER-NAO-PG   PIC S9(13)V99 COMP-3.
           10 SLIGFH-VANTES-PER-NAO-PG-N PIC  X(01).
           10 SLIGFH-VVCTO-PER-PG        PIC S9(13)V99 COMP-3.
           10 SLIGFH-VVCTO-PER-PG-N      PIC  X(01).
           10 SLIGFH-VVCTO-PER-NAO-PG    PIC S9(13)V99 COMP-3.
           10 SLIGFH-VVCTO-PER-NAO-PG-N  PIC  X(01).
           10 SLIGFH-VVCTO-PER-BXADO     PIC S9(13)V99 COMP-3.
           10 SLIGFH-VVCTO-PER-BXADO-N   PIC  X(01).
           10 SLIGFH-VVCTO-APOS-PER-PG   PIC S9(13)V99 COMP-3.
           10 SLIGFH-VVCTO-APOS-PER-PG-N PIC  X(01).
           10 SLIGFH-VAPOS-PER-NAO-PG    PIC S9(13)V99 COMP-3.
           10 SLIGFH-VAPOS-PER-NAO-PG-N  PIC  X(01).
           10 SLIGFH-VAPOS-PER-BXADO     PIC S9(13)V99 COMP-3.
           10 SLIGFH-VAPOS-PER-BXADO-N   PIC  X(01).
           10 SLIGFH-VTOT-PRROG-TITLO    PIC S9(13)V99 COMP-3.
           10 SLIGFH-VTOT-PRROG-TITLO-N  PIC  X(01).
           10 SLIGFH-VTOT-ABTMT-TITLO    PIC S9(13)V99 COMP-3.
           10 SLIGFH-VTOT-ABTMT-TITLO-N  PIC  X(01).
CNPJ7C     10 SLIGFH-CCGC-CPF-CLI-N      PIC  X(09).
CNPJ7C     10 SLIGFH-CCGC-CPF-SACDO-N    PIC  X(09).
