      ***===========================================================***
CNPJ7C*** NOME INC                              LENGTH=(0099) 108   ***
      *** I#SLIGF5                                                  ***
      ***-----------------------------------------------------------***
      **                                                             **
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVO DE UNLOAD DA TABELA SLIGB018            **
      **                                                             **
      ***===========================================================***
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
       05  SLIGF5-REGISTRO.
           10 SLIGF5-CCGC-CPF-CLI        PIC S9(09)    COMP-3.
           10 SLIGF5-CINDCD-PSSOA-CLI    PIC  X(01).
           10 SLIGF5-CIDTFD-CART-LIQDZ   PIC S9(02)    COMP-3.
           10 SLIGF5-DPER-ABRGC          PIC  X(10).
           10 SLIGF5-VANTES-PER-NAO-PG   PIC S9(13)V99 COMP-3.
           10 SLIGF5-VANTES-PER-NAO-PG-N PIC  X(01).
           10 SLIGF5-VVCTO-PER-PG        PIC S9(13)V99 COMP-3.
           10 SLIGF5-VVCTO-PER-PG-N      PIC  X(01).
           10 SLIGF5-VVCTO-PER-NAO-PG    PIC S9(13)V99 COMP-3.
           10 SLIGF5-VVCTO-PER-NAO-PG-N  PIC  X(01).
           10 SLIGF5-VVCTO-PER-BXADO     PIC S9(13)V99 COMP-3.
           10 SLIGF5-VVCTO-PER-BXADO-N   PIC  X(01).
           10 SLIGF5-VVCTO-APOS-PER-PG   PIC S9(13)V99 COMP-3.
           10 SLIGF5-VVCTO-APOS-PER-PG-N PIC  X(01).
           10 SLIGF5-VAPOS-PER-NAO-PG    PIC S9(13)V99 COMP-3.
           10 SLIGF5-VAPOS-PER-NAO-PG-N  PIC  X(01).
           10 SLIGF5-VAPOS-PER-BXADO     PIC S9(13)V99 COMP-3.
           10 SLIGF5-VAPOS-PER-BXADO-N   PIC  X(01).
           10 SLIGF5-VTOT-PRROG-TITLO    PIC S9(13)V99 COMP-3.
           10 SLIGF5-VTOT-PRROG-TITLO-N  PIC  X(01).
           10 SLIGF5-VTOT-ABTMT-TITLO    PIC S9(13)V99 COMP-3.
           10 SLIGF5-VTOT-ABTMT-TITLO-N  PIC  X(01).
CNPJ7C     10 SLIGF5-CCGC-CPF-CLI-N      PIC  X(09).
