
      ***===========================================================***
      *** NOME INC                                     LENGTH=095   ***
      *** I#SLIG03                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO IMAGEM DA TABELA                       ***
      ***            CLIENTE-SACADO-CONSOLIDADO                     ***
      ***===========================================================***
      *
       01  REG-CLI-SACDO-TITLO.
           05 SLIG03-CCGC-CPF-CLI           PIC S9(09)    COMP-3.
           05 SLIG03-CINDCD-PSSOA-CLI       PIC X(001).
           05 SLIG03-CCGC-CPF-SACDO         PIC S9(09)    COMP-3.
           05 SLIG03-CINDCD-PSSOA-SACDO     PIC X(001).
           05 SLIG03-CIDTFD-CART-LIQDZ      PIC S9(02)    COMP-3.
           05 SLIG03-VANTES-PER-NAO-PG      PIC S9(13)V99 COMP-3.
           05 SLIG03-VANTES-PER-NAO-PG-NULL PIC X(001).
           05 SLIG03-VVCTO-PER-PG           PIC S9(13)V99 COMP-3.
           05 SLIG03-VVCTO-PER-PG-NULL      PIC X(001).
           05 SLIG03-VVCTO-PER-NAO-PG       PIC S9(13)V99 COMP-3.
           05 SLIG03-VVCTO-PER-NAO-PG-NULL  PIC X(001).
           05 SLIG03-VVCTO-PER-BXADO        PIC S9(13)V99 COMP-3.
           05 SLIG03-VVCTO-PER-BXADO-NULL   PIC X(001).
           05 SLIG03-VVCTO-APOS-PER-PG      PIC S9(13)V99 COMP-3.
           05 SLIG03-VVCTO-APOS-PER-PG-NULL PIC X(001).
           05 SLIG03-VAPOS-PER-NAO-PG       PIC S9(13)V99 COMP-3.
           05 SLIG03-VAPOS-PER-NAO-PG-NULL  PIC X(001).
           05 SLIG03-VAPOS-PER-BXADO        PIC S9(13)V99 COMP-3.
           05 SLIG03-VAPOS-PER-BXADO-NULL   PIC X(001).
           05 SLIG03-VTOT-PRROG             PIC S9(13)V99 COMP-3.
           05 SLIG03-VTOT-PRROG-NULL        PIC X(001).
           05 SLIG03-VTOT-ABTMT             PIC S9(13)V99 COMP-3.
           05 SLIG03-VTOT-ABTMT-NULL        PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG03-CCGC-CPF-CLI-ST        PIC X(009).
           05 SLIG03-CCGC-CPF-SACDO-ST      PIC X(009).
