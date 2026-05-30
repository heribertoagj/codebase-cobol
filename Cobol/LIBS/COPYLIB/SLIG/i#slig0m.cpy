      ***===========================================================***
      *** NOME INC                                     LENGTH=109   ***
      *** I#SLIG0M                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO IMAGEM DA TABELA                       ***
      ***            NEGOCIACAO-SACADO-TITULO (CONSOLIDADO)         ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-NEG-SACDO-TITLO.
           05 SLIG0M-CBCO-PRODT-COBR        PIC S9(03)    COMP-3.
           05 SLIG0M-CIDTFD-PRODT-COBR      PIC S9(02)    COMP-3.
           05 SLIG0M-CNEGOC-COBR            PIC S9(18)    COMP-3.
           05 SLIG0M-CCGC-CPF-SACDO         PIC S9(09)    COMP-3.
           05 SLIG0M-CINDCD-PSSOA-SACDO     PIC X(001).
           05 SLIG0M-CIDTFD-CART-LIQDZ      PIC S9(02)    COMP-3.
           05 SLIG0M-CCGC-CPF-CLI           PIC S9(09)    COMP-3.
           05 SLIG0M-CINDCD-PSSOA-CLI       PIC X(001).
           05 SLIG0M-VANTES-PER-NAO-PG      PIC S9(13)V99 COMP-3.
           05 SLIG0M-VANTES-PER-NAO-PG-NULL PIC X(001).
           05 SLIG0M-VVCTO-PER-PG           PIC S9(13)V99 COMP-3.
           05 SLIG0M-VVCTO-PER-PG-NULL      PIC X(001).
           05 SLIG0M-VVCTO-PER-NAO-PG       PIC S9(13)V99 COMP-3.
           05 SLIG0M-VVCTO-PER-NAO-PG-NULL  PIC X(001).
           05 SLIG0M-VVCTO-PER-BXADO        PIC S9(13)V99 COMP-3.
           05 SLIG0M-VVCTO-PER-BXADO-NULL   PIC X(001).
           05 SLIG0M-VVCTO-APOS-PER-PG      PIC S9(13)V99 COMP-3.
           05 SLIG0M-VVCTO-APOS-PER-PG-NULL PIC X(001).
           05 SLIG0M-VAPOS-PER-NAO-PG       PIC S9(13)V99 COMP-3.
           05 SLIG0M-VAPOS-PER-NAO-PG-NULL  PIC X(001).
           05 SLIG0M-VAPOS-PER-BXADO        PIC S9(13)V99 COMP-3.
           05 SLIG0M-VAPOS-PER-BXADO-NULL   PIC X(001).
           05 SLIG0M-VTOT-PRROG             PIC S9(13)V99 COMP-3.
           05 SLIG0M-VTOT-PRROG-NULL        PIC X(001).
           05 SLIG0M-VTOT-ABTMT             PIC S9(13)V99 COMP-3.
           05 SLIG0M-VTOT-ABTMT-NULL        PIC X(001).
           05 SLIG0M-CCGC-CPF-SACDO-ST      PIC X(009).
           05 SLIG0M-CCGC-CPF-CLI-ST        PIC X(009).
