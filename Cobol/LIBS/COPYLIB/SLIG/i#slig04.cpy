
      ***===========================================================***
      *** NOME INC                                     LENGTH=0136  ***
      *** I#SLIG04                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO IMAGEM DA TABELA                       ***
      ***            CLIENTE-TITULO-CONSOLIDADO                     ***
      ***===========================================================***
      *
       01  REG-CLI-TITLO-CONSL.
           05 SLIG04-CEMPR-INC                PIC S9(05)    COMP-3.
           05 SLIG04-CDEPDC                   PIC S9(05)    COMP-3.
           05 SLIG04-CCGC-CPF-CLI             PIC S9(09)    COMP-3.
           05 SLIG04-CINDCD-PSSOA-CLI         PIC X(001).
           05 SLIG04-CIDTFD-CART-LIQDZ        PIC S9(02)    COMP-3.
           05 SLIG04-ICOPLT-CLI               PIC X(040).
           05 SLIG04-ICOPLT-CLI-NULL          PIC X(001).
           05 SLIG04-VANTES-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG04-VANTES-PER-NAO-PG-NULL   PIC X(001).
           05 SLIG04-VVCTO-PER-PG             PIC S9(13)V99 COMP-3.
           05 SLIG04-VVCTO-PER-PG-NULL        PIC X(001).
           05 SLIG04-VVCTO-PER-NAO-PG         PIC S9(13)V99 COMP-3.
           05 SLIG04-VVCTO-PER-NAO-PG-NULL    PIC X(001).
           05 SLIG04-VVCTO-PER-BXADO          PIC S9(13)V99 COMP-3.
           05 SLIG04-VVCTO-PER-BXADO-NULL     PIC X(001).
           05 SLIG04-VVCTO-APOS-PER-PG        PIC S9(13)V99 COMP-3.
           05 SLIG04-VVCTO-APOS-PER-PG-NULL   PIC X(001).
           05 SLIG04-VAPOS-PER-NAO-PG         PIC S9(13)V99 COMP-3.
           05 SLIG04-VAPOS-PER-NAO-PG-NULL    PIC X(001).
           05 SLIG04-VAPOS-PER-BXADO          PIC S9(13)V99 COMP-3.
           05 SLIG04-VAPOS-PER-BXADO-NULL     PIC X(001).
           05 SLIG04-VTOT-PRROG               PIC S9(13)V99 COMP-3.
           05 SLIG04-VTOT-PRROG-NULL          PIC X(001).
           05 SLIG04-VTOT-ABTMT               PIC S9(13)V99 COMP-3.
           05 SLIG04-VTOT-ABTMT-NULL          PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG04-CCGC-CPF-CLI-ST          PIC X(009).
