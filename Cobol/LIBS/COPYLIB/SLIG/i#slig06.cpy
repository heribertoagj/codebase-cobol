      ***===========================================================***
      *** NOME INC                                     LENGTH=101   ***
      *** I#SLIG06                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO IMAGEM DA TABELA                       ***
      ***            SACADO-TITULO-CONSOLIDADO                      ***
      ***===========================================================***
      *
       01  REG-SACDO-TITLO-CONSL.
           05 SLIG06-CEMPR-INC                PIC S9(05)    COMP-3.
           05 SLIG06-CDEPDC                   PIC S9(05)    COMP-3.
           05 SLIG06-CCGC-CPF-SACDO           PIC S9(09)    COMP-3.
           05 SLIG06-CINDCD-PSSOA-SACDO       PIC X(001).
           05 SLIG06-CIDTFD-CART-LIQDZ        PIC S9(02)    COMP-3.
           05 SLIG06-CGRP-ECONM               PIC S9(09)    COMP-3.
           05 SLIG06-CGRP-ECONM-NULL          PIC X(001).
           05 SLIG06-VANTES-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG06-VANTES-PER-NAO-PG-NULL   PIC X(001).
           05 SLIG06-VVCTO-PER-PG             PIC S9(13)V99 COMP-3.
           05 SLIG06-VVCTO-PER-PG-NULL        PIC X(001).
           05 SLIG06-VVCTO-PER-NAO-PG         PIC S9(13)V99 COMP-3.
           05 SLIG06-VVCTO-PER-NAO-PG-NULL    PIC X(001).
           05 SLIG06-VVCTO-PER-BXADO          PIC S9(13)V99 COMP-3.
           05 SLIG06-VVCTO-PER-BXADO-NULL     PIC X(001).
           05 SLIG06-VVCTO-APOS-PER-PG        PIC S9(13)V99 COMP-3.
           05 SLIG06-VVCTO-APOS-PER-PG-NULL   PIC X(001).
           05 SLIG06-VAPOS-PER-NAO-PG         PIC S9(13)V99 COMP-3.
           05 SLIG06-VAPOS-PER-NAO-PG-NULL    PIC X(001).
           05 SLIG06-VAPOS-PER-BXADO          PIC S9(13)V99 COMP-3.
           05 SLIG06-VAPOS-PER-BXADO-NULL     PIC X(001).
           05 SLIG06-VTOT-PRROG               PIC S9(13)V99 COMP-3.
           05 SLIG06-VTOT-PRROG-NULL          PIC X(001).
           05 SLIG06-VTOT-ABTMT               PIC S9(13)V99 COMP-3.
           05 SLIG06-VTOT-ABTMT-NULL          PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG06-CCGC-CPF-SACDO-ST        PIC X(009).
