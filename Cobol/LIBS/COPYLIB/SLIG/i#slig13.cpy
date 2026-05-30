      ***===========================================================***
      *** NOME INC                                     LENGTH=00113 ***
      *** I#SLIG13                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO IMAGEM DA TABELA                       ***
      ***            HISTORICO-SACADO-TITULO-CONSL                  ***
      ***===========================================================***
      *
       01  REG-HIST-SACDO-TITLO.
           05 SLIG13-CEMPR-INC               PIC S9(05)    COMP-3.
           05 SLIG13-CDEPDC                  PIC S9(05)    COMP-3.
           05 SLIG13-CCGC-CPF-SACDO          PIC S9(09)    COMP-3.
           05 SLIG13-CINDCD-PSSOA-SACDO      PIC X(001).
           05 SLIG13-CIDTFD-CART-LIQDZ       PIC S9(02)    COMP-3.
           05 SLIG13-DPER-REFT               PIC X(010).
           05 SLIG13-CGRP-ECONM              PIC S9(09)    COMP-3.
           05 SLIG13-CGRP-ECONM-NULL         PIC X(001).
           05 SLIG13-VANTES-PER-NAO-PG       PIC S9(13)V99 COMP-3.
           05 SLIG13-VANTES-PER-NAO-PG-NULL  PIC X(001).
           05 SLIG13-VVCTO-PER-PG            PIC S9(13)V99 COMP-3.
           05 SLIG13-VVCTO-PER-PG-NULL       PIC X(001).
           05 SLIG13-VVCTO-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG13-VVCTO-PER-NAO-PG-NULL   PIC X(001).
           05 SLIG13-VVCTO-PER-BXADO         PIC S9(13)V99 COMP-3.
           05 SLIG13-VVCTO-PER-BXADO-NULL    PIC X(001).
           05 SLIG13-VVCTO-APOS-PER-PG       PIC S9(13)V99 COMP-3.
           05 SLIG13-VVCTO-APOS-PER-PG-NULL  PIC X(001).
           05 SLIG13-VAPOS-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG13-VAPOS-PER-NAO-PG-NULL   PIC X(001).
           05 SLIG13-VAPOS-PER-BXADO         PIC S9(13)V99 COMP-3.
           05 SLIG13-VAPOS-PER-BXADO-NULL    PIC X(001).
           05 SLIG13-VTOT-PRROG              PIC S9(13)V99 COMP-3.
           05 SLIG13-VTOT-PRROG-NULL         PIC X(001).
           05 SLIG13-VTOT-ABTMT              PIC S9(13)V99 COMP-3.
           05 SLIG13-VTOT-ABTMT-NULL         PIC X(001).
           05 SLIG13-CNUM-PTCAO-HIST         PIC S9(03)    COMP-3.
           05 SLIG13-CCGC-CPF-SACDO-ST       PIC X(009).
