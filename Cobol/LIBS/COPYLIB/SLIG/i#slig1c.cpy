      ***===========================================================***
      *** NOME INC                                     LENGTH=00118 ***
      *** I#SLIG1C                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO IMAGEM DA TABELA                       ***
      ***            HISTORICO-SACADO-TITULO-CONSL                  ***
      ***           (COPIA DA I#SLIG13                              ***
      ***            INCLUIDO FILIAL E CONTROLE DO CNPJ)            ***
      ***===========================================================***
      *
       01  REG-HIST-SACDO-TITLO.
           05 SLIG1C-CEMPR-INC                PIC S9(05)    COMP-3.
           05 SLIG1C-CDEPDC                   PIC S9(05)    COMP-3.
           05 SLIG1C-CCGC-CPF-SACDO           PIC S9(09)    COMP-3.
           05 SLIG1C-CCGC-FILIAL-SACDO        PIC S9(05)    COMP-3.
           05 SLIG1C-CIDTFD-CART-LIQDZ        PIC S9(02)    COMP-3.
           05 SLIG1C-DPER-REFT               PIC X(010).
           05 SLIG1C-CCGC-CONTROLE-SACDO      PIC S9(02)    COMP-3.
           05 SLIG1C-CINDCD-PSSOA-SACDO       PIC X(001).
           05 SLIG1C-CGRP-ECONM              PIC S9(09)    COMP-3.
           05 SLIG1C-CGRP-ECONM-NULL         PIC X(001).
           05 SLIG1C-VANTES-PER-NAO-PG       PIC S9(13)V99 COMP-3.
           05 SLIG1C-VANTES-PER-NAO-PG-NULL  PIC X(001).
           05 SLIG1C-VVCTO-PER-PG            PIC S9(13)V99 COMP-3.
           05 SLIG1C-VVCTO-PER-PG-NULL       PIC X(001).
           05 SLIG1C-VVCTO-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG1C-VVCTO-PER-NAO-PG-NULL   PIC X(001).
           05 SLIG1C-VVCTO-PER-BXADO         PIC S9(13)V99 COMP-3.
           05 SLIG1C-VVCTO-PER-BXADO-NULL    PIC X(001).
           05 SLIG1C-VVCTO-APOS-PER-PG       PIC S9(13)V99 COMP-3.
           05 SLIG1C-VVCTO-APOS-PER-PG-NULL  PIC X(001).
           05 SLIG1C-VAPOS-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG1C-VAPOS-PER-NAO-PG-NULL   PIC X(001).
           05 SLIG1C-VAPOS-PER-BXADO         PIC S9(13)V99 COMP-3.
           05 SLIG1C-VAPOS-PER-BXADO-NULL    PIC X(001).
           05 SLIG1C-VTOT-PRROG              PIC S9(13)V99 COMP-3.
           05 SLIG1C-VTOT-PRROG-NULL         PIC X(001).
           05 SLIG1C-VTOT-ABTMT              PIC S9(13)V99 COMP-3.
           05 SLIG1C-VTOT-ABTMT-NULL         PIC X(001).
           05 SLIG1C-CNUM-PTCAO-HIST         PIC S9(03)    COMP-3.
           05 SLIG1C-CCGC-CPF-SACDO-ST        PIC X(009).
           05 SLIG1C-CCGC-FILIAL-SACDO-ST     PIC X(005).
           05 SLIG1C-CCGC-CONTROLE-SACDO-ST   PIC S9(02)    COMP-3.
