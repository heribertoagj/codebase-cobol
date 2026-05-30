      ***===========================================================***
      *** NOME INC                                     LENGTH=0149  ***
      *** I#SLIG0N                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO IMAGEM DA TABELA                       ***
      ***            NEGOCIACAO-TITULO-CONSOLIDADO                  ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-NEG-TITLO-CONSL.
           05 SLIG0N-CEMPR-INC                PIC S9(03)    COMP-3.
           05 SLIG0N-CDEPDC                   PIC S9(05)    COMP-3.
           05 SLIG0N-CBCO-PRODT-COBR          PIC S9(03)    COMP-3.
           05 SLIG0N-CIDTFD-PRODT-COBR        PIC S9(02)    COMP-3.
           05 SLIG0N-CNEGOC-COBR              PIC S9(18)    COMP-3.
           05 SLIG0N-CIDTFD-CART-LIQDZ        PIC S9(02)    COMP-3.
           05 SLIG0N-CCGC-CPF-CLI             PIC S9(09)    COMP-3.
           05 SLIG0N-CINDCD-PSSOA-CLI         PIC X(001).
           05 SLIG0N-ICOPLT-CLI               PIC X(040).
           05 SLIG0N-ICOPLT-CLI-NULL          PIC X(001).
           05 SLIG0N-VANTES-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG0N-VANTES-PER-NAO-PG-NULL   PIC X(001).
           05 SLIG0N-VVCTO-PER-PG             PIC S9(13)V99 COMP-3.
           05 SLIG0N-VVCTO-PER-PG-NULL        PIC X(001).
           05 SLIG0N-VVCTO-PER-NAO-PG         PIC S9(13)V99 COMP-3.
           05 SLIG0N-VVCTO-PER-NAO-PG-NULL    PIC X(001).
           05 SLIG0N-VVCTO-PER-BXADO          PIC S9(13)V99 COMP-3.
           05 SLIG0N-VVCTO-PER-BXADO-NULL     PIC X(001).
           05 SLIG0N-VVCTO-APOS-PER-PG        PIC S9(13)V99 COMP-3.
           05 SLIG0N-VVCTO-APOS-PER-PG-NULL   PIC X(001).
           05 SLIG0N-VAPOS-PER-NAO-PG         PIC S9(13)V99 COMP-3.
           05 SLIG0N-VAPOS-PER-NAO-PG-NULL    PIC X(001).
           05 SLIG0N-VAPOS-PER-BXADO          PIC S9(13)V99 COMP-3.
           05 SLIG0N-VAPOS-PER-BXADO-NULL     PIC X(001).
           05 SLIG0N-VTOT-PRROG               PIC S9(13)V99 COMP-3.
           05 SLIG0N-VTOT-PRROG-NULL          PIC X(001).
           05 SLIG0N-VTOT-ABTMT               PIC S9(13)V99 COMP-3.
           05 SLIG0N-VTOT-ABTMT-NULL          PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG0N-CCGC-CPF-CLI-ST          PIC X(009).
