
      ***===========================================================***
      *** NOME INC                                     LENGTH=00148 ***
      *** I#SLIG10                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO IMAGEM DA TABELA                       ***
      ***            HISTORICO-CLIENTE-TITULO-CONSL                 ***
      ***===========================================================***
      *
       01  REG-HIST-CLI-TITLO.
           05 SLIG10-CEMPR-INC                PIC S9(05) COMP-3.
           05 SLIG10-CDEPDC                   PIC S9(05) COMP-3.
           05 SLIG10-CCGC-CPF-CLI             PIC S9(09) COMP-3.
           05 SLIG10-CINDCD-PSSOA-CLI         PIC X(001).
           05 SLIG10-CIDTFD-CART-LIQDZ        PIC S9(002) COMP-3.
           05 SLIG10-DPER-REFT                PIC X(010).
           05 SLIG10-ICOPLT-CLI               PIC X(040).
           05 SLIG10-ICOPLT-CLI-NULL          PIC X(001).
           05 SLIG10-VANTES-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG10-VANTES-PER-NAO-PG-NULL   PIC X(001).
           05 SLIG10-VVCTO-PER-PG             PIC S9(13)V99 COMP-3.
           05 SLIG10-VVCTO-PER-PG-NULL        PIC X(001).
           05 SLIG10-VVCTO-PER-NAO-PG         PIC S9(13)V99 COMP-3.
           05 SLIG10-VVCTO-PER-NAO-PG-NULL    PIC X(001).
           05 SLIG10-VVCTO-PER-BXADO          PIC S9(13)V99 COMP-3.
           05 SLIG10-VVCTO-PER-BXADO-NULL     PIC X(001).
           05 SLIG10-VVCTO-APOS-PER-PG        PIC S9(13)V99 COMP-3.
           05 SLIG10-VVCTO-APOS-PER-PG-NULL   PIC X(001).
           05 SLIG10-VAPOS-PER-NAO-PG         PIC S9(13)V99 COMP-3.
           05 SLIG10-VAPOS-PER-NAO-PG-NULL    PIC X(001).
           05 SLIG10-VAPOS-PER-BXADO          PIC S9(13)V99 COMP-3.
           05 SLIG10-VAPOS-PER-BXADO-NULL     PIC X(001).
           05 SLIG10-VTOT-PRROG               PIC S9(13)V99 COMP-3.
           05 SLIG10-VTOT-PRROG-NULL          PIC X(001).
           05 SLIG10-VTOT-ABTMT               PIC S9(13)V99 COMP-3.
           05 SLIG10-VTOT-ABTMT-NULL          PIC X(001).
           05 SLIG10-CNUM-PTCAO-HIST          PIC S9(03)    COMP-3.
           05 SLIG10-CCGC-CPF-CLI-ST          PIC X(009).
