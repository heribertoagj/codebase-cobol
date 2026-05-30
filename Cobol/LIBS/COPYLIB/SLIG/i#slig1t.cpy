      ***===========================================================***
      *** NOME INC                                     LENGTH=00161 ***
      *** I#SLIG1T                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO IMAGEM DA TABELA                       ***
      ***            HISTORICO-NEGOCIACAO-TITULO-CONSL              ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-HIST-NEG-TITLO.

           05 SLIG1T-CEMPR-INC                PIC S9(03)    COMP-3.
           05 SLIG1T-CDEPDC                   PIC S9(05)    COMP-3.
           05 SLIG1T-CBCO-PRODT-COBR          PIC S9(03)    COMP-3.
           05 SLIG1T-CIDTFD-PRODT-COBR        PIC S9(02)    COMP-3.
           05 SLIG1T-CNEGOC-COBR              PIC S9(18)    COMP-3.
           05 SLIG1T-CIDTFD-CART-LIQDZ        PIC S9(02)    COMP-3.
           05 SLIG1T-DPER-REFT                PIC X(010).
           05 SLIG1T-CCGC-CPF-CLI             PIC S9(09)    COMP-3.
           05 SLIG1T-CINDCD-PSSOA-CLI         PIC X(001).
           05 SLIG1T-ICOPLT-CLI               PIC X(040).
           05 SLIG1T-ICOPLT-CLI-NULL          PIC X(001).
           05 SLIG1T-VANTES-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG1T-VANTES-PER-NAO-PG-NULL   PIC X(001).
           05 SLIG1T-VVCTO-PER-PG             PIC S9(13)V99 COMP-3.
           05 SLIG1T-VVCTO-PER-PG-NULL        PIC X(001).
           05 SLIG1T-VVCTO-PER-NAO-PG         PIC S9(13)V99 COMP-3.
           05 SLIG1T-VVCTO-PER-NAO-PG-NULL    PIC X(001).
           05 SLIG1T-VVCTO-PER-BXADO          PIC S9(13)V99 COMP-3.
           05 SLIG1T-VVCTO-PER-BXADO-NULL     PIC X(001).
           05 SLIG1T-VVCTO-APOS-PER-PG        PIC S9(13)V99 COMP-3.
           05 SLIG1T-VVCTO-APOS-PER-PG-NULL   PIC X(001).
           05 SLIG1T-VAPOS-PER-NAO-PG         PIC S9(13)V99 COMP-3.
           05 SLIG1T-VAPOS-PER-NAO-PG-NULL    PIC X(001).
           05 SLIG1T-VAPOS-PER-BXADO          PIC S9(13)V99 COMP-3.
           05 SLIG1T-VAPOS-PER-BXADO-NULL     PIC X(001).
           05 SLIG1T-VTOT-PRROG               PIC S9(13)V99 COMP-3.
           05 SLIG1T-VTOT-PRROG-NULL          PIC X(001).
           05 SLIG1T-VTOT-ABTMT               PIC S9(13)V99 COMP-3.
           05 SLIG1T-VTOT-ABTMT-NULL          PIC X(001).
           05 SLIG1T-CNUM-PTCAO-HIST          PIC S9(03)    COMP-3.
           05 SLIG1T-CCGC-CPF-CLI-ST          PIC X(009).
