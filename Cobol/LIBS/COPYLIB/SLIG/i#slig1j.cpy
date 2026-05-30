
      ***===========================================================***
      *** NOME INC                                     LENGTH=00153 ***
      *** I#SLIG1J                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO IMAGEM DA TABELA                       ***
      ***            HISTORICO-CLIENTE-TITULO-CONSL                 ***
      ***           (COPIA DA I#SLIG10                              ***
      ***            INCLUIDO FILIAL E CONTROLE DO CNPJ)            ***
      ***===========================================================***
      *
       01  REG-HIST-CLI-TITLO.
           05 SLIG1J-CEMPR-INC                PIC S9(05)    COMP-3.
           05 SLIG1J-CDEPDC                   PIC S9(05)    COMP-3.
           05 SLIG1J-CCGC-CPF-CLI             PIC S9(09)    COMP-3.
           05 SLIG1J-CCGC-FILIAL-CLI          PIC S9(05)    COMP-3.
           05 SLIG1J-CIDTFD-CART-LIQDZ        PIC S9(02)    COMP-3.
           05 SLIG1J-DPER-REFT                PIC X(010).
           05 SLIG1J-CCGC-CONTROLE-CLI        PIC S9(02)    COMP-3.
           05 SLIG1J-CINDCD-PSSOA-CLI         PIC X(001).
           05 SLIG1J-ICOPLT-CLI               PIC X(040).
           05 SLIG1J-ICOPLT-CLI-NULL          PIC X(001).
           05 SLIG1J-VANTES-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG1J-VANTES-PER-NAO-PG-NULL   PIC X(001).
           05 SLIG1J-VVCTO-PER-PG             PIC S9(13)V99 COMP-3.
           05 SLIG1J-VVCTO-PER-PG-NULL        PIC X(001).
           05 SLIG1J-VVCTO-PER-NAO-PG         PIC S9(13)V99 COMP-3.
           05 SLIG1J-VVCTO-PER-NAO-PG-NULL    PIC X(001).
           05 SLIG1J-VVCTO-PER-BXADO          PIC S9(13)V99 COMP-3.
           05 SLIG1J-VVCTO-PER-BXADO-NULL     PIC X(001).
           05 SLIG1J-VVCTO-APOS-PER-PG        PIC S9(13)V99 COMP-3.
           05 SLIG1J-VVCTO-APOS-PER-PG-NULL   PIC X(001).
           05 SLIG1J-VAPOS-PER-NAO-PG         PIC S9(13)V99 COMP-3.
           05 SLIG1J-VAPOS-PER-NAO-PG-NULL    PIC X(001).
           05 SLIG1J-VAPOS-PER-BXADO          PIC S9(13)V99 COMP-3.
           05 SLIG1J-VAPOS-PER-BXADO-NULL     PIC X(001).
           05 SLIG1J-VTOT-PRROG               PIC S9(13)V99 COMP-3.
           05 SLIG1J-VTOT-PRROG-NULL          PIC X(001).
           05 SLIG1J-VTOT-ABTMT               PIC S9(13)V99 COMP-3.
           05 SLIG1J-VTOT-ABTMT-NULL          PIC X(001).
           05 SLIG1J-CNUM-PTCAO-HIST          PIC S9(03)    COMP-3.
           05 SLIG1J-CCGC-CPF-CLI-ST          PIC X(009).
           05 SLIG1J-CCGC-FILIAL-CLI-ST       PIC X(005).
           05 SLIG1J-CCGC-CONTROLE-CLI-ST     PIC S9(02)    COMP-3.
