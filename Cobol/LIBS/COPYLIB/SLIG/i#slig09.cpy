      ***===========================================================***
      *** NOME INC                                     LENGTH=0096  ***
      *** I#SLIG09                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO IMAGEM DA TABELA                       ***
      ***            HISTORICO-CLI-SACDO-TITLO-CONSL                ***
      ***===========================================================***
      *
       01  REG-HIS-CLI-SACDO-TITL.
           05 SLIG09-CCGC-CPF-CLI        PIC 9(009) COMP-3.
           05 SLIG09-CINDCD-PSSOA-CLI    PIC X(001).
           05 SLIG09-CCGC-CPF-SACDO      PIC 9(009) COMP-3.
           05 SLIG09-CINDCD-PSSOA-SACDO  PIC X(001).
           05 SLIG09-CIDTFD-CART-LIQDZ   PIC 9(002).
           05 SLIG09-DPER-REFT           PIC X(010).
           05 SLIG09-VANTES-PER-NAO-PG   PIC S9(13)V99 COMP-3.
           05 SLIG09-VVCTO-PER-PG        PIC S9(13)V99 COMP-3.
           05 SLIG09-VVCTO-PER-NAO-PG    PIC S9(13)V99 COMP-3.
           05 SLIG09-VVCTO-PER-BXADO     PIC S9(13)V99 COMP-3.
           05 SLIG09-VVCTO-APOS-PER-PG   PIC S9(13)V99 COMP-3.
           05 SLIG09-VAPOS-PER-NAO-PG    PIC S9(13)V99 COMP-3.
           05 SLIG09-VAPOS-PER-BXADO     PIC S9(13)V99 COMP-3.
           05 SLIG09-VTOT-PRROG          PIC S9(13)V99 COMP-3.
           05 SLIG09-VTOT-ABTMT          PIC S9(13)V99 COMP-3.

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG09-CCGC-CPF-CLI-ST     PIC X(009).
           05 SLIG09-CCGC-CPF-SACDO-ST   PIC X(009).
