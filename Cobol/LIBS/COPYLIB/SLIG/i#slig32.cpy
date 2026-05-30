      ***===========================================================***
      *** NOME INC                                     LENGTH=00300 ***
      *** I#SLIG32                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO VENDOR - CONCILIADO                    ***
      ***                                                           ***
      ***===========================================================***
      *
       01 REG-VENDOR-CONCILIADO.
          05 SLIG32-CGC-CPF-COMP           PIC 9(009) COMP-3.
          05 SLIG32-TIPO-PESSOA-COMP       PIC X(001).
          05 SLIG32-BCO-COMP               PIC 9(003) COMP-3.
          05 SLIG32-AGENCIA-COMP           PIC 9(005) COMP-3.
          05 SLIG32-CONTA-COMP             PIC 9(013) COMP-3.
          05 SLIG32-CGC-CPF-FORN           PIC 9(009) COMP-3.
          05 SLIG32-TIPO-PESSOA-FORN       PIC X(001).
          05 SLIG32-BCO-FORN               PIC 9(003) COMP-3.
          05 SLIG32-AGENCIA-FORN           PIC 9(005) COMP-3.
          05 SLIG32-CONTA-FORN             PIC 9(013) COMP-3.
          05 SLIG32-NOME-FORN              PIC X(040).
          05 SLIG32-NOME-COMP              PIC X(040).
          05 SLIG32-BCO-DIR-REGIONAL       PIC 9(003) COMP-3.
          05 SLIG32-DIR-REGIONAL           PIC 9(005) COMP-3.
          05 SLIG32-BCO-GERC-REGIONAL      PIC 9(003) COMP-3.
          05 SLIG32-GERC-REGIONAL          PIC 9(005) COMP-3.
          05 SLIG32-GRP-ECONOMICO-COMP     PIC 9(009) COMP-3.
          05 SLIG32-GRP-ECONOMICO-FORN     PIC 9(009) COMP-3.
          05 SLIG32-VCTO-ANTES-PER-NAO-PG  PIC 9(013)V99 COMP-3.
          05 SLIG32-VCTO-PER-PG            PIC 9(013)V99 COMP-3.
          05 SLIG32-VCTO-PER-NAO-PG        PIC 9(013)V99 COMP-3.
          05 SLIG32-VCTO-PER-BXADO         PIC 9(013)V99 COMP-3.
          05 SLIG32-VCTO-APOS-PER-PG       PIC 9(013)V99 COMP-3.
          05 SLIG32-VCTO-APOS-PER-NAO-PG   PIC 9(013)V99 COMP-3.
          05 SLIG32-VCTO-APOS-PER-BXADO    PIC 9(013)V99 COMP-3.
          05 SLIG32-PGTO-PER-FORN          PIC 9(013)V99 COMP-3.
          05 SLIG32-PGTO-PER-COMP          PIC 9(013)V99 COMP-3.
          05 SLIG32-PGTO-ANTCP-FORN        PIC 9(013)V99 COMP-3.
          05 SLIG32-PGTO-ANTCP-COMP        PIC 9(013)V99 COMP-3.

      * === CAMPOS DUPLICADOS PARA STRING ===
          05 SLIG32-CGC-CPF-COMP-ST        PIC X(009).
          05 SLIG32-CGC-CPF-FORN-ST        PIC X(009).
          05 FILLER                        PIC X(058).
