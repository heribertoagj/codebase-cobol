      ***===========================================================***
      *** NOME INC                                     LENGTH=00300 ***
      *** I#SLIG3B                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO VENDOR - CONCILIADO                    ***
      ***                                                           ***
BRQ250***           (COPIA DA I#SLIG32)                             ***
BRQ250***            INCLUIDO FILIAL E CONTROLE DO CNPJ             ***
BRQ250***            PARA CLIENTE E SACADO                          ***
      ***===========================================================***
      *
       01 REG-VENDOR-CONCILIADO.
          05 SLIG3B-CGC-CPF-COMP           PIC 9(009) COMP-3.
          05 SLIG3B-FILIAL-COMP            PIC 9(005) COMP-3.
          05 SLIG3B-CONTROLE-COMP          PIC 9(002) COMP-3.
          05 SLIG3B-TIPO-PESSOA-COMP       PIC X(001).
          05 SLIG3B-BCO-COMP               PIC 9(003) COMP-3.
          05 SLIG3B-AGENCIA-COMP           PIC 9(005) COMP-3.
          05 SLIG3B-CONTA-COMP             PIC 9(013) COMP-3.
          05 SLIG3B-CGC-CPF-FORN           PIC 9(009) COMP-3.
          05 SLIG3B-FILIAL-FORN            PIC 9(005) COMP-3.
          05 SLIG3B-CONTROLE-FORN          PIC 9(002) COMP-3.
          05 SLIG3B-TIPO-PESSOA-FORN       PIC X(001).
          05 SLIG3B-BCO-FORN               PIC 9(003) COMP-3.
          05 SLIG3B-AGENCIA-FORN           PIC 9(005) COMP-3.
          05 SLIG3B-CONTA-FORN             PIC 9(013) COMP-3.
          05 SLIG3B-NOME-FORN              PIC X(040).
          05 SLIG3B-NOME-COMP              PIC X(040).
          05 SLIG3B-BCO-DIR-REGIONAL       PIC 9(003) COMP-3.
          05 SLIG3B-DIR-REGIONAL           PIC 9(005) COMP-3.
          05 SLIG3B-BCO-GERC-REGIONAL      PIC 9(003) COMP-3.
          05 SLIG3B-GERC-REGIONAL          PIC 9(005) COMP-3.
          05 SLIG3B-GRP-ECONOMICO-COMP     PIC 9(009) COMP-3.
          05 SLIG3B-GRP-ECONOMICO-FORN     PIC 9(009) COMP-3.
          05 SLIG3B-VCTO-ANTES-PER-NAO-PG  PIC 9(013)V99 COMP-3.
          05 SLIG3B-VCTO-PER-PG            PIC 9(013)V99 COMP-3.
          05 SLIG3B-VCTO-PER-NAO-PG        PIC 9(013)V99 COMP-3.
          05 SLIG3B-VCTO-PER-BXADO         PIC 9(013)V99 COMP-3.
          05 SLIG3B-VCTO-APOS-PER-PG       PIC 9(013)V99 COMP-3.
          05 SLIG3B-VCTO-APOS-PER-NAO-PG   PIC 9(013)V99 COMP-3.
          05 SLIG3B-VCTO-APOS-PER-BXADO    PIC 9(013)V99 COMP-3.
          05 SLIG3B-PGTO-PER-FORN          PIC 9(013)V99 COMP-3.
          05 SLIG3B-PGTO-PER-COMP          PIC 9(013)V99 COMP-3.
          05 SLIG3B-PGTO-ANTCP-FORN        PIC 9(013)V99 COMP-3.
          05 SLIG3B-PGTO-ANTCP-COMP        PIC 9(013)V99 COMP-3.

      * === CAMPOS DUPLICADOS PARA STRING ===
          05 SLIG3B-CGC-CPF-COMP-ST        PIC X(009).
          05 SLIG3B-FILIAL-COMP-ST         PIC X(005).
          05 SLIG3B-CONTROLE-COMP-ST       PIC 9(002) COMP-3.
          05 SLIG3B-CGC-CPF-FORN-ST        PIC X(009).
          05 SLIG3B-FILIAL-FORN-ST         PIC X(005).
          05 SLIG3B-CONTROLE-FORN-ST       PIC 9(002) COMP-3.
          05 FILLER                        PIC X(034).
