      ***===========================================================***
      *** NOME INC                                     LENGTH=00056 ***
      *** I#SLIG84                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO INDICES CALCULADOS TITULOS             ***
      ***                                                           ***
      ***===========================================================***
      *
       01  SLIG84-REGISTRO.
      *-------- CHAVE COMPLETA TITULOS (SEM CARTEIRA)
         03 SLIG84-CHAVE-COMPLETA.
      *-------- BANCO CLIENTE
           05 SLIG84-CEMPR-INC                   PIC S9(005)   COMP-3.
      *-------- AGENCIA CLIENTE
           05 SLIG84-CDEPDC                      PIC S9(005)   COMP-3.
      *-------- J CNPJ / F CPF CLIENTE
           05 SLIG84-CCGC-CPF-CLI                PIC S9(09)    COMP-3.
      *-------- TIPO PESSOA CLIENTE J OU F
           05 SLIG84-CINDCD-PSSOA-CLI            PIC X(001).
      *-------- GRUPO ECONOMICO CLIENTE
           05 SLIG84-CGRP-ECONM                  PIC S9(09)    COMP-3.
      *-------- J CNPJ / F CPF SACADO
           05 SLIG84-CCGC-CPF-SACDO              PIC S9(009)   COMP-3.
      *-------- TIPO PESSOA SACADO J OU F
           05 SLIG84-CINDCD-PSSOA-SACDO          PIC X(001).
      *
      *
      *-------- INDICES CALCULADOS
         03 SLIG84-DADOS.
      *--------
           05 SLIG84-MAIOR-IC                    PIC S9(03)V99 COMP-3.
           05 SLIG84-MENOR-IL                    PIC S9(03)V99 COMP-3.
      *-------- INDICE LIQUIDEZ
           05 SLIG84-IL-SIMPLES                  PIC S9(03)V99 COMP-3.
           05 SLIG84-IL-SIMPLES-NULL             PIC  X(01).
           05 SLIG84-IL-CAUCIONADA               PIC S9(03)V99 COMP-3.
           05 SLIG84-IL-CAUCIONADA-NULL          PIC  X(01).
           05 SLIG84-IL-DESCONTO                 PIC S9(03)V99 COMP-3.
           05 SLIG84-IL-DESCONTO-NULL            PIC  X(01).
      *-------- INDICE CONCENTRACAO PARTICIPACAO
           05 SLIG84-IC-SIMPLES                  PIC S9(03)V99 COMP-3.
           05 SLIG84-IC-SIMPLES-NULL             PIC  X(01).
           05 SLIG84-IC-CAUCIONADA               PIC S9(03)V99 COMP-3.
           05 SLIG84-IC-CAUCIONADA-NULL          PIC  X(01).
           05 SLIG84-IC-DESCONTO                 PIC S9(03)V99 COMP-3.
           05 SLIG84-IC-DESCONTO-NULL            PIC  X(01).
      *--------
           05 SLIG84-FILLER                      PIC  X(03).

      * === CAMPOS DUPLICADOS PARA STRING ===
         03 SLIG84-DADOS-CCGC-ALPHA.
           05 SLIG84-CCGC-CPF-CLI-ST             PIC X(009).
           05 SLIG84-CCGC-CPF-SACDO-ST           PIC X(009).
