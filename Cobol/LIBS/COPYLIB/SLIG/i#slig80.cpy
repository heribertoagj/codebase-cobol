      ***===========================================================***
      *** NOME INC                                     LENGTH=00136 ***
      *** I#SLIG80                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO INTERMEDIARIO GENERICO CHEQUES         ***
      ***            INFORMACOES PARA CALCULO LIQUIDEZ PARTICIPACAO ***
      ***===========================================================***
      *
       01  SLIG80-REGISTRO.
      *-------- CHAVE COMPLETA CHEQUES
         03 SLIG80-CHAVE-COMPLETA.
      *-------- BANCO CLIENTE
           05 SLIG80-CEMPR-INC                   PIC S9(005)   COMP-3.
      *-------- AGENCIA CLIENTE
           05 SLIG80-CDEPDC                      PIC S9(005)   COMP-3.
      *-------- CNPJ / CPF CHAVE
           05 SLIG80-CCGC-CPF-CHV                PIC S9(09)    COMP-3.
      *-------- TIPO PESSOA CHAVE J OU F
           05 SLIG80-CINDCD-PSSOA-CHV            PIC X(001).
      *-------- GRUPO ECONOMICO CLIENTE
           05 SLIG80-CGRP-ECONM                  PIC S9(09)    COMP-3.
      *
         03 SLIG80-DETALHE.
      *-------- J CNPJ / F CPF CLIENTE
           05 SLIG80-CCGC-CPF-CLI                PIC S9(09)    COMP-3.
      *-------- TIPO PESSOA CLIENTE J OU F
           05 SLIG80-CINDCD-PSSOA-CLI            PIC X(001).
      *-------- J CNPJ / F CPF SACADO
           05 SLIG80-CCGC-CPF-SACDO              PIC S9(009)   COMP-3.
      *-------- TIPO PESSOA SACADO J OU F
           05 SLIG80-CINDCD-PSSOA-SACDO          PIC X(001).
      *-------- BANCO SACADO
           05 SLIG80-CBCO-SACDO                  PIC S9(003)   COMP-3.
      *-------- AGENCIA SACADO
           05 SLIG80-CAGEN-SACDO                 PIC S9(005)   COMP-3.
      *-------- CONTA SACADO
           05 SLIG80-CCONTA-SACDO                PIC S9(013)   COMP-3.
      *-------- CARTEIRA 1 SIMPLES, 2 CAUCIONADA, 3 DESCONTO
           05 SLIG80-IDTFD-CART-LIQDZ            PIC S9(002)   COMP-3.
      *
      *
      *-------- INFORMACOES DE VALORES PARA CALCULO
         03 SLIG80-DADOS.
      *--------
           05 SLIG80-VVCTO-PER-PG                PIC S9(13)V99 COMP-3.
           05 SLIG80-VVCTO-PER-NAO-PG            PIC S9(13)V99 COMP-3.
           05 SLIG80-VVCTO-PER-BXADO             PIC S9(13)V99 COMP-3.
           05 SLIG80-VAPOS-PER-PG                PIC S9(13)V99 COMP-3.
           05 SLIG80-VAPOS-PER-NAO-PG            PIC S9(13)V99 COMP-3.
           05 SLIG80-VAPOS-PER-BXADO             PIC S9(13)V99 COMP-3.
           05 SLIG80-VCHEQ-PRIM-DEVLC            PIC S9(13)V99 COMP-3.
           05 SLIG80-VCHEQ-REAP                  PIC S9(13)V99 COMP-3.
           05 SLIG80-VCHEQ-REAP-DEVOL            PIC S9(13)V99 COMP-3.
           05 SLIG80-VCHEQ-REAP-PG               PIC S9(13)V99 COMP-3.
           05 SLIG80-VCHEQ-DEVOL-ANTER           PIC S9(13)V99 COMP-3.
      *--------
           05 SLIG80-FILLER                      PIC X(005).

      * === CAMPOS DUPLICADOS PARA STRING ===
         03 SLIG80-DADOS-CCGC-ALPHA.
           05 SLIG80-CCGC-CPF-CHV-ST             PIC X(009).
           05 SLIG80-CCGC-CPF-CLI-ST             PIC X(009).
           05 SLIG80-CCGC-CPF-SACDO-ST           PIC X(009).
