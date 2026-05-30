      ***===========================================================***
      *** NOME INC                                     LENGTH=00151 ***
      *** I#SLIG8J                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO INTERMEDIARIO GENERICO CHEQUES         ***
      ***            INFORMACOES PARA CALCULO LIQUIDEZ PARTICIPACAO ***
      ***                                                           ***
      ***           (COPIA DA I#SLIG80                              ***
BRQ250***            INCLUIDO FILIAL E CONTROLE DO CNPJ             ***
BRQ250***            PARA CHAVE, CLIENTE E EMITENTE                 ***
      ***===========================================================***
      *
       01  SLIG8J-REGISTRO.
      *-------- CHAVE COMPLETA CHEQUES
         03 SLIG8J-CHAVE-COMPLETA.
      *-------- BANCO CLIENTE
           05 SLIG8J-CEMPR-INC                   PIC S9(005)   COMP-3.
      *-------- AGENCIA CLIENTE
           05 SLIG8J-CDEPDC                      PIC S9(005)   COMP-3.
      *-------- CNPJ / CPF CHAVE
           05 SLIG8J-CCGC-CPF-CHV                PIC S9(09)    COMP-3.
      *-------- FILIAL CNPJ / CPF CHAVE
           05 SLIG8J-CFLIAL-CPF-CHV              PIC S9(05)    COMP-3.
      *-------- CONTROLE CNPJ / CPF CHAVE
           05 SLIG8J-CCTRL-CPF-CHV               PIC S9(02)    COMP-3.
      *-------- TIPO PESSOA CHAVE J OU F
           05 SLIG8J-CINDCD-PSSOA-CHV            PIC X(001).
      *-------- GRUPO ECONOMICO CLIENTE
           05 SLIG8J-CGRP-ECONM                  PIC S9(09)    COMP-3.
      *
         03 SLIG8J-DETALHE.
      *-------- J CNPJ / F CPF CLIENTE
           05 SLIG8J-CCGC-CPF-CLI                PIC S9(09)    COMP-3.
      *-------- FILIAL CNPJ / CPF CLIENTE
           05 SLIG8J-CFLIAL-CPF-CLI              PIC S9(05)    COMP-3.
      *-------- CONTROLE CNPJ / CPF CLIENTE
           05 SLIG8J-CCTRL-CPF-CLI               PIC S9(02)    COMP-3.
      *-------- TIPO PESSOA CLIENTE J OU F
           05 SLIG8J-CINDCD-PSSOA-CLI            PIC X(001).
      *-------- J CNPJ / F CPF SACADO
           05 SLIG8J-CCGC-CPF-SACDO              PIC S9(09)    COMP-3.
      *-------- FILIAL CNPJ / CPF SACADO
           05 SLIG8J-CFLIAL-CPF-SACDO            PIC S9(05)    COMP-3.
      *-------- CONTROLE CNPJ / CPF SACADO
           05 SLIG8J-CCTRL-CPF-SACDO             PIC S9(02)    COMP-3.
      *-------- TIPO PESSOA SACADO J OU F
           05 SLIG8J-CINDCD-PSSOA-SACDO          PIC X(001).
      *-------- BANCO SACADO
           05 SLIG8J-CBCO-SACDO                  PIC S9(003)   COMP-3.
      *-------- AGENCIA SACADO
           05 SLIG8J-CAGEN-SACDO                 PIC S9(005)   COMP-3.
      *-------- CONTA SACADO
           05 SLIG8J-CCONTA-SACDO                PIC S9(013)   COMP-3.
      *-------- CARTEIRA 1 SIMPLES, 2 CAUCIONADA, 3 DESCONTO
           05 SLIG8J-IDTFD-CART-LIQDZ            PIC S9(002)   COMP-3.
      *
      *
      *-------- INFORMACOES DE VALORES PARA CALCULO
         03 SLIG8J-DADOS.
      *--------
           05 SLIG8J-VVCTO-PER-PG                PIC S9(13)V99 COMP-3.
           05 SLIG8J-VVCTO-PER-NAO-PG            PIC S9(13)V99 COMP-3.
           05 SLIG8J-VVCTO-PER-BXADO             PIC S9(13)V99 COMP-3.
           05 SLIG8J-VAPOS-PER-PG                PIC S9(13)V99 COMP-3.
           05 SLIG8J-VAPOS-PER-NAO-PG            PIC S9(13)V99 COMP-3.
           05 SLIG8J-VAPOS-PER-BXADO             PIC S9(13)V99 COMP-3.
           05 SLIG8J-VCHEQ-PRIM-DEVLC            PIC S9(13)V99 COMP-3.
           05 SLIG8J-VCHEQ-REAP                  PIC S9(13)V99 COMP-3.
           05 SLIG8J-VCHEQ-REAP-DEVOL            PIC S9(13)V99 COMP-3.
           05 SLIG8J-VCHEQ-REAP-PG               PIC S9(13)V99 COMP-3.
           05 SLIG8J-VCHEQ-DEVOL-ANTER           PIC S9(13)V99 COMP-3.
      *--------
           05 SLIG8J-FILLER                      PIC X(005).

      * === CAMPOS DUPLICADOS PARA STRING ===
         03 SLIG8J-DADOS-CCGC-ALPHA.
           05 SLIG8J-CCGC-CPF-CHV-ST             PIC X(009).
           05 SLIG8J-CFLIAL-CPF-CHV-ST           PIC X(005).
           05 SLIG8J-CCTRL-CPF-CHV-ST            PIC S9(02)    COMP-3.
           05 SLIG8J-CCGC-CPF-CLI-ST             PIC X(009).
           05 SLIG8J-CFLIAL-CPF-CLI-ST           PIC X(005).
           05 SLIG8J-CCTRL-CPF-CLI-ST            PIC S9(02)    COMP-3.
           05 SLIG8J-CCGC-CPF-SACDO-ST           PIC X(009).
           05 SLIG8J-CFLIAL-CPF-SACDO-ST         PIC X(005).
           05 SLIG8J-CCTRL-CPF-SACDO-ST          PIC S9(02)    COMP-3.
