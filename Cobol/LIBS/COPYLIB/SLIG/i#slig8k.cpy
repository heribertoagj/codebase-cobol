      ***===========================================================***
      *** NOME INC                                     LENGTH=00101 ***
      *** I#SLIG8K                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO INTERMEDIARIO GENERICO TITULO          ***
      ***            INFORMACOES PARA CALCULO LIQUIDEZ PARTICIPACAO ***
      ***            PELA NEGOCIACAO DA COBRANCA                    ***
      ***===========================================================***
      *
       01  SLIG8K-REGISTRO.
      *-------- CHAVE COMPLETA
         03 SLIG8K-CHAVE-COMPLETA.
      *-------- BANCO CLIENTE
           05 SLIG8K-CBCO-PRODT-COBR             PIC S9(03)    COMP-3.
      *-------- CODIGO DO PRODUTO DA COBRANCA
           05 SLIG8K-CIDTFD-PRODT-COBR           PIC S9(02)    COMP-3.
      *-------- CODIGO DO NEGOCIACAO DA COBRANCA
           05 SLIG8K-CNEGOC-COBR                 PIC S9(18)    COMP-3.
      *
         03 SLIG8K-DETALHE.
      *-------- J CNPJ / F CPF CLIENTE
           05 SLIG8K-CCGC-CPF-CLI                PIC S9(09)    COMP-3.
      *-------- TIPO PESSOA CLIENTE J OU F
           05 SLIG8K-CINDCD-PSSOA-CLI            PIC X(001).
      *-------- J CNPJ / F CPF SACADO
           05 SLIG8K-CCGC-CPF-SACDO              PIC S9(009)   COMP-3.
      *-------- TIPO PESSOA SACADO J OU F
           05 SLIG8K-CINDCD-PSSOA-SACDO          PIC X(001).
      *-------- CARTEIRA 1 SIMPLES, 2 CAUCIONADA, 3 DESCONTO
           05 SLIG8K-IDTFD-CART-LIQDZ            PIC S9(002)   COMP-3.
      *
      *
      *-------- INFORMACOES DE VALORES PARA CALCULO
         03 SLIG8K-DADOS.
      *--------
           05 SLIG8K-VANTES-PER-NAO-PG           PIC S9(13)V99 COMP-3.
           05 SLIG8K-VVCTO-PER-PG                PIC S9(13)V99 COMP-3.
           05 SLIG8K-VVCTO-PER-NAO-PG            PIC S9(13)V99 COMP-3.
           05 SLIG8K-VVCTO-PER-BXADO             PIC S9(13)V99 COMP-3.
           05 SLIG8K-VAPOS-PER-PG                PIC S9(13)V99 COMP-3.
           05 SLIG8K-VAPOS-PER-NAO-PG            PIC S9(13)V99 COMP-3.
           05 SLIG8K-VAPOS-PER-BXADO             PIC S9(13)V99 COMP-3.
           05 SLIG8K-VTOT-PRROG                  PIC S9(13)V99 COMP-3.
           05 SLIG8K-VTOT-ABTMT                  PIC S9(13)V99 COMP-3.
      *--------
           05 SLIG8K-FILLER                      PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
         03 SLIG8K-DADOS-CCGC-ALPHA.
           05 SLIG8K-CCGC-CPF-CLI-ST             PIC X(009).
           05 SLIG8K-CCGC-CPF-SACDO-ST           PIC X(009).
