      ***===========================================================***
      *** NOME INC                                     LENGTH=00129 ***
      *** I#SLIG8T                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO INTERMEDIARIO GENERICO CHEQUES         ***
      ***            INFORMACOES PARA CALCULO LIQUIDEZ PARTICIPACAO ***
      ***                                                           ***
      ***===========================================================***
      *
       01  SLIG8T-REGISTRO.
      *-------- CHAVE COMPLETA CHEQUES
         03 SLIG8T-CHAVE-COMPLETA.
      *-------- BANCO CLIENTE
           05 SLIG8T-CBCO                        PIC S9(03)    COMP-3.
      *-------- AGENCIA CLIENTE
           05 SLIG8T-CAG-BCRIA                   PIC S9(05)    COMP-3.
      *-------- CONTA CLIENTE
           05 SLIG8T-CCTA-BCRIA-CLI              PIC S9(13)    COMP-3.
      *
         03 SLIG8T-DETALHE.
      *-------- J CNPJ / F CPF CLIENTE
           05 SLIG8T-CCGC-CPF-CLI                PIC S9(09)    COMP-3.
      *-------- FILIAL CNPJ / CPF CLIENTE
           05 SLIG8T-CFLIAL-CPF-CLI              PIC S9(05)    COMP-3.
      *-------- CONTROLE CNPJ / CPF CLIENTE
           05 SLIG8T-CCTRL-CPF-CLI               PIC S9(02)    COMP-3.
      *-------- TIPO PESSOA CLIENTE J OU F
           05 SLIG8T-CINDCD-PSSOA-CLI            PIC X(001).
      *-------- J CNPJ / F CPF SACADO
           05 SLIG8T-CCGC-CPF-SACDO              PIC S9(09)    COMP-3.
      *-------- FILIAL CNPJ / CPF SACADO
           05 SLIG8T-CFLIAL-CPF-SACDO            PIC S9(05)    COMP-3.
      *-------- CONTROLE CNPJ / CPF SACADO
           05 SLIG8T-CCTRL-CPF-SACDO             PIC S9(02)    COMP-3.
      *-------- TIPO PESSOA SACADO J OU F
           05 SLIG8T-CINDCD-PSSOA-SACDO          PIC X(001).
      *-------- CARTEIRA 1 SIMPLES, 2 CAUCIONADA, 3 DESCONTO
           05 SLIG8T-IDTFD-CART-LIQDZ            PIC S9(002)   COMP-3.
      *
      *
      *-------- INFORMACOES DE VALORES PARA CALCULO
         03 SLIG8T-DADOS.
      *--------
           05 SLIG8T-VVCTO-PER-PG                PIC S9(13)V99 COMP-3.
           05 SLIG8T-VVCTO-PER-NAO-PG            PIC S9(13)V99 COMP-3.
           05 SLIG8T-VVCTO-PER-BXADO             PIC S9(13)V99 COMP-3.
           05 SLIG8T-VAPOS-PER-PG                PIC S9(13)V99 COMP-3.
           05 SLIG8T-VAPOS-PER-NAO-PG            PIC S9(13)V99 COMP-3.
           05 SLIG8T-VAPOS-PER-BXADO             PIC S9(13)V99 COMP-3.
           05 SLIG8T-VCHEQ-PRIM-DEVLC            PIC S9(13)V99 COMP-3.
           05 SLIG8T-VCHEQ-REAP                  PIC S9(13)V99 COMP-3.
           05 SLIG8T-VCHEQ-REAP-DEVOL            PIC S9(13)V99 COMP-3.
           05 SLIG8T-VCHEQ-REAP-PG               PIC S9(13)V99 COMP-3.
           05 SLIG8T-VCHEQ-DEVOL-ANTER           PIC S9(13)V99 COMP-3.
      *--------
           05 SLIG8T-FILLER                      PIC X(005).

      * === CAMPOS DUPLICADOS PARA STRING ===
         03 SLIG8T-DADOS-CCGC-ALPHA.
           05 SLIG8T-CCGC-CPF-CLI-ST             PIC X(009).
           05 SLIG8T-CFLIAL-CPF-CLI-ST           PIC X(005).
           05 SLIG8T-CCTRL-CPF-CLI-ST            PIC S9(02)    COMP-3.
           05 SLIG8T-CCGC-CPF-SACDO-ST           PIC X(009).
           05 SLIG8T-CFLIAL-CPF-SACDO-ST         PIC X(005).
           05 SLIG8T-CCTRL-CPF-SACDO-ST          PIC S9(02)    COMP-3.
