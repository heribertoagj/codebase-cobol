      ***===========================================================***
      *** NOME INC                                     LENGTH=00038 ***
      *** I#SLIG9J                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: IMAGEM DA TABELA                               ***
      ***            LIQUIDEZ-COMPRADOR-CLIENTE-VENDOR              ***
      ***                                                           ***
BRQ250***           (COPIA DA I#SLIG90)                             ***
BRQ250***            INCLUIDO FILIAL E CONTROLE DO CNPJ             ***
BRQ250***            PARA SACADO E CLIENTE                          ***
      ***===========================================================***
      *
       01  SLIG9J-REGISTRO.
      *-------- J CNPJ / F CPF SACADO
           05 SLIG9J-CCGC-CPF-SACDO              PIC S9(09)    COMP-3.
      *-------- J CNPJ / FILIAL SACADO
           05 SLIG9J-FILIAL-SACDO                PIC S9(05)    COMP-3.
      *-------- J CNPJ / F CPF CONTROLE SACADO
           05 SLIG9J-CONTROLE-SACDO              PIC S9(02)    COMP-3.
      *-------- TIPO PESSOA SACADO J OU F
           05 SLIG9J-CINDCD-PSSOA-SACDO          PIC X(001).
      *-------- SEQUENCIA DA CLASSIFICACAO
           05 SLIG9J-CSEQ-CLASF-LIQDZ            PIC S9(15)    COMP-3.
      *-------- J CNPJ / F CPF CLIENTE
           05 SLIG9J-CCGC-CPF-CLI                PIC S9(09)    COMP-3.
      *-------- J CNPJ / FILIAL CLIENTE
           05 SLIG9J-FILIAL-CLI                  PIC S9(05)    COMP-3.
      *-------- J CNPJ / F CPF CONTROLE CLIENTE
           05 SLIG9J-CONTROLE-CLI                PIC S9(02)    COMP-3.
      *-------- TIPO PESSOA CLIENTE J OU F
           05 SLIG9J-CINDCD-PSSOA-CLI            PIC X(001).
      *-------- INDICE LIQUIDEZ COMPRADOR
           05 SLIG9J-ILC-SIMPLES                 PIC S9(03)V99 COMP-3.
           05 SLIG9J-ILC-SIMPLES-NULL            PIC  X(01).
      *-------- INDICE LIQUIDEZ FORNECEDOR
           05 SLIG9J-ILF-SIMPLES                 PIC S9(03)V99 COMP-3.
           05 SLIG9J-ILF-SIMPLES-NULL            PIC  X(01).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG9J-CCGC-CPF-SACDO-ST           PIC X(009).
           05 SLIG9J-FILIAL-SACDO-ST             PIC X(005).
           05 SLIG9J-CONTROLE-SACDO-ST           PIC S9(02)    COMP-3.
           05 SLIG9J-CCGC-CPF-CLI-ST             PIC X(009).
           05 SLIG9J-FILIAL-CLI-ST               PIC X(005).
           05 SLIG9J-CONTROLE-CLI-ST             PIC S9(02)    COMP-3.
