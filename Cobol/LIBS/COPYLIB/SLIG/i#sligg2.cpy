      ***===========================================================***
      *** NOME INC                                     LENGTH=0080  ***
      *** I#SLIGG2                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            AREA DE COMUNICACAO DO MODULO SLIG8083         ***
      ***                    BANCO   X   CLIENTE  -  CHEQUES        ***
      *** OBJETIVO : ENVIO DE DADOS PARA O SISTEMA GATI             ***
      ***            - INDICES CARTEIRA SIMPLES                     ***
      ***            - INDICES CARTEIRA CAUCIONADA                  ***
      ***            - INDICES CARTEIRA DE DESCONTOS                ***
      ***                                                           ***
      ***===========================================================***
      *** 04/10/25 - AJUSTE CPF/CNPJ ALFANUMERICO                   ***
      ***===========================================================***
      *
       01  REG-SLIGG2-DADOS.
         03  REG-SLIGG2-ENTR.
           05 SLIGG2-CGC-CPF       PIC S9(09) COMP-3.
           05 SLIGG2-TIPO-PESSOA   PIC X(001).
041025     05 SLIGG2-CGC-CPF-ST    PIC X(009).
         03  REG-SLIGG2-SAIDA.
           05 SLIGG2-PCT-CONC-S    PIC S9(03)V9(2) COMP-3.
           05 SLIGG2-VLR-TCONC-S   PIC S9(13)V9(2) COMP-3.
           05 SLIGG2-PCT-CONC-C    PIC S9(03)V9(2) COMP-3.
           05 SLIGG2-VLR-TCONC-C   PIC S9(13)V9(2) COMP-3.
           05 SLIGG2-PCT-CONC-D    PIC S9(03)V9(2) COMP-3.
           05 SLIGG2-VLR-TCONC-D   PIC S9(13)V9(2) COMP-3.
           05 SLIGG2-COD-RETORNO   PIC S9(05) COMP-3.
           05 SLIGG2-MSG-ERRO      PIC X(030).
           05 SLIGG2-FILLER        PIC X(005).
      *
      ***===========================================================***
      *
