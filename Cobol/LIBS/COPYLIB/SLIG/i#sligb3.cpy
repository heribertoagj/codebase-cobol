      ***===========================================================***
      *** NOME INC                                     LENGTH=0100  ***
      *** I#SLIGB3                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            LAY OUT INDICE DE LIQUIDEZ DE VENDOR  / PESSOA ***
      ***            PARA O SISTEMA RISCO.                          ***
      ***            SLIGB3-HD-DT-REFERENCIA      DDMMAAAA          ***
      ***===========================================================***
      *
       01 REG-CHEQUE-PESSOA.
             05 SLIGB3-HEADER.
                07 SLIGB3-HD-TIPO-REGISTRO       PIC  9(01).
                07 SLIGB3-HD-DT-REFERENCIA       PIC  9(08).
                07 SLIGB3-HD-FILLER              PIC  X(91).
             05 SLIGB3-DADOS-NIVEL-DETALHE REDEFINES
                SLIGB3-HEADER.
                07 SLIGB3-DET-TIPO-REGISTRO       PIC  9(01).
                07 SLIGB3-DET-TIPO-NRO-SEQ        PIC  9(09).
                07 SLIGB3-DET-CPF-CNPJ-ST.
                   09 SLIGB3-DET-CPF-CNPJ         PIC  9(11).
                07 SLIGB3-DET-TIPO-PESSOA         PIC  9(01).
                07 SLIGB3-DET-FG-VENDO-FOR        PIC  X(01).
                07 SLIGB3-DET-ID-VENDO-FOR        PIC S9(03)V99.
                07 SLIGB3-DET-FG-VENDO-COMP       PIC  X(01).
                07 SLIGB3-DET-ID-VENDO-COMP       PIC S9(03)V99.
                07 SLIGB3-DET-FILLER              PIC  X(66).
             05 SLIGB3-TRAILLER REDEFINES SLIGB3-HEADER.
                07 SLIGB3-TR-TIPO-REGISTRO       PIC  9(01).
                07 SLIGB3-TR-TOTAL-REGISTROS     PIC  9(09).
                07 SLIGB3-TR-FILLER              PIC  X(90).
