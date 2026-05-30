      ***===========================================================***
      *** NOME INC                                     LENGTH=0100  ***
      *** I#SLIGB1                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            LAY OUT INDICE DE LIQUIDEZ DE CHEQUES / PESSOA ***
      ***            PARA O SISTEMA RISCO.                          ***
      ***            SLIGB1-HD-DT-REFERENCIA   DDMMAAAA             ***
      ***===========================================================***
      *
       01 REG-CHEQUE-PESSOA.
             05 SLIGB1-HEADER.
                07 SLIGB1-HD-TIPO-REGISTRO       PIC  9(01).
                07 SLIGB1-HD-DT-REFERENCIA       PIC  9(08).
                07 SLIGB1-HD-FILLER              PIC  X(91).
             05 SLIGB1-DADOS-NIVEL-DETALHE REDEFINES
                SLIGB1-HEADER.
                07 SLIGB1-DET-TIPO-REGISTRO       PIC  9(01).
                07 SLIGB1-DET-NRO-SEQ             PIC  9(09).
                07 SLIGB1-DET-CPF-CNPJ-ST.
                   09 SLIGB1-DET-CPF-CNPJ         PIC  9(11).
                07 SLIGB1-DET-TIPO-PESSOA         PIC  9(01).
                07 SLIGB1-DET-FG-LIQDZ-SIMP       PIC  X(01).
                07 SLIGB1-DET-ID-LIQDZ-SIMP       PIC S9(03)V99.
                07 SLIGB1-DET-FG-LIQDZ-CAUC       PIC  X(01).
                07 SLIGB1-DET-ID-LIQDZ-CAUC       PIC S9(03)V99.
                07 SLIGB1-DET-FG-LIQDZ-DESC       PIC  X(01).
                07 SLIGB1-DET-ID-LIQDZ-DESC       PIC S9(03)V99.
                07 SLIGB1-DET-FG-LIQDZ-EMI-SIMP   PIC  X(01).
                07 SLIGB1-DET-ID-LIQDZ-EMI-SIMP   PIC S9(03)V99.
                07 SLIGB1-DET-FG-LIQDZ-EMI-CAUC   PIC  X(01).
                07 SLIGB1-DET-ID-LIQDZ-EMI-CAUC   PIC S9(03)V99.
                07 SLIGB1-DET-FG-LIQDZ-EMI-DESC   PIC  X(01).
                07 SLIGB1-DET-ID-LIQDZ-EMI-DESC   PIC S9(03)V99.
                07 SLIGB1-DET-FILLER              PIC  X(42).
             05 SLIGB1-TRAILLER REDEFINES SLIGB1-HEADER.
                07 SLIGB1-TR-TIP-REGISTRO       PIC  9(01).
                07 SLIGB1-TR-TOT-REGISTROS      PIC  9(09).
                07 SLIGB1-TR-FILLER             PIC  X(90).
