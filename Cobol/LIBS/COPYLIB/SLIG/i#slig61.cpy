      ***===========================================================***
      *** NOME INC                                     LENGTH=0150  ***
      *** I#SLIG61                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO REDUZIDO DE T.TULOS PENDENTES          ***
      *** OBS. LAYOUT BASEADO NO I#SLIG01 ACRESCIDO DE DI-          ***
      ***      RETORIA/GERENCIA REGIONAL E GRUPO ECONOMICO          ***
      ***===========================================================***
      *
       01  REG-SLIG61.
           05 SLIG61-BCO-CLIENTE        PIC 9(003) COMP-3.
           05 SLIG61-AGENCIA-CLIENTE    PIC 9(005) COMP-3.
           05 SLIG61-CONTA-CLIENTE      PIC 9(013) COMP-3.
           05 SLIG61-CTPO-CART          PIC 9(003) COMP-3.
           05 SLIG61-CCLI-COBR          PIC 9(009) COMP-3.
           05 SLIG61-CGC-CPF-CLIENTE    PIC 9(009) COMP-3.
           05 SLIG61-FILIAL-CLIENTE     PIC 9(005) COMP-3.
           05 SLIG61-CONTROLE-CLIENTE   PIC 9(002).
           05 SLIG61-BCO-SACADO         PIC 9(003) COMP-3.
           05 SLIG61-AGENCIA-SACADO     PIC 9(005) COMP-3.
           05 SLIG61-CONTA-SACADO       PIC 9(013) COMP-3.
           05 SLIG61-CGC-CPF-SACADO     PIC 9(009) COMP-3.
           05 SLIG61-FILIAL-SACADO      PIC 9(005) COMP-3.
           05 SLIG61-CONTROLE-SACADO    PIC 9(002).
           05 SLIG61-VL-PRINC-PROD      PIC 9(017) COMP-3.
           05 SLIG61-VL-PAGO-PROD       PIC 9(015) COMP-3.
           05 SLIG61-VL-DESC-ABAT-PROD  PIC 9(015) COMP-3.
           05 SLIG61-DT-VENCTO-PROD     PIC 9(009) COMP-3.
           05 SLIG61-DT-PAGTO-PROD      PIC 9(009) COMP-3.
           05 SLIG61-DT-ENTRADA-ORIGEM  PIC 9(009) COMP-3.
           05 SLIG61-DT-ENTRADA-SISTEMA PIC 9(009) COMP-3.
           05 SLIG61-DT-VENCTO-ANTERIOR PIC 9(009) COMP-3.
           05 SLIG61-CD-PRODUTO         PIC 9(002).
           05 SLIG61-CD-CNEGOC          PIC 9(011) COMP-3.
           05 SLIG61-CD-NOSNRO          PIC 9(011) COMP-3.
           05 SLIG61-CD-SEQUENCIA       PIC 9(003) COMP-3.
           05 SLIG61-CD-COD-NTAX        PIC 9(005) COMP-3.
           05 SLIG61-CD-OCO-REM-L       PIC 9(003) COMP-3.
           05 SLIG61-CD-MOT-REM-SIS     PIC 9(003) COMP-3.
           05 SLIG61-CD-ESPECIE         PIC 9(003) COMP-3.
           05 SLIG61-CD-ORIGEM          PIC 9(003) COMP-3.
           05 SLIG61-DT-INICIO-PER      PIC 9(009) COMP-3.
           05 SLIG61-DT-FINAL-PER       PIC 9(009) COMP-3.
           05 SLIG61-DIR-REG            PIC 9(005) COMP-3.
           05 SLIG61-GER-REG            PIC 9(005) COMP-3.
           05 SLIG61-GRP-ECONM          PIC 9(009) COMP-3.
           05 SLIG61-CGC-CPF-CLIENTE-ST PIC X(009).
           05 SLIG61-FILIAL-CLIENTE-ST  PIC X(005).
           05 SLIG61-CONTROLE-CLIENTE-ST PIC 9(002).
           05 SLIG61-CGC-CPF-SACADO-ST  PIC X(009).
           05 SLIG61-FILIAL-SACADO-ST   PIC X(005).
           05 SLIG61-CONTROLE-SACADO-ST PIC 9(002).
           05 SLIG61-FILLER             PIC X(009).
